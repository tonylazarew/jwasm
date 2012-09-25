/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  ELF output routines
*
****************************************************************************/

#include <ctype.h>
#include <time.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "mangle.h"
#include "fixup.h"
#include "segment.h"
#include "extern.h"
#include "elf.h"
#include "elfspec.h"
#include "myassert.h"

#if ELF_SUPPORT

/* v2.03: create weak externals for ALIAS symbols.
 * Since the syntax of the ALIAS directive requires
 * 2 names and, OTOH, the ELF implementation of weak
 * externals has no "default resolution", the usefullness
 * of this option is questionable. Additionally, there's
 * the "EXTERN <name> (<altname>)" syntax, which also allows
 * to define a weak external.
 */
#define ELFALIAS 0

/* start label is always public for COFF/ELF, no need to add it */
#define ADDSTARTLABEL 0

/* there's no STT_IMPORT type for ELF, it's OW specific */
#define OWELFIMPORT 0

/* use GNU extensions for LD ( 16bit and 8bit relocations ) */
#define GNURELOCS 1

#define MANGLE_BYTES 8 /* extra size required for name decoration */

#define IsWeak( x ) ( x.iscomm == FALSE && x.altname )

/* section attributes for ELF
 *         execute write  alloc  type
 *---------------------------------------
 * CODE      x              x    progbits
 * CONST                    x    progbits
 * DATA              x      x    progbits
 * BSS               x      x    nobits
 * STACK             x      x    progbits
 * others            x      x    progbits
 *
 * todo: translate section bits:
 * - INFO    -> SHT_NOTE  (added in v2.07)
 * - DISCARD ->
 * - SHARED  ->
 * - EXECUTE -> SHF_EXECINSTR
 * - READ    ->
 * - WRITE   -> SHF_WRITE
 */

//static uint_32 size_drectve;   /* size of .drectve section */
static uint_32 symindex;       /* entries in symbol table */
static uint_32 start_globals;  /* start index globals in symbol table */
static struct dsym *directives;

static uint_32 SizeLongNames;
static char *srcname; /* name of source module (name + extension) */

#if GNURELOCS
static bool extused;
#endif

struct localname {
    void *next;
    struct asym *sym;
};

struct intseg {
    char *name;
    uint type;
    uint size;
    uint offset;
    void *data;
};

static struct intseg internal_segs[] = {
    { ".shstrtab", SHT_STRTAB, 0, 0, NULL },
    { ".symtab",   SHT_SYMTAB, 0, 0, NULL },
    { ".strtab",   SHT_STRTAB, 0, 0, NULL },
};

#define NUM_INTSEGS ( sizeof(internal_segs) / sizeof(internal_segs[0]) )

#define SHSTRTAB_IDX 0
#define SYMTAB_IDX   1
#define STRTAB_IDX   2

struct conv_section {
    uint_8 len;
    uint_8 flags;
    const char *src;
    const char *dst;
};
enum cvs_flags {
    CSF_GRPCHK = 1
};

static const struct conv_section cst[] = {
    { 5, CSF_GRPCHK, "_TEXT", ".text"  },
    { 5, CSF_GRPCHK, "_DATA", ".data"  },
    { 5, CSF_GRPCHK, "CONST", ".rodata" }, /* v2.05: .rdata -> .rodata */
    { 4, 0,          "_BSS",  ".bss"   }
};


/* translate section names:
 * see cst[] above for details.
 */
static char *ElfConvertSectionName( const struct asym *sym )
/**********************************************************/
{
    int i;
    static char name[MAX_ID_LEN+1];

    for ( i = 0; i < sizeof( cst ) / sizeof( cst[0] ); i++ ) {
        if ( memcmp( sym->name, cst[i].src, cst[i].len ) == 0 ) {
            if ( sym->name[cst[i].len] == NULLC )
                return( (char *)cst[i].dst );
            else if ( ( cst[i].flags & CSF_GRPCHK )  && sym->name[cst[i].len] == '$' ) {
                strcpy( name, cst[i].dst );
                strcat( name, sym->name+cst[i].len );
                return( name );
            }
        }
    }
    return( sym->name );
}

static int get_num_reloc_sections( void )
/***************************************/
{
    struct dsym    *curr;
    int num = 0;

    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->FixupListHead )
            num++;
    }
    return( num );
}

#if AMD64_SUPPORT

/* write entries for ELF64 symbol table:
 typedef struct {
    uint_32  st_name;        //  +0 symbol name index into string table
    uint_8   st_info;        //  +4 symbol's type and binding attribs.
    uint_8   st_other;       //  +5 no meaning yet.
    uint_16  st_shndx;       //  +6 section index
    uint_64  st_value;       //  +8 symbol "value"
    uint_64  st_size;        // +16 symbol size
} Elf64_Sym;
 */

static uint_32 set_symtab_64( uint_32 entries, struct localname *localshead )
/***************************************************************************/
{
    uint_32   strsize = 1;
    uint_32   len;
    uint_8    stt;
    struct dsym   *curr;
    struct asym   *sym;
    struct localname *localscurr;
    void      *vp;
    Elf64_Sym *p64;
    char      buffer[MAX_ID_LEN + MANGLE_BYTES + 1];

    internal_segs[SYMTAB_IDX].size = entries * sizeof( Elf64_Sym );
    internal_segs[SYMTAB_IDX].data = LclAlloc( internal_segs[SYMTAB_IDX].size );
    memset( internal_segs[SYMTAB_IDX].data, 0, internal_segs[SYMTAB_IDX].size );

    p64 = (Elf64_Sym *)internal_segs[SYMTAB_IDX].data;

    p64++; /* skip NULL entry */

    /* 1. make file entry */
    p64->st_name = strsize;  /* symbol's name in string table */
    strsize += strlen(srcname) + 1;
    p64->st_value = 0;
    p64->st_size = 0;
    p64->st_info = ELF64_ST_INFO(STB_LOCAL, STT_FILE); /* symbol's type and binding info */
    p64->st_shndx = SHN_ABS; /* section index */
    p64++;

    /* 2. make section entries */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        //p64->st_name = ?;  /* name isn't set */
        p64->st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
        p64->st_shndx = GetSegIdx( curr->sym.segment );
        p64++;
    }

    /* 3. locals */

    for ( localscurr = localshead ; localscurr ; localscurr = localscurr->next ) {
        len = Mangle( localscurr->sym, buffer );
        p64->st_name = strsize;
        curr = (struct dsym *)localscurr->sym->segment;
        if ( curr && curr->e.seginfo->segtype != SEGTYPE_CODE )
            stt = STT_OBJECT;
        else
            stt = STT_FUNC;
        p64->st_info = ELF64_ST_INFO(STB_LOCAL, stt);
        p64->st_value = localscurr->sym->offset;
#if 1 /* v2.07: changed - to make MT_ABS obsolete */
        if ( curr )
            p64->st_shndx = GetSegIdx( &curr->sym );
        else
            p64->st_shndx = SHN_ABS;
#else
        if ( localscurr->sym->mem_type == MT_ABS )
            p64->st_shndx = SHN_ABS;
        else
            p64->st_shndx = GetSegIdx( &curr->sym );
#endif
        strsize += len + 1;
        DebugMsg(("set_symtab_64, LOCAL: symbol %s, value=%" I64X_SPEC "\n", buffer, p64->st_value));
        p64++;
    }

    /* 4. externals + communals ( + protos [since v2.01]) */

    for( curr = SymTables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* skip "weak" (=unused) externdefs */
        if ( curr->sym.iscomm == FALSE && curr->sym.weak == TRUE )
            continue;
        len = Mangle( &curr->sym, buffer );

        p64->st_name = strsize;

        /* for COMMUNALs, store their size in the Value field */
        if ( curr->sym.iscomm == TRUE ) {
            p64->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_COMMON);
            p64->st_value = curr->sym.total_size;
            p64->st_shndx = SHN_COMMON;
        } else {
#if OWELFIMPORT
            p64->st_info = ( IsWeak( curr->sym ) ? ELF64_ST_INFO(STB_WEAK, STT_IMPORT) : ELF64_ST_INFO(STB_GLOBAL, STT_IMPORT) );
#else
            /* todo: set STT_FUNC for prototypes??? */
            p64->st_info = ( IsWeak( curr->sym ) ? ELF64_ST_INFO(STB_WEAK, STT_NOTYPE) : ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE) );
#endif
            p64->st_value = curr->sym.offset; /* is always 0 */
            p64->st_shndx = SHN_UNDEF;
        }

        strsize += len + 1;
        DebugMsg(("set_symtab_64, EXTERNAL: symbol %s, info=%X, shndx=%X, value=%" I64X_SPEC "\n", buffer, p64->st_info, p64->st_shndx, p64->st_value));
        p64++;
    }

#if ELFALIAS
    /* 5. aliases */

    for( curr = SymTables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
        len = Mangle( &curr->sym, buffer );

        p64->st_name = strsize;

#if OWELFIMPORT
        p64->st_info = ELF64_ST_INFO(STB_WEAK, STT_IMPORT);
#else
        p64->st_info = ELF64_ST_INFO(STB_WEAK, STT_NOTYPE);
#endif
        p64->st_value = 0; /* is always 0 */
        p64->st_shndx = SHN_UNDEF;

        strsize += len + 1;
        DebugMsg(("set_symtab_64, ALIASES: symbol %s, value=%" I64X_SPEC "\n", buffer, p64->st_value));
        p64++;
    }
#endif

    /* 6. PUBLIC entries */
    vp = NULL;
    while ( sym = GetPublicData( &vp ) ) {
        len = Mangle( sym, buffer );

        curr = (struct dsym *)sym->segment;
        if ( curr && curr->e.seginfo->segtype != SEGTYPE_CODE )
            stt = STT_OBJECT;
        else
            stt = STT_FUNC;

        p64->st_name = strsize;
        p64->st_info = ELF64_ST_INFO(STB_GLOBAL, stt);
        p64->st_value = sym->offset;
#if 1 /* v2.07: changed - to make MT_ABS obsolete */
        if ( sym->state == SYM_INTERNAL )
            if ( curr )
                p64->st_shndx = GetSegIdx( &curr->sym );
            else
                p64->st_shndx = SHN_ABS;
        else
            p64->st_shndx = SHN_UNDEF;
#else
        if ( sym->mem_type == MT_ABS )
            p64->st_shndx = SHN_ABS;
        else if ( curr )
            p64->st_shndx = GetSegIdx( &curr->sym );
        else
            p64->st_shndx = SHN_UNDEF;
#endif
        strsize += len + 1;

        DebugMsg(("set_symtab_64, PUBLIC+LOCAL: symbol %s, info=%X, shndx=%X, value=%" I64X_SPEC "\n", buffer, p64->st_info, p64->st_shndx, p64->st_value));

        p64++;
    }
#if ADDSTARTLABEL
    if ( ModuleInfo.start_label ) {
        len = Mangle( ModuleInfo.start_label, buffer );
        p64->st_name = strsize;
        p64->st_info = ELF64_ST_INFO(STB_ENTRY, STT_FUNC);
        p64->st_value = ModuleInfo.start_label->offset;
        p64->st_shndx = GetSegIdx( ModuleInfo.start_label->segment );
        strsize += len + 1;
        DebugMsg(("set_symtab_64, ENTRY: symbol %s, value=%" I64X_SPEC "\n", buffer, p64->st_value));
        p64++;
    }
#endif
    return( strsize );
}
#endif

/* write entries for ELF32 symbol table */

static uint_32 set_symtab_32( uint_32 entries, struct localname *localshead )
/***************************************************************************/
{
    uint_32   strsize = 1;
    uint_32   len;
    uint_8    stt;
    struct dsym   *curr;
    struct asym   *sym;
    struct localname *localscurr;
    void      *vp;
    Elf32_Sym *p32;
    char      buffer[MAX_ID_LEN + MANGLE_BYTES + 1];

    internal_segs[SYMTAB_IDX].size = entries * sizeof( Elf32_Sym );
    internal_segs[SYMTAB_IDX].data = LclAlloc( internal_segs[SYMTAB_IDX].size );
    memset( internal_segs[SYMTAB_IDX].data, 0, internal_segs[SYMTAB_IDX].size );

    p32 = (Elf32_Sym *)internal_segs[SYMTAB_IDX].data;

    p32++; /* skip NULL entry */

    /* 1. make file entry */
    p32->st_name = strsize;  /* symbol's name in string table */
    strsize += strlen(srcname) + 1;
    p32->st_value = 0;
    p32->st_size = 0;
    p32->st_info = ELF32_ST_INFO(STB_LOCAL, STT_FILE); /* symbol's type and binding info */
    p32->st_shndx = SHN_ABS; /* section index */
    p32++;

    /* 2. make section entries */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        //p32->st_name = ?;  /* name isn't set */
        p32->st_info = ELF32_ST_INFO(STB_LOCAL, STT_SECTION);
        p32->st_shndx = GetSegIdx( curr->sym.segment );
        p32++;
    }

    /* 3. locals */

    for ( localscurr = localshead ; localscurr ; localscurr = localscurr->next ) {
        len = Mangle( localscurr->sym, buffer );
        p32->st_name = strsize;
        curr = (struct dsym *)localscurr->sym->segment;
        if ( curr && curr->e.seginfo->segtype != SEGTYPE_CODE )
            stt = STT_OBJECT;
        else
            stt = STT_FUNC;
        p32->st_info = ELF32_ST_INFO(STB_LOCAL, stt);
        p32->st_value = localscurr->sym->offset;
#if 1 /* v2.07: changed - to make MT_ABS obsolete */
        if ( curr )
            p32->st_shndx = GetSegIdx( &curr->sym );
        else
            p32->st_shndx = SHN_ABS;
#else
        if ( localscurr->sym->mem_type == MT_ABS )
            p32->st_shndx = SHN_ABS;
        else
            p32->st_shndx = GetSegIdx( &curr->sym );
#endif
        strsize += len + 1;
        DebugMsg(("set_symtab_32, LOCAL: symbol %s, value=%X\n", buffer, p32->st_value));
        p32++;
    }

    /* 4. externals + communals (+ protos [since v2.01]) */

    for( curr = SymTables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* skip "weak" (=unused) externdefs */
        if ( curr->sym.iscomm == FALSE && curr->sym.weak == TRUE )
            continue;
        len = Mangle( &curr->sym, buffer );

        p32->st_name = strsize;

        /* for COMMUNALs, store their size in the Value field */
        if ( curr->sym.iscomm == TRUE ) {
            p32->st_info = ELF32_ST_INFO(STB_GLOBAL, STT_COMMON);
            p32->st_value = curr->sym.total_size;
            p32->st_shndx = SHN_COMMON;
        } else {
#if OWELFIMPORT
            p32->st_info = ( IsWeak( curr->sym ) ? ELF32_ST_INFO(STB_WEAK, STT_IMPORT) : ELF32_ST_INFO(STB_GLOBAL, STT_IMPORT) );
#else
            /* todo: set STT_FUNC for prototypes/code labels??? */
            p32->st_info = ( IsWeak( curr->sym ) ? ELF32_ST_INFO(STB_WEAK, STT_NOTYPE) : ELF32_ST_INFO(STB_GLOBAL, STT_NOTYPE) );
#endif
            p32->st_value = curr->sym.offset; /* is always 0 */
            p32->st_shndx = SHN_UNDEF;
        }

        strsize += len + 1;
        DebugMsg(("set_symtab_32, EXTERNAL: symbol %s, info=%X, shndx=%X, value=%X\n", buffer, p32->st_info, p32->st_shndx, p32->st_value));
        p32++;
    }

#if ELFALIAS
    /* 5. aliases */
    for( curr = SymTables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
        len = Mangle( &curr->sym, buffer );

        p32->st_name = strsize;

#if OWELFIMPORT
        p32->st_info = ELF32_ST_INFO(STB_WEAK, STT_IMPORT);
#else
        p32->st_info = ELF32_ST_INFO(STB_WEAK, STT_NOTYPE);
#endif
        p32->st_value = 0; /* is always 0 */
        p32->st_shndx = SHN_UNDEF;

        strsize += len + 1;
        DebugMsg(("set_symtab_32, ALIAS: symbol %s, value=%X\n", buffer, p32->st_value));
        p32++;
    }
#endif

    /* 6. PUBLIC entries */
    vp = NULL;
    while ( sym = GetPublicData( &vp ) ) {
        len = Mangle( sym, buffer );

        curr = (struct dsym *)sym->segment;
        if ( curr && curr->e.seginfo->segtype != SEGTYPE_CODE )
            stt = STT_OBJECT;
        else
            stt = STT_FUNC;

        p32->st_name = strsize;
        p32->st_info = ELF32_ST_INFO(STB_GLOBAL, stt);
        p32->st_value = sym->offset;
#if 1 /* v2.07: changed - to make MT_ABS obsolete */
        if ( sym->state == SYM_INTERNAL )
            if ( curr )
                p32->st_shndx = GetSegIdx( &curr->sym );
            else
                p32->st_shndx = SHN_ABS;
        else
            p32->st_shndx = SHN_UNDEF;
#else
        if ( sym->mem_type == MT_ABS )
            p32->st_shndx = SHN_ABS;
        else if ( curr )
            p32->st_shndx = GetSegIdx( &curr->sym );
        else
            p32->st_shndx = SHN_UNDEF;
#endif
        strsize += len + 1;

        DebugMsg(("set_symtab_32, PUBLIC+LOCAL: symbol %s, value=%X\n", buffer, p32->st_value));

        p32++;
    }
#if ADDSTARTLABEL
    if ( ModuleInfo.start_label ) {
        len = Mangle( ModuleInfo.start_label, buffer );
        p32->st_name = strsize;
        p32->st_info = ELF32_ST_INFO(STB_ENTRY, STT_FUNC);
        p32->st_value = ModuleInfo.start_label->offset;
        p32->st_shndx = GetSegIdx( ModuleInfo.start_label->segment );
        strsize += len + 1;
        DebugMsg(("set_symtab_32, ENTRY: symbol %s, value=%X\n", buffer, p32->st_value));
        p32++;
    }
#endif
    return( strsize );
}

/* calculate size of .symtab + .strtab section.
 * set content of these sections
 */
static void set_symtab_values( void *hdr )
/****************************************/
{
    uint_32 strsize;
    uint_32 entries;
    struct dsym *curr;
    struct asym *sym;
    char *p2;
    void *vp;
    struct localname *localshead = NULL;
    struct localname *localstail = NULL;
    struct localname *localscurr;

    /* symbol table. there is
     - 1 NULL entry,
     - 1 entry for the module/file,
     - 1 entry for each section and
     - n entries for local symbols
     - m entries for global symbols
     */

    /* symbol table starts with 1 NULL entry + 1 file entry */
    symindex = 1 + 1;

    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next )
        curr->sym.ext_idx = symindex++;

    /* add local symbols to symbol table */

    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->num_relocs ) {
            struct fixup * fix = curr->e.seginfo->FixupListHead;
            for ( ; fix; fix = fix->nextrlc ) {
                /* if it's not EXTERNAL/PUBLIC, add symbol. */
                /* however, if it's an assembly time variable */
                /* use a raw section reference */
                if ( fix->sym->variable ) {
                    fix->sym = fix->segment;
                } else if ( ( fix->sym->state == SYM_INTERNAL ) &&
                    fix->sym->included == FALSE &&
                    fix->sym->public == FALSE ) {
                    fix->sym->included = TRUE;
                    localscurr = LclAlloc( sizeof( struct localname ) );
                    localscurr->next = NULL;
                    localscurr->sym = fix->sym;
                    if (localstail) {
                        localstail->next = localscurr;
                        localstail = localscurr;
                    } else {
                        localshead = localstail = localscurr;
                    }
                    fix->sym->ext_idx = symindex++;
                }
            }
        }
    }
    DebugMsg(("set_symtab_values: index after sections: %u\n", symindex));
    start_globals = symindex;

    /* count EXTERNs and used EXTERNDEFs (and PROTOs [since v2.01]) */
    for( curr = SymTables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        if ( curr->sym.iscomm == FALSE && curr->sym.weak == TRUE )
            continue;
        curr->sym.ext_idx = symindex++;
    }
    DebugMsg(("set_symtab_values: index after EXTERNALs: %u\n", symindex));

#if ELFALIAS
    /* count aliases */
    for( curr = SymTables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
        curr->sym.idx = symindex++;
    }
    DebugMsg(("set_symtab_values: index after ALIASES: %u\n", symindex));
#endif

    /* count publics */
    vp = NULL;
    while( sym = GetPublicData( &vp ) ) {
        sym->ext_idx = symindex++;
    }
    DebugMsg(("set_symtab_values: index after PUBLICs: %u\n", symindex));

    /* size of symbol table is defined */

    entries = symindex;

#if ADDSTARTLABEL
    if ( ModuleInfo.start_label )
        entries++;
#endif

#if AMD64_SUPPORT
    if ( ModuleInfo.header_format == HFORMAT_ELF64 )
        strsize = set_symtab_64( entries, localshead );
    else
#endif
        strsize = set_symtab_32( entries, localshead );

    /* generate the string table */
    DebugMsg(("set_symtab_values: creating string table, size=%X\n", strsize));

    internal_segs[STRTAB_IDX].size = strsize;
    internal_segs[STRTAB_IDX].data = LclAlloc(internal_segs[STRTAB_IDX].size);
    memset( internal_segs[STRTAB_IDX].data, 0, internal_segs[STRTAB_IDX].size );
    p2 = internal_segs[STRTAB_IDX].data;
    *p2++ = NULLC;

    strcpy( p2, srcname );
    p2 += strlen( p2 ) + 1;

    for ( localscurr = localshead ; localscurr ; localscurr = localscurr->next ) {
        p2 += Mangle( localscurr->sym, p2 ) + 1;
    }

    for( curr = SymTables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        if ( curr->sym.iscomm == FALSE && curr->sym.weak == TRUE )
            continue;
        p2 += Mangle( &curr->sym, p2 ) + 1;
    }

#if ELFALIAS
    for( curr = SymTables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
        p2 += Mangle( &curr->sym, p2 ) + 1;
    }
#endif

    vp = NULL;
    while ( sym = GetPublicData( &vp ) ) {
        p2 += Mangle( sym, p2 ) + 1;
    }
#if ADDSTARTLABEL
    if ( ModuleInfo.start_label ) {
        Mangle( ModuleInfo.start_label, p2 );
    }
#endif
    DebugMsg(("set_symtab_values: exit\n"));
    return;
}

/* set content + size of .shstrtab section
 * alloc .shstrtab
 */
static void set_shstrtab_values( void )
/*************************************/
{
    int         i;
    struct dsym *curr;
    char        *p;
    unsigned int size = 1;

    /* get program + reloc section sizes */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        /* v2.07: ALIAS name defined? */
        p = ( curr->e.seginfo->aliasname ? curr->e.seginfo->aliasname : ElfConvertSectionName( &curr->sym ) );
        size += strlen( p ) + 1;
        if ( curr->e.seginfo->FixupListHead )
            size += strlen( p ) +
#if AMD64_SUPPORT
                (( ModuleInfo.header_format == HFORMAT_ELF64 ) ? sizeof(".rela") : sizeof(".rel"));
#else
                sizeof(".rel");
#endif
    }
    /* get internal sections sizes */
    for ( i = 0; i < NUM_INTSEGS; i++ ) {
        size += strlen( internal_segs[i].name ) + 1;
    }

    internal_segs[SHSTRTAB_IDX].size = size;

    /* size is set, now alloc the section and fill it */

    internal_segs[SHSTRTAB_IDX].data = LclAlloc(size);
    p = (char *)internal_segs[SHSTRTAB_IDX].data;
    *p++ = NULLC;

    /* names of program sections */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        strcpy( p, curr->e.seginfo->aliasname ? curr->e.seginfo->aliasname : ElfConvertSectionName( &curr->sym ) );
        p += strlen( p ) + 1;
    }
    /* names of internal sections */
    for ( i = 0; i < NUM_INTSEGS; i++ ) {
        strcpy( p, internal_segs[i].name );
        p += strlen( p ) + 1;
    }
    /* names of "relocation" sections */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->FixupListHead ) {
#if AMD64_SUPPORT
            strcpy( p, (( ModuleInfo.header_format == HFORMAT_ELF64 ) ? ".rela": ".rel") );
#else
            strcpy( p, ".rel" );
#endif
            p += strlen( p );
            strcpy( p, curr->e.seginfo->aliasname ? curr->e.seginfo->aliasname : ElfConvertSectionName( &curr->sym ) );
            p += strlen( p ) + 1;
        }
    }
    myassert( size == (void *)p - internal_segs[SHSTRTAB_IDX].data );
    DebugMsg(("set_shstrtab_values: size=%X\n", size));
    return;
}


static unsigned int Get_Num_Relocs( struct dsym *curr )
/*****************************************************/
{
    unsigned relocs;
    struct fixup *fix;

    for ( relocs = 0, fix = curr->e.seginfo->FixupListHead; fix ; fix = fix->nextrlc, relocs++ );

    return( relocs );
}

static unsigned int Get_Alignment( struct dsym *curr)
/***************************************************/
{
    if ( curr->e.seginfo->alignment == MAX_SEGALIGNMENT )
        return( 0 );
    return( 1 << curr->e.seginfo->alignment );
}

#if AMD64_SUPPORT
/* write ELF64 section table */

static int elf_write_section_table64( struct module_info *modinfo, Elf64_Ehdr *hdr, uint offset )
/***********************************************************************************************/
{
    int         i;
    struct dsym *curr;
    char        *clname;
    uint_8      *p;
    //uint        offset;
    uint        entrysize;
    Elf64_Shdr  shdr64;

    DebugMsg(("elf_write_section_table64: enter\n"));

    //offset = sizeof(Elf64_Ehdr) + ehdr->e_shnum * ehdr->e_shentsize;
    offset = (offset + 0xF) & ~0xF;

    set_shstrtab_values();

    entrysize = sizeof( Elf64_Shdr );
    /* write the NULL entry */
    memset( &shdr64, 0, sizeof( shdr64) );
    if ( fwrite( &shdr64, 1, sizeof(shdr64), CurrFile[OBJ] ) != sizeof(shdr64) ) /* write the empty NULL entry */
        WriteError();

    p = (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
    p++;

    /* write the section headers defined in the module */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {

        memset( &shdr64, 0, sizeof(shdr64) );

        shdr64.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr64.sh_type = SHT_PROGBITS;
        if ( curr->e.seginfo->segtype == SEGTYPE_BSS ) {
            shdr64.sh_flags = SHF_WRITE | SHF_ALLOC;
            shdr64.sh_type = SHT_NOBITS;
        } else if ( curr->e.seginfo->segtype == SEGTYPE_CODE ) {
            shdr64.sh_flags = SHF_EXECINSTR | SHF_ALLOC;
        } else if ( curr->e.seginfo->readonly == TRUE ) {
            shdr64.sh_flags = SHF_ALLOC;
        } else if ( curr->e.seginfo->info == TRUE ) { /* v2.07:added */
            shdr64.sh_type = SHT_NOTE;
        } else if (( clname = GetLname( curr->e.seginfo->class_name_idx ) ) && strcmp( clname, "CONST" ) == 0 ) {
            shdr64.sh_flags = SHF_ALLOC; /* v2.07: added */
        } else {
            shdr64.sh_flags = SHF_WRITE | SHF_ALLOC;
        }
        shdr64.sh_addr = 0;
        if ( curr->e.seginfo->segtype != SEGTYPE_BSS ) {
            shdr64.sh_offset = offset; /* start of section in file */
            /* size of section in file */
            //shdr64.sh_size = curr->sym.max_offset;
        }
        /* v2.07: also set size for .bss sections */
        shdr64.sh_size = curr->sym.max_offset;
        shdr64.sh_link = 0;
        shdr64.sh_info = 0;
        shdr64.sh_addralign = Get_Alignment( curr );
        shdr64.sh_entsize = 0;

        if ( fwrite( &shdr64, 1, sizeof(shdr64), CurrFile[OBJ] ) != sizeof(shdr64) )
            WriteError();
        curr->e.seginfo->fileoffset = offset;
        offset += shdr64.sh_size;

        curr->e.seginfo->num_relocs = Get_Num_Relocs(curr);

        offset = (offset + 0xF) & ~0xF;

        DebugMsg(("elf_write_section_table64(%s): numrelocs=%u\n", curr->sym.name, curr->e.seginfo->num_relocs));

    }

    set_symtab_values( hdr );

    /* write headers of internal sections */
    for ( i = 0; i < NUM_INTSEGS; i++ ) {

        shdr64.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr64.sh_type = internal_segs[i].type;
        shdr64.sh_flags = 0;
        shdr64.sh_offset = offset; /* start of section in file */
        internal_segs[i].offset = offset;
        shdr64.sh_size = internal_segs[i].size;
        if ( internal_segs[i].type == SHT_SYMTAB ) {
            shdr64.sh_link = 1 + modinfo->g.num_segs + STRTAB_IDX;
            shdr64.sh_info = start_globals;
            shdr64.sh_addralign = 4;
            shdr64.sh_entsize = sizeof( Elf64_Sym );
        } else {
            shdr64.sh_link = 0;
            shdr64.sh_info = 0;
            shdr64.sh_addralign = 1;
            shdr64.sh_entsize = 0;
        }
        if ( fwrite( &shdr64, 1, sizeof( shdr64 ), CurrFile[OBJ] ) != sizeof( shdr64 ) )
            WriteError();
        offset += shdr64.sh_size;

        offset = (offset + 0xF) & ~0xF;
        DebugMsg(("elf_write_section_table64(%s): ofs=%X size=%X\n", internal_segs[i].name, shdr64.sh_offset, shdr64.sh_size));
    }

    /* write headers of reloc sections */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->FixupListHead == NULL )
            continue;

        memset( &shdr64, 0, sizeof(shdr64) );

        shdr64.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr64.sh_type = SHT_RELA; /* v2.05: changed REL to RELA */
        shdr64.sh_flags = 0;
        shdr64.sh_addr = 0;
        shdr64.sh_offset = offset; /* start of section in file */
        /* save the file offset in the slot reserved for ELF relocs */
        curr->e.seginfo->reloc_offset = offset;
        /* size of section in file */
        shdr64.sh_size = curr->e.seginfo->num_relocs * sizeof( Elf64_Rela );
        shdr64.sh_link = 1 + modinfo->g.num_segs + SYMTAB_IDX;
        /* set info to the src section index */
        shdr64.sh_info = GetSegIdx( curr->sym.segment );
        shdr64.sh_addralign = 4;
        shdr64.sh_entsize = sizeof( Elf64_Rela );

        if ( fwrite( &shdr64, 1, sizeof( shdr64 ), CurrFile[OBJ] ) != sizeof( shdr64 ) )
            WriteError();

        offset += shdr64.sh_size;

        offset = (offset + 0xF) & ~0xF;
        DebugMsg(("elf_write_section_table64(%s): relocs, ofs=%X size=%X\n", curr->sym.name, shdr64.sh_offset, shdr64.sh_size));
    }
    DebugMsg(("elf_write_section_table64: exit\n"));
    return( NOT_ERROR );
}
#endif

/* write ELF32 section table */

static int elf_write_section_table32( struct module_info *modinfo, Elf32_Ehdr *hdr, uint offset )
/***********************************************************************************************/
{
    int         i;
    struct dsym *curr;
    char        *clname;
    uint_8      *p;
    //uint        offset;
    uint        entrysize;
    Elf32_Shdr  shdr32;

    DebugMsg(("elf_write_section_table32: enter\n"));

    //offset = sizeof(Elf64_Ehdr) + ehdr->e_shnum * ehdr->e_shentsize;
    offset = (offset + 0xF) & ~0xF;

    set_shstrtab_values();

    entrysize = sizeof( Elf32_Shdr );
    /* write the NULL entry */
    memset( &shdr32, 0, sizeof( shdr32) );
    if ( fwrite( &shdr32, 1, sizeof(shdr32), CurrFile[OBJ] ) != sizeof(shdr32) ) /* write the empty NULL entry */
        WriteError();
    p = (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
    p++;

    /* write the section headers defined in the module */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {

        memset( &shdr32, 0, sizeof(shdr32) );

        shdr32.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr32.sh_type = SHT_PROGBITS;
        if ( curr->e.seginfo->segtype == SEGTYPE_BSS ) {
            shdr32.sh_flags = SHF_WRITE | SHF_ALLOC;
            shdr32.sh_type = SHT_NOBITS;
        } else if ( curr->e.seginfo->segtype == SEGTYPE_CODE ) {
            shdr32.sh_flags = SHF_EXECINSTR | SHF_ALLOC;
        } else if ( curr->e.seginfo->readonly == TRUE ) {
            shdr32.sh_flags = SHF_ALLOC;
        } else if ( curr->e.seginfo->info == TRUE ) { /* v2.07:added */
            shdr32.sh_type = SHT_NOTE;
        } else if (( clname = GetLname( curr->e.seginfo->class_name_idx ) ) && strcmp( clname, "CONST" ) == 0 ) {
            shdr32.sh_flags = SHF_ALLOC; /* v2.07: added */
        } else {
            shdr32.sh_flags = SHF_WRITE | SHF_ALLOC;
        }

#if 0
        /* todo: translate values in field <characteristics> to
         * elf section flags.
         */
        if ( curr->e.seginfo->characteristics == ??? ) {
        }
#endif

        shdr32.sh_addr = 0;
        if ( curr->e.seginfo->segtype != SEGTYPE_BSS ) {
            shdr32.sh_offset = offset; /* start of section in file */
            /* size of section in file */
            //shdr32.sh_size = curr->sym.max_offset;
        }
        /* v2.07: also set size for .bss sections */
        shdr32.sh_size = curr->sym.max_offset;
        shdr32.sh_link = 0;
        shdr32.sh_info = 0;
        shdr32.sh_addralign = Get_Alignment( curr );
        shdr32.sh_entsize = 0;

        if ( fwrite( &shdr32, 1, sizeof(shdr32), CurrFile[OBJ] ) != sizeof(shdr32) )
            WriteError();
        /* save the file offset in the segment item */
        curr->e.seginfo->fileoffset = offset;
        offset += shdr32.sh_size;
        curr->e.seginfo->num_relocs = Get_Num_Relocs(curr);

        offset = (offset + 0xF) & ~0xF;

        DebugMsg(("elf_write_section_table32(%s): ofs=%X size=%X numrelocs=%u\n", curr->sym.name, shdr32.sh_offset, shdr32.sh_size, curr->e.seginfo->num_relocs));

    }

    set_symtab_values( hdr );

    /* write headers of internal sections */
    for ( i = 0; i < NUM_INTSEGS; i++ ) {
        shdr32.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr32.sh_type = internal_segs[i].type;
        shdr32.sh_flags = 0;
        shdr32.sh_offset = offset; /* start of section in file */
        internal_segs[i].offset = offset;
        shdr32.sh_size = internal_segs[i].size;
        if ( internal_segs[i].type == SHT_SYMTAB ) {
            shdr32.sh_link = 1 + modinfo->g.num_segs + STRTAB_IDX;
            shdr32.sh_info = start_globals;
            shdr32.sh_addralign = 4;
            shdr32.sh_entsize = sizeof( Elf32_Sym );
        } else {
            shdr32.sh_link = 0;
            shdr32.sh_info = 0;
            shdr32.sh_addralign = 1;
            shdr32.sh_entsize = 0;
        }
        if ( fwrite( &shdr32, 1, sizeof( shdr32 ), CurrFile[OBJ] ) != sizeof( shdr32 ) )
            WriteError();
        offset += shdr32.sh_size;

        offset = (offset + 0xF) & ~0xF;
        DebugMsg(("elf_write_section_table32(%s): ofs=%X size=%X\n", internal_segs[i].name, shdr32.sh_offset, shdr32.sh_size));
    }

    /* write headers of reloc sections */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->FixupListHead == NULL )
            continue;

        memset( &shdr32, 0, sizeof( shdr32 ) );

        shdr32.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr32.sh_type = SHT_REL;
        shdr32.sh_flags = 0;
        shdr32.sh_addr = 0;
        shdr32.sh_offset = offset; /* start of section in file */
        /* save the file offset in the slot reserved for ELF relocs */
        curr->e.seginfo->reloc_offset = offset;
        /* size of section in file */
        shdr32.sh_size = curr->e.seginfo->num_relocs * sizeof( Elf32_Rel );
        shdr32.sh_link = 1 + modinfo->g.num_segs + SYMTAB_IDX;
        /* set info to the src section index */
        shdr32.sh_info = GetSegIdx( curr->sym.segment );
        shdr32.sh_addralign = 4;
        shdr32.sh_entsize = sizeof( Elf32_Rel );

        if ( fwrite( &shdr32, 1, sizeof( shdr32 ), CurrFile[OBJ] ) != sizeof( shdr32 ) )
            WriteError();

        offset += shdr32.sh_size;
        offset = (offset + 0xF) & ~0xF;
        DebugMsg(("elf_write_section_table32(%s): relocs, ofs=%X size=%X\n", curr->sym.name, shdr32.sh_offset, shdr32.sh_size));

    }
    DebugMsg(("elf_write_section_table32: exit\n"));
    return( NOT_ERROR );
}

/* write ELF header + section table
 */
ret_code elf_write_header( struct module_info *modinfo )
/******************************************************/
{
    //struct dsym  *dir;
    Elf32_Ehdr ehdr32;
#if AMD64_SUPPORT
    Elf64_Ehdr ehdr64;
#endif

    DebugMsg(("elf_write_header: enter\n"));

    SizeLongNames = sizeof(uint_32);

    srcname = CurrFName[ASM];
    srcname += strlen(srcname);
    while (srcname > CurrFName[ASM] &&
           *(srcname-1) != '/' &&
           *(srcname-1) != '\\') srcname--;

    directives = NULL;
#if GNURELOCS
    extused = FALSE;
#endif

    switch ( modinfo->header_format ) {
#if AMD64_SUPPORT
    case HFORMAT_ELF64:
        memset(&ehdr64, 0, sizeof( ehdr64 ) );
        memcpy(&ehdr64.e_ident, ELF_SIGNATURE, ELF_SIGNATURE_LEN);
        ehdr64.e_ident[EI_CLASS] = ELFCLASS64;
        ehdr64.e_ident[EI_DATA] = ELFDATA2LSB;
        ehdr64.e_ident[EI_VERSION] = EV_CURRENT;
        ehdr64.e_ident[EI_OSABI] = modinfo->osabi;
        /* v2.07: set abiversion to 0 */
        //ehdr64.e_ident[EI_ABIVERSION] = EV_CURRENT;
        ehdr64.e_ident[EI_ABIVERSION] = 0;
        ehdr64.e_type = ET_REL; /* file type */
        ehdr64.e_machine = EM_X86_64;
        ehdr64.e_version = EV_CURRENT;
        ehdr64.e_entry = 0; /* no entry for relocatable objects */
        ehdr64.e_phoff = 0; /* no progheaders for relocatable objects */
        ehdr64.e_shoff = sizeof( ehdr64 );
        ehdr64.e_flags = 0;
        ehdr64.e_ehsize = sizeof( ehdr64 );
        ehdr64.e_phentsize = 0; /* no progheaders for relocatable objects */
        ehdr64.e_phnum = 0;
        ehdr64.e_shentsize = sizeof( Elf64_Shdr );
        /* 4 additional segment entries:
         - NULL entry
         - .shstrtab
         - .symtab
         - .strtab
         - .rel<xxx> entries
         */
        ehdr64.e_shnum = 1 + modinfo->g.num_segs + 3 + get_num_reloc_sections();
        ehdr64.e_shstrndx = 1 + modinfo->g.num_segs + SHSTRTAB_IDX;
        fseek( CurrFile[OBJ], 0, SEEK_SET );
        if ( fwrite( &ehdr64, 1, sizeof( ehdr64 ), CurrFile[OBJ] ) != sizeof( ehdr64 ) )
            WriteError();
        elf_write_section_table64( modinfo, &ehdr64,
                                  sizeof( Elf64_Ehdr ) + ehdr64.e_shnum * ehdr64.e_shentsize );
        break;
#endif
    default:
        memset(&ehdr32, 0, sizeof( ehdr32 ) );
        memcpy(&ehdr32.e_ident, ELF_SIGNATURE, ELF_SIGNATURE_LEN);
        ehdr32.e_ident[EI_CLASS] = ELFCLASS32;
        ehdr32.e_ident[EI_DATA] = ELFDATA2LSB;
        ehdr32.e_ident[EI_VERSION] = EV_CURRENT;
        ehdr32.e_ident[EI_OSABI] = modinfo->osabi;
        /* v2.07: set abiversion to 0 */
        //ehdr32.e_ident[EI_ABIVERSION] = EV_CURRENT;
        ehdr32.e_ident[EI_ABIVERSION] = 0;
        ehdr32.e_type = ET_REL; /* file type */
        ehdr32.e_machine = EM_386;
        ehdr32.e_version = EV_CURRENT;
        ehdr32.e_entry = 0; /* no entry for relocatable objects */
        ehdr32.e_phoff = 0; /* no progheaders for relocatable objects */
        ehdr32.e_shoff = sizeof( ehdr32 );
        ehdr32.e_flags = 0;
        ehdr32.e_ehsize = sizeof( ehdr32 );
        ehdr32.e_phentsize = 0; /* no progheaders for relocatable objects */
        ehdr32.e_phnum = 0;
        ehdr32.e_shentsize = sizeof( Elf32_Shdr );
        /* 4 additional segment entries:
         - NULL entry
         - .shstrtab
         - .symtab
         - .strtab
         - .rel<xxx> entries
         */
        ehdr32.e_shnum = 1 + modinfo->g.num_segs + 3 + get_num_reloc_sections();
        ehdr32.e_shstrndx = 1 + modinfo->g.num_segs + SHSTRTAB_IDX;
        fseek( CurrFile[OBJ], 0, SEEK_SET );
        if ( fwrite( &ehdr32, 1, sizeof( ehdr32 ), CurrFile[OBJ] ) != sizeof( ehdr32 ) )
            WriteError();
        elf_write_section_table32( modinfo, &ehdr32,
                                  sizeof( Elf32_Ehdr ) + ehdr32.e_shnum * ehdr32.e_shentsize );
    };
    DebugMsg(("elf_write_header: exit\n"));
    return( NOT_ERROR );
}

#if AMD64_SUPPORT

/* write one section's relocations (64-bit) */

static void write_relocs64( struct dsym *curr )
/*********************************************/
{
    uint_8 elftype;
    struct fixup *fixup;
    Elf64_Rela reloc64; /* v2.05: changed to Rela */

    DebugMsg(("write_relocs64: enter\n"));
    for ( fixup = curr->e.seginfo->FixupListHead; fixup; fixup = fixup->nextrlc ) {
        uint symidx = fixup->sym->ext_idx;
        reloc64.r_offset = fixup->location;
        /* v2.07: addend wasn't handled correctly.
         * Also note the type cast for fixup.offset -
         * r_addend has type int_64, while fixup.offset has type uint_32!
         */
        //reloc64.r_addend = fixup->offset;
        /* the following line depends on what's done in store_fixup().
         * if the inline addend is set to 0 there, the fixup->offset
         * must be used in the calculation ( it's 32-bit only!!! ).
         */
        //reloc64.r_addend = (int_32)fixup->offset - fixup->addbytes;
        /*
         * if the inline addend is not touched in store_fixup(),
         * we just have to use the addbytes field.
         */
        reloc64.r_addend = - fixup->addbytes;
        DebugMsg(("write_relocs64(): reloc loc=%X type=%u idx=%u sym=%s ofs=%X addbyt=%u\n",
                  fixup->location, fixup->type, fixup->sym->ext_idx, fixup->sym->name, fixup->offset, fixup->addbytes ));
        switch ( fixup->type ) {
        case FIX_RELOFF32:
#if 0  /* v2.07: activate if the section's index is to be used as symtab ref */
            if ( fixup->sym->segment != &curr->sym ) {
                //printf("PC-relative fixup to another section: %s\n", fixup->sym->name );
                reloc64.r_addend += fixup->sym->offset;
                symidx = fixup->sym->segment->ext_idx;
            }
#endif
            elftype = R_X86_64_PC32;
            break;
        case FIX_OFF64:        elftype = R_X86_64_64;          break;
        //case FIX_???:        elftype = R_X86_64_GOT32;       break;
        //case FIX_???:        elftype = R_X86_64_PLT32;       break;
        //case FIX_???:        elftype = R_X86_64_COPY;        break;
        //case FIX_???:        elftype = R_X86_64_GLOB_DAT;    break;
        //case FIX_???:        elftype = R_X86_64_JMP_SLOT;    break;
        case FIX_OFF32_IMGREL: elftype = R_X86_64_RELATIVE;    break;
        //case FIX_???:        elftype = R_X86_64_GOTPCREL;    break;
        case FIX_OFF32:        elftype = R_X86_64_32;          break;
        //case FIX_???:        elftype = R_X86_64_32S;         break;
        case FIX_OFF16:        elftype = R_X86_64_16;          break;
        case FIX_RELOFF16:     elftype = R_X86_64_PC16;        break;
        case FIX_OFF8:         elftype = R_X86_64_8;           break;
        case FIX_RELOFF8:      elftype = R_X86_64_PC8;         break;
        //case FIX_???:        elftype = R_X86_64_DPTMOD64;    break;
        //case FIX_???:        elftype = R_X86_64_DPTOFF64;    break;
        //case FIX_???:        elftype = R_X86_64_TPOFF64;     break;
        //case FIX_???:        elftype = R_X86_64_TLSGD;       break;
        //case FIX_???:        elftype = R_X86_64_TLSLD;       break;
        //case FIX_???:        elftype = R_X86_64_DPTOFF32;    break;
        //case FIX_???:        elftype = R_X86_64_GOTTPOFF;    break;
        //case FIX_???:        elftype = R_X86_64_TPOFF32;     break;
        //case FIX_???:        elftype = R_X86_64_PC64;        break;
        //case FIX_???:        elftype = R_X86_64_GOTOFF64;    break;
        //case FIX_???:        elftype = R_X86_64_GOTPC32;     break;
        //case FIX_???:        elftype = R_X86_64_SIZE32;      break;
        //case FIX_???:        elftype = R_X86_64_SIZE64;      break;
        default:
            DebugMsg(("write_relocs64(): unhandled reloc loc=%X type=%u idx=%u sym=%s\n",
                      fixup->location, fixup->type, fixup->sym->ext_idx, fixup->sym->name));
            elftype = R_X86_64_NONE;
            if ( fixup->type < FIX_LAST ) {
                EmitErr( INVALID_FIXUP_TYPE, ModuleInfo.fmtopt->formatname, fixup->type, curr->sym.name, fixup->location );
            } else
                EmitErr( UNKNOWN_FIXUP_TYPE, fixup->type, curr->sym.name, fixup->location );
        }
        /* the low 8 bits of info are type */
        /* the high 24 bits are symbol table index */
        reloc64.r_info = ELF64_R_INFO( symidx, elftype );
        if ( fwrite( &reloc64, 1, sizeof( reloc64 ), CurrFile[OBJ] ) != sizeof(reloc64) )
            WriteError();
    }
    DebugMsg(("write_relocs64: exit\n"));
    return;
}
#endif

/* write one section's relocations (32-bit) */

static void write_relocs32( struct dsym *curr )
/*********************************************/
{
    uint_8 elftype;
    struct fixup *fixup;
    Elf32_Rel reloc32;

    DebugMsg(("write_relocs32: enter\n"));
    for ( fixup = curr->e.seginfo->FixupListHead; fixup; fixup = fixup->nextrlc ) {
        reloc32.r_offset = fixup->location;
        switch ( fixup->type ) {
        case FIX_OFF32:         elftype = R_386_32;             break;
        case FIX_RELOFF32:      elftype = R_386_PC32;           break;
        //case FIX_???:         elftype = R_386_GOT32;          break;
        //case FIX_???:         elftype = R_386_PLT32;          break;
        //case FIX_???:         elftype = R_386_COPY;           break;
        //case FIX_???:         elftype = R_386_GLOB_DAT;       break;
        //case FIX_???:         elftype = R_386_JMP_SLOT;       break;
        case FIX_OFF32_IMGREL:  elftype = R_386_RELATIVE;       break;
        //case FIX_???:         elftype = R_386_GOTOFF;         break;
        //case FIX_???:         elftype = R_386_GOTPC;          break;
#if GNURELOCS
        case FIX_OFF16:    extused = TRUE; elftype = R_386_16;   break;
        case FIX_RELOFF16: extused = TRUE; elftype = R_386_PC16; break;
        case FIX_OFF8:     extused = TRUE; elftype = R_386_8;    break;
        case FIX_RELOFF8:  extused = TRUE; elftype = R_386_PC8;  break;
#endif
        default:
            DebugMsg(("write_relocs32(): unhandled reloc loc=%X type=%u idx=%u sym=%s\n",
                      fixup->location, fixup->type, fixup->sym->ext_idx, fixup->sym->name));
            elftype = R_386_NONE;
            if ( fixup->type < FIX_LAST ) {
                EmitErr( INVALID_FIXUP_TYPE, ModuleInfo.fmtopt->formatname, fixup->type, curr->sym.name, fixup->location );
            } else
                EmitErr( UNKNOWN_FIXUP_TYPE, fixup->type, curr->sym.name, fixup->location );
        }
        /* the low 8 bits of info are type */
        /* the high 24 bits are symbol table index */
        reloc32.r_info = ELF32_R_INFO( fixup->sym->ext_idx, elftype );
        if ( fwrite( &reloc32, 1, sizeof(reloc32), CurrFile[OBJ] ) != sizeof(reloc32) )
            WriteError();
    }
    DebugMsg(("write_relocs32: exit\n"));
    return;
}

/* write section contents and fixups
 * this is done after the last step only!
 */
ret_code elf_write_data( struct module_info *modinfo )
/****************************************************/
{
    struct dsym *curr;
    //int seg_index;
    //uint_32 offset = 0;
    uint_32     size;
    int         i;

    DebugMsg(("elf_write_data: enter\n"));

    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        size = curr->sym.max_offset - curr->e.seginfo->start_loc;
        DebugMsg(("elf_write_data(%s): program data at ofs=%X, size=%X\n", curr->sym.name, curr->e.seginfo->fileoffset, size ));
        if ( curr->e.seginfo->segtype != SEGTYPE_BSS && curr->sym.max_offset != 0 ) {
            fseek( CurrFile[OBJ], curr->e.seginfo->fileoffset + curr->e.seginfo->start_loc, SEEK_SET );
            if ( curr->e.seginfo->CodeBuffer ) {
                if ( fwrite( curr->e.seginfo->CodeBuffer, 1, size, CurrFile[OBJ] ) != size )
                    WriteError();
            }
        }
    }

    /* write internal sections */
    for ( i = 0; i < NUM_INTSEGS; i++ ) {
        if ( internal_segs[i].data ) {
            DebugMsg(("elf_write_data(%s): internal at ofs=%X, size=%X\n", internal_segs[i].name, internal_segs[i].offset, internal_segs[i].size));
            fseek( CurrFile[OBJ], internal_segs[i].offset, SEEK_SET );
            if ( fwrite( internal_segs[i].data, 1, internal_segs[i].size, CurrFile[OBJ] ) != internal_segs[i].size )
                WriteError();
        }
    }

    /* write reloc sections content */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->num_relocs ) {
            DebugMsg(("elf_write_data(%s): relocs at ofs=%X, size=%X\n", curr->sym.name, curr->e.seginfo->reloc_offset, curr->e.seginfo->num_relocs * sizeof(Elf32_Rel)));
            fseek( CurrFile[OBJ], curr->e.seginfo->reloc_offset, SEEK_SET );
#if AMD64_SUPPORT
            if ( modinfo->header_format == HFORMAT_ELF64 )
                write_relocs64( curr );
            else
#endif
                write_relocs32( curr );
        }
    }
#if GNURELOCS
    if ( extused ) {
        EmitWarn( 2, ELF_GNU_EXTENSIONS_USED );
    }
#endif

    DebugMsg(("elf_write_data: exit\n"));

    return( NOT_ERROR );
}

/* format-specific init.
 * called once per module.
 */

void elf_init( struct module_info *ModuleInfo )
/*********************************************/
{
    ModuleInfo->osabi = ELFOSABI_LINUX;
    return;
}

#endif
