/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  STRUCT, UNION, RECORD and TYPEDEF directives.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "segment.h"
#include "proc.h"
#include "input.h"
#include "tokenize.h"
#include "types.h"
#include "expreval.h"
#include "labels.h"
#include "listing.h"
#include "fastpass.h"
#include "myassert.h"

/* v2.04: changed to 0 */
//#define ANYNAME 1 /* fixme: this probably should be changed to 0 */
#define ANYNAME 0

struct dsym *CurrStruct;
static struct dsym *redef_struct;
/* text constants for 'Non-benign <x> redefinition' error msg */
static const char szStructure[] = "structure";
static const char szRecord[] = "record";

void TypesInit( void )
/********************/
{
    CurrStruct   = NULL;
    redef_struct = NULL;
}

/* create a SYM_TYPE symbol.
 * <sym> must be NULL or of state SYM_UNDEFINED.
 * <name> and <global> are only used if <sym> is NULL.
 * <name> might be an empty string.
 */

struct asym *CreateTypeSymbol( struct asym *sym, const char *name, bool global )
/******************************************************************************/
{
    struct struct_info *si;

    if ( sym )
        sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
    else
        sym = ( global ? SymCreate( name ) : SymAlloc( name ) );

    if ( sym ) {
        sym->state = SYM_TYPE;
        ((struct dsym *)sym)->e.structinfo = si = LclAlloc( sizeof( struct struct_info ) );
        si->head = NULL;
        si->tail = NULL;
        sym->typekind = TYPE_NONE;
        si->alignment = 0;
        si->flags = 0;
    }
    return( sym );
}

/* search a name in a struct's fieldlist */

struct asym *SearchNameInStruct( const struct asym *tstruct, const char *name, uint_32 *poffset, int level )
/**********************************************************************************************************/
{
    int len = strlen( name );
    struct field_item *fl = ((struct dsym *)tstruct)->e.structinfo->head;
    struct asym *sym = NULL;

    //if (ModuleInfo.oldstructs == TRUE) {
    //    return( SymSearch( name ) );
    //}
    if ( level >= MAX_STRUCT_NESTING ) {
        EmitError( NESTING_LEVEL_TOO_DEEP );
        return( NULL );
    }
    level++;
    for ( ; fl; fl = fl->next ) {
        /* recursion: if member has no name, check if it is a structure
         and scan this structure's fieldlist then */
        if ( *( fl->sym->name ) == 0 ) {
            /* there are 2 cases: an anonymous inline struct ... */
            if ( fl->sym->state == SYM_TYPE ) {
                if ( sym = SearchNameInStruct( fl->sym, name, poffset, level ) ) {
                    *poffset += fl->sym->offset;
                    break;
                }
            /* or an anonymous structured field */
            } else if ( fl->sym->mem_type == MT_TYPE ) {
                if ( sym = SearchNameInStruct( fl->sym->type, name, poffset, level ) ) {
                    *poffset += fl->sym->offset;
                    break;
                }
            }
        } else if ( len == fl->sym->name_size && SymCmpFunc( name, fl->sym->name, len ) == 0 ) {
            DebugMsg(("SearchNameInStruct: '%s' found in struct %s\n", name, tstruct->name ));
            sym = fl->sym;
            break;
        }
    }
    return( sym );
}

/* check if a struct has changed */

static bool AreStructsEqual( const struct dsym *newstr, const struct dsym *oldstr )
/*********************************************************************************/
{
    struct field_item *fold = oldstr->e.structinfo->head;
    struct field_item *fnew = newstr->e.structinfo->head;

    DebugMsg(("AreStructsEqual(%s) enter\n", oldstr->sym.name ));

    /* kind of structs must be identical */
    if ( oldstr->sym.typekind != newstr->sym.typekind )
        return( FALSE );

    for ( ; fold; fold = fold->next, fnew = fnew->next ) {
        if ( !fnew ) {
            DebugMsg(("AreStructsEqual: fields don't match\n"));
            return( FALSE );
        }
        /* for global member names, don't check the name if it's "" */
        if ( ModuleInfo.oldstructs && *fnew->sym->name == NULLC )
            ;
        else if ( 0 != strcmp( fold->sym->name, fnew->sym->name ) ) {
            DebugMsg(("AreStructsEqual: type name of field changed\n"));
            return( FALSE );
        }
        if ( fold->sym->offset != fnew->sym->offset ) {
            DebugMsg(("AreStructsEqual: offset of field %s changed: %u - %u\n", fold->sym->name, fold->sym->offset, fnew->sym->offset));
            return( FALSE );
        }
        if ( fold->sym->total_size != fnew->sym->total_size ) {
            DebugMsg(("AreStructsEqual: total_size of field changed\n"));
            return( FALSE );
        }
    }
    if ( fnew )
        return( FALSE );
    return( TRUE );
}

/* handle STRUCT, STRUC, UNION directives
 * i = index of directive token
 */
ret_code StructDirective( int i, struct asm_tok tokenarray[] )
/************************************************************/
{
    char *name;
    unsigned alignment;
    uint_32 offset;
    int cmd = tokenarray[i].tokval;
    //unsigned int size;
    struct asym *sym;
    struct dsym *dir;

    DebugMsg1(("StructDirective(%s) enter, i=%u, CurrStruct=%s\n", tokenarray[i].string_ptr, i, CurrStruct ? CurrStruct->sym.name : "NULL" ));

    /* top level structs/unions must have an identifier at pos 0.
     * for embedded structs, the directive must be at pos 0,
     * an identifier is optional then.
     */
    if (( CurrStruct == NULL && i != 1 ) ||
        ( CurrStruct != NULL && i != 0 ) ) {
        DebugMsg(("StructDirective(%s): error: either currstruct or i must be 0\n", tokenarray[i].string_ptr ));
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    alignment = ( 1 << ModuleInfo.fieldalign );

    i++; /* go past STRUCT/UNION */

    if ( i == 1 ) { /* embedded struct? */
        /* scan for the optional name */
#if ANYNAME
        /* the name might be a reserved word!
         * Masm won't allow those.
         */
        if ( tokenarray[i].token != T_FINAL &&
            is_valid_id_first_char(*(tokenarray[i].string_ptr) ) ) {
#else
        if ( tokenarray[i].token == T_ID ) {
#endif
            name = tokenarray[i].string_ptr;
            i++;
        } else {
            name = "";
        }
    } else {
        name = tokenarray[0].string_ptr;
    }

    /* get an optional alignment argument: 1,2,4,8,16 or 32 */
    if ( CurrStruct == NULL && tokenarray[i].token != T_FINAL ) {
        unsigned int power;
        struct expr opndx;
        /* get the optional alignment parameter */
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) != ERROR ) {
            if (opndx.kind == EXPR_EMPTY ) {
                ;
            } else if (opndx.kind != EXPR_CONST ) {
                EmitError( CONSTANT_EXPECTED );
            } else if( opndx.value > MAX_STRUCT_ALIGN ) {
                EmitError( STRUCT_ALIGN_TOO_HIGH );
            } else {
                for( power = 1; power < opndx.value; power <<= 1 );
                if( power != opndx.value ) {
                    EmitError( POWER_OF_2 );
                } else
                    alignment = opndx.value;
            }
            DebugMsg1(("StructDirective(%s) alignment=%u\n", name, alignment));
        }
        /* there might also be the NONUNIQUE parameter */
        if ( tokenarray[i].token == T_COMMA ) {
            i++;
            if ( tokenarray[i].token == T_ID &&
                (_stricmp( tokenarray[i].string_ptr, "NONUNIQUE" ) == 0 ) ) {
                /* currently NONUNIQUE is ignored */
                _strupr( tokenarray[i].string_ptr );
                EmitWarn( 2, TOKEN_IGNORED, tokenarray[i].string_ptr );
                i++;
            }
        }
    }
    if ( tokenarray[i].token != T_FINAL ) {
        DebugMsg(("StructDirective(%s): error: unexpected token %u >%s<\n", tokenarray[i].token, tokenarray[i].tokpos ));
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
        return( ERROR );
    }

    /* does struct have a name? */
    if ( *name ) {
        if ( CurrStruct == NULL ) {
            /* the "top-level" struct is part of the global namespace */
            sym = SymSearch( name );
            DebugMsg1(("StructDirective: SymSearch (%s)=%X (curr struct=%X)\n", name, sym, CurrStruct ));
        } else {
            sym = SearchNameInStruct( (struct asym *)CurrStruct, name, &offset, 0 );
            DebugMsg1(("StructDirective(%s): SearchNameInStruc()=%X\n", name, sym));
        }
    } else {
        sym = NULL;   /* anonymous struct */
    }

    if ( ModuleInfo.list ) {
        if ( CurrStruct )
            LstWrite( LSTTYPE_STRUCT, CurrStruct->sym.total_size, NULL );
        else
            LstWrite( LSTTYPE_STRUCT, 0, NULL );
    }

    /* if pass is > 1, update struct stack + CurrStruct.offset and exit */
    if ( Parse_Pass > PASS_1 ) {
        /* v2.04 changed. the previous implementation was insecure.
         * See also change in data.c, behind AddFieldToStruct().
         */
        if ( CurrStruct ) {
            sym = CurrStruct->e.structinfo->tail->sym->type;
            /**/myassert( sym );
            CurrStruct->e.structinfo->tail = CurrStruct->e.structinfo->tail->next;
        }
        /**/myassert( sym );
        dir = (struct dsym *)sym;
        dir->e.structinfo->tail = dir->e.structinfo->head;
        sym->offset = 0;
        sym->isdefined = TRUE;
        ((struct dsym *)sym)->next = CurrStruct;
        CurrStruct = (struct dsym *)sym;
        return( NOT_ERROR );
    }

    if( sym == NULL ) {
        /* is it a global STRUCT? */
        if ( CurrStruct == NULL )
            dir = (struct dsym *)CreateTypeSymbol( NULL, name, TRUE );
        else {
            /* a nested structure is split in an anonymous STRUCT type
             * and a struct field with/without name
             */
            //struct field_item *f;
            dir = (struct dsym *)CreateTypeSymbol( NULL, name, FALSE );
            /* v2: don't call AddFieldToStruct() here. First the
             * structure must be read in ( because of alignment issues
             */
            // sym = AddFieldToStruct( name_loc, -1, MT_TYPE, dir, 0 );

            alignment = CurrStruct->e.structinfo->alignment;
        }
    } else {
        /* the symbol exists already */
        dir = (struct dsym *)sym;
        if( sym->state == SYM_UNDEFINED ) {
            CreateTypeSymbol( sym, NULL, CurrStruct != NULL );
        } else if( sym->state == SYM_TYPE && CurrStruct == NULL ) {
            /* was symbol forward referenced? */
            if ( dir->sym.typekind != TYPE_NONE ) {
                /* structure redefinition! */
                redef_struct = dir;
                dir = (struct dsym *)CreateTypeSymbol( NULL, name, FALSE );
            }
        } else {
            EmitErr( SYMBOL_REDEFINITION, sym->name );
            return( ERROR );
        }
    }

    dir->e.structinfo->alignment = alignment;
    dir->sym.offset = 0;
    dir->e.structinfo->isOpen = TRUE;
    if ( cmd == T_UNION )
        dir->sym.typekind = TYPE_UNION;
    else
        dir->sym.typekind = TYPE_STRUCT;

    if ( CurrStruct )
        dir->e.structinfo->isInline = TRUE;

    dir->next = CurrStruct;
    CurrStruct = dir;

#if 0 //def DEBUG_OUT
    {
        struct dsym *struc;
        for ( struc = CurrStruct; struc; struc = struc->next ) {
            DebugMsg(("StructDirective stack: %X, name=>%s<\n", struc, struc->sym.name ));
        }
    }
#endif

    return( NOT_ERROR );
}

/* handle ENDS directive when a struct definition is active */

ret_code EndstructDirective( int i, struct asm_tok tokenarray[] )
/***************************************************************/
{
    //char *name;
    //unsigned int offset;
    unsigned int size;
    //struct asym *sym;
    //memtype mem_type;
    struct dsym *dir;

    dir = CurrStruct;

    DebugMsg1(("EndstructDirective(%s), ofs=%" FU32 ", struct size=%" FU32 ", max_mbr=%" FU32 ", alignment=%u\n",
              dir->sym.name,
              dir->sym.offset,
              dir->sym.total_size,
              dir->sym.max_mbr_size,
              dir->e.structinfo->alignment));

    /* if pass is > 1 just do minimal work */
    if ( Parse_Pass > PASS_1 ) {
        CurrStruct->sym.offset = 0;
        size = CurrStruct->sym.total_size;
        CurrStruct = CurrStruct->next;
        if ( CurrStruct )
            UpdateStructSize( (struct asym *)dir );
        if ( CurrFile[LST] )
            LstWrite( LSTTYPE_STRUCT, size, dir );
        return( NOT_ERROR );
    }

    if ( ( i == 1 && dir->next == NULL ) ||
        ( i == 0 && dir->next != NULL ) ) {
        ;
    } else {
        /* v2.04: error msg improved */
        //EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        if ( i == 1)
            EmitErr( UNMATCHED_BLOCK_NESTING, tokenarray[0].string_ptr );
        else
            EmitErr( UNMATCHED_BLOCK_NESTING, "" );
        return( ERROR );
    }

    if ( i == 1 ) { /* an global struct ends with <name ENDS>  */
        if ( SymCmpFunc( tokenarray[0].string_ptr, dir->sym.name, dir->sym.name_size ) != 0 ) {
            /* names don't match */
            DebugMsg(("EndstructDirective: names don't match, i=%u, name=%s - %s\n", i, tokenarray[0].string_ptr, dir->sym.name));
            EmitErr( UNMATCHED_BLOCK_NESTING, tokenarray[0].string_ptr );
            return( ERROR );
        }
    }

    i++; /* go past ENDS */

    /* v2.07: if ORG was used inside the struct, the struct's size
     * has to be calculated now - there may exist negative offsets.
     */
    if ( dir->e.structinfo->OrgInside ) {
        struct field_item *f;
        int_32 min = 0;
        for ( f = dir->e.structinfo->head; f; f = f->next )
            if ( f->sym->offset < min )
                min = f->sym->offset;
        dir->sym.total_size = dir->sym.total_size - min;
    }

    /* Pad bytes at the end of the structure. */
#if 1
    /* v2.02: this is to be done in any case, whether -Zg is set or not */
    //if ( dir->e.structinfo->alignment > 1 && Options.masm_compat_gencode == FALSE ) {
    if ( dir->e.structinfo->alignment > 1 ) {
        size = dir->sym.max_mbr_size;
        if ( size == 0 )
            size++;
        if ( size > dir->e.structinfo->alignment )
            size = dir->e.structinfo->alignment;
        dir->sym.total_size = (dir->sym.total_size + size - 1) & (-size);
        DebugMsg1(("EndstructDirective:, struct size after final alignment=%" FU32 "\n", dir->sym.total_size));
    }
#endif
    dir->e.structinfo->isOpen = FALSE;
    dir->sym.isdefined = TRUE;

    /* if there's a negative offset, size will be wrong! */
    size = dir->sym.total_size;

    /* reset offset, it's just used during the definition */
    dir->sym.offset = 0;

    CurrStruct = dir->next;
    /* v2.0: add the embedded struct AFTER it has been parsed! */
    if ( i == 1 ) {
        struct asym *sym;
        /* v2.06: the struct name is needed for checks */
        sym = AddFieldToStruct( -1, NULL, *dir->sym.name ? dir->sym.name : NULL, MT_TYPE, dir, dir->sym.total_size );
        /* the member name was stored in the type name */
        //sym->name = dir->sym.name;
        //sym->name_size = strlen( dir->sym.name );
        sym->total_size = dir->sym.total_size;
        dir->sym.name = ""; /* the type becomes anonymous */
        dir->sym.name_size = 0;
    }

    if ( CurrFile[LST] ) {
        LstWrite( LSTTYPE_STRUCT, size, dir );
    }
#if 1
    /* to allow direct structure access */
    switch ( dir->sym.total_size ) {
    case 1:  dir->sym.mem_type = MT_BYTE;   break;
    case 2:  dir->sym.mem_type = MT_WORD;   break;
    case 4:  dir->sym.mem_type = MT_DWORD;  break;
    case 6:  dir->sym.mem_type = MT_FWORD;  break;
    case 8:  dir->sym.mem_type = MT_QWORD;  break;
    default:
        /* set something which cannot be accessed by a reg */
        /* there might exist a better solution, once the mess
         in the parser has been removed */
        // dir->sym.mem_type = MT_OWORD;
        dir->sym.mem_type = MT_EMPTY;
    }
#endif
    /* reset redefine */
    if ( CurrStruct == NULL ) {
        if ( redef_struct ) {
            if ( AreStructsEqual( dir, redef_struct) == FALSE ) {
                EmitErr( NON_BENIGN_XXX_REDEFINITION, szStructure, dir->sym.name );
            }
            DebugMsg(("EndstructDirective: delete the redefinition of %s\n", dir->sym.name ));
            SymFree( (struct asym *)dir );
            redef_struct = NULL;
        }
    } else {

        if ( dir->sym.max_mbr_size > CurrStruct->sym.max_mbr_size )
            CurrStruct->sym.max_mbr_size = dir->sym.max_mbr_size;

        UpdateStructSize( (struct asym *)dir );
        DebugMsg1(("EndstructDirective: new size of restored structure=%u\n", CurrStruct->sym.total_size));
    }
    //dir->sym.max_mbr_size = 0;
    if ( tokenarray[i].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    return( NOT_ERROR );
}

/* v2.06: new function to check fields of anonymous struct members */

static ret_code CheckAnonymousStruct( struct dsym *type )
/*******************************************************/
{
    uint_32 disp;
    struct asym  *sym;
    struct field_item *f;

    for ( f = type->e.structinfo->head; f; f = f->next ) {
        if ( *f->sym->name ) {
            sym = SearchNameInStruct((struct asym *)CurrStruct, f->sym->name, &disp, 0 );
            if ( sym ) {
                EmitErr( SYMBOL_REDEFINITION, sym->name );
                return( ERROR );
            }
        } else if ( f->sym->type ) {
            struct dsym *stype = (struct dsym *)f->sym->type;
            if ( stype->sym.typekind == TYPE_STRUCT ||
                stype->sym.typekind == TYPE_UNION ) {
            if ( CheckAnonymousStruct( stype ) == ERROR )
                return( ERROR );
            }
        }
    }
    return( NOT_ERROR );
}

/* this function is called in pass 1 only.
 * - loc: initializer location, may be -1
 * - name: field name, may be NULL
 * - mem_type: mem_type of item
 * - vartype: arbitrary type of item if memtype is MT_TYPE
 * - size: size of type - used for alignment only
 */
struct asym *AddFieldToStruct( int loc, struct asm_tok tokenarray[], const char *name, enum memtype mem_type, struct dsym *vartype, uint_32 size )
/************************************************************************************************************************************************/
{
    int_32 offset;
    //int count;
    int i;
    uint_32 disp;
    char *init;
    struct struct_info *si;
    struct field_item *f;
    struct asym  *sym;
    struct asym  *gsym;

    si = CurrStruct->e.structinfo;
    offset = CurrStruct->sym.offset;

    DebugMsg1(("AddFieldToStruct(%s): name=%s, curr ofs=%" FU32 ", vartype=%s, size=%" FU32 "\n",
               CurrStruct->sym.name, name ? name : "<anonymous>", offset,
               vartype ? vartype->sym.name : "NULL", size ));

    if ( name ) {
        sym = SearchNameInStruct((struct asym *)CurrStruct, name, &disp, 0 );
        if ( sym ) {
            EmitErr( SYMBOL_ALREADY_DEFINED, sym->name );
            return( NULL );
        }
    } else {
        /* v2.06: check fields of anonymous struct member */
        if ( vartype &&
            ( vartype->sym.typekind == TYPE_STRUCT ||
             vartype->sym.typekind == TYPE_UNION ) ) {
            CheckAnonymousStruct( vartype );
        }
        name = "";
    }

    /* alloc the items needed */

    sym = SymAlloc( name );
    sym->state = SYM_STRUCT_FIELD;
    sym->isdefined = TRUE;
    sym->mem_type = mem_type;
    sym->type = &vartype->sym;
    /* ok to do? */
    // sym->total_size = SizeFromMemtype( mem_type, ModuleInfo.Ofssize );

    f = LclAlloc( sizeof( struct field_item ) );

    f->next = NULL;
    f->sym = sym;

    if ( loc != -1 ) {

        i = strlen( tokenarray[loc].string_ptr ) + 1;
        DebugMsg1(("AddFieldToStruct(%s): type=>%s<\n", CurrStruct->sym.name, tokenarray[loc].string_ptr ));
        f->initializer = LclAlloc( i );
        memcpy( f->initializer, tokenarray[loc].string_ptr, i );

        /* now add the value to initialize the struct to */

        /* v2.03: the initializer value may contain assembly time
         * variables ( $ inside structs is also one ). It's crucial that
         * the variable's CURRENT value is used then.
         * v2.08: modified. avoid usage of token->string_ptr,
         * and prefer to use token->tokpos.
         */
        init = StringBufferEnd;
        for ( i = loc+1; tokenarray[i].token != T_FINAL; i++ ) {
            if ( tokenarray[i].token == T_ID ) {
                struct asym *sym2 = SymSearch( tokenarray[i].string_ptr );
                if ( sym2 && sym2->variable ) {
                    if ( sym2->predefined && sym2->sfunc_ptr )
                        sym2->sfunc_ptr( sym2 );
                    myltoa( sym2->uvalue, init, ModuleInfo.radix, sym2->value3264 < 0, TRUE );
                    init += strlen( init );
                    *init++= ' ';
                    continue;
                }
            }
            memcpy( init, tokenarray[i].tokpos, tokenarray[i+1].tokpos - tokenarray[i].tokpos );
            init += tokenarray[i+1].tokpos - tokenarray[i].tokpos;
        }
        *init = NULLC;
        f->value = LclAlloc( init - StringBufferEnd + 1 );
        strcpy( f->value, StringBufferEnd );
        DebugMsg1(("AddFieldToStruct(%s): initializer=>%s<\n", CurrStruct->sym.name, f->value ));

    } else {
        DebugMsg1(("AddFieldToStruct(%s): no initializer<\n", CurrStruct->sym.name ));
        f->initializer = NULL;
        f->value = NULL;
    }

    if( si->head == NULL ) {
        si->head = si->tail = f;
    } else {
        si->tail->next = f;
        si->tail = f;
    }

#if 1
    /* v2.0: for STRUCTs, don't use the struct's size for alignment calculations,
     * instead use the size of the "max" member!
     */
    if ( mem_type == MT_TYPE &&
        ( vartype->sym.typekind == TYPE_STRUCT ||
         vartype->sym.typekind == TYPE_UNION ) ) {
        size = vartype->sym.max_mbr_size;
    }
#endif
    /* align the field if an alignment argument was given */
    if ( si->alignment > 1 ) {
        //enum memtype mt;
        //struct dsym *tdir;
        DebugMsg1(("AddFieldToStruct(%s): align=%u, size=%u, ofs=%u\n", CurrStruct->sym.name, si->alignment, size, offset ));
        /* if it's the first field to add, use offset of the parent's current field */
#if 0
        /* v2: removed. An embedded struct is now added AFTER it has
         * been parsed. */
        if ( offset == 0 && CurrStruct->next ) {
            struct dsym *parent = CurrStruct->next;
            if ( si->alignment < size )
                parent->e.structinfo->tail->sym->offset =
                    (parent->e.structinfo->tail->sym->offset + (si->alignment - 1)) & ( - si->alignment);
            else if ( size )
                parent->e.structinfo->tail->sym->offset =
                    (parent->e.structinfo->tail->sym->offset + (size - 1)) & (-size);
        } else
#endif
        {
            if ( si->alignment < size )
                offset = (offset + (si->alignment - 1)) & ( - si->alignment);
            else if ( size )
                offset = (offset + (size - 1)) & (-size);
        }
        /* adjust the struct's current offset + size.
         The field's size is added in UpdateStructSize()
         */
        if ( CurrStruct->sym.typekind != TYPE_UNION ) {
            CurrStruct->sym.offset = offset;
            if ( offset > CurrStruct->sym.total_size )
                CurrStruct->sym.total_size = offset;
        }
    }
    /* v2.0: for padding, save the max member size */
    if ( size > CurrStruct->sym.max_mbr_size ) {
        DebugMsg1(("AddFieldToStruct(%s): max_mbr_size set to %u\n", CurrStruct->sym.name, size ));
        CurrStruct->sym.max_mbr_size = size;
    }
    sym->offset = offset;

    /* if -Zm is on, create a global symbol */
    if ( ModuleInfo.oldstructs == TRUE && *name != NULLC ) {
        DebugMsg(("AddFieldToStruct(%s): Masm51 compat on, lookup %s in global symbol table\n", CurrStruct->sym.name, name ));
        gsym  = SymLookup( name );
        if ( gsym ) {
            if ( gsym->state == SYM_UNDEFINED )
                gsym->state = SYM_STRUCT_FIELD;
            if ( gsym->state == SYM_STRUCT_FIELD ) {
                struct dsym *dir;
                gsym->mem_type = mem_type;
                gsym->type = &vartype->sym;
                gsym->offset = offset; /* added v2.0 */
                /* v2.01: must be the full offset.
                 * (there's still a problem if alignment is > 1!)
                 */
                for ( dir = CurrStruct->next; dir; dir = dir->next )
                    gsym->offset += dir->sym.offset;
                gsym->isdefined = TRUE;
            }
        }
    }

    return( sym );
}

/* called by AlignDirective() if ALIGN/EVEN has been found inside
 * a struct. It's already verified that <value> is a power of 2.
 */
ret_code AlignInStruct( int value )
/*********************************/
{
    if ( CurrStruct->sym.typekind != TYPE_UNION ) {
        int offset;
        offset = CurrStruct->sym.offset;
        offset = (offset + (value - 1)) & (-value);
        CurrStruct->sym.offset = offset;
        if ( offset > CurrStruct->sym.total_size )
            CurrStruct->sym.total_size = offset;
    }
    return( NOT_ERROR );
}

/* called by data_dir() when a structure field has been created.
 */
void UpdateStructSize( struct asym *sym )
/***************************************/
{
    if ( CurrStruct->sym.typekind == TYPE_UNION ) {
        //if ( no_of_bytes > CurrStruct->sym.total_size )
        //    CurrStruct->sym.total_size = no_of_bytes;
        if ( sym->total_size > CurrStruct->sym.total_size )
            CurrStruct->sym.total_size = sym->total_size;
    } else {
        CurrStruct->sym.offset += sym->total_size;
        if ( CurrStruct->sym.offset > (int_32)CurrStruct->sym.total_size )
            CurrStruct->sym.total_size = CurrStruct->sym.offset;
    }
    DebugMsg1(("UpdateStructSize(%s.%s): %s, curr mbr size=%u curr struc/union size=%u\n",
               CurrStruct->sym.name,
               sym->name,
               CurrStruct->sym.typekind == TYPE_UNION ? "union" : "struct",
               sym->total_size,
               CurrStruct->sym.total_size));
    return;
}

/* called if ORG occurs inside STRUCT/UNION definition */

ret_code SetStructCurrentOffset( int_32 offset )
/**********************************************/
{
    if ( CurrStruct->sym.typekind == TYPE_UNION ) {
        EmitError( ORG_NOT_ALLOWED_IN_UNIONS );
        return( ERROR );
    }
    CurrStruct->sym.offset = offset;
    /* if an ORG is inside the struct, it cannot be instanced anymore */
    CurrStruct->e.structinfo->OrgInside = TRUE;
    if ( offset > (int_32)CurrStruct->sym.total_size )
        CurrStruct->sym.total_size = offset;

    return( NOT_ERROR );
}

/* get a qualified type.
 * Used by
 * - TYPEDEF
 * - PROC/PROTO params and LOCALs
 * - EXTERNDEF
 * - EXTERN
 * - LABEL
 * - ASSUME for GPRs
 */
ret_code GetQualifiedType( int *pi, struct asm_tok tokenarray[], struct qualified_type *pti )
/*******************************************************************************************/
{
    int             type;
    int             tmp;
    enum memtype    mem_type;
    int             i = *pi;
    int             distance = FALSE;
    struct asym     *sym;

    /* convert "directive" PROC to a type qualifier */
    for ( tmp = i; tokenarray[tmp].token != T_FINAL && tokenarray[tmp].token != T_COMMA; tmp++ )
        if ( tokenarray[tmp].token == T_DIRECTIVE && tokenarray[tmp].tokval == T_PROC ) {
            tokenarray[tmp].token = T_STYPE;
            /* v2.06: avoid to use ST_PROC */
            tokenarray[tmp].tokval = ( ( SIZE_CODEPTR & ( 1 << ModuleInfo.model ) ) ? T_FAR : T_NEAR );
        }
    /* with NEAR/FAR, there are several syntax variants allowed:
     * 1. NEARxx | FARxx
     * 2. PTR NEARxx | FARxx
     * 3. NEARxx | FARxx PTR [<type>]
     */
    /* read qualified type */
    for ( type = ERROR; tokenarray[i].token == T_STYPE || tokenarray[i].token == T_BINARY_OPERATOR; i++ ) {
        if ( tokenarray[i].token == T_STYPE ) {
            tmp = tokenarray[i].tokval;
            if ( type == ERROR )
                type = tmp;
            mem_type = GetMemtypeSp( tmp );
            if ( mem_type == MT_FAR || mem_type == MT_NEAR ) {
                if ( distance == FALSE ) {
                    uint_8 Ofssize = GetSflagsSp( tmp );
                    pti->is_far = ( mem_type == MT_FAR );
                    if ( Ofssize != USE_EMPTY )
                        pti->Ofssize = Ofssize;
                    distance = TRUE;
                } else if ( tokenarray[i-1].tokval != T_PTR )
                    break;
            } else {
                if ( pti->is_ptr )
                    pti->ptr_memtype = mem_type;
                i++;
                break;
            }
        } else if ( tokenarray[i].tokval == T_PTR ) {
            /* v2.06: avoid to use ST_PTR */
            //type = ST_PTR;
            type = EMPTY;
            pti->is_ptr++;
        } else
            break;
    }

    /* v2.06: don't use ST_PTR anymore! */
    //if ( type == ST_PTR ) {
    if ( type == EMPTY ) {
        if ( tokenarray[i].token == T_ID && tokenarray[i-1].tokval == T_PTR ) {
            pti->symtype = SymSearch( tokenarray[i].string_ptr );
            if ( pti->symtype == NULL || pti->symtype->state == SYM_UNDEFINED )
                pti->symtype = CreateTypeSymbol( pti->symtype, tokenarray[i].string_ptr, TRUE );
            else if ( pti->symtype->state != SYM_TYPE ) {
                EmitErr( INVALID_QUALIFIED_TYPE, tokenarray[i].string_ptr );
                return( ERROR );
            } else {
                sym = pti->symtype;
                /* if it's a typedef, simplify the info */
                if ( sym->typekind == TYPE_TYPEDEF ) {
                    pti->is_ptr     += sym->is_ptr;
                    if ( sym->is_ptr == 0 ) {
                        /* v2.06b: alias types have MT_TYPE, dont use for ptr_memtype! */
                        //pti->ptr_memtype = sym->mem_type;
                        pti->ptr_memtype = ( sym->mem_type != MT_TYPE ? sym->mem_type : MT_EMPTY );
                        if ( distance == FALSE && pti->is_ptr == 1 &&
                            ( sym->mem_type == MT_NEAR ||
                             sym->mem_type == MT_PROC ||
                             sym->mem_type == MT_FAR ) )
                            pti->is_far = sym->isfar;
                            if ( sym->Ofssize != USE_EMPTY )
                                pti->Ofssize = sym->Ofssize;
                    } else {
                        pti->ptr_memtype = sym->ptr_memtype;
                        if ( distance == FALSE && pti->is_ptr == 1 ) {
                            pti->is_far = sym->isfar;
                            if ( sym->Ofssize != USE_EMPTY )
                                pti->Ofssize = sym->Ofssize;
                        }
                    }
                    if ( sym->mem_type == MT_TYPE )
                        pti->symtype  = sym->type;
                    else
                        pti->symtype  = sym->target_type;
                }
            }
            i++;
        }
    }

    if( type == ERROR ) {
        if ( tokenarray[i].token != T_ID ) {
            if ( tokenarray[i].token == T_FINAL || tokenarray[i].token == T_COMMA )
                EmitError( QUALIFIED_TYPE_EXPECTED );
            else {
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
                i++;
            }
            return( ERROR );
        }
        pti->symtype = SymSearch( tokenarray[i].string_ptr );
        if( pti->symtype == NULL || pti->symtype->state != SYM_TYPE ) {
            DebugMsg(("GetQualifiedType: invalid type : %s\n", tokenarray[i].string_ptr ));
            if ( pti->symtype == NULL || pti->symtype ->state == SYM_UNDEFINED )
                EmitErr( SYMBOL_NOT_DEFINED, tokenarray[i].string_ptr );
            else
                EmitErr( INVALID_QUALIFIED_TYPE, tokenarray[i].string_ptr );
            return( ERROR );
        }
        sym = pti->symtype;
        if ( sym->typekind == TYPE_TYPEDEF ) {
            pti->mem_type = sym->mem_type;
            pti->is_far   = sym->isfar;
            pti->is_ptr   = sym->is_ptr;
            pti->Ofssize  = sym->Ofssize;
            pti->size     = sym->total_size;
            pti->ptr_memtype = sym->ptr_memtype;
            if ( sym->mem_type == MT_TYPE )
                pti->symtype  = sym->type;
            else
                pti->symtype  = sym->target_type;
        } else {
            pti->mem_type = MT_TYPE;
            pti->size = sym->total_size;
        }
        i++;
    } else {
        if ( pti->is_ptr )
            pti->mem_type = MT_PTR;
        else
            pti->mem_type = GetMemtypeSp( type );
        if ( pti->mem_type == MT_PTR )
            pti->size = SizeFromMemtype( pti->is_far ? MT_FAR : MT_NEAR, pti->Ofssize, NULL );
        else
            pti->size = SizeFromMemtype( pti->mem_type, pti->Ofssize, NULL );
    }
    *pi = i;
    DebugMsg1(("GetQualifiedType: i=%u, memtype=%Xh, ptr=%u, far=%u, ofssize=%d, arbtype=%s:%X\n",
               i, pti->mem_type, pti->is_ptr, pti->is_far, pti->Ofssize,
               pti->symtype ? pti->symtype->name : "NULL",
               pti->symtype ? pti->symtype->mem_type : 0 ));
    return( NOT_ERROR );
}

/* TYPEDEF directive. Syntax is:
 * <type name> TYPEDEF [proto|[far|near [ptr]]<qualified type>]
 */
ret_code TypedefDirective( int i, struct asm_tok tokenarray[] )
/*************************************************************/
{
    struct asym         *sym;
    struct dsym         *dir;
    char                *name;
    struct qualified_type ti;

    DebugMsg1(("TypedefDirective(%d) enter\n", i));

    if( i != 1 ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    name = tokenarray[0].string_ptr;

    i++; /* go past TYPEDEF */

    sym = SymSearch( name );
    if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
        sym = CreateTypeSymbol( sym, name, TRUE );
        if ( sym == NULL )
            return( ERROR );
    } else {
        /* MASM allows to have the TYPEDEF included multiple times */
        /* but the types must be identical! */
        if ( ( sym->state != SYM_TYPE ) ||
            ( sym->typekind != TYPE_TYPEDEF &&
             sym->typekind != TYPE_NONE ) ) {
            EmitErr( SYMBOL_REDEFINITION, sym->name );
            return( ERROR );
        }
    }

    sym->isdefined = TRUE;
    if ( Parse_Pass > PASS_1 )
        return( NOT_ERROR );
    dir = (struct dsym *)sym;
    dir->sym.typekind = TYPE_TYPEDEF;

    /* PROTO is special */
    if ( tokenarray[i].token == T_DIRECTIVE && tokenarray[i].tokval == T_PROTO) {
        struct dsym *dir2;  /* create a PROTOtype item without name */
        /* v2.04: added check if prototype is set already */
        if ( sym->target_type == NULL && sym->mem_type == MT_EMPTY ) {
            dir2 = (struct dsym *)CreateProc( NULL, "", FALSE );
            DebugMsg1(("TypedefDirective PROTO, created new unnamed prototype %p\n", dir2 ));
        } else if ( sym->mem_type == MT_PROC ) {
            dir2 = (struct dsym *)sym->target_type;
        } else {
            EmitErr( SYMBOL_TYPE_CONFLICT, sym->name );
            return( ERROR );
        }
        i++;
        DebugMsg1(("TypedefDirective PROTO, call ExamineProc(sym=%p i=%d, 0)\n", dir2, i));
        if( ExamineProc( i, tokenarray, dir2, FALSE ) == ERROR )
            return ( ERROR );
        DebugMsg1(("TypedefDirective PROTO, ExamineProc() returned status ok\n"));
        sym->mem_type = MT_PROC;
        dir2->sym.isproc = TRUE; /* v2.05: added */
        sym->Ofssize = dir2->sym.seg_ofssize;
        /* v2.03: set value of field total_size (previously was 0) */
        sym->total_size = (2 << sym->Ofssize);
        if( dir2->sym.mem_type != MT_NEAR ) {
            sym->isfar = TRUE; /* v2.04: added */
            sym->total_size += 2;
        }
        sym->target_type = (struct asym *)dir2;
        DebugMsg1(("TypedefDirective(%s) ok, mem_type=%Xh, ofssize=%u\n", sym->name, sym->mem_type, sym->Ofssize ));
        return( NOT_ERROR );
    }
    ti.size = 0;
    ti.is_ptr = 0;
    ti.is_far = FALSE;
    ti.mem_type = MT_EMPTY;
    ti.ptr_memtype = MT_EMPTY;
    ti.symtype = NULL;
    ti.Ofssize = ModuleInfo.Ofssize;

    /* "empty" type is ok for TYPEDEF */
    if ( tokenarray[i].token == T_FINAL || tokenarray[i].token == T_COMMA )
        ;
    else if ( GetQualifiedType( &i, tokenarray, &ti ) == ERROR )
        return( ERROR );

    /* if type did exist already, check for type conflicts
     * v2.05: this code has been rewritten */
    if ( sym->mem_type != MT_EMPTY ) {
        struct asym *to;
        struct asym *tn;
        char oo;
        char on;
        for( tn = ti.symtype; tn && tn->type; tn = tn->type );
        to = ( sym->mem_type == MT_TYPE ) ? sym->type : sym->target_type;
        for( ; to && to->type; to = to->type );
        oo = ( sym->Ofssize != USE_EMPTY ) ? sym->Ofssize : ModuleInfo.Ofssize;
        on = ( ti.Ofssize != USE_EMPTY ) ? ti.Ofssize : ModuleInfo.Ofssize;
        if ( ti.mem_type != sym->mem_type ||
            ( ti.mem_type == MT_TYPE && tn != to ) ||
            ( ti.mem_type == MT_PTR &&
             ( ti.is_far != sym->isfar ||
              on != oo ||
              ti.ptr_memtype != sym->ptr_memtype ||
              tn != to ))) {
            DebugMsg(("TypedefDirective: old-new memtype=%X-%X type=%X(%s)-%X(%s) far=%u-%u ind=%u-%u ofss=%d-%d pmt=%X-%X\n",
                      sym->mem_type, ti.mem_type,
                      (sym->mem_type == MT_TYPE) ? sym->type : sym->target_type,
                      (sym->mem_type == MT_TYPE) ? sym->type->name : sym->target_type ? sym->target_type->name : "",
                      ti.symtype, ti.symtype ? ti.symtype->name : "",
                      sym->isfar, ti.is_far,
                      sym->is_ptr, ti.is_ptr,
                      sym->Ofssize, ti.Ofssize,
                      sym->ptr_memtype, ti.ptr_memtype ));
            EmitErr( SYMBOL_TYPE_CONFLICT, name );
            return( ERROR );
        }
    }

    sym->mem_type = ti.mem_type;
    sym->Ofssize = ti.Ofssize;
    sym->total_size = ti.size;
    sym->is_ptr = ti.is_ptr;
    sym->isfar = ti.is_far;
    if ( ti.mem_type == MT_TYPE )
        sym->type = ti.symtype;
    else
        sym->target_type = ti.symtype;
    sym->ptr_memtype = ti.ptr_memtype;
    DebugMsg1(("TypedefDirective(%s) ok, mem_type=MT_TYPE, size=%" FU32 ", type=%p type.memtype=%X\n",
               sym->name, sym->total_size, sym->type, ti.symtype ? ti.symtype->mem_type : 0 ));

    DebugMsg1(("TypedefDirective(%s) ok, mem_type=%Xh, size=%u, indirection=%u target=%p\n", sym->name, sym->mem_type, ti.size, ti.is_ptr, ti.symtype ));

    if ( tokenarray[i].token != T_FINAL ) {
        DebugMsg(("TypedefDirective: unexpected token %u, idx=%u\n", tokenarray[i].token, i));
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    return( NOT_ERROR );
}

/* generate a RECORD. Called on pass 1 only */

#if AMD64_SUPPORT
#define MAXRECBITS ( ModuleInfo.Ofssize == USE64 ? 64 : 32 )
#else
#define MAXRECBITS 32
#endif

ret_code RecordDirective( int i, struct asm_tok tokenarray[] )
/************************************************************/
{
    char *name;
    struct asym *sym;
    struct dsym *dir;
    struct dsym *oldr = NULL;
    struct field_item *f;
    int num;
    int redef_err;
    int count;
    //int value;
    int name_loc;
    int init_loc;
    struct expr opndx;

    DebugMsg1(("RecordDirective(%d) enter\n", i));
    if ( i != 1 ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
#if 0
    /* a record definition can be placed
     * inside a STRUCT, but it will still be global
     */
    if ( StructDef.struct_depth > 0 ) {
        EmitError( SYNTAX_ERROR );
        return( ERROR );
    }
#endif

    name = tokenarray[0].string_ptr;
    sym = SymSearch( name );
    if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
        sym = CreateTypeSymbol( sym, name, TRUE );
    } else if ( sym->state == SYM_TYPE &&
               ( sym->typekind == TYPE_RECORD ||
               sym->typekind == TYPE_NONE ) ) {
        /* v2.04: allow redefinition of record and forward references */
        if ( Parse_Pass == PASS_1 && sym->typekind == TYPE_RECORD ) {
            oldr = (struct dsym *)sym;
            sym = CreateTypeSymbol( NULL, name, FALSE );
            redef_err = 0;
        }
    } else {
        EmitErr( SYMBOL_REDEFINITION, name );
        return( ERROR );
    }
    sym->isdefined = TRUE;
    if ( Parse_Pass > PASS_1 )
        return( NOT_ERROR );
    dir = (struct dsym *)sym;
    dir->sym.typekind = TYPE_RECORD;

    i++; /* go past RECORD */

    num = 0; /* counter for total of bits in record */
    do {
        if ( tokenarray[i].token != T_ID ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            break;
        }
        name_loc = i;
        i++;
        if ( tokenarray[i].token != T_COLON ) {
            EmitError( COLON_EXPECTED );
            break;
        }
        i++;
        /* get width */
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
            break;
        if ( opndx.kind != EXPR_CONST ) {
            EmitError( CONSTANT_EXPECTED );
            opndx.value = 1;
        }
        if ( opndx.value + num > MAXRECBITS ) {
            EmitErr( TOO_MANY_BITS_IN_RECORD, tokenarray[name_loc].string_ptr );
            break;
        }
        count = 0;
        /* is there an initializer? ('=') */
        if ( tokenarray[i].token == T_DIRECTIVE &&
            tokenarray[i].dirtype == DRT_EQUALSGN ) {
            i++;
            for( init_loc = i; tokenarray[i].token != T_FINAL && tokenarray[i].token != T_COMMA; i++ );
            if ( init_loc == i ) {
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
                break;
            }
            count = tokenarray[i].tokpos - tokenarray[init_loc].tokpos;
        }
        /* record field names are global! (Masm design flaw) */
        if ( oldr ) {
            sym = SymSearch( tokenarray[name_loc].string_ptr );
            if ( sym == NULL ||
                sym->state != SYM_STRUCT_FIELD ||
                sym->mem_type != MT_BITS ||
                sym->total_size != opndx.value ) {
                EmitErr( NON_BENIGN_XXX_REDEFINITION, szRecord, tokenarray[name_loc].string_ptr );
                redef_err++;
                sym = NULL; /* v2.06: added */
            }
        } else {
            sym = SymLookup( tokenarray[name_loc].string_ptr );
            if ( !sym )
                break;
            if ( sym->state != SYM_UNDEFINED ) {
                EmitErr( SYMBOL_REDEFINITION, sym->name );
                break;
            }
            sym->state = SYM_STRUCT_FIELD;
            sym->mem_type = MT_BITS;
            sym->total_size = opndx.value;
        }

        if ( sym ) { /* v2.06: don't add field if there was an error */
            num += opndx.value;
            f = LclAlloc( sizeof( struct field_item ) );
            f->next = NULL;
            f->sym = sym;
            f->value = NULL;
            f->initializer = NULL;
            if( dir->e.structinfo->head == NULL ) {
                dir->e.structinfo->head = dir->e.structinfo->tail = f;
            } else {
                dir->e.structinfo->tail->next = f;
                dir->e.structinfo->tail = f;
            }
            if ( count ) {
                f->value = LclAlloc( count + 1 );
                memcpy( f->value, tokenarray[init_loc].tokpos, count );
                *(f->value + count) = NULLC;
            }
        }

        if ( i < Token_Count ) {
            if ( tokenarray[i].token != T_COMMA || tokenarray[i+1].token == T_FINAL ) {
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
                break;
            }
            i++;
        }

    } while ( i < Token_Count );

    /* now calc size in bytes and set the bit positions */

    if ( num > 16 ) {
#if AMD64_SUPPORT
        if ( num > 32 ) {
            dir->sym.total_size = sizeof( uint_64 );
            dir->sym.mem_type = MT_QWORD;
        } else {
#endif
            dir->sym.total_size = sizeof( uint_32 );
            dir->sym.mem_type = MT_DWORD;
#if AMD64_SUPPORT
        }
#endif
    } else if ( num > 8 ) {
        dir->sym.total_size = sizeof( uint_16 );
        dir->sym.mem_type = MT_WORD;
    } else {
        dir->sym.total_size = sizeof( uint_8 );
        dir->sym.mem_type = MT_BYTE;
    }
    /* if the BYTE/WORD/DWORD isn't used fully, shift bits to the right! */
    // num = dir->sym.total_size * 8;

    /* set the bit position */
    for ( f = dir->e.structinfo->head; f; f = f->next ) {
        num = num - f->sym->total_size;
        f->sym->offset = num;
    }
    if ( oldr ) {
        if ( redef_err > 0 ||
            AreStructsEqual( dir, oldr ) == FALSE )
            EmitErr( NON_BENIGN_XXX_REDEFINITION, szRecord, dir->sym.name );
        /* record can be freed, because the record's fields are global items */
        SymFree( (struct asym *)dir );
    }
    DebugMsg(("RecordDirective(%s) exit, no error\n", name ));
    return( NOT_ERROR );
}

void DeleteType( struct dsym *dir )
/*********************************/
{
    struct field_item      *curr;
    struct field_item      *next;

    DebugMsg(("DeleteType(%s) enter\n", dir->sym.name ));
    for( curr = dir->e.structinfo->head; curr != NULL; curr = next ) {
        /* bitfields field names are global, don't free them here! */
        if ( dir->sym.typekind != TYPE_RECORD )
            SymFree( curr->sym );
        LclFree( curr->initializer );
        LclFree( curr->value );
        next = curr->next;
        LclFree( curr );
    }
    LclFree( dir->e.structinfo );
    return;
}

