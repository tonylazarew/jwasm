/****************************************************************************
*
*                            Open Watcom Project
*
*    Portions Copyright (c) 1983-2002 Sybase, Inc. All Rights Reserved.
*
*  ========================================================================
*
*    This file contains Original Code and/or Modifications of Original
*    Code as defined in and that are subject to the Sybase Open Watcom
*    Public License version 1.0 (the 'License'). You may not use this file
*    except in compliance with the License. BY USING THIS FILE YOU AGREE TO
*    ALL TERMS AND CONDITIONS OF THE LICENSE. A copy of the License is
*    provided with the Original Code and Modifications, and is also
*    available at www.sybase.com/developer/opensource.
*
*    The Original Code and all software distributed under the License are
*    distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
*    EXPRESS OR IMPLIED, AND SYBASE AND ALL CONTRIBUTORS HEREBY DISCLAIM
*    ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
*    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR
*    NON-INFRINGEMENT. Please see the License for the specific language
*    governing rights and limitations under the License.
*
*  ========================================================================
*
* Description:  defines symbol structures asym and dsym,
*               and prototypes of functions in symbols.c.
*               This file is included by parser.h.
*
****************************************************************************/


#ifndef _SYMBOLS_H_
#define _SYMBOLS_H_

/*
 * SYM_LIB  - library paths are no longer added to the symbol table
 * SYM_LNAME has been removed.
 * It was used for the null-entry in the LNAME table only
 * v2.01: SYM_PROC has been removed.
 * v2.01: SYM_LIB has been removed.
 */
enum sym_state {
    SYM_UNDEFINED,
    SYM_INTERNAL,       /*  1 internal label */
    SYM_EXTERNAL,       /*  2 external       */
    SYM_SEG,            /*  3 segment        */
    SYM_GRP,            /*  4 group          */
    SYM_STACK,          /*  5 stack variable */
    SYM_STRUCT_FIELD,   /*  6 struct member  */
    SYM_TYPE,           /*  7 structure, union, typedef, record */
    SYM_ALIAS,          /*  8 alias name     */
    SYM_MACRO,          /*  9 macro          */
    SYM_TMACRO,         /* 10 text macro     */
    SYM_CLASS_LNAME     /* 11 lname item for segm class - not in symbol table */
};

/* v2.04: MT_SHORT removed */
/* v2.07: MT_ABS (0xC2) removed */

enum memtype {
    MT_SIZE_MASK = 0x1F, /* if MT_SPECIAL==0 then bits 0-4 = size - 1 */
    MT_FLOAT  = 0x20, /* bit 5=1 */
    MT_SIGNED = 0x40, /* bit 6=1 */
    MT_BYTE  = 1 - 1,
    MT_SBYTE = MT_BYTE | MT_SIGNED,
    MT_WORD  = 2 - 1,
    MT_SWORD = MT_WORD | MT_SIGNED,
    MT_DWORD = 4 - 1,
    MT_SDWORD= MT_DWORD | MT_SIGNED,
    MT_REAL4 = MT_DWORD | MT_FLOAT,
    MT_FWORD = 6 - 1,
    MT_QWORD = 8 - 1,
    MT_SQWORD= MT_QWORD | MT_SIGNED,
    MT_REAL8 = MT_QWORD | MT_FLOAT,
    MT_TBYTE = 10 - 1,
    MT_REAL10= MT_TBYTE | MT_FLOAT,
    MT_OWORD = 16 - 1,
#if AVXSUPP
    MT_YMMWORD = 32 - 1,
#endif
    MT_PROC  = 0x80,
    MT_NEAR  = 0x81,
    MT_FAR   = 0x82,
    MT_EMPTY = 0xC0,
    MT_BITS  = 0xC1,   /* record field */
    MT_PTR   = 0xC3,   /* v2.05: changed, old value 0x83 */
    MT_TYPE  = 0xC4,   /* structured variable */
    MT_SPECIAL = 0x80, /* bit 7 */
    MT_SPECIAL_MASK = 0xC0, /* bit 6+7 */
    MT_ADDRESS = 0x80, /* bit 7=1, bit 6 = 0 */
};

#define IS_SIGNED(x)  (((x) & MT_SPECIAL_MASK) == MT_SIGNED)

/* symbols can be
 * - "labels" (data or code, internal, external, stack)
 *   mem_type is MT_BYTE..MT_OWORD, MT_NEAR, MT_FAR, MT_PTR
 * - constants (EQU) or assembly time variables ("="),
 *   mem_type "usually" is MT_EMPTY.
 * - types (STRUCT, UNION, TYPEDEF, RECORD), mem_type = MT_TYPE
 * - preprocessor items (macros and text macros), which have no
 *   mem_type (MT_EMPTY).
 */
struct macro_instance;

typedef ret_code (* macro_func)( struct macro_instance *, char *, struct asm_tok * );
typedef void (* internal_func)( struct asym * );

struct debug_info {
    uint_32 start_line;  /* procs's start line */
    uint_32 end_line;    /* procs's last line */
    uint_32 ln_fileofs;  /* file offset to line numbers */
    uint_16 line_numbers;/* line numbers in function */
    uint_16 file;        /* proc's start file */
    uint next_proc;      /* index next proc */
    uint next_file;      /* index next file */
};


struct asym {
    struct asym     *next;
    char            *name;         /* symbol name */
    union {
        int_32         offset;     /* used by SYM_INTERNAL (labels), SYM_TYPE */
        int_32         value;      /* used by SYM_INTERNAL (equates) */
        uint_32        uvalue;     /* v2.01: equates (they are 33-bit!) */
        char           *string_ptr;/* used by SYM_TMACRO */
        struct asym    *substitute;/* v2.04b: used by SYM_ALIAS */
        macro_func     func_ptr;   /* used by SYM_MACRO if predefined==1 */
        int_32         max_offset; /* used by SYM_SEG */
        int_32         class_lname_idx;/* used by SYM_CLASS_LNAME */
    };
    struct asym     *segment;      /* used by SYM_INTERNAL, SYM_EXTERNAL */
    enum sym_state  state;
    enum memtype    mem_type;
    unsigned char   used:1,       /* symbol has been referenced */
                    isdefined:1,  /* symbol is "defined" in this pass */
                    scoped:1,     /* symbol is local label or LOCAL */
                    /* v2.07: removed */
                    //isglobal:1,   /* symbol has been added to the globals queue */
                    iat_used:1,   /* v2.07: IAT entry of symbol used */
                    isequate:1,   /* symbol has been defined with EQU */
                    predefined:1, /* symbol is predefined */
                    variable:1,   /* symbol is variable ('=' directive) */
                    public:1;     /* symbol has been added to the publics queue */
    unsigned char   list:1,       /* symbol is to be listed */
                    isarray:1,    /* symbol is an array (total_length is valid) */
                    included:1,   /* COFF: static symbol added to public queue. ELF:symbol added to symbol table */
                    saved:1,      /* assembly time variables only: symbol has been saved ("fast pass") */
                    isproc:1,     /* symbol is PROC or PROTO */
#if FASTMEM==0
                    staticmem:1,  /* symbol stored in static memory */
#endif
#ifdef DEBUG_OUT
                    forward:1,    /* symbol was forward referenced */
#endif
                    isdata:1;     /* field first_size is valid */
    union {
        /* for SYM_INTERNAL (memtype != NEAR|FAR), SYM_STRUCT_FIELD */
        uint_32         first_size;   /* size of 1st initializer in bytes */
        /* for SYM_INTERNAL (memtype == NEAR|FAR),
         * SYM_GRP (Ofssize),
         * SYM_EXTERNAL (Ofssize, comm, weak, isfar, is_ptr, ptr_memtype),
         * SYM_STACK (Ofssize, isfar, is_vararg, is_ptr, ptr_memtype ),
         * SYM_TYPE, TYPE_TYPEDEF (Ofssize, isfar, is_ptr, ptr_memtype )
         */
        struct {
            unsigned char   Ofssize;   /* offset size (USE16, USE32) */
            unsigned char   is_ptr;    /* PTR indirection */
            union {
                unsigned char ptr_memtype;/* pointer target type */
                unsigned char asmpass;    /* SYM_INTERNAL (mem_type NEAR|FAR) */
            };
            unsigned char   seg_ofssize:2;    /* SYM_EXTERNAL only */
            unsigned char   iscomm:1;  /* is communal */
            unsigned char   weak:1;    /* 1 if an unused "externdef" */
            unsigned char   isfar:1;   /* SYM_EXTERNAL, SYM_TYPE, SYM_STACK */
            unsigned char   is_vararg:1;/* SYM_STACK, VARARG param */
        };
        /* for SYM_MACRO */
        struct {
            unsigned char   mac_vararg:1,/* accept additional params */
                            isfunc:1,   /* it's a macro function */
#if MACROLABEL
                            label:1,    /* macro is "label-aware" */
#endif
                            purged:1;   /* macro has been PURGEd */
        };
    };
    union {
        /* first_length is used for data items only
         * for SYM_INTERNAL ("data labels" only), SYM_STRUCT_FIELD */
        uint_32         first_length; /* size of 1st initializer--elts. dup'd */
        /* SYM_TYPE (TYPEKIND_STRUCT or TYPEKIND_UNION) */
        uint_32         max_mbr_size; /* max size members */
        /* SYM_STACK, SYM_TYPE (TYPEKIND_TYPEDEF), SYM_EXTERNAL, SYM_INTERNAL (code labels) */
        struct asym     *target_type; /* set if ptr_memtype is MT_TYPE */
    };
    union {
        /* for SYM_INTERNAL, SYM_STRUCT_FIELD,
         * SYM_TYPE, SYM_STACK,
         * SYM_EXTERNAL (comm=1)
         * SYM_TMACRO: size of buffer allocated for the text in string_ptr
         */
        uint_32         total_size;   /* total number of bytes (sizeof) */
        /* for SYM_INTERNAL, isequate=1 (numeric equates) */
        int_32          value3264;    /* high bits for equates */
#if DLLIMPORT
        const char     *dllname;      /* SYM_EXTERNAL (isproc=1) */
#endif
    };
    union {
        /* SYM_INTERNAL, SYM_STRUCT_FIELD,
         * SYM_STACK, SYM_EXTERNAL (comm==1):
         * total number of elements (LENGTHOF)
         */
        uint_32        total_length;
        struct asym    *altname;     /* SYM_EXTERNAL (comm==0): alternative name */
        struct debug_info *debuginfo;/* SYM_INTERNAL (isproc==1): debug info (COFF) */
        internal_func  sfunc_ptr;    /* SYM_INTERNAL+predefined */
        struct { /* SYM_TYPE */
            /* codeview type index (used after assembly steps)
             * v2.04: moved from first_length, were it didn't work anymore
             * since the addition of field max_mbr_size.
             */
            uint_16        cv_typeref;
            uint_8         typekind;
        };
    };
#if (MAX_ID_LEN <= 255)
    uint_8          name_size;
#else
    uint_16         name_size;
#endif
    enum lang_type  langtype;
    struct asym     *type;        /* set if memtype is MT_TYPE */
    union {
        struct fixup *fixup;      /* SYM_INTERNAL, SYM_UNDEFINED, SYM_EXTERNAL? only */
        /* for SYM_EXTERNAL */
        uint         ext_idx;     /* (external definition) index */
    };
};

/*---------------------------------------------------------------------------*/
/* Structures for grpdef, segdef, externdef, pubdef, included library,       */
/* procedure and symbolic integer constants.                                 */
/*---------------------------------------------------------------------------*/

typedef int     direct_idx;     /* directive index, such as segment index, */
                                /* group index or lname index, etc.        */

struct seg_item {
    struct seg_item     *next;
    struct dsym         *seg;
};

struct grp_info {
    struct seg_item     *seglist;       /* list of segments in the group */
    direct_idx          grp_idx;        /* its group index (OMF) */
    direct_idx          lname_idx;      /* LNAME index (OMF only) */
    uint                numseg;         /* number of segments in the group */
};

enum seg_type {
    SEGTYPE_UNDEF,
    SEGTYPE_CODE,
    SEGTYPE_DATA,
    SEGTYPE_BSS,
    SEGTYPE_STACK,
    SEGTYPE_ABS,
    SEGTYPE_ERROR
};

struct seg_info {
    struct asym         *group;         /* segment's group or NULL */
    uint_32             start_loc;      /* starting offset of current ledata or lidata */
    union {
        uint_32         current_loc;    /* current offset in current ledata or lidata */
        uint_32         reloc_offset;   /* ELF: reloc file offset */
        uint_32         start_offset;   /* BIN: start offset in group */
    };
#ifdef __I86__
    uint_8 huge         *CodeBuffer;
#else
    uint_8              *CodeBuffer;
#endif
    uint_32             bytes_written;  /* initialized bytes in segment */
    struct asym         *labels;        /* linked list of labels in this seg */
    struct fixup        *FixupListHead; /* fixup queue head */
    struct fixup        *FixupListTail; /* fixup queue tail */
    union {
        void            *LinnumQueue;   /* for COFF line numbers */
        uint_32         fileoffset;     /* used by BIN + ELF */
        uint_32         num_linnums;    /* used by COFF (after LinnumQueue has been read) */
    };
    uint_32             num_relocs;     /* used by COFF/ELF */
    short               seg_idx;        /* segment # */
    enum seg_type       segtype;        /* segment's type (code, data, ...) */
    direct_idx          lname_idx;      /* segment's name LNAME index (OMF only) */
    direct_idx          class_name_idx; /* segment's class LNAME index */
    union {
        uint_16         abs_frame;      /* ABS seg, frame number (OMF,BIN) */
#if COMDATSUPP
        uint_16         comdat_number;  /* associated COMDAT segno (COFF) */
#endif
    };
    union {
        uint_32         abs_offset;     /* ABS seg, offset (OMF only) */
        char            *aliasname;     /* ALIAS name (COFF/ELF only) */
    };
    unsigned char       Ofssize;        /* segment's offset size */
    unsigned char       characteristics;/* used by COFF/ELF */
    unsigned char       alignment:4;    /* is value 2^x */
    unsigned char       readonly:1;     /* if segment is readonly */
    unsigned char       info:1;         /* if segment is info only (COFF/ELF) */
    unsigned char       force32:1;      /* force 32bit segdef (OMF only) */
    unsigned char       data_in_code:1; /* data items in code segm (OMF only) */
    unsigned char       written:1;      /* code/data just written */
    unsigned char       combine:3;
#if COMDATSUPP
    unsigned char       comdat_selection:3; /* COFF only */
#endif
};

#define MAX_SEGALIGNMENT 0x0F

/* PROC item */

struct proc_info {
    uint_16             *regslist;      /* PROC: list of registers to be saved */
    struct dsym         *paralist;      /* list of parameters */
    struct dsym         *locallist;     /* PROC: list of local variables */
    struct dsym         *labellist;     /* PROC: list of local labels */
    int                 parasize;       /* total no. of bytes used by parameters */
    int                 localsize;      /* PROC: total no. of bytes used by local variables */
    char                *prologuearg;   /* PROC: prologuearg attribute */
#if AMD64_SUPPORT
    struct asym         *exc_handler;   /* PROC: exc handler set by FRAME */
    int                 ReservedStack;  /* win64: additional reserved stack */
#endif
    uint_32             list_pos;       /* PROC: prologue list pos */
    union {
        unsigned char   flags;
        struct {
            unsigned char  is_vararg:1; /* if last param is VARARG */
            unsigned char  pe_type:1;   /* epilog code, 1=use LEAVE */
            unsigned char  export:1;    /* EXPORT attribute set */
            unsigned char  init:1;      /* has ExamineProc() been called? */
            unsigned char  forceframe:1;/* FORCEFRAME prologuearg? */
            unsigned char  loadds:1;    /* LOADDS prologuearg? */
            unsigned char  stackparam:1;/* for FASTCALL: 1=a stack param exists */
#if AMD64_SUPPORT
            unsigned char  isframe:1;   /* FRAME set? */
#endif
        };
    };
};

/* macro parameter */

struct mparm_list {
    //const char          *label;         /* name of parameter */
    char                *deflt;         /* optional default parm */
    unsigned char       required:1;     /* is parm required (REQ) */
};

/* macro line */

struct srcline {
    struct srcline      *next;
    uint_8              ph_count; /* placeholders contained in this line */
    char                line[1];
};

/* macro item */

struct macro_info {
    uint_16             parmcnt;    /* no of params */
    union {
        uint_16         localcnt;   /* no of locals */
        uint_16         autoexp;    /* auto-expansion flags if predefined macro */
    };
    struct mparm_list   *parmlist;  /* array of parameter items */
    struct srcline      *data;      /* prepared macro source lines */
#ifdef DEBUG_OUT
    uint_32             count;      /* no of times the macro was invoked */
#endif
    uint                srcfile;    /* sourcefile index */
};

/* STRUCT field item */

struct field_item {
    struct field_item   *next;      /* next field in STRUCT,UNION,RECORD */
    char                *initializer; /* not used by record fields */
    char                *value;
    struct asym         *sym;
};

enum type_kind {
    TYPE_NONE,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_TYPEDEF,
    TYPE_RECORD
};

struct struct_info {
    struct field_item   *head; /* start of struct's field list */
    struct field_item   *tail; /* used during parsing of struct only */
    /* v2.08: typekind moved to struct asym */
    //#ifdef __WATCOMC__
    //    enum type_kind      typekind;
    //#else
    //    uint_8              typekind;
    //#endif
    uint_8              alignment;   /* STRUCT: 1,2,4,8,16 or 32 */
    union {
        uint_8          flags;
        struct {
            unsigned char   isInline:1;  /* STRUCT/UNION: inline */
            unsigned char   isOpen:1;    /* STRUCT/UNION: set until the matching ENDS is found */
            unsigned char   OrgInside:1; /* STRUCT: struct contains an ORG */
        };
    };
};

/* dsym originally was a "directive_node"
 * However, currently all symbols are allocated as a dsym
 * the additional 3 fields are used differently depending on symbol's type.
 */

struct dsym {
    struct asym sym;
    union {
        struct seg_info     *seginfo;   /* SYM_SEG (segments) */
        struct grp_info     *grpinfo;   /* SYM_GRP (groups) */
        struct proc_info    *procinfo;  /* SYM_INTERNAL|SYM_EXTERNAL (procs, isproc=1) */
        struct struct_info  *structinfo;/* SYM_TYPE (structs, unions, typedefs, records) */
        struct macro_info   *macroinfo; /* SYM_MACRO (macros) */
        /* SYM_STACK, SYM_INTERNAL (code labels, isproc=0)
         * used to save the local hash table (contains PROC locals: params,
         * locals, labels). Details see SymGetLocal(), SymSetLocal() in symbols.c
         */
        struct dsym *nextll;
    } e;
    /* for SYM_UNDEFINED, SYM_SEG, SYM_GRP, SYM_EXTERNAL, SYM_ALIAS:
     * linked list of this type of symbol.
     * for SYM_INTERNAL:
     * linked list of labels for current segment (used for BackPatch)
     */
    struct dsym *next;
    union {
        /* for SYM_UNDEFINED, SYM_SEG, SYM_GRP, SYM_EXTERNAL, SYM_INTERNAL(procs)
         * linked list of this type of symbol, to allow fast removes.
         * Actually, the only symbols which have a "chance" to be
         * removed are SYM_UNDEFINED and SYM_EXTERNAL (weak=TRUE ) during
         * pass one.
         */
        struct dsym *prev;
        /* used by PROC for linked list */
        struct dsym *nextproc;
        /* used by PROC locals (SYM_STACK) for linked list */
        struct dsym *nextlocal;
        /* used by PROC params (SYM_STACK) for linked list */
        struct dsym *nextparam;
        /* used by SYM_EXTERNAL (weak=FALSE) if altname is set */
        struct dsym *nextext;
    };
};

extern  struct asym     *SymAlloc( const char * );
extern  void            SymFree( struct asym * );

extern  struct asym     *SymCreate( const char * );
extern  struct asym     *SymLCreate( const char * );
extern  struct asym     *SymAddGlobal( struct asym * );
extern  struct asym     *SymAddLocal( struct asym *, const char * );
extern  struct asym     *SymLookup( const char * );
extern  struct asym     *SymLookupLabel( const char *, int bDefine );

extern  struct asym     *SymFind( const char *name );
#define SymSearch(x) SymFind(x)

extern  void            SymInit( void );
extern  void            SymFini( void );
extern  void            SymPassInit( int pass );
extern  void            SymMakeAllSymbolsPublic( void );
extern  void            SymGetAll( struct asym ** );
extern  int             SymEnum( struct asym * *, int * );
extern  uint_32         SymGetCount( void );

#ifdef __WATCOMC__
typedef int (__watcall * StrCmpFunc)(const void *, const void *, unsigned );
#else
typedef int (* StrCmpFunc)(const void *, const void *, size_t );
#endif
extern StrCmpFunc SymCmpFunc;

extern  void            SymSetCmpFunc( void );
extern  void            SymClearLocal( void );
extern  void            SymSetLocal( struct asym * );
extern  void            SymGetLocal( struct asym * );

#endif
