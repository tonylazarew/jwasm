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
* Description:  Processing of segment and group related directives:
*               - SEGMENT, ENDS, GROUP
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "reswords.h"
#include "segment.h"
#include "expreval.h"
#include "omf.h"
#include "omfspec.h"
#include "fastpass.h"
#include "coffspec.h"
#include "assume.h"
#include "listing.h"
#include "msgtext.h"
#include "types.h"
#include "fixup.h"

#include "myassert.h"

extern ret_code    EndstructDirective( int, struct asm_tok tokenarray[] );

struct asym  symPC = { NULL,"$", 0 };  /* the '$' symbol */
struct asym  *symCurSeg;     /* @CurSeg symbol */

#define INIT_ATTR         0x01 /* READONLY attribute */
#define INIT_ALIGN        0x02 /* BYTE, WORD, PARA, DWORD, ... */
#define INIT_ALIGN_PARAM  (0x80 | INIT_ALIGN) /* ALIGN(x) */
#define INIT_COMBINE      0x04 /* PRIVATE, PUBLIC, STACK, COMMON */
#define INIT_COMBINE_AT   (0x80 | INIT_COMBINE) /* AT */
#if COMDATSUPP
#define INIT_COMBINE_COMDAT (0xC0 | INIT_COMBINE) /* COMDAT */
#endif
#define INIT_OFSSIZE      0x08 /* USE16, USE32, ... */
#define INIT_OFSSIZE_FLAT (0x80 | INIT_OFSSIZE) /* FLAT */
#define INIT_ALIAS        0x10 /* ALIAS(x) */
#define INIT_CHAR         0x20 /* DISCARD, SHARED, EXECUTE, ... */
#define INIT_CHAR_INFO    (0x80 | INIT_CHAR) /* INFO */

#define INIT_EXCL_MASK    0x1F  /* exclusive bits */

struct typeinfo {
    uint_8    value;      /* value assigned to the token */
    uint_8    init;       /* kind of token */
};

static const char * const SegAttrToken[] = {
#define sitem( text, value, init ) text,
#include "segattr.h"
#undef sitem
};
static const struct typeinfo SegAttrValue[] = {
#define sitem( text, value, init ) { value, init },
#include "segattr.h"
#undef sitem
};

static uint             grpdefidx;      /* Number of group definitions   */
static uint             LnamesIdx;      /* Number of LNAMES definitions  */

static struct dsym      *SegStack[MAX_SEG_NESTING]; /* stack of open segments */
static int              stkindex;       /* current top of stack */

#if FASTPASS
/* saved state */
static struct dsym      *saved_CurrSeg;
static struct dsym      **saved_SegStack;
static int              saved_stkindex;
#endif

/* generic byte buffer, used for OMF LEDATA records only */
static uint_8           codebuf[ 1024 ];
static uint_32          buffer_size; /* total size of code buffer */

/* find token in a string table */

static int FindToken( const char *token, const char * const *table, int size )
/****************************************************************************/
{
    int i;
    for( i = 0; i < size; i++, table++ ) {
        if( _stricmp( *table, token ) == 0 ) {
            return( i );
        }
    }
    return( -1 );  /* Not found */
}

void SetCurPC( struct asym *sym )
/*******************************/
{
    if( CurrStruct ) {
        //symPC.segment = NULL;
        //symPC.mem_type = MT_ABS;
        symPC.mem_type = MT_EMPTY;
        symPC.segment = NULL; /* v2.07: needed again */
        symPC.offset = CurrStruct->sym.offset + (CurrStruct->next ? CurrStruct->next->sym.offset : 0);
    } else {
        symPC.mem_type = MT_NEAR;
        symPC.segment = (struct asym *)CurrSeg;
        symPC.offset = GetCurrOffset();
    }
    DebugMsg1(("SetCurPC: curr value=%" FX32 "h\n", symPC.offset ));
}

#if 0
/* value of text macros can't be set by internal function call yet! */
void SetCurSeg( struct asym *sym )
/********************************/
{
    symCurSeg->string_ptr = CurrSeg ? CurrSeg->sym.name : "";
    DebugMsg1(("SetCurSeg: curr value=>%s<\n", symCurSeg->string_ptr ));
}
#endif

/* find a class name.
 * those names aren't in the symbol table!
 */
char *GetLname( direct_idx idx )
/******************************/
{
    struct qnode    *node;
    struct asym     *sym;

    for( node = ModuleInfo.g.LnameQueue.head; node != NULL; node = node->next ) {
        sym = (struct asym *)node->elmt;
        if( sym->state == SYM_CLASS_LNAME && sym->class_lname_idx == idx ) {
            return( sym->name );
        }
    }
    return( NULL );
}

/* find a class index.
 * the classes aren't in the symbol table!
 */
static direct_idx FindLnameIdx( const char *name )
/************************************************/
{
    struct qnode    *node;
    struct asym     *sym;

    for( node = ModuleInfo.g.LnameQueue.head; node != NULL; node = node->next ) {
        sym = (struct asym *)node->elmt;
        if( sym->state != SYM_CLASS_LNAME )
            continue;
        if( _stricmp( sym->name, name ) == 0 ) {
            return( sym->class_lname_idx );
        }
    }
    return( LNAME_NULL );
}

static void AddLnameData( struct asym *sym )
/******************************************/
{
    QAddItem( &ModuleInfo.g.LnameQueue, sym );
}

/* what's inserted into the LNAMES queue:
 * SYM_SEG: segment names
 * SYM_GRP: group names
 * SYM_CLASS_LNAME : class names
*/

void FreeLnameQueue( void )
/*************************/
{
    struct asym  *sym;
    struct qnode *curr;
    struct qnode *next;

    DebugMsg(("FreeLnameQueue enter\n" ));
    for( curr = ModuleInfo.g.LnameQueue.head; curr; curr = next ) {
        next = curr->next;
        sym = (struct asym *)curr->elmt;
        /* the class name symbols are not part of the
         * symbol table and hence must be freed now.
         */
        if( sym->state == SYM_CLASS_LNAME ) {
            SymFree( sym );
        }
        LclFree( curr );
    }
}

/* set CS assume entry whenever current segment is changed.
 * Also updates values of text macro @CurSeg.
 */

static void UpdateCurrSegVars( void )
/***********************************/
{
    struct assume_info *info;

    DebugMsg1(("UpdateCurrSegVars(%s)\n", CurrSeg ? CurrSeg->sym.name : "NULL" ));
    info = &(SegAssumeTable[ ASSUME_CS ]);
    if( CurrSeg == NULL ) {
        info->symbol = NULL;
        info->flat = FALSE;
        info->error = TRUE;
        symCurSeg->string_ptr = "";
        //symPC.segment = NULL; /* v2.05: removed */
    } else {
        info->flat = FALSE;
        info->error = FALSE;
        /* fixme: OPTION OFFSET:SEGMENT? */
        if( CurrSeg->e.seginfo->group != NULL ) {
            info->symbol = CurrSeg->e.seginfo->group;
            if ( info->symbol == &ModuleInfo.flat_grp->sym )
                info->flat = TRUE;
        } else {
            info->symbol = &CurrSeg->sym;
        }
        symCurSeg->string_ptr = CurrSeg->sym.name;
        //symPC.segment = &CurrSeg->sym; /* v2.05: removed */
    }
    return;
}

static void push_seg( struct dsym *seg )
/**************************************/
/* Push a segment into the current segment stack */
{
    //pushitem( &CurrSeg, seg ); /* changed in v1.96 */
    if ( stkindex >= MAX_SEG_NESTING ) {
        EmitError( NESTING_LEVEL_TOO_DEEP );
        return;
    }
    SegStack[stkindex] = CurrSeg;
    stkindex++;
    CurrSeg = seg;
    UpdateCurrSegVars();
    return;
}

static void pop_seg( void )
/*************************/
/* Pop a segment out of the current segment stack */
{
    //seg = popitem( &CurrSeg ); /* changed in v1.96 */
    /* it's already checked that CurrSeg is != NULL, so
     * stkindex must be > 0, but anyway ...
     */
    if ( stkindex ) {
        stkindex--;
        CurrSeg = SegStack[stkindex];
        UpdateCurrSegVars();
    }
    return;
}

/* add a class name to the queue of names */

static direct_idx InsertClassLname( const char *name )
/****************************************************/
{
    struct asym *sym;

    if( strlen( name ) > MAX_LNAME ) {
        EmitError( CLASS_NAME_TOO_LONG );
        return( LNAME_NULL );
    }

    /* the classes aren't inserted into the symbol table
     but they are in a queue */

    sym = SymAlloc( name );
    sym->state = SYM_CLASS_LNAME;
    sym->class_lname_idx = ++LnamesIdx;

    /* put it into the lname table */

    AddLnameData( sym );

    return( LnamesIdx );
}

uint_32 GetCurrOffset( void )
/***************************/
{
    return( CurrSeg ? CurrSeg->e.seginfo->current_loc : 0 );
}

#if 0
struct dsym *GetCurrSeg( void )
/*****************************/
{
    return( CurrSeg );
}
#endif

#if 0
int GetCurrClass( void )
/***********************/
{
    if( CurrSeg == NULL )
        return( 0 );
    return( CurrSeg->e.seginfo->segrec->d.segdef.class_name_idx );
}
#endif

uint_32 GetCurrSegAlign( void )
/*****************************/
{
    if( CurrSeg == NULL )
        return( 0 );
    if ( CurrSeg->e.seginfo->alignment == MAX_SEGALIGNMENT ) /* ABS? */
        return( 0x10 ); /* assume PARA alignment for AT segments */
    return( 1 << CurrSeg->e.seginfo->alignment );
}

static struct dsym *CreateGroup( const char *name )
/*************************************************/
{
    struct dsym    *grp;

    grp = (struct dsym *)SymSearch( name );

    if( grp == NULL || grp->sym.state == SYM_UNDEFINED ) {
        if ( grp == NULL )
            grp = (struct dsym *)SymCreate( name );
        else
            sym_remove_table( &SymTables[TAB_UNDEF], grp );

        grp->sym.state = SYM_GRP;
        grp->e.grpinfo = LclAlloc( sizeof( struct grp_info ) );
        grp->e.grpinfo->seglist = NULL;
        //grp->e.grpinfo->grp_idx = 0;
        //grp->e.grpinfo->lname_idx = 0;
        grp->e.grpinfo->numseg = 0;
        sym_add_table( &SymTables[TAB_GRP], grp );

        grp->sym.list = TRUE;
        grp->e.grpinfo->grp_idx = ++grpdefidx;
        grp->e.grpinfo->lname_idx = ++LnamesIdx;
        AddLnameData( &grp->sym );
    } else if( grp->sym.state != SYM_GRP ) {
        EmitErr( SYMBOL_REDEFINITION, name );
        return( NULL );
    }
    grp->sym.isdefined = TRUE;
    return( grp );
}

static struct dsym *CreateSegment( struct dsym *seg, const char *name, bool add_global )
/**************************************************************************************/
{
    if ( seg == NULL )
        seg = ( add_global ? (struct dsym *)SymCreate( name ) : (struct dsym *)SymAlloc( name ) );
    else if ( seg->sym.state == SYM_UNDEFINED )
        sym_remove_table( &SymTables[TAB_UNDEF], seg );

    if ( seg ) {
        seg->sym.state = SYM_SEG;
        seg->e.seginfo = LclAlloc( sizeof( struct seg_info ) );
        memset( seg->e.seginfo, 0, sizeof( struct seg_info ) );
        seg->e.seginfo->Ofssize = ModuleInfo.defOfssize;
        seg->e.seginfo->alignment = 4; /* this is PARA (2^4) */
        seg->e.seginfo->combine = COMB_INVALID;
        /* null class name, in case none is mentioned */
        seg->e.seginfo->class_name_idx = 1;
        seg->next = NULL;
        /* don't use sym_add_table(). Thus the "prev" member
         * becomes free for another use.
         */
        if ( SymTables[TAB_SEG].head == NULL )
            SymTables[TAB_SEG].head = SymTables[TAB_SEG].tail = seg;
        else {
            SymTables[TAB_SEG].tail->next = seg;
            SymTables[TAB_SEG].tail = seg;
        }
    }
    return( seg );
}


void DeleteGroup( struct dsym *dir )
/**********************************/
{
    struct seg_item    *curr;
    struct seg_item    *next;

    for( curr = dir->e.grpinfo->seglist; curr; curr = next ) {
        next = curr->next;
        DebugMsg(("DeleteGroup(%s): free seg_item=%p\n", dir->sym.name, curr ));
        LclFree( curr );
    }
    DebugMsg(("DeleteGroup(%s): extension %p will be freed\n", dir->sym.name, dir->e.grpinfo ));
    LclFree( dir->e.grpinfo );
    return;
}

/* handle GROUP directive */

ret_code GrpDir( int i, struct asm_tok tokenarray[] )
/***************************************************/
{
    char        *name;
    struct dsym *grp;
    struct dsym *seg;

    /* GROUP directive must be at pos 1, needs a name at pos 0 */
    if( i != 1 ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
#if COFF_SUPPORT || ELF_SUPPORT
    /* GROUP valid for OMF + BIN only */
    if ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
        || Options.output_format == OFORMAT_ELF
#endif
       ) {
        EmitError( GROUP_DIRECTIVE_INVALID_FOR_COFF );
        return( ERROR );
    }
#endif
    grp = CreateGroup( tokenarray[0].string_ptr );
    if( grp == NULL )
        return( ERROR );

    i++; /* go past GROUP */

    do {

        /* get segment name */
        if ( tokenarray[i].token != T_ID ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
        name = tokenarray[i].string_ptr;
        i++;

        seg = (struct dsym *)SymSearch( name );
        if ( Parse_Pass == PASS_1 ) {
            if( seg == NULL || seg->sym.state == SYM_UNDEFINED ) {
                seg = CreateSegment( seg, name, TRUE );
                /* inherit the offset magnitude from the group */
                if ( grp->e.grpinfo->seglist )
                    seg->e.seginfo->Ofssize = grp->sym.Ofssize;
            } else if( seg->sym.state != SYM_SEG ) {
                EmitErr( SEGMENT_EXPECTED, name );
                return( ERROR );
            } else if( seg->e.seginfo->group != NULL &&
                       seg->e.seginfo->group != &grp->sym ) {
                /* segment is in another group */
                DebugMsg(("GrpDir: segment >%s< is in group >%s< already\n", name, seg->e.seginfo->group->name));
                EmitErr( SEGMENT_IN_ANOTHER_GROUP, name );
                return( ERROR );
            }
            /* the first segment will define the group's word size */
            if( grp->e.grpinfo->seglist == NULL ) {
                grp->sym.Ofssize = seg->e.seginfo->Ofssize;
            } else if ( grp->sym.Ofssize != seg->e.seginfo->Ofssize ) {
                EmitErr( GROUP_SEGMENT_SIZE_CONFLICT, grp->sym.name, seg->sym.name );
                return( ERROR );
            }
        } else {
            /* v2.04: don't check the "defined" flag. It's for IFDEF only! */
            //if( seg == NULL || seg->sym.state != SYM_SEG || seg->sym.defined == FALSE ) {
            /* v2.07: check the "segment" field instead of "defined" flag! */
            //if( seg == NULL || seg->sym.state != SYM_SEG ) {
            if( seg == NULL || seg->sym.state != SYM_SEG || seg->sym.segment == NULL ) {
                EmitErr( SEG_NOT_DEFINED, name );
                return( ERROR );
            }
        }

        /* insert segment in group if it's not there already */
        if ( seg->e.seginfo->group == NULL ) {
            struct seg_item    *si;

            /* set the segment's grp */
            seg->e.seginfo->group = &grp->sym;

            si = LclAlloc( sizeof( struct seg_item ) );
            si->seg = seg;
            si->next = NULL;
            grp->e.grpinfo->numseg++;

            /* insert the segment at the end of linked list */
            if( grp->e.grpinfo->seglist == NULL ) {
                grp->e.grpinfo->seglist = si;
            } else {
                struct seg_item *curr;
                curr = grp->e.grpinfo->seglist;
                while( curr->next != NULL ) {
                    curr = curr->next;
                }
                curr->next = si;
            }
        }

        if ( i < Token_Count ) {
            if ( tokenarray[i].token != T_COMMA || tokenarray[i+1].token == T_FINAL ) {
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
                return( ERROR );
            }
            i++;
        }

    } while ( i < Token_Count );

    return( NOT_ERROR );
}

ret_code SetOfssize( void )
/*************************/
{
    if( CurrSeg == NULL ) {
        ModuleInfo.Ofssize = ModuleInfo.defOfssize;
    } else {
        ModuleInfo.Ofssize = CurrSeg->e.seginfo->Ofssize;
        if( ModuleInfo.Ofssize > USE16 && ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) ) {
            DebugMsg(("SetOfssize, error: CurrSeg=%s, ModuleInfo.Ofssize=%u, curr_cpu=%X, defOfssize=%u\n",
                      CurrSeg->sym.name, ModuleInfo.Ofssize, ModuleInfo.curr_cpu, ModuleInfo.defOfssize ));
            EmitError( INCOMPATIBLE_CPU_MODE_FOR_32BIT_SEGMENT );
            return( ERROR );
        }
    }

    WordSize.value = (2 << ModuleInfo.Ofssize);

#if AMD64_SUPPORT
    Set64Bit( ModuleInfo.Ofssize == USE64 );
#endif

    return( NOT_ERROR );
}

/* close segment */

static ret_code CloseSeg( const char *name )
/******************************************/
{
    //struct asym      *sym;

    DebugMsg1(("CloseSeg(%s) enter\n", name));

    if( CurrSeg == NULL || ( SymCmpFunc( CurrSeg->sym.name, name, CurrSeg->sym.name_size ) != 0 ) ) {
        DebugMsg(("CloseSeg(%s): nesting error, CurrSeg=%s\n", name, CurrSeg ? CurrSeg->sym.name : "(null)" ));
        EmitErr( BLOCK_NESTING_ERROR, name );
        return( ERROR );
    }

    DebugMsg1(("CloseSeg(%s): current ofs=%" FX32 "\n", name, CurrSeg->e.seginfo->current_loc));

    if ( write_to_file && ( Options.output_format == OFORMAT_OMF ) ) {

        //if ( !omf_FlushCurrSeg() ) /* v2: error check is obsolete */
        //    EmitErr( INTERNAL_ERROR, "CloseSeg", 1 ); /* coding error! */
        omf_FlushCurrSeg();
        if ( Options.no_comment_data_in_code_records == FALSE )
            omf_OutSelect( FALSE );
    }

    pop_seg();

    return( NOT_ERROR );
}

void DefineFlatGroup( void )
/**************************/
{
    if( ModuleInfo.flat_grp == NULL ) {
        /* can't fail because <FLAT> is a reserved word */
        ModuleInfo.flat_grp = CreateGroup( "FLAT" );
        ModuleInfo.flat_grp->sym.Ofssize = ModuleInfo.defOfssize;
        //ModuleInfo.flatgrp_idx = ModuleInfo.flat_grp->e.grpinfo->grp_idx;
    }
}

uint GetSegIdx( const struct asym *sym )
/**************************************/
/* get idx to sym's segment */
{
    if( sym )
        return( ((struct dsym *)sym)->e.seginfo->seg_idx );
    return( 0 );
}

struct asym *GetGroup( const struct asym *sym )
/*********************************************/
/* get a symbol's group */
{
    struct dsym  *curr;

    curr = GetSegm( sym );
    if( curr != NULL )
        return( curr->e.seginfo->group );
    return( NULL );
}

int GetSymOfssize( const struct asym *sym )
/*****************************************/
/* get sym's offset size (64=2, 32=1, 16=0) */
{
    struct dsym   *curr;

    /* v2.07: MT_ABS has been removed */
    //if ( sym->mem_type == MT_ABS )
    //    return( USE16 );

    curr = GetSegm( sym );
    if( curr == NULL ) {
        /* v2.04: SYM_STACK added */
        //if( sym->state == SYM_EXTERNAL || ( sym->state == SYM_INTERNAL && sym->isproc ) || sym->state == SYM_GRP )
        if( sym->state == SYM_EXTERNAL )
            return( sym->seg_ofssize );
        if( sym->state == SYM_STACK || sym->state == SYM_GRP )
            return( sym->Ofssize );
        if( sym->state == SYM_SEG  )
            return( ((struct dsym *)sym)->e.seginfo->Ofssize );
        /* v2.07: added */
        if ( sym->mem_type == MT_EMPTY )
            return( USE16 );
    } else {
        return( curr->e.seginfo->Ofssize );
    }
    return( ModuleInfo.Ofssize );
}

void SetSymSegOfs( struct asym *sym )
/***********************************/
{
    sym->segment = &CurrSeg->sym;
    sym->offset = GetCurrOffset();
}


static enum seg_type TypeFromClassName( const struct dsym *dir, const char *name )
/********************************************************************************/
{
    int     slen;
    char    uname[MAX_ID_LEN+1];

    if ( dir->e.seginfo->alignment == MAX_SEGALIGNMENT )
        return( SEGTYPE_ABS );

    /* v2.03: added */
    if ( dir->e.seginfo->combine == COMB_STACK )
        return( SEGTYPE_STACK );

    if( name == NULL )
        return( SEGTYPE_UNDEF );

    if( _stricmp( name, GetCodeClass() ) == 0 )
        return( SEGTYPE_CODE );

    slen = strlen( name );
    strcpy( uname, name );
    _strupr( uname );
    switch( slen ) {
    default:
    case 5:
        if( memcmp( uname, "CONST", 6 ) == 0 )
            return( SEGTYPE_DATA );
        //if( memcmp( uname, "STACK", 6 ) == 0 )
        //    return( SEGTYPE_DATA );
        if( memcmp( uname, "DBTYP", 6 ) == 0 )
            return( SEGTYPE_DATA );
        if( memcmp( uname, "DBSYM", 6 ) == 0 )
            return( SEGTYPE_DATA );
    case 4:
        /* v2.03: changed */
        //if( memcmp( uname , "CODE", 5 ) == 0 )
        //    return( SEGTYPE_CODE );
        if( memcmp( uname + slen - 4, "CODE", 4 ) == 0 )
            return( SEGTYPE_CODE );
        if( memcmp( uname + slen - 4, "DATA", 4 ) == 0 )
            return( SEGTYPE_DATA );
    case 3:
        if( memcmp( uname + slen - 3, "BSS", 3 ) == 0 )
            return( SEGTYPE_BSS );
    case 2:
    case 1:
    case 0:
        return( SEGTYPE_UNDEF );
    }
}

#if 0   /* v2.03: obsolete */

/* get the type of the segment by checking it's name.
 * this is called only if the class gives no hint
 */
static enum seg_type TypeFromSegmentName( const char *name )
/**********************************************************/
{
    int     slen;
    char    uname[MAX_ID_LEN+1];

    slen = strlen( name );
    strcpy( uname, name );
    _strupr( uname );
    switch( slen ) {
    default:
    case 5:
        if ( Options.output_format != OFORMAT_COFF ) {
            /* '..._TEXT'? */
            if( memcmp( uname + slen - 5, SegmNamesDef[SIM_CODE], 5 ) == 0 )
                return( SEGTYPE_CODE );
        }
        /* '..._DATA' */
        if( memcmp( uname + slen - 5, SegmNamesDef[SIM_DATA], 5 ) == 0 )
            return( SEGTYPE_DATA );
        /* 'CONST' */
        if( memcmp( uname + slen - 5, SegmNamesDef[SIM_CONST], 5 ) == 0 )
            return( SEGTYPE_DATA );
    case 4:
        if ( Options.output_format != OFORMAT_COFF ) {
            /* '..._BSS' */
            if( memcmp( uname + slen - 4, "_BSS", 4 ) == 0 )
                return( SEGTYPE_BSS );
        }
    case 3:
    case 2:
    case 1:
    case 0:
        return( SEGTYPE_UNDEF );
    }
}
#endif

/* set the segment's class. report an error if the class has been set
 * already and the new value differs. */

static direct_idx SetSegmentClass( struct asym *seg, const char *classname )
/**************************************************************************/
{
    direct_idx          classidx;

    classidx = FindLnameIdx( classname );
    if( classidx == LNAME_NULL ) {
        classidx = InsertClassLname( classname );
        if( classidx == LNAME_NULL ) {
            return( ERROR );
        }
    }
    /* default class name index is 1, which is the NULL class name */
    if ( ((struct dsym *)seg)->e.seginfo->class_name_idx == 1 )
        ((struct dsym *)seg)->e.seginfo->class_name_idx = classidx;
    else if ( ((struct dsym *)seg)->e.seginfo->class_name_idx != classidx ) {
        EmitErr( SEGDEF_CHANGED, seg->name, MsgGetEx( TXT_CLASS ) );
        return( ERROR );
    }
    return( classidx );
}

/* CreateIntSegment(), used for internally defined segments:
 * codeview debugging segments, COFF .drectve, COFF .sxdata
 */

struct asym *CreateIntSegment( const char *name, const char *classname, uint_8 alignment, uint_8 Ofssize, bool add_global )
/*************************************************************************************************************************/
{
    struct dsym *seg;
    if ( add_global ) {
        seg = (struct dsym *)SymSearch( name );
        if ( seg == NULL || seg->sym.state == SYM_UNDEFINED )
            seg = CreateSegment( seg, name, add_global );
        else if ( seg->sym.state != SYM_SEG ) {
            EmitErr( SYMBOL_REDEFINITION, name );
            return( NULL );
        }
    } else
        seg = CreateSegment( NULL, name, FALSE );
    if ( seg ) {
        if( seg->e.seginfo->lname_idx == 0 ) {
            seg->e.seginfo->seg_idx = ++ModuleInfo.g.num_segs;
            seg->e.seginfo->lname_idx = ++LnamesIdx;
            AddLnameData( &seg->sym );
        }
        seg->sym.segment = &seg->sym;
        seg->e.seginfo->alignment = alignment;
        seg->e.seginfo->Ofssize = Ofssize;
        SetSegmentClass( (struct asym *)seg, classname );
        return( &seg->sym );
    }
    return( NULL );
}

/* ENDS directive */

ret_code EndsDir( int i, struct asm_tok tokenarray[] )
/****************************************************/
{
    if( CurrStruct != NULL ) {
        return( EndstructDirective( i, tokenarray ) );
    }
    /* a label must precede ENDS */
    if( i != 1 ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    if ( Parse_Pass != PASS_1 ) {
        if ( ModuleInfo.list )
            LstWrite( LSTTYPE_LABEL, 0, NULL );
    }
    if ( CloseSeg( tokenarray[0].string_ptr ) == ERROR )
        return( ERROR );
    i++;
    if ( tokenarray[i].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
    }
    return( SetOfssize() );
}

/* SEGMENT directive if pass is > 1 */

static ret_code SetCurrSeg( int i, struct asm_tok tokenarray[] )
/**************************************************************/
{
    struct asym *sym;

    sym = SymSearch( tokenarray[0].string_ptr );
    DebugMsg1(("SetCurrSeg(%s) sym=%p\n", tokenarray[0].string_ptr, sym));
    if ( sym == NULL || sym->state != SYM_SEG ) {
        EmitErr( SEG_NOT_DEFINED, tokenarray[0].string_ptr );
        return( ERROR );
    }
    /* v2.04: added */
    sym->isdefined = TRUE;
    if ( CurrSeg && Options.output_format == OFORMAT_OMF ) {
        omf_FlushCurrSeg();
        if ( Options.no_comment_data_in_code_records == FALSE )
            omf_OutSelect( FALSE );
    }
    push_seg( (struct dsym *)sym );

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_LABEL, 0, NULL );

    return( SetOfssize() );
}

static void UnlinkSeg( struct dsym *dir )
/***************************************/
{
    struct dsym *curr;
    for ( curr = SymTables[TAB_SEG].head; curr; curr = curr->next )
        if ( curr == dir ) {
            SymTables[TAB_SEG].head = dir->next;
            break;
        } else if ( curr->next == dir ) {
            curr->next = dir->next;
            break;
        }
    /* if segment is last, set a new tail */
    if ( dir->next == NULL ) {
        for ( curr = SymTables[TAB_SEG].head; curr && curr->next; curr = curr->next );
        SymTables[TAB_SEG].tail = curr;
    }
    return;
}

/* SEGMENT directive */

ret_code SegmentDir( int i, struct asm_tok tokenarray[] )
/*******************************************************/
{
    char                is_old;
    char                *token;
    int                 typeidx;
    const struct typeinfo *type;          /* type of option */
    int                 temp;
    int                 temp2;
    uint                initstate = 0;  /* flags for attribute initialization */
    unsigned char       oldreadonly;    /* readonly value of a defined segment */
    //unsigned char       oldsegtype;
    unsigned char       oldOfssize;
    char                oldalign;
    char                oldcombine;
    uint                oldclassidx;
    uint_8              oldcharacteristics;
    struct dsym         *dir;
    char                *name;
    struct asym         *sym;
    struct expr         opndx;

    if ( Parse_Pass != PASS_1 )
        return( SetCurrSeg( i, tokenarray ) );

    if( i != 1 ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    name = tokenarray[0].string_ptr;

    DebugMsg1(("SegmentDir(%s) enter: ModuleInfo.Ofssize=%u, num_seg=%u\n", name, ModuleInfo.Ofssize, ModuleInfo.g.num_segs ));

    /* See if the segment is already defined */
    sym = SymSearch( name );
    if( sym == NULL || sym->state == SYM_UNDEFINED ) {
        /* segment is not defined (yet) */
        sym = (struct asym *)CreateSegment( (struct dsym *)sym, name, TRUE );
        sym->list = TRUE; /* always list segments */
        dir = (struct dsym *)sym;
        dir->e.seginfo->seg_idx = ++ModuleInfo.g.num_segs;
        is_old = FALSE;
        /*
         * initialize segment with values from the one without suffix
         */
#if COFF_SUPPORT || ELF_SUPPORT
        if (Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
            || Options.output_format == OFORMAT_ELF
#endif
           ) {
            char *p;
            if ( p = strchr(sym->name, '$') ) {
                char buffer[MAX_ID_LEN+1];
                struct dsym *dir2;
                memcpy(buffer, sym->name, p - sym->name);
                buffer[p - sym->name] = NULLC;
                if ((dir2 = (struct dsym *)SymSearch(buffer)) && dir2->sym.state == SYM_SEG) {
                    dir->e.seginfo->readonly = dir2->e.seginfo->readonly;
                    dir->e.seginfo->segtype  = dir2->e.seginfo->segtype;
                    dir->e.seginfo->Ofssize  = dir2->e.seginfo->Ofssize;
                    dir->e.seginfo->alignment= dir2->e.seginfo->alignment;
                    dir->e.seginfo->characteristics = dir2->e.seginfo->characteristics;
                    dir->e.seginfo->combine         = dir2->e.seginfo->combine;
                    dir->e.seginfo->class_name_idx  = dir2->e.seginfo->class_name_idx;
                }
            }
        }
#endif
    } else if ( sym->state == SYM_SEG ) {
        /* segment already defined */
        dir = (struct dsym *)sym;
        is_old = TRUE;
        oldreadonly = dir->e.seginfo->readonly;
        //oldsegtype  = dir->e.seginfo->segtype;
        oldOfssize  = dir->e.seginfo->Ofssize;
        oldalign    = dir->e.seginfo->alignment;
        oldcharacteristics = dir->e.seginfo->characteristics;
        oldcombine  = dir->e.seginfo->combine;
        oldclassidx = dir->e.seginfo->class_name_idx;
        if( dir->e.seginfo->lname_idx == 0 ) {
            /* segment was mentioned in a group statement, but not really set up */
            is_old = FALSE;
            /* the segment list is to be sorted.
             * So unlink the segment and add it at the end.
             */
            UnlinkSeg( dir );
            dir->e.seginfo->seg_idx = ++ModuleInfo.g.num_segs;
            dir->next = NULL;
            if ( SymTables[TAB_SEG].head == NULL )
                SymTables[TAB_SEG].head = SymTables[TAB_SEG].tail = dir;
            else {
                SymTables[TAB_SEG].tail->next = dir;
                SymTables[TAB_SEG].tail = dir;
            }
        }
    } else {
        /* symbol is different kind, error */
        DebugMsg(("SegmentDir(%s): symbol redefinition\n", name ));
        EmitErr( SYMBOL_REDEFINITION, name );
        return( ERROR );
    }

    i++; /* go past SEGMENT */

    for( ; i < Token_Count; i++ ) {
        token = tokenarray[i].string_ptr;
        DebugMsg1(("SegmentDir(%s): i=%u, string=%s token=%X\n", name, i, token, tokenarray[i].token ));
        if( tokenarray[i].token == T_STRING ) {

            /* the class name - the only token which is of type STRING */
            /* string must be delimited by [double]quotes */
            if ( tokenarray[i].string_delim != '"' &&
                tokenarray[i].string_delim != '\'' ) {
                EmitErr( SYNTAX_ERROR_EX, token );
                continue;
            }
            /* remove the quote delimiters */
            token++;
            *(token+tokenarray[i].stringlen) = NULLC;

            SetSegmentClass( &dir->sym, token );

            DebugMsg1(("SegmentDir(%s): class found: %s\n", name, token ));
            continue;
        }

        /* check the rest of segment attributes.
         */
        typeidx = FindToken( token, SegAttrToken, sizeof( SegAttrToken )/sizeof( SegAttrToken[0] ) );
        if( typeidx < 0 ) {
            EmitErr( UNKNOWN_SEGMENT_ATTRIBUTE, token );
            continue;
        }
        type = &SegAttrValue[typeidx];

        /* initstate is used to check if any field is already
         * initialized
         */
        if( initstate & INIT_EXCL_MASK & type->init ) {
            EmitErr( SEGMENT_ATTRIBUTE_DEFINED_ALREADY, token );
            continue;
        } else {
            initstate |= type->init; /* mark it initialized */
        }

        switch ( type->init ) {
        case INIT_ATTR:
            dir->e.seginfo->readonly = TRUE;
            break;
        case INIT_ALIGN:
            DebugMsg1(("SegmentDir(%s): align attribute found\n", name ));
            dir->e.seginfo->alignment = type->value;
            break;
        case INIT_ALIGN_PARAM:
            DebugMsg1(("SegmentDir(%s): ALIGN() found\n", name ));
            if ( Options.output_format == OFORMAT_OMF ) {
                EmitErr( NOT_SUPPORTED_WITH_OMF_FORMAT, tokenarray[i].string_ptr );
                i = Token_Count; /* stop further parsing of this line */
                break;
            }
            i++;
            if ( tokenarray[i].token != T_OP_BRACKET ) {
                EmitErr( EXPECTED, "(" );
                break;
            }
            i++;
            if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
                break;
            if ( tokenarray[i].token != T_CL_BRACKET ) {
                EmitErr( EXPECTED, ")" );
                break;
            }
            if ( opndx.kind != EXPR_CONST ) {
                EmitError( CONSTANT_EXPECTED );
                break;
            }
            /*
             COFF allows alignment values
             1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192
             */
            for( temp = 1, temp2 = 0; temp < opndx.value && temp < 8192 ; temp <<= 1, temp2++ );
            if( temp != opndx.value ) {
                EmitError( POWER_OF_2 );
            }
            dir->e.seginfo->alignment = temp2;
            break;
        case INIT_COMBINE:
            DebugMsg1(("SegmentDir(%s): combine attribute found\n", name ));
            dir->e.seginfo->combine = type->value;
            break;
        case INIT_COMBINE_AT:
            DebugMsg1(("SegmentDir(%s): AT found\n", name ));
            dir->e.seginfo->combine = type->value;
            /* v2.05: always use MAX_SEGALIGNMENT */
            //dir->e.seginfo->alignment = -1;
            dir->e.seginfo->alignment = MAX_SEGALIGNMENT;
            i++;
            if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) != ERROR ) {
                if ( opndx.kind == EXPR_CONST ) {
                    dir->e.seginfo->abs_frame = opndx.value;
                    dir->e.seginfo->abs_offset = 0;
                } else {
                    EmitError( CONSTANT_EXPECTED );
                }
            }
            break;
#if COMDATSUPP
        case INIT_COMBINE_COMDAT:
            DebugMsg1(("SegmentDir(%s): COMDAT found\n", name ));
            if ( Options.output_format != OFORMAT_COFF ) {
                EmitErr( NOT_SUPPORTED_WITH_CURR_FORMAT, tokenarray[i].string_ptr );
                i = Token_Count; /* stop further parsing of this line */
                break;
            }
            i++;
            if ( tokenarray[i].token != T_OP_BRACKET ) {
                EmitErr( EXPECTED, "(" );
                break;
            }
            i++;
            if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
                break;
            if ( opndx.kind != EXPR_CONST ) {
                EmitError( CONSTANT_EXPECTED );
                i = Token_Count; /* stop further parsing of this line */
                break;
            }
            if ( opndx.value < 1 || opndx.value > 6 ) {
                EmitErr( VALUE_NOT_WITHIN_ALLOWED_RANGE, "1-6" );
            } else {
                /* if value is IMAGE_COMDAT_SELECT_ASSOCIATIVE,
                 * get the associated segment name argument.
                 */
                if ( opndx.value == 5 ) {
                    struct asym *sym2;
                    if ( tokenarray[i].token != T_COMMA ) {
                        EmitError( EXPECTING_COMMA );
                        i = Token_Count; /* stop further parsing of this line */
                        break;
                    }
                    i++;
                    if ( tokenarray[i].token != T_ID ) {
                        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
                        i = Token_Count; /* stop further parsing of this line */
                        break;
                    }
                    /* associated segment must be COMDAT, but not associative */
                    sym2 = SymSearch( tokenarray[i].string_ptr );
                    if ( sym2 == NULL ||
                        sym2->state != SYM_SEG ||
                        ((struct dsym *)sym2)->e.seginfo->comdat_selection == 0 ||
                        ((struct dsym *)sym2)->e.seginfo->comdat_selection == 5 )
                        EmitErr( INVALID_ASSOCIATED_SEGMENT, tokenarray[i].string_ptr );
                    else
                        dir->e.seginfo->comdat_number = ((struct dsym *)sym2)->e.seginfo->seg_idx;
                    i++;
                }
            }
            if ( tokenarray[i].token != T_CL_BRACKET ) {
                EmitErr( EXPECTED, ")" );
                break;
            }
            dir->e.seginfo->comdat_selection = opndx.value;
            dir->e.seginfo->combine = type->value;
            break;
#endif
        case INIT_OFSSIZE:
        case INIT_OFSSIZE_FLAT:
            /* v2.07: check for compatible cpu mode */
            if ( type->value == USE32 && ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 )
#if AMD64_SUPPORT
                || type->value == USE64 && ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_64 )
#endif
               ) {
                EmitError( INSTRUCTION_OR_REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
                break;
            }
            if ( type->init == INIT_OFSSIZE_FLAT ) {
                DefineFlatGroup();
#if AMD64_SUPPORT
                dir->e.seginfo->Ofssize = ModuleInfo.defOfssize;
#else
                dir->e.seginfo->Ofssize = USE32;
#endif
                /* put the segment into the FLAT group.
                 * this is not quite Masm-compatible, because trying to put
                 * the segment into another group will cause an error.
                 */
                dir->e.seginfo->group = &ModuleInfo.flat_grp->sym;
            } else
                dir->e.seginfo->Ofssize = type->value;
            break;
#if COFF_SUPPORT || ELF_SUPPORT
        case INIT_CHAR_INFO:
            dir->e.seginfo->info = TRUE;
            break;
        case INIT_CHAR:
            DebugMsg1(("SegmentDir(%s): characteristics found\n", name ));
            ; /* characteristics are restricted to COFF/ELF */
            if ( Options.output_format == OFORMAT_OMF || Options.output_format == OFORMAT_BIN ) {
                EmitErr( NOT_SUPPORTED_WITH_CURR_FORMAT, tokenarray[i].string_ptr );
            } else
                dir->e.seginfo->characteristics |= type->value;
            break;
        case INIT_ALIAS:
            DebugMsg1(("SegmentDir(%s): ALIAS found\n", name ));
            if ( Options.output_format != OFORMAT_COFF &&
                Options.output_format != OFORMAT_ELF ) {
                EmitErr( NOT_SUPPORTED_WITH_CURR_FORMAT, tokenarray[i].string_ptr );
                i = Token_Count; /* stop further parsing of this line */
                break;
            }
            i++;
            if ( tokenarray[i].token != T_OP_BRACKET ) {
                EmitErr( EXPECTED, "(" );
                break;
            }
            i++;
            if ( tokenarray[i].token != T_STRING ||
                ( tokenarray[i].string_delim != '"' &&
                tokenarray[i].string_delim != '\'' ) ) {
                EmitErr( SYNTAX_ERROR_EX, token );
                i = Token_Count; /* stop further parsing of this line */
                break;
            }
            temp = i;
            i++;
            if ( tokenarray[i].token != T_CL_BRACKET ) {
                EmitErr( EXPECTED, ")" );
                break;
            }
            dir->e.seginfo->aliasname = LclAlloc( tokenarray[temp].stringlen );
            memcpy( dir->e.seginfo->aliasname, tokenarray[temp].string_ptr+1, tokenarray[temp].stringlen );
            *(dir->e.seginfo->aliasname+tokenarray[temp].stringlen) = NULLC;
            break;
#endif
#ifdef DEBUG_OUT
        default: /* shouldn't happen */
            myassert( 0 );
            break;
#endif
        }
    } /* end for */

    /* make a guess about the segment's type */
    if( dir->e.seginfo->segtype != SEGTYPE_CODE ) {
        enum seg_type res;

        token = GetLname( dir->e.seginfo->class_name_idx );
        res = TypeFromClassName( dir, token );
        if( res != SEGTYPE_UNDEF ) {
            dir->e.seginfo->segtype = res;
        }
#if 0 /* v2.03: removed */
        else {
            res = TypeFromSegmentName( name );
            dir->e.seginfo->segtype = res;
        }
#endif
    }

    if( is_old ) {
        int txt = 0;

        /* Check if new definition is different from previous one */

        // oldobj = dir->e.seginfo->segrec;
        if(  oldreadonly      != dir->e.seginfo->readonly )
            txt = TXT_READONLY;
        else if ( oldalign    != dir->e.seginfo->alignment )
            txt = TXT_ALIGNMENT;
        else if ( oldcombine  != dir->e.seginfo->combine )
            txt = TXT_COMBINE;
        else if ( oldOfssize  != dir->e.seginfo->Ofssize )
            txt = TXT_SEG_WORD_SIZE;
        else if ( oldclassidx != dir->e.seginfo->class_name_idx )
            txt = TXT_CLASS; /* Masm warns only! */
        else if ( oldcharacteristics != dir->e.seginfo->characteristics )
            txt = TXT_CHARACTERISTICS;

        if ( txt ) {
            EmitErr( SEGDEF_CHANGED, dir->sym.name, MsgGetEx( txt ) );
            //return( ERROR ); /* v2: display error, but continue */
        }

    } else {
        /* A new definition */

        sym = &dir->sym;
        sym->isdefined = TRUE;
        sym->segment = sym;
        sym->offset = 0;
        if( dir->e.seginfo->lname_idx == 0 ) {
            dir->e.seginfo->lname_idx = ++LnamesIdx;
            AddLnameData( sym );
        }

    }
    push_seg( dir ); /* set CurrSeg */

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_LABEL, 0, NULL );

    return( SetOfssize() );
}

/* END directive has been found. */

ret_code SegmentModuleExit( void )
/********************************/
{
    if ( ModuleInfo.model != MODEL_NONE )
        ModelSimSegmExit();
    /* if there's still an open segment, it's an error */
    if ( CurrSeg ) {
        EmitErr( BLOCK_NESTING_ERROR, CurrSeg->sym.name );
        /* but close the still open segments anyway */
        while( CurrSeg && ( CloseSeg( CurrSeg->sym.name ) == NOT_ERROR ) );
    }

    return( NOT_ERROR );
}

/* this is called once per module after the last pass is finished */

#ifdef DEBUG_OUT
void SegmentFini( void )
/**********************/
{
#if FASTPASS
#if FASTMEM==0
    struct dsym    *curr;
    /* this is debugging code only. Usually FASTPASS and FASTMEM
     * are both either TRUE or FALSE.
     * It's active if both DEBUG and TRMEM is set in Makefile.
     */
    DebugMsg(("SegmentFini() enter\n"));
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        struct fixup *fix;
        DebugMsg(("SegmentFini(): segment %s\n", curr->sym.name ));
        for ( fix = curr->e.seginfo->FixupListHead; fix ; ) {
            struct fixup *next = fix->nextrlc;
            LclFree( fix );
            fix = next;
        }
    }
    DebugMsg(("SegmentFini() exit\n"));
#endif
#endif
}
#endif

/* init. called for each pass */

void SegmentInit( int pass )
/**************************/
{
    struct dsym *curr;
    uint_32     i;
#ifdef __I86__
    char __huge *p;
#else
    char        *p;
#endif
    //struct fixup *fix;

    DebugMsg(("SegmentInit(%u) enter\n", pass ));
    CurrSeg      = NULL;
    stkindex     = 0;

    if ( pass == PASS_1 ) {
        grpdefidx   = 0;
        LnamesIdx   = 1; /* the first Lname is a null-string */
        //pCodeBuff = NULL;
        buffer_size = 0;
        //flat_grp    = NULL;

        symPC.sfunc_ptr = &SetCurPC;
        symPC.mem_type = MT_NEAR;
        symPC.state = SYM_INTERNAL;
        symPC.isdefined = TRUE;
        symPC.predefined = TRUE;
        symPC.variable = TRUE; /* added v1.96. Important for fixup creation */
#if FASTMEM==0
        symPC.staticmem = TRUE;
#endif
        symPC.name_size = 1; /* sizeof("$") */
        symPC.list = FALSE; /* don't display the '$' symbol in symbol list */
        SymAddGlobal( &symPC );

#if 0 /* v2.03: obsolete, also belongs to simplified segment handling */
        /* set ModuleInfo.code_class */
        if( Options.code_class  )
            size = strlen( Options.code_class ) + 1;
        else
            size = 4 + 1;
        ModuleInfo.code_class = LclAlloc( size );
        if ( Options.code_class )
            strcpy( ModuleInfo.code_class, Options.code_class );
        else
            strcpy( ModuleInfo.code_class, "CODE" );
#endif
    }

    /*
     * alloc a buffer for the contents
     */

    if ( ModuleInfo.pCodeBuff == NULL && Options.output_format != OFORMAT_OMF ) {
        for( curr = SymTables[TAB_SEG].head, buffer_size = 0; curr; curr = curr->next ) {
            /* v2.04: can't happen */
            //if( ( curr->sym.state != SYM_SEG ) || ( curr->sym.segment == NULL ) )
            //    continue;
            if ( curr->e.seginfo->bytes_written ) {
                i = curr->sym.max_offset - curr->e.seginfo->start_loc;
                /* the segment can grow in step 2-n due to forward references.
                 * for a quick solution just add 25% to the size if segment
                 * is a code segment. (v2.02: previously if was added only if
                 * code segment contained labels, but this isn't sufficient.)
                 */
                //if ( curr->e.seginfo->labels ) /* v2.02: changed */
                if ( curr->e.seginfo->segtype == SEGTYPE_CODE )
                    i = i + (i >> 2);
                DebugMsg(("SegmentInit(%u), %s: max_ofs=%" FX32 ", alloc_size=%" FX32 "h\n", pass, curr->sym.name, curr->sym.max_offset, i ));
                buffer_size += i;
            }
        }
        if ( buffer_size ) {
            ModuleInfo.pCodeBuff = LclAlloc( buffer_size );
            DebugMsg(("SegmentInit(%u): total buffer size=%" FX32 ", start=%p\n", pass, buffer_size, ModuleInfo.pCodeBuff ));
        }
    }
    /* Reset length of all segments to zero.
     * set start of segment buffers.
     */
#if FASTMEM==0
    /* fastmem clears the memory blocks, but malloc() won't */
    if ( ModuleInfo.pCodeBuff )
        memset( ModuleInfo.pCodeBuff, 0, buffer_size );
#endif
    for( curr = SymTables[TAB_SEG].head, p = ModuleInfo.pCodeBuff; curr; curr = curr->next ) {
        /* v2.04: can't happen */
        //if( ( curr->sym.state != SYM_SEG ) || ( curr->sym.segment == NULL ) ) {
        //    DebugMsg(("SegmentInit(%u): strange item in segment queue >%s<\n", pass, curr->sym.name ));
        //    continue;
        //}
        if ( curr->e.seginfo->bytes_written ) {
            if ( Options.output_format == OFORMAT_OMF ) {
                curr->e.seginfo->CodeBuffer = codebuf;
                DebugMsg(("SegmentInit(%u), %s: buffer=%p\n", pass, curr->sym.name, codebuf ));
            } else {
                curr->e.seginfo->CodeBuffer = p;
                i = curr->sym.max_offset - curr->e.seginfo->start_loc;
                DebugMsg(("SegmentInit(%u), %s: size=%" FX32 " buffer=%p\n", pass, curr->sym.name, i, p ));
                p += i;
            }
        }
        if( curr->e.seginfo->combine != COMB_STACK ) {
            curr->sym.max_offset = 0;
        }
        if ( Options.output_format == OFORMAT_OMF ) { /* v2.03: do this selectively */
            curr->e.seginfo->start_loc = 0;
            curr->e.seginfo->data_in_code = FALSE;
        }
        curr->e.seginfo->current_loc = 0;
        curr->e.seginfo->bytes_written = 0;

        //if ( Options.output_format != OFORMAT_OMF ) {
            curr->e.seginfo->FixupListHead = NULL;
            curr->e.seginfo->FixupListTail = NULL;
        //}
    }

    ModuleInfo.Ofssize = USE16;

#if FASTPASS
    if ( pass != PASS_1 && UseSavedState == TRUE ) {
        CurrSeg = saved_CurrSeg;
        stkindex = saved_stkindex;
        if ( stkindex )
            memcpy( &SegStack, saved_SegStack, stkindex * sizeof(struct dsym *) );

        //symCurSeg->string_ptr = saved_CurSeg_name;

        UpdateCurrSegVars();
    }
#endif
}
#if FASTPASS
void SegmentSaveState( void )
/***************************/
{
    int i;

    i = stkindex;

    saved_CurrSeg = CurrSeg;
    saved_stkindex = stkindex;
    if ( stkindex ) {
        saved_SegStack = LclAlloc( stkindex * sizeof(struct dsym *) );
        memcpy( saved_SegStack, &SegStack, stkindex * sizeof(struct dsym *) );
    }

    //saved_CurSeg_name  = symCurSeg->string_ptr;
}
#endif
