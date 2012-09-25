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
* Description:  handle OMF output format.
*
****************************************************************************/

#include <ctype.h>
#include <time.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "segment.h"
#include "mangle.h"
#include "extern.h"
#include "fixup.h"
#include "omf.h"
#include "omfint.h"
#include "omfspec.h"
#include "fastpass.h"
#include "myassert.h"
#include "tokenize.h" /* needed because of StringBufferEnd usage */
#include "input.h"
#include "linnum.h"

#define SEPARATE_FIXUPP_16_32
#define TRUNCATE 1
#define MANGLE_BYTES 8 /* extra size required for name decoration */
#define MAX_ID_LEN_OMF 247

#if TRUNCATE
#if defined(__UNIX__) || defined(__CYGWIN__) || defined(__DJGPP__)
#include "unistd.h"
#else
#include "io.h"
#endif
#endif

#define TruncRec(objr)       (void)( (objr)->length = (objr)->curoff )

enum {
    TIME_SEC_B  = 0,
    TIME_SEC_F  = 0x001f,
    TIME_MIN_B  = 5,
    TIME_MIN_F  = 0x07e0,
    TIME_HOUR_B = 11,
    TIME_HOUR_F = 0xf800
};

enum {
    DATE_DAY_B  = 0,
    DATE_DAY_F  = 0x001f,
    DATE_MON_B  = 5,
    DATE_MON_F  = 0x01e0,
    DATE_YEAR_B = 9,
    DATE_YEAR_F = 0xfe00
};

union DOS_DATETIME {
    struct {
        unsigned short time;
        unsigned short date;
    } dos;
    time_t timet;
};

extern void cv_write_debug_tables( struct dsym *, struct dsym *);

extern struct qdesc LinnumQueue;    /* queue of line_num_info items */

//extern struct fname_list *FNames;
//extern uint       cnt_fnames;

extern const char szNull[];

struct omf_wfile *file_out;
uint_32       LastCodeBufSize;

static unsigned long    seg_pos;        /* file pos of SEGDEF record(s) */
static unsigned long    public_pos;     /* file pos of PUBDEF record(s) */
static unsigned long    end_of_header;  /* file pos of "end of header"  */

static const char szCVSymbols[]  = { "$$SYMBOLS"};
static const char szCVTypes[]    = { "$$TYPES"};
static const char szCVSymClass[] = { "DEBSYM" };
static const char szCVTypClass[] = { "DEBTYP" };

static void InitRec( struct omf_rec *obj, uint_8 command )
/********************************************************/
{
    obj->command = command;
    obj->data = NULL;
    obj->length = 0;
    obj->curoff = 0;
    obj->is_32 = FALSE;
    DebugMsg(("InitRec(%p, %X)\n", obj, command ));
    return;
}

static time_t timet2dostime( time_t x )
/*************************************/
{
    struct tm *    ltime;
    union DOS_DATETIME dt;

    ltime = localtime( &x );
    dt.dos.date = (( ltime->tm_year - 80 ) << DATE_YEAR_B )
             | (( ltime->tm_mon + 1 ) << DATE_MON_B )
             | (( ltime->tm_mday ) << DATE_DAY_B );
    dt.dos.time = (( ltime->tm_hour ) << TIME_HOUR_B )
             | (( ltime->tm_min ) << TIME_MIN_B )
             | (( ltime->tm_sec / 2 ) << TIME_SEC_B );
    return( dt.timet );
}

static void Put8( struct omf_rec *objr, uint_8 byte )
/***************************************************/
{
/**/myassert( objr != NULL && objr->data != NULL );
    objr->data[ objr->curoff++ ] = byte;
}

static void Put16( struct omf_rec *objr, uint_16 word )
/*****************************************************/
{
/**/myassert( objr != NULL && objr->data != NULL );

    WriteU16( objr->data + objr->curoff, word );
    objr->curoff += sizeof( uint_16 );
}

static void Put32( struct omf_rec *objr, uint_32 dword )
/******************************************************/
{
/**/myassert( objr != NULL && objr->data != NULL );

    WriteU32( objr->data + objr->curoff, dword );
    objr->curoff += sizeof( uint_32 );
}

#if 0
static void PutEither( struct omf_rec *objr, uint_32 data )
/*********************************************************/
{
/**/myassert( objr != NULL && objr->data != NULL );
    if( objr->is_32 ) {
        WriteU32( objr->data + objr->curoff, data );
        objr->curoff += sizeof( uint_32 );
    } else {
        WriteU16( objr->data + objr->curoff, data );
        objr->curoff += sizeof( uint_16 );
    }
}
#endif

static void PutIndex( struct omf_rec *objr, size_t idx )
/******************************************************/
{
/**/myassert( objr != NULL && objr->data != NULL );
    if( idx > 0x7f ) {
        objr->data[objr->curoff++] = ( idx >> 8 ) | 0x80;
    }
    objr->data[objr->curoff++] = idx & 0xff;
}

static void PutData( struct omf_rec *objr, const uint_8 *data, size_t len )
/*************************************************************************/
{
    /**/myassert( objr != NULL && objr->data != NULL );
    memcpy( objr->data + objr->curoff, data, len );
    objr->curoff += len;
}

static void PutName( struct omf_rec *objr, const char *name, size_t len )
/***********************************************************************/
{
    /**/myassert( objr != NULL && objr->data != NULL );
#if MAX_ID_LEN > MAX_ID_LEN_OMF
    if ( len > MAX_ID_LEN_OMF ) {
        EmitWarn( 1, IDENTIFIER_TOO_LONG );
        len = MAX_ID_LEN_OMF;
    }
#endif
    objr->data[objr->curoff++] = len;
    PutData( objr, (uint_8 *)name, len );
}

static void AttachData( struct omf_rec *objr, uint_8 *data, size_t len )
/**********************************************************************/
{
/**/myassert( objr->data == NULL );
    objr->data = data;
    objr->length = len;
}

#if 0
static void AllocData( struct omf_rec *objr, size_t len )
/*******************************************************/
{
/**/myassert( objr->data == NULL );
    objr->data = LclAlloc( len );
    objr->length = len;
}
#endif

/* return a group's index */

uint omf_GetGrpIdx( struct asym *sym )
/************************************/
{
    if( sym == NULL )
        return( 0 );
    return( ((struct dsym *)sym)->e.grpinfo->grp_idx );
}

/*
 * write OMF comment records about data in code.
 */

void omf_OutSelect( bool is_data )
/********************************/
{
    struct omf_rec      obj;
    char                buffer[12];
    uint_32             currofs;
    int                 sel_idx;
    static uint_32      sel_start;  /* start offset of data items */

    if( is_data ) {
        /* do nothing if it isn't the first data item or
         * if current segment isn't code
         */
        if( CurrSeg->e.seginfo->data_in_code ||
           ( CurrSeg->e.seginfo->segtype != SEGTYPE_CODE ) )
            return;
        sel_start = GetCurrOffset();
        CurrSeg->e.seginfo->data_in_code = TRUE;
        DebugMsg(("omf_OutSelect: data in code segment (%s), starting at %" FX32 "\n", CurrSeg->sym.name, sel_start ));
    } else if ( CurrSeg->e.seginfo->data_in_code ) { /* data items written? */
        
        CurrSeg->e.seginfo->data_in_code = FALSE;

        if( write_to_file == TRUE ) {
            InitRec( &obj, CMD_COMENT );
            obj.d.coment.attr = CMT_TNP;
            obj.d.coment.class = CMT_DISASM_DIRECTIVE;

            sel_idx = GetSegIdx( &CurrSeg->sym );

            //AllocData( objr, 11 );  /* 11 = 1 + 2 + 4 + 4 */
            AttachData( &obj, buffer, 11 );  /* 11 = 1 + 2 + 4 + 4 */
            currofs = GetCurrOffset();
            DebugMsg(("omf_OutSelect: writing coment record about data in code: start=%" FX32 " curofs=%" FX32 "\n", sel_start, currofs ));
            if( ( sel_start > 0xffffUL ) || ( currofs > 0xffffUL ) ) {
                Put8( &obj, DDIR_SCAN_TABLE_32 );
                PutIndex( &obj, sel_idx );
                Put32( &obj, sel_start );
                Put32( &obj, currofs );
            } else {
                Put8( &obj, DDIR_SCAN_TABLE );
                PutIndex( &obj, sel_idx );
                Put16( &obj, sel_start );
                Put16( &obj, currofs );
            }
            TruncRec( &obj );
            omf_write_record( &obj );
        }
    }
}

/* get line numbers for OMF.*/

static int GetLinnumData( struct linnum_data *ldata, bool *need32 )
/*****************************************************************/
{
    struct line_num_info    *node;
    struct line_num_info    *next;
    int count = 0;

    *need32 = FALSE;
    for( node = LinnumQueue.head; node; count++, ldata++, node = next ) {
        next = node->next;
        ldata->number = node->number;
        ldata->offset = node->offset;
        if( node->offset > 0xffffUL ) {
            *need32 = TRUE;
        }
        LclFree( node );
    }
    LinnumQueue.head = NULL;
    return( count );
}

/* write line number debug info */

void omf_write_linnum( void )
/***************************/
{
    int                 count;
    struct omf_rec      obj;
    bool                need_32;

    count = GetLinnumData( (struct linnum_data *)StringBufferEnd, &need_32 );
    if( count ) {
        InitRec( &obj, CMD_LINNUM );
        obj.is_32 = need_32;
        obj.d.linnum.num_lines = count;
        obj.d.linnum.lines = (struct linnum_data *)StringBufferEnd;
        obj.d.linnum.d.base.grp_idx = omf_GetGrpIdx( GetGroup( &CurrSeg->sym ) ); /* fixme ? */
        obj.d.linnum.d.base.seg_idx = CurrSeg->e.seginfo->seg_idx;
        obj.d.linnum.d.base.frame = 0; /* fixme ? */
        omf_write_record( &obj );
    }
    return;
}

#ifdef SEPARATE_FIXUPP_16_32

static void split_fixup_list( struct dsym *seg, struct fixup **fl16, struct fixup **fl32 )
/****************************************************************************************/
{
/* divide fixup record list to the 16-bit or 32-bit list of a fixup record */

    struct fixup *fix;
    struct fixup *fix16;
    struct fixup *fix32;

#ifdef DEBUG_OUT
    int cnt16 = 0;
    int cnt32 = 0;
#endif

    fix16 = NULL;
    fix32 = NULL;
    for( fix = seg->e.seginfo->FixupListHead; fix; fix = fix->nextrlc ) {
        switch( fix->type ) {
        case FIX_RELOFF32:
        case FIX_OFF32:
        case FIX_PTR32:
#ifdef DEBUG_OUT
            cnt32++;
#endif
            if( fix32 == NULL ) {
                *fl32 = fix;
            } else {
                fix32->nextrlc = fix;
            }
            fix32 = fix;
            break;
        default:
#ifdef DEBUG_OUT
            cnt16++;
#endif
            if( fix16 == NULL ) {
                *fl16 = fix;
            } else {
                fix16->nextrlc = fix;
            }
            fix16 = fix;
            break;
        }
    }
    if( fix32 != NULL ) {
        fix32->nextrlc = NULL;
        DebugMsg(("split_fixup_list: %u 32-bit fixups\n", cnt32 ));
    }
    if( fix16 != NULL ) {
        fix16->nextrlc = NULL;
        DebugMsg(("split_fixup_list: %u 16-bit fixups\n", cnt16 ));
    }
}

#else

static int check_need_32bit( struct dsym *seg )
/*********************************************/
{
/* figure out if we need the 16-bit or 32-bit form of a fixup record */

    struct fixup  *fix;

    for( fix = seg->e.seginfo->FixupListHead; fix; fix = fix->nextrlc ) {

        switch( fix->loc_method ) {
        case FIX_RELOFF32:
        case FIX_OFF32:
        case FIX_PTR32:
            return( 1 );
        default:
            if( (uint_32)fix->lr.target_offset > 0xffffUL )
                return( 1 );
        }
    }
}

#endif


static void free_fixup( struct fixup *cur )
/*****************************************/
{
    struct fixup  *next;

    while( cur ) {
        next = cur->nextrlc;
        LclFree( cur );
        cur = next;
    }
}

/* write an LEDATA record, optionally write fixups */

void omf_write_ledata( struct dsym *seg )
/***************************************/
{
    struct omf_rec  obj;
    uint_32         size;
#ifdef SEPARATE_FIXUPP_16_32
    struct fixup *fl16 = NULL;
    struct fixup *fl32 = NULL;
#endif

    size = seg->e.seginfo->current_loc - seg->e.seginfo->start_loc;
    DebugMsg1(( "omf_write_ledata enter, buffer=%p start ofs=%" FX32 ", size=%" FX32 "\n",
              seg->e.seginfo->CodeBuffer, seg->e.seginfo->start_loc, size ));
    if( size > 0 && write_to_file == TRUE ) {
        LastCodeBufSize = size;
        InitRec( &obj, CMD_LEDATA );
        AttachData( &obj, seg->e.seginfo->CodeBuffer, size );
        obj.d.ledata.idx = seg->e.seginfo->seg_idx;
        obj.d.ledata.offset = seg->e.seginfo->start_loc;
        if( obj.d.ledata.offset > 0xffffUL )
            obj.is_32 = TRUE;
        omf_write_record( &obj );

        /* process Fixup, if any */
        if( seg->e.seginfo->FixupListHead != NULL ) {
            DebugMsg(( "omf_write_ledata: write fixups\n" ));
#ifdef SEPARATE_FIXUPP_16_32
            split_fixup_list( seg, &fl16, &fl32 );
            /* Process Fixup, if any */
            if( fl16 != NULL ) {
                InitRec( &obj, CMD_FIXUP );
                obj.is_32 = FALSE;
                obj.d.fixup.fixup = fl16;
                omf_write_record( &obj );
                free_fixup( fl16 );
            }
            if( fl32 != NULL ) {
                InitRec( &obj, CMD_FIXUP );
                obj.is_32 = TRUE;
                obj.d.fixup.fixup = fl32;
                omf_write_record( &obj );
                free_fixup( fl32 );
            }
#else
            InitRec( &obj, CMD_FIXUP );
            obj.d.fixup.fixup = seg->e.seginfo->FixupListHead;
            check_need_32bit( &obj );
            omf_write_record( &obj );
            free_fixup( obj.d.fixup.fixup );
#endif
            seg->e.seginfo->FixupListHead = seg->e.seginfo->FixupListTail = NULL;
        }
    }
    seg->e.seginfo->start_loc = seg->e.seginfo->current_loc;
}

/*
 * flush current segment.
 * write_to_file is always TRUE here
 */

void omf_FlushCurrSeg( void )
/***************************/
{
    //unsigned i;
    //unsigned size;

    DebugMsg1(( "omf_FlushCurrSeg enter, CurrSeg=%s\n", CurrSeg ? CurrSeg->sym.name : "NULL" ));

    omf_write_ledata( CurrSeg );
    /* add line numbers if debugging info is desired */
    //if( write_to_file && Options.line_numbers ) {
    if( Options.line_numbers ) {
        omf_write_linnum();
    }
    //if ( Options.no_comment_data_in_code_records == FALSE )
    //    omf_OutSelect( FALSE );
    return;
}

/*------------------------------------------------------*/

void omf_end_of_pass1( void )
/***************************/
{
    struct omf_rec obj;

    InitRec( &obj, CMD_COMENT );
    obj.d.coment.attr = 0x00;
    obj.d.coment.class = CMT_MS_END_PASS_1;
    AttachData( &obj, (uint_8 *)"\x001", 1 );
    omf_write_record( &obj );
    end_of_header = ftell( file_out->file );
}

void omf_set_filepos( void )
/**************************/
{
    DebugMsg(( "omf_set_filepos: reset file pos to %X\n", end_of_header ));
    fseek( file_out->file, end_of_header, SEEK_SET );
}

void omf_write_dosseg( void )
/***************************/
{
    struct omf_rec obj;

    InitRec( &obj, CMD_COMENT );
    obj.d.coment.attr = CMT_TNP;
    obj.d.coment.class = CMT_DOSSEG;
    AttachData( &obj, (uint_8 *)"", 0 );
    omf_write_record( &obj );
}

void omf_write_lib( void )
/************************/
{
    struct omf_rec      obj;
    struct qnode        *curr;
    struct qnode        *next;
    char                *name;

    DebugMsg(("omf_write_lib() enter\n"));
    for( curr = ModuleInfo.g.LibQueue.head; curr; curr = next ) {
        next = curr->next;
        name = (char *)curr->elmt;
        InitRec( &obj, CMD_COMENT );
        obj.d.coment.attr = CMT_TNP;
        obj.d.coment.class = CMT_DEFAULT_LIBRARY;
        AttachData( &obj, (uint_8 *)name, strlen( name ) );
        omf_write_record( &obj );
    }
    DebugMsg(("omf_write_lib() exit\n"));
}

/* subtypes for CMD_COMENT, comment class 0xA0 (CMT_DLL_ENTRY) */
enum omf_ext_subtype {
    CMT_EXT_IMPDEF = 0x01, /* imported names, MS ext for OS/2 and Windows */
    CMT_EXT_EXPDEF = 0x02, /* exported names, MS ext */
    CMT_EXT_INCDEF = 0x03,
    CMT_EXT_PMLIB  = 0x04,
    CMT_EXT_LNKDIR = 0x05,
    CMT_EXT_BIGEND = 0x06,
    CMT_EXT_PRECOMP = 0x07,
};

void omf_write_export( void )
/***************************/
{
    uint_8      parmcnt;
    struct dsym *dir;
    struct dsym *parm;
    struct omf_rec obj;
    int         len;
    //char        *name;
    char        buffer[MAX_ID_LEN + MANGLE_BYTES + 1 + 4];

    for( dir = SymTables[TAB_PROC].head; dir != NULL; dir = dir->nextproc ) {
        if( dir->e.procinfo->export ) {

            InitRec( &obj, CMD_COMENT );
            obj.d.coment.attr = 0x00;
            obj.d.coment.class = CMT_DLL_ENTRY;

            if ( Options.no_export_decoration == FALSE )
                len = Mangle( &dir->sym, buffer+3 );
            else {
                strcpy( buffer+3, dir->sym.name );
                len = dir->sym.name_size;
            }
#if MAX_ID_LEN > 255
            if ( len > 255 )
                len = 255; /* restrict name to 255 chars */
#endif
            AttachData( &obj, buffer, len + 4 );
            Put8( &obj, CMT_EXT_EXPDEF );
            /* write the "Exported Flag" byte:
             * bits 0-4: parameter count
             * bit 5: no data (entry doesn't use initialized data )
             * bit 6: resident (name should be kept resident)
             * bit 7: ordinal ( if 1, 2 byte index must follow name)
             */
            for ( parm = dir->e.procinfo->paralist, parmcnt = 0; parm; parm = parm->nextparam, parmcnt++ );
            parmcnt &= 0x1F; /* ensure bits 5-7 are still 0 */
            Put8( &obj, parmcnt ); /* v2.01: changed from fix 0x00 */
            //PutName( &obj, buffer, strlen( buffer ) );
            Put8( &obj, len );
            obj.curoff += len;
            Put8( &obj, 0 );
            omf_write_record( &obj );
        }
    }
}

/* write OMF GRPDEF records */

void omf_write_grp( void )
/************************/
{
    struct dsym     *curr;
    struct dsym     *segminfo;
    struct seg_item *seg;
    struct omf_rec  grp;
    //char            writeseg;

    DebugMsg(("omf_write_grp enter\n"));
    //line_num = LineNumber;

    /* size of group records may exceed 1024! */
    for( curr = SymTables[TAB_GRP].head; curr; curr = curr->next ) {

        InitRec( &grp, CMD_GRPDEF );

        grp.d.grpdef.idx = curr->e.grpinfo->grp_idx;

        /* we might need:
         * - 1 or 2 bytes for the group name index
         * - 2 or 3 bytes for each segment in the group
         */
        AttachData( &grp, StringBufferEnd, 2 + 3 * curr->e.grpinfo->numseg );
        /* v2.01: the LName index of the group may be > 0xff */
        /* v2.03: use the group index directly */
        PutIndex( &grp, curr->e.grpinfo->lname_idx );

        for( seg = curr->e.grpinfo->seglist; seg; seg = seg->next ) {
            //writeseg = TRUE;
            segminfo = (struct dsym *)(seg->seg);
            Put8( &grp, GRP_SEGIDX );
            PutIndex( &grp, segminfo->e.seginfo->seg_idx );
            /* truncate the group record if it comes near 4096! */
            if ( grp.curoff > OBJ_BUFFER_SIZE - 10 ) {
                EmitWarn( 2, GROUP_DEFINITION_TOO_LARGE, curr->sym.name );
                break;
            }
        }
        TruncRec( &grp );
        omf_write_record( &grp );
    }
    DebugMsg(("omf_write_grp exit\n"));
}

/* write segment table.
 * This is done after pass 1.
 * There might exist entries of undefined segments in
 * the segment list!
 */

void omf_write_seg( bool initial )
/********************************/
{
    struct dsym    *curr;
    struct omf_rec obj;
    uint        seg_index;
    uint_8      buffer[4];

    DebugMsg(("omf_write_seg enter\n"));

    /* in pass one, save current file pos */
    if ( initial ) {
        seg_pos = ftell( file_out->file );
    } else {
        fseek( file_out->file , seg_pos, SEEK_SET );
    }

    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {

        seg_index = GetSegIdx( &curr->sym );
        InitRec( &obj, CMD_SEGDEF );
        if ( curr->e.seginfo->Ofssize > USE16 ) {
            obj.is_32 = ( curr->e.seginfo->force32 || ( curr->sym.max_offset >= 0x10000 ) );
        } else {
            obj.is_32 = FALSE;
        }
        obj.d.segdef.seg_length = curr->sym.max_offset;
        switch ( curr->e.seginfo->alignment ) {
        case  1:
            obj.d.segdef.align = SEGDEF_ALIGN_WORD;
            break;
        case  2:
            obj.d.segdef.align = SEGDEF_ALIGN_DWORD;
            break;
        case  4:
            obj.d.segdef.align = SEGDEF_ALIGN_PARA;
            break;
        case  8:
            obj.d.segdef.align = SEGDEF_ALIGN_PAGE;
            break;
#if PAGE4K
        case 12: /* this is probably invalid for MS OMF */
            obj.d.segdef.align = SEGDEF_ALIGN_4KPAGE;
            break;
#endif
        case MAX_SEGALIGNMENT:
            obj.d.segdef.align = SEGDEF_ALIGN_ABS;
            break;
        default:
            obj.d.segdef.align = SEGDEF_ALIGN_BYTE;
            break;
        }
        obj.d.segdef.use_32 = ( curr->e.seginfo->Ofssize > USE16 );
        obj.d.segdef.ovl_name_idx = 1;
        /* v2.03: store index directly */
        obj.d.segdef.seg_name_idx = curr->e.seginfo->lname_idx;

        obj.d.segdef.combine        = curr->e.seginfo->combine;
        obj.d.segdef.idx            = curr->e.seginfo->seg_idx;
        obj.d.segdef.class_name_idx = curr->e.seginfo->class_name_idx;
        obj.d.segdef.abs.frame      = curr->e.seginfo->abs_frame;
        obj.d.segdef.abs.offset     = curr->e.seginfo->abs_offset;

        omf_write_record( &obj );
        DebugMsg(("omf_write_seg(%u): %s, len=%" FX32 " seg_idx=%u class_idx=%u ovl_idx=%u align=%u comb=%u use32=%u\n",
                  seg_index,
                  curr->sym.name,
                  obj.d.segdef.seg_length,
                  obj.d.segdef.seg_name_idx,
                  obj.d.segdef.class_name_idx,
                  obj.d.segdef.ovl_name_idx,
                  obj.d.segdef.align,
                  obj.d.segdef.combine,
                  obj.d.segdef.use_32
                 ));
        /* write a comment for the linker.
         * this is something not done by Masm, it has
         * been inherited from Wasm.
         */
        if( curr->e.seginfo->segtype == SEGTYPE_CODE && Options.no_opt_farcall == FALSE ) {
            InitRec( &obj, CMD_COMENT );
            obj.d.coment.attr = CMT_TNP;
            obj.d.coment.class = CMT_LINKER_DIRECTIVE;
            AttachData( &obj, buffer, 3 );
            Put8( &obj, LDIR_OPT_FAR_CALLS );
            PutIndex( &obj, seg_index );
            /* v2.04: added. cut off the 3. byte if not needed */
            TruncRec( &obj );
            omf_write_record( &obj );
        }
    }
    DebugMsg(("omf_write_seg exit\n"));
}

static struct asym * GetLnameData( void **pq )
/********************************************/
{
    struct qnode *curr = *pq;

    if ( curr == NULL ) {
        curr = ModuleInfo.g.LnameQueue.head;
    } else {
        curr = curr->next;
    }
    *pq = curr;
    if ( curr )
        return( (struct asym *)(curr->elmt) );
    return( NULL );
}

/* the lnames are stored in a queue. read
 * the items one by one and take care that
 * the record size doesn't exceed 1024 bytes.
 */

#define MAX_LNAME_SIZE 1024

void omf_write_lnames( void )
/***************************/
{
    struct omf_rec obj;
    int         size;
    int         items;
    int         startitem;
    char        *p;
    void        *pv = NULL;
    struct asym *sym;
    char        buffer[MAX_LNAME_SIZE];

    DebugMsg(("omf_write_lnames() enter\n"));
    p = buffer;
    *p++ = NULLC; /* start with the NULL entry */
    items = 1;
    startitem = 1;

    for (;;) {
        sym = GetLnameData( &pv );
        size = p - buffer;
        /* v2.04: changed extra bytes from 1 to 4 (CMD, RECLEN, CHKSUM) */
        //if ( sym == NULL || ( ( size + sym->name_size + 1 ) > MAX_LNAME_SIZE )) {
        if ( sym == NULL || ( ( size + sym->name_size + 4 ) > MAX_LNAME_SIZE )) {
            if( size ) {
                InitRec( &obj, CMD_LNAMES );
                /* first_idx and num_names are NOT
                 * written to the LNAMES record!
                 * In fact, they aren't used at all.
                 */
                obj.d.lnames.first_idx = startitem;
                obj.d.lnames.num_names = items;
                AttachData( &obj, buffer, size );
                omf_write_record( &obj );
                startitem = items;
            }
            if ( sym == NULL )
                break;
            p = buffer;
        }
        *p++ = (char)sym->name_size;
        /* copy 1 byte more - the NULLC - for _strupr() */
        memcpy( p, sym->name, sym->name_size + 1 );
        /* lnames are converted for casemaps ALL and NOTPUBLIC */
        if ( ModuleInfo.case_sensitive == FALSE )
            _strupr( p );
        DebugMsg(("omf_write_lnames: %u=%s\n", items, p ));
        p += sym->name_size; /* overwrite the null char */
        items++;
    };

    DebugMsg(("omf_write_lnames() exit\n"));
}

struct readext {
    struct dsym *p;
    uint_16 index;
    uint_8 method;
};

/* read items for EXTDEF records.
 * there are 2 sources:
 * - the TAB_EXT queue of externals
 * - the AltQueue of weak externals
 */

static struct asym *GetExt( struct readext *r )
/*********************************************/
{
    if ( r->method == 0 ) {
        do {
            if ( r->p == NULL )
                r->p = SymTables[TAB_EXT].head;
            else
                r->p = r->p->next;
            if ( r->p ) {
                if ( r->p->sym.state == SYM_EXTERNAL &&
                    ( r->p->sym.iscomm == TRUE ) || ( r->p->sym.weak == TRUE ) )
                    continue;
                r->p->sym.included = TRUE;
                r->index++;
                return( (struct asym *)r->p );
            }
        } while ( r->p );
        r->method++;
    }
    do {
        if ( r->p == NULL )
            r->p = ModuleInfo.g.AltQueue.head;
        else
            r->p = r->p->nextext;
        if ( r->p ) {
            if ( r->p->sym.altname->included )
                continue;
            r->index++;
            r->p->sym.altname->ext_idx = r->index;
            r->p->sym.altname->included = TRUE;
            return( r->p->sym.altname );
        }
    } while ( r->p );
    return( NULL );
}

/* write EXTDEF records */

void omf_write_extdef( void )
/***************************/
{
    struct omf_rec obj;
    struct asym *sym;
    struct dsym *dir;
    uint        rec_size;
    uint        len;
    struct readext r;
    char        name[MAX_EXT_LENGTH];
    char        buffer[MAX_ID_LEN + MANGLE_BYTES + 1];

    DebugMsg(("omf_write_extdef enter\n"));
    r.p = NULL;
    r.method = 0;
    r.index = 0;
    InitRec( &obj, CMD_EXTDEF );
    obj.d.extdef.first_idx = 0;
    obj.d.extdef.num_names = 0;
    rec_size = 0;

    /* scan the EXTERN/EXTERNDEF items */

    while ( 1 ) {
        sym = GetExt( &r );
        if ( sym == NULL )
            break;
        //DebugMsg(("omf_write_extdef: %s, weak=%u, used=%u\n", curr->sym.name, curr->sym.weak, curr->sym.used ));
        DebugMsg(("omf_write_extdef: %s\n", sym->name));
        len = Mangle( sym, buffer );
#if MAX_ID_LEN > 255
        if ( len > 255 )
            len = 255; /* length is 1 byte only */
#endif
        if ( ModuleInfo.convert_uppercase )
            _strupr( buffer );

        if( rec_size + len + 2 >= MAX_EXT_LENGTH ) {
            DebugMsg(("omf_write_extdef: write record, names=%u, size=%u, MAX=%u\n", obj.d.extdef.num_names, rec_size, MAX_EXT_LENGTH ));
            AttachData( &obj, (uint_8 *)name, rec_size );
            omf_write_record( &obj );
            InitRec( &obj, CMD_EXTDEF );
            obj.d.extdef.first_idx += obj.d.extdef.num_names;
            obj.d.extdef.num_names = 0;
            rec_size = 0;
        }
        obj.d.extdef.num_names++;

        name[rec_size++] = (char)len;
        memcpy( name + rec_size, buffer, len );
        rec_size += len;
        name[rec_size++] = 0;      /* for the type index */
    }

    if( rec_size != 0 ) {
        DebugMsg(("omf_write_extdef: write record, names=%u, size=%u, MAX=%u\n", obj.d.extdef.num_names, rec_size, MAX_EXT_LENGTH ));
        AttachData( &obj, (uint_8 *)name, rec_size );
        omf_write_record( &obj );
    }

    /* v2.04: write WKEXT coment records */

    for ( dir = ModuleInfo.g.AltQueue.head; dir; dir = dir->nextext ) {
        InitRec( &obj, CMD_COMENT );
        obj.d.coment.attr = CMT_TNP;
        obj.d.coment.class = CMT_WKEXT;
        AttachData( &obj, buffer, 4 );
        PutIndex( &obj, dir->sym.ext_idx );
        PutIndex( &obj, dir->sym.altname->ext_idx );
        TruncRec( &obj );
        omf_write_record( &obj );
    }
    /* v2.05: clear the index field again */
    for ( dir = ModuleInfo.g.AltQueue.head; dir; dir = dir->nextext )
        dir->sym.altname->ext_idx = 0;

    DebugMsg(("omf_write_extdef exit\n"));
    return;
}

#define THREE_BYTE_MAX ( (1UL << 24) - 1 )

static int get_size_of_comdef_number( unsigned long value )
/*********************************************************/
{
    /* The spec allows up to 128 in a one byte size field, but lots
       of software has problems with that, so we'll restrict ourselves
       to 127.
    */
    if( value < 128 ) {
        return( 1 );    /* 1 byte value */
    } else if( value <= USHRT_MAX ) {
        return( 3 );    /* 1 byte flag + 2 byte value */
    } else if( value <= THREE_BYTE_MAX ) {
        return( 4 );    /* 1 byte flag + 3 byte value */
    } else { /* if( value <= ULONG_MAX ) */
        return( 5 );    /* 1 byte flag + 4 byte value */
    }
}

/* for COMDEF: write item size (or number of items) */

static uint put_comdef_number( uint_8 *buffer, uint_32 value )
/************************************************************/
{
    uint i;
    uint symsize;

    symsize = get_size_of_comdef_number( value );
    switch( symsize ) {
    case 1:  *buffer = value; break;
    case 3:  *buffer++ = COMDEF_LEAF_2;  break; /* 0x81 */
    case 4:  *buffer++ = COMDEF_LEAF_3;  break; /* 0x84 */
    case 5:  *buffer++ = COMDEF_LEAF_4;  break; /* 0x88 */
    }

    for( i = 1; i < symsize; i++ ) {
        *buffer++ = value % ( UCHAR_MAX + 1 );
        value >>= 8;
    }
    return( symsize );
}

/* write OMF COMDEF records */

ret_code omf_write_comdef( void )
/*******************************/
{
    struct omf_rec obj;
    struct dsym    *curr;
    uint        num;
    uint        recsize;
    uint        numsize;
    uint        symsize;
    uint_32     varsize;
    uint        start = 0; /* record's start index (not used) */
    char        buffer[MAX_ID_LEN + MANGLE_BYTES + 1];
    char        name[MAX_EXT_LENGTH];
    char        number[16];

    DebugMsg(("omf_write_comdef enter\n"));
    curr = SymTables[TAB_EXT].head;
    while ( curr ) {
        for( num = 0, recsize = 0; curr != NULL ; curr = curr->next ) {
            if ( curr->sym.iscomm == FALSE )
                continue;
            symsize = Mangle( &curr->sym, buffer );
#if MAX_ID_LEN > 255
            if ( symsize > 255 )
                symsize = 255; /* length is 1 byte only */
#endif
            varsize = SizeFromMemtype( curr->sym.mem_type, ModuleInfo.Ofssize, curr->sym.type );

            DebugMsg(("omf_write_comdef: %s, size=%u, sym.total_size=%u, sym.total_length=%u, sym.isfar=%u\n",
                      curr->sym.name, varsize, curr->sym.total_size, curr->sym.total_length, curr->sym.isfar ));
            if ( varsize == 0 )
                varsize = curr->sym.total_size / curr->sym.total_length;

            numsize = 1;
            if ( curr->sym.isfar == TRUE ) {
                number[0] = COMDEF_FAR;  /* 0x61 */
                numsize += put_comdef_number( &number[1], curr->sym.total_length );
                numsize += put_comdef_number( &number[numsize], varsize );
                DebugMsg(("omf_write_comdef: numsize=%u, length=%u, varsize=%u\n",
                          numsize, curr->sym.total_length, varsize ));
            } else {
                number[0] = COMDEF_NEAR; /* 0x62 */
                numsize += put_comdef_number( &number[1], curr->sym.total_length * varsize );
                DebugMsg(("omf_write_comdef: numsize=%u, value=%u\n",
                          numsize, curr->sym.total_length * varsize ));
            }
            /* make sure the record's size doesn't exceed 1024.
             * 2 = 1 (name len) + 1 (type index)
             */
            if ( ( recsize + symsize + numsize + 2 ) > MAX_EXT_LENGTH )
                break;

            /* copy name ( including size prefix ), type, number */
            name[recsize++] = (char)symsize;
            memcpy( name + recsize, buffer, symsize );
            recsize += symsize;
            name[recsize++] = 0;      /* for the type index */
            memcpy( name + recsize, number, numsize );
            recsize += numsize;

            num++;

        } /* end for */

        if( num > 0 ) {
            InitRec( &obj, CMD_COMDEF );
            obj.d.comdef.first_idx = start; /* unused */
            AttachData( &obj, (uint_8 *)name, recsize );
            obj.d.comdef.num_names = num; /* unused */
            omf_write_record( &obj );
            start += num;
        }
    }
    DebugMsg(("omf_write_comdef exit\n"));
    return( NOT_ERROR );
}

/* Write a THEADR record. If -Zi is set, a comment class
 * A1 record (MS extensions present) is also written.
 */
void omf_write_header( void )
/***************************/
{
    struct omf_rec obj;
    unsigned    len;
    char        *name;
    //const struct fname_list *fn;

    DebugMsg(("omf_write_header() enter\n"));

    InitRec( &obj, CMD_THEADR );
#if 1
    /* v2.08: use the name given at the cmdline, that's what Masm does.
     * Masm emits either a relative or a full path, depending on what
     * was given as filename!
     */
    name = CurrFName[ASM];
#else
    if( Options.names[OPTN_MODULE_NAME] != NULL ) {
        name = Options.names[OPTN_MODULE_NAME];
    } else {
        fn = GetFName( ModuleInfo.srcfile );
        name = fn->fullname;
        len = strlen( name );
        name += len;
        for (;name > fn->fullname && *(name-1) != '/' && *(name-1) != '\\';name-- );
    }
#endif
    len = strlen( name );
    AttachData( &obj, StringBufferEnd, len + 1 );
    PutName( &obj, name, len );
    omf_write_record( &obj );

    /* -Zi option set? */
    if ( Options.debug_symbols )
        omf_write_header_dbgcv();

    DebugMsg(("omf_write_header() exit\n"));
}

ret_code omf_write_autodep( void )
/********************************/
{
    struct omf_rec  obj;
    struct fname_list *curr;
    char            *p = StringBufferEnd;
    unsigned int    len;
    unsigned        idx;

    DebugMsg(("omf_write_autodep() enter\n"));
    for( idx = 0, curr = FNamesTab; idx < ModuleInfo.g.cnt_fnames; idx++, curr++ ) {
        DebugMsg(("omf_write_autodep(): write record for %s\n", curr->name ));
        InitRec( &obj, CMD_COMENT );
        obj.d.coment.attr = CMT_TNP;
        obj.d.coment.class = CMT_DEPENDENCY; /* 0xE9 */

        len = strlen( curr->name );
#if MAX_STRING_LEN > 255
        if ( len > 255 )
            len = 255; /* length is 1 byte only */
#endif
        *((time_t *)p) = timet2dostime( curr->mtime );
        *(p + 4) = (unsigned char)len;
        memcpy( p + 5, curr->name, len );
        AttachData( &obj, (uint_8 *)p, len + 5 );
        omf_write_record( &obj );
    }
    /* one NULL dependency record must be on the end */
    InitRec( &obj, CMD_COMENT );
    obj.d.coment.attr = CMT_TNP;
    obj.d.coment.class = CMT_DEPENDENCY;
    AttachData( &obj, (uint_8 *)"", 0 );
    omf_write_record( &obj );
    DebugMsg(("omf_write_autodep() exit\n"));
    return( NOT_ERROR );
}

void omf_write_alias( void )
/**************************/
{
    struct omf_rec      obj;
    char                *p;
    uint_8              len1;
    uint_8              len2;
    //bool                first = TRUE;
    struct dsym         *curr;
    char                buff[2*MAX_ID_LEN_OMF + 2];

    for( curr = SymTables[TAB_ALIAS].head; curr; curr = curr->next ) {

        /* output an alias record for this alias */
        len1 = curr->sym.name_size;
        len2 = curr->sym.substitute->name_size;
#if MAX_ID_LEN > MAX_ID_LEN_OMF
        if ( len1 > MAX_ID_LEN_OMF )
            len1 = MAX_ID_LEN_OMF;
        if ( len2 > MAX_ID_LEN_OMF )
            len2 = MAX_ID_LEN_OMF;
#endif

        p = buff;
        *p++ = len1;
        memcpy( p, curr->sym.name, len1 );
        p += len1;
        *p++ = len2;
        memcpy( p, curr->sym.substitute->name, len2 );

        InitRec( &obj, CMD_ALIAS );
        AttachData( &obj, buff, len1 + len2 + 2 );
        omf_write_record( &obj );
        //first = FALSE;
    }
}

static void WritePubRec( uint_8 cmd, struct asym *curr_seg, uint count, bool need32, struct pubdef_data *data)
/************************************************************************************************************/
{
    struct omf_rec      obj;
    uint                seg;
    uint                grp;
    //uint                i;
    //struct pubdef_data  *d;

    if( curr_seg == NULL ) { /* absolute symbol, no segment */
        seg = 0;
        grp = 0;
    } else {
        seg = GetSegIdx( curr_seg );
        grp = omf_GetGrpIdx( GetGroup( curr_seg ) );
    }
    InitRec( &obj, cmd );
    obj.is_32 = need32;
    obj.d.pubdef.base.grp_idx = grp;
    obj.d.pubdef.base.seg_idx = seg;
    obj.d.pubdef.base.frame = 0;
    obj.d.pubdef.num_pubs = count;
    obj.d.pubdef.pubs = data;
    omf_write_record( &obj );
    return;
}

/* max. sizes offset(4) + index(2) + name len(1).
 * offset is 2 only for 16-bit, but we don't know
 * whether a 32-bit public is in the queue until we have
 * read it.
 * index is always 0 and hence will need 1 byte only.
 * So there are always a few bytes wasted - it won't matter.
 */
#define PUBITEMBASELEN (4+2+1)

ret_code omf_write_public( bool initial )
/***************************************/
{
    struct asym         *sym;
    struct asym         *curr_seg;
    struct pubdef_data  *d;
    void                *vp;
    uint                count;
    uint                size;
    uint                symsize;
    uint_8              cmd = CMD_PUBDEF;
    bool                need32;

    DebugMsg(("omf_write_pub enter\n"));

    if ( initial ) {
        public_pos = ftell( file_out->file );
    } else {
        fseek( file_out->file, public_pos, SEEK_SET);
    }

    /* v2.07: struct pubdef_data has been modified to match
     * the data to be written to the object module more closely.
     * This fixed a possible overrun if too many publics were written.
     */

    vp = NULL;
    d = (struct pubdef_data *)StringBufferEnd;
    size = 10; /* =size of an empty PUBDEF record */
    count = 0;
    need32 = FALSE;
    while ( sym = (struct asym *)GetPublicData( &vp ) ) {
        symsize = Mangle( sym, d->name );
        /* if segment changes, or record becomes too big, write record */
        if( ( count && ( sym->segment != curr_seg )) ||
           ( ( size + symsize + PUBITEMBASELEN ) > MAX_PUB_LENGTH )) {
            WritePubRec( cmd, curr_seg, count, need32, (struct pubdef_data *)StringBufferEnd );
            d = (struct pubdef_data *)StringBufferEnd;
            Mangle( sym, d->name );
            size = 10; /* =size of an empty PUBDEF record */
            count = 0;
            need32 = FALSE;
        }
        if ( ModuleInfo.convert_uppercase )
            _strupr( d->name );
        curr_seg = sym->segment;
        if( sym->offset > 0xffffUL )
            need32 = TRUE;

        size += symsize + PUBITEMBASELEN;
        d->len = symsize;
        d->offset = sym->offset;
        d->type.idx = 0;
        count++;
        DebugMsg(("omf_write_pub(%u): %s, ofs=%Xh, rec_size=%u\n", count, d->name, d->offset, size ));
        d = (struct pubdef_data *)(d->name + d->len);
    }
    if ( count )
        WritePubRec( cmd, curr_seg, count, need32, (struct pubdef_data *)StringBufferEnd );

    DebugMsg(("omf_write_pub exit\n"));
    return( NOT_ERROR );
}

void omf_write_modend( struct fixup *fixup, uint_32 displ )
/*********************************************************/
{
    struct omf_rec  obj;
    struct asym     *sym;
#if TRUNCATE
    int fh;
    uint_32 size;
#endif

    DebugMsg(("omf_write_modend()\n"));

    InitRec( &obj, CMD_MODEND );

    if( fixup == NULL ) {
        obj.d.modend.main_module = FALSE;
        obj.d.modend.start_addrs = FALSE;
    } else {

        obj.d.modend.start_addrs = TRUE;
        obj.d.modend.is_logical = TRUE;
        obj.d.modend.main_module = TRUE;
        obj.is_32 = GetSymOfssize( fixup->sym ); /* USE16 or USE32 */

        sym = fixup->sym;

        /* fill the logref part for MODEND:
        struct logref {
          uint_8  frame       :3;
          uint_8  target      :3;
          uint_8  is_secondary:1;
          uint_16 frame_datum;
          uint_16 target_datum;
          int_32  target_offset;
        };
        */
        obj.d.modend.ref.log.is_secondary = FALSE;
        obj.d.modend.ref.log.target_offset = fixup->sym->offset + displ;

        /* symbol is always a label, internal or external */
        /* now set Target and Frame */

        if( sym->state == SYM_EXTERNAL ) {
            DebugMsg(("omf_write_modend(%X): EXTERNAL %s\n", fixup, sym->name));

            obj.d.modend.ref.log.target = TARGET_EXT & TARGET_WITH_DISPL;
            obj.d.modend.ref.log.target_datum = sym->ext_idx;

            if( fixup->frame_type == FRAME_GRP && fixup->frame_datum == 0 ) {
                /* set the frame to the frame of the corresponding segment */
                fixup->frame_datum = omf_GetGrpIdx( sym );
            }
        } else { /* SYM_INTERNAL */
            DebugMsg(("omf_write_modend_fixup(%X): fixup->frame, datum=%u.%u sym->name=%s state=%X segm=%X\n",
                      fixup, fixup->frame_type, fixup->frame_datum, sym->name, sym->state, sym->segment ));

            obj.d.modend.ref.log.target = TARGET_SEG & TARGET_WITH_DISPL;
            obj.d.modend.ref.log.target_datum = GetSegIdx( sym->segment );
        }

        if( fixup->frame_type != FRAME_NONE && fixup->frame_type != FRAME_SEG ) {
            obj.d.modend.ref.log.frame = (uint_8)fixup->frame_type;
        } else {
            obj.d.modend.ref.log.frame = FRAME_TARG;
        }
        obj.d.modend.ref.log.frame_datum = fixup->frame_datum;
    }
    omf_write_record( &obj );

#if TRUNCATE
    /* under some very rare conditions, the object
     * module might become shorter! Hence the file
     * must be truncated now. The problem is that there
     * is no stream function for this task.
     * the final solution will be to save the segment contents
     * in buffers and write the object module once everything
     * is done ( as it is done for the other formats already).
     * v2.03: most likely no longer necessary, since the file
     * won't become shorter anymore.
     */
    size = ftell( file_out->file );
#if defined(__UNIX__) || defined(__CYGWIN__) || defined(__DJGPP__)
    fh = fileno( file_out->file );
    ftruncate( fh, size );
#elif defined(__BORLANDC__)
    fh = _fileno( file_out->file );
    chsize( fh, size );
#else
    fh = _fileno( file_out->file );
    _chsize( fh, size );
#endif

#endif
}

/* add segments $$SYMBOLS, $$TYPES to the segment table */

void omf_write_header_dbgcv( void )
/*********************************/
{
    struct omf_rec obj;
    struct asym    *symbols;
    struct asym    *types;

    InitRec( &obj, CMD_COMENT );
    obj.d.coment.attr = 0x00;
    obj.d.coment.class = CMT_MS_OMF; /* MS extensions present */
    AttachData( &obj, "\001CV", 3 );
    omf_write_record( &obj );
    if ( symbols = CreateIntSegment( szCVSymbols, szCVSymClass, 0, USE32, TRUE ) ) {
        ((struct dsym *)symbols)->e.seginfo->force32 = TRUE;
        if ( types = CreateIntSegment( szCVTypes, szCVTypClass, 0, USE32, TRUE ) ) {
            ((struct dsym *)types)->e.seginfo->force32 = TRUE;
        }
    }
    return;
}

/* write contents of segments $$SYMBOLS and $$TYPES */

void omf_write_debug_tables( void )
/*********************************/
{
    struct dsym *types = (struct dsym *)SymSearch( szCVTypes );
    struct dsym *symbols = (struct dsym *)SymSearch( szCVSymbols );
    cv_write_debug_tables( symbols, types );
}

/* init. called once per module */

void omf_init( struct module_info *ModuleInfo, FILE *objfile )
/************************************************************/
{
    DebugMsg(("omf_init enter\n"));
    file_out = LclAlloc( sizeof( struct omf_wfile ) + OBJ_BUFFER_SIZE );
    file_out->file = objfile;
    file_out->cmd = 0;
    return;
}

void omf_fini( void )
/*******************/
{
    DebugMsg(("omf_fini enter, file_out=%X\n", file_out ));
    if ( file_out ) {
        LclFree( file_out );
        file_out = NULL;
    }
    return;
}

