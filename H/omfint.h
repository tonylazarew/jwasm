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
* Description:  OMF internal definitions.
*
****************************************************************************/


#ifndef OMFINT_H
#define OMFINT_H    1

#define WriteU16(p,n)   (*(uint_16*)(p) = (uint_16)(n))
#define WriteU32(p,n)   (*(uint_32*)(p) = (uint_32)(n))
//#define WriteS16(p,n)   (*(int_16*)(p) = (int_16)(n))
//#define WriteS32(p,n)   (*(int_32*)(p) = (int_32)(n))

#pragma pack( push, 1 )

struct omf_wfile {
    FILE        *file;      /* file                                         */
    size_t      in_buf;     /* number of bytes in buffer                    */
    struct {
        /* must be in this order! cmd, length, buffer */
        uint_8      cmd;      /* record cmd, also a flag if object is used  */
        uint_16     reclen;   /* record length                              */
        uint_8      buffer[1];/* for writing                                */
    };
};

#define OBJ_BUFFER_SIZE 0x1000      /* 4k (must be less than 64k) */
#define OBJ_MAX_REC     0x1000      /* maximum record size (<64k) */

#if OBJ_MAX_REC > OBJ_BUFFER_SIZE
#error "OBJ_MAX_REC must be smaller than OBJ_BUFFER_SIZE"
#endif

/*-----------------------------------------------------*/

enum fixgen_types {
    FIX_GEN_INTEL,
    FIX_GEN_MS386
};
#define FIX_GEN_MAX     11   /* max number of bytes OmfFixGenFix requires */

/*-----------------------------------------------------*/
struct coment_info {
    uint_8  attr;           /* attribute field from coment record       */
    uint_8  class;          /* class field from coment record           */
};
/*
    A COMENT record is created by filling in the above fields, and attaching
    any appropriate data with the Obj...() functions below.
*/


struct physref {
    uint_16 frame;          /* frame number of physical reference       */
    uint_32 offset;         /* offset into reference                    */
};

struct logref {
    uint_8  frame       :3; /* F_ types from omfpc.h                    */
    uint_8  target      :3; /* T_ types from omfpc.h (only T0-T3)       */
    uint_8  is_secondary:1; /* can write target in a secondary manner   */

    uint_16 frame_datum;    /* datum for different frame methods        */
    uint_16 target_datum;   /* datum for different target methods       */
    int_32  target_offset;  /* offset of target for target method       */
};

union logphys {
    struct logref  log;
    struct physref phys;
};

struct modend_info {
    uint_8  main_module :1; /* module is a main module                  */
    uint_8  start_addrs :1; /* module has start address                 */
    uint_8  is_logical  :1; /* is logical or physical reference         */
    union logphys ref;      /* a logical or physical reference          */
};
/*
    A MODEND is described completely by the above information; no data
    should be attached to a MODEND.
*/


struct lnames_info {
    uint_16 first_idx;      /* index of first name in this record       */
    uint_16 num_names;      /* number of names in this record           */
};
/*
    LNAMES, EXTDEFs, and COMDEFs all use this structure.  The actual
    LNAMES/etc are in the data attached to the record.
*/


struct grpdef_info {
    uint_16 idx;            /* index of this grpdef record              */
};
/*
    The data that defines the GRPDEF should be attached to this record.
*/


enum segdef_align_values {
    SEGDEF_ALIGN_ABS        = 0,/* absolute segment - no alignment          */
    SEGDEF_ALIGN_BYTE       = 1,/* relocatable seg  - byte aligned          */
    SEGDEF_ALIGN_WORD       = 2,/*                  - word aligned          */
    SEGDEF_ALIGN_PARA       = 3,/*                  - para aligned          */
    SEGDEF_ALIGN_PAGE       = 4,/*                  - page aligned          */
    SEGDEF_ALIGN_DWORD      = 5,/*                  - dword aligned         */
    SEGDEF_ALIGN_4KPAGE     = 6 /*                  - 4k page aligned       */
    /* if more than 8 types then adjust bitfield width in segdef_info */
};

struct segdef_info {
    uint_16 idx;            /* index for this segment                   */
    uint_8  use_32      :1; /* use_32 for this segment                  */
    uint_8  align       :3; /* align field (enum segdef_align_values)   */
    uint_8  combine     :4; /* combine field (values in omfpc.h)        */
    //uint_8  access_valid:1; /* does next field have valid value         */
    //uint_8  access_attr :2; /* easy omf access attributes (see omfpc.h) */
    struct physref abs;     /* (conditional) absolute physical reference*/
    uint_32 seg_length;     /* length of this segment                   */
    uint_16 seg_name_idx;   /* name index of this segment               */
    uint_16 class_name_idx; /* class name index of this segment         */
    uint_16 ovl_name_idx;   /* overlay name index of this segment       */
};
/*
    All data necessary for a SEGDEF is defined in the above structure.  No
    data should be attached to the record.
*/


struct ledata_info {
    uint_16 idx;            /* index of segment the data belongs to     */
    uint_32 offset;         /* offset into segment of start of data     */
};
/*
    LEDATAs and LIDATAs both use this structure.  The data that comprises the
    record should be attached.
*/


struct base_info {
    uint_16 grp_idx;        /* index of the group base                  */
    uint_16 seg_idx;        /* index of the segment                     */
    uint_16 frame;          /* valid if grp_idx == 0 && seg_idx == 0    */
};                          /* appears at beginning of appropriate recs */
/*
    This appears at the beginning of LINNUMs and PUBDEFs.  (see the
    appropriate structures.
*/


struct comdat_info {
    struct base_info base;
    uint_8      flags;
    uint_8      attributes;
    uint_8      align;
    uint_32     offset;
    uint_16     type_idx;
    uint_16     public_name_idx;
};
/*
    The data the comprises the record should be attached.
*/


struct fixup_info {
    //struct omf_rec *data_rec;   /* ptr to the data record this belongs to   */
    struct fixup *fixup;   /* linked list of processed fixups          */
};
/*
    No data should be attached to these records; all information is in
    the linked list of fixup records.
*/


struct linnum_info {
    union {
        struct base_info base;/* base information                       */
        struct {
            uint_8 flags;       /* for LINSYM records                   */
            uint_16 public_name_idx; /* for LINSYM records              */
        } linsym;
    } d;
    uint_16 num_lines;      /* number of elements in following array    */
    struct linnum_data {
        uint_16 number;     /* line number in source file               */
        uint_32 offset;     /* offset into segment                      */
    } *lines;               /* array of size num_lines                  */
};
/*
    No data should be attached to these records.  All necessary information
    is in the lines array.
*/


struct pubdef_info {
    struct base_info base;  /* base information                         */
    uint_16 num_pubs;       /* number of publics in following array     */
    struct pubdef_data {
        uint_32 offset;     /* public's offset/value                    */
        union {             /* see PUBDEF.h for more information        */
            uint_16 idx;    /* Intel OMF type index                     */
        } type;
        uint_8 len;
        char name[1];       /* name of this public                      */
    } *pubs;                /* array of size num_pubs                   */
};
/*
 No data should be attached to this record.  Everything is described by
 the pubs array.
*/


union omfrec_info {
    struct coment_info  coment;
    struct modend_info  modend;
    struct lnames_info  lnames;
    struct lnames_info  llnames;
    struct lnames_info  extdef;
    struct lnames_info  comdef;
    struct lnames_info  cextdf;
    struct grpdef_info  grpdef;
    struct segdef_info  segdef;
    struct ledata_info  ledata;
    struct ledata_info  lidata;
    struct base_info    base;
    struct fixup_info   fixup;
    struct linnum_info  linnum;
    struct linnum_info  linsym;
    struct pubdef_info  pubdef;
    struct comdat_info  comdat;
};

struct omf_rec {
    uint_16     length;  /* the length field for this record  (PRIVATE)  */
    uint_16     curoff;  /* offset of next read within record (PRIVATE)  */
    uint_8      *data;   /* data for this record              (PRIVATE)  */
    uint_8      command; /* the command field for this record            */
    uint_8      is_32:1; /* is this a Microsoft 32bit record             */
    union omfrec_info d; /* data depending on record type                */
};

#pragma pack( pop )

extern void   omf_write_record( struct omf_rec * );

extern size_t OmfFixGenFix( struct fixup *fix, uint_8 *buf, int type );
extern size_t OmfFixGenRef( union logphys *lp, int is_logical, uint_8 *buf, int type );

#endif
