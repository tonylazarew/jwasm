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
* Description:  Label directive, (anonymous) code labels
*
****************************************************************************/

#include "globals.h"
#include "parser.h"
#include "fixup.h"
#include "segment.h"
#include "proc.h"
#include "assume.h"
#include "types.h"
#include "labels.h"
#include "listing.h"

void LabelsInit( void )
/*********************/
{
    ModuleInfo.anonymous_label = 0;
}

char *GetAnonymousLabel( char *buffer, int value )
/************************************************/
{
    sprintf( buffer, "L&_%04u", ModuleInfo.anonymous_label + value );
    return( buffer );
}

/* define a (code) label
 * name: name of the label
 * mem_type: its memory type
 * ti: qualified type pointer, used if memtype is MT_TYPE or MT_PROC
 * bLocal: code label should be defined locally if inside a PROC
 */
struct asym *CreateLabel( const char *name, enum memtype mem_type, struct qualified_type *ti, bool bLocal )
/*********************************************************************************************************/
{
    struct asym         *sym;
    uint_32             addr;
    char                buffer[20];

    DebugMsg1(("CreateLabel(%s, memtype=%Xh, %" FX32 "h, %u) enter\n", name, mem_type, ti, bLocal));

    if( CurrSeg == NULL ) {
        EmitError( MUST_BE_IN_SEGMENT_BLOCK );
        return( NULL );
    }
    /* v2.06: don't allow a code label (NEAR, FAR, PROC) if CS is
     * assumed ERROR. This was previously checked for labels with
     * trailing colon only [in ParseLine()].
     */
    if ( ( mem_type & MT_SPECIAL_MASK) == MT_ADDRESS ) {
        if ( SegAssumeTable[ASSUME_CS].error ) { /* CS assumed to ERROR? */
            DebugMsg(("CreateLabel: code label and CS assumed error\n" ));
            EmitError( USE_OF_REGISTER_ASSUMED_TO_ERROR );
            return( NULL );
        }
    }

    //if( strcmp( name, "@@" ) == 0 ) {
    if( name[0] == '@' && name[1] == '@' && name[2] == NULLC ) {
        sprintf( buffer, "L&_%04u", ++ModuleInfo.anonymous_label );
        name = buffer;
    }

    sym = SymLookupLabel( name, bLocal );
    if( sym == NULL )
        return( NULL );
    if( Parse_Pass == PASS_1 ) {
        if( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) {
            /* don't accept EXTERNDEF for a local label! */
            /* v2.04: also never accept PROTOs for extern to intern conversion */
            //if ( bLocal && CurrProc ) {
            if ( sym->isproc || ( bLocal && CurrProc ) ) {
                DebugMsg(("CreateLabel(%s): error, EXTERNDEF for local label\n", sym->name));
                EmitErr( SYMBOL_REDEFINITION, name );
                return( NULL );
            }
            /* ensure that type of symbol is compatible! */
            if ( sym->mem_type != MT_EMPTY &&
                 sym->mem_type != mem_type ) {
                DebugMsg(("CreateLabel(%s): error, memtype conflict %X-%X\n", sym->name, sym->mem_type, mem_type));
                EmitErr( SYMBOL_TYPE_CONFLICT, name );
            }
            sym_ext2int( sym );
        } else if( sym->state == SYM_UNDEFINED ) {
            sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
            sym->state = SYM_INTERNAL;
        } else {
            /* v2.04: emit a more distinctive error msg */
            if ( sym->state == SYM_INTERNAL && sym->mem_type == mem_type )
                EmitErr( SYMBOL_ALREADY_DEFINED, name );
            else
                EmitErr( SYMBOL_REDEFINITION, name );
            return( NULL );
        }
        /* add the label to the linked list attached to curr segment */
        /* this allows to reduce the number of passes (see fixup.c) */
        ((struct dsym *)sym)->next = (struct dsym *)CurrSeg->e.seginfo->labels;
        CurrSeg->e.seginfo->labels = sym;

        /* a possible language type set by EXTERNDEF must be kept! */
        if ( sym->langtype == LANG_NONE )
            sym->langtype = ModuleInfo.langtype;

        /* v2.05: added to accept type prototypes */
        if ( mem_type == MT_PROC ) {
            if ( sym->isproc == FALSE ) {
                CreateProc( sym, NULL, TRUE );
                CopyPrototype( (struct dsym *)sym, (struct dsym *)ti->symtype );
            }
            mem_type = ti->symtype->mem_type;
            ti->symtype = NULL;
        }

        sym->mem_type = mem_type;
        if ( ti ) {
            if ( mem_type == MT_TYPE )
                sym->type = ti->symtype;
            else {
                sym->Ofssize = ti->Ofssize;
                sym->is_ptr = ti->is_ptr;
                sym->isfar = ti->is_far;
                sym->target_type = ti->symtype;
                sym->ptr_memtype = ti->ptr_memtype;
            }
        }
    } else {
        /* save old offset */
        addr = sym->offset;
    }

    sym->isdefined = TRUE;
    /* v2.05: the label may be "data" - due to the way struct initialization
     * is handled. Then fields first_size and first_length must not be
     * touched!
     */
    if ( sym->isdata == FALSE )
        sym->asmpass = Parse_Pass;
    SetSymSegOfs( sym );
//  DebugMsg(("CreateLabel(%s): ofs=%X\n", sym->name, sym->offset));

    if( Parse_Pass != PASS_1 && sym->offset != addr ) {
#ifdef DEBUG_OUT
        if ( !ModuleInfo.PhaseError )
            DebugMsg(("CreateLabel: Phase error, pass %u, sym >%s< first time, new=%" FX32 " - old=%" FX32 "\n", Parse_Pass+1, sym->name, sym->offset, addr));
        else
            DebugMsg(("CreateLabel: pass %u, sym >%s< changed, new=%" FX32 " - old=%" FX32 "\n", Parse_Pass+1, sym->name, sym->offset, addr));
#endif
        ModuleInfo.PhaseError = TRUE;
    }
    BackPatch( sym );
    return( sym );
}

/* LABEL directive.
 * syntax: <label_name> LABEL <qualified type>
 */

ret_code LabelDirective( int i, struct asm_tok tokenarray[] )
/***********************************************************/
{
    struct qualified_type ti;
    struct asym *sym;

    if( i != 1 ) {  /* LABEL must be preceded by an ID */
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    i++;

    ti.size = 0;
    ti.is_ptr = 0;
    ti.is_far = FALSE;
    ti.mem_type = MT_EMPTY;
    ti.ptr_memtype = MT_EMPTY;
    ti.symtype = NULL;
    ti.Ofssize = ModuleInfo.Ofssize;
    if ( GetQualifiedType( &i, tokenarray, &ti ) == ERROR )
        return( ERROR );

    DebugMsg1(("LabelDirective(%s): memtype=%Xh, far=%u, ptr=%u, type=%s)\n",
               tokenarray[0].string_ptr, ti.mem_type, ti.is_far, ti.is_ptr, ti.symtype ? ti.symtype->name : "NULL" ));

    if ( tokenarray[i].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    /* dont allow near16/far16/near32/far32 if size won't match */
    if ( ( ti.mem_type == MT_NEAR || ti.mem_type == MT_FAR ) &&
        ti.Ofssize != USE_EMPTY &&
        ModuleInfo.Ofssize != ti.Ofssize ) {
        EmitError( OFFSET_SIZE_MISMATCH );
        return( ERROR );
    }

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_LABEL, 0, NULL );

    /* v2.08: if label has a true memory type, set total_size and total_length */
    if ( sym = CreateLabel( tokenarray[0].string_ptr, ti.mem_type, &ti, FALSE ) ) {
        DebugMsg1(("LabelDirective(%s): label created, memtype=%Xh size=%u\n", sym->name, sym->mem_type, ti.size ));
        /* sym->isdata must be 0, else the label directive was generated within data_item()
         * and fields total_size & total_length must not be modified then!
         */
        if ( sym->isdata == FALSE && ( sym->mem_type & MT_SPECIAL_MASK ) != MT_ADDRESS ) {
            sym->total_size = ti.size;
            sym->total_length = 1;
        }
        return( NOT_ERROR );
    }
    return( ERROR );
}
