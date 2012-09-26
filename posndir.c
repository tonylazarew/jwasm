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
* Description:  ALIGN, EVEN, ORG directives
*
****************************************************************************/

#include "globals.h"
#include "parser.h"
#include "segment.h"
#include "expreval.h"
#include "types.h"
#include "listing.h"
#include "posndir.h"
#include "fastpass.h"
#include "fixup.h"
#include "input.h"

#include "myassert.h"

static const uint_8 NopList16[] = {
    3,                  /* objlen of first NOP pattern */
    0x2E, 0x8b, 0xc0,   /* MOV AX,AX */
    0x89, 0xc0,         /* MOV AX,AX */
    0x90                /* NOP */
};

/* 32bit alignment fillers.
 For 5 bytes, Masm uses "add eax,dword ptr 0",
 which modifies the flags!
 */

static const uint_8 NopList32[] = {
    7,
    0x8d,0xa4,0x24,0,0,0,0,         /* lea     esp,[esp+00000000] */
    0x8d,0x80,0,0,0,0,              /* lea     eax,[eax+00000000] */
#if 0
    0x8d,0x40,0x00,                 /* lea     eax,[eax+00] */
    0x8b,0xc9,                      /* mov     ecx,ecx */
#else
    0x2e,0x8d,0x44,0x20,0x00,       /* lea     eax,cs:[eax+no_index_reg+00H] */
#endif
    0x8d,0x44,0x20,0x00,            /* lea     eax,[eax+no_index_reg+00H] */
    0x8d,0x40,0x00,                 /* lea     eax,[eax+00H] */
    0x8b,0xff,                      /* mov     edi,edi */
    0x90                            /* nop */
};

#if AMD64_SUPPORT
static const uint_8 NopList64[] = {
    7,
    0x0f,0x1f,0x80,0,0,0,0,         /* nop dword ptr [rax+0] */
    0x66,0x0f,0x1f,0x44,0,0,        /* nop word ptr [rax+rax] */
    0x0f,0x1f,0x44,0,0,             /* nop dword ptr [rax+rax] */
    0x0f,0x1f,0x40,0,               /* nop dword ptr [rax] */
    0x0f,0x1f,0,                    /* nop dword ptr [rax] */
    0x66,0x90,                      /* xchg ax,ax */
    0x90,                           /* nop */
};

/* just use the 32bit nops for 64bit */
static const uint_8 * const NopLists[] = { NopList16, NopList32, NopList64 };
#else
static const uint_8 * const NopLists[] = { NopList16, NopList32 };
#endif

ret_code OrgDirective( int i, struct asm_tok tokenarray[] )
/*********************************************************/
{
    //struct asym  *sym;
    //int_32       value = 0;
    struct expr opndx;

    DebugMsg1(("OrgDirective(%u) enter\n", i));
    i++;
    if ( ( ERROR == EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) ) )
        return( ERROR );
    if ( tokenarray[i].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    if ( CurrStruct ) {
        if ( opndx.kind == EXPR_CONST )
            return( SetStructCurrentOffset( opndx.value ) );
    } else {
        if( CurrSeg == NULL ) {
            EmitError( MUST_BE_IN_SEGMENT_BLOCK );
            return( ERROR );
        }
#if FASTPASS
        if ( StoreState == FALSE )
            FStoreLine(0);
#endif
        /* v2.04: added */
        if ( Parse_Pass == PASS_1 && CurrSeg->e.seginfo->FixupListHead )
            CurrSeg->e.seginfo->FixupListHead->orgoccured = TRUE;

        if ( opndx.kind == EXPR_CONST )
            return( SetCurrOffset( CurrSeg, opndx.value, FALSE, FALSE ) );
        else if ( opndx.kind == EXPR_ADDR && opndx.indirect == FALSE )
            return( SetCurrOffset( CurrSeg, opndx.sym->offset + opndx.value, FALSE, FALSE ) );
    }
    EmitError( ORG_NEEDS_A_CONSTANT_OR_LOCAL_OFFSET );
    return( ERROR );
}

static void fill_in_objfile_space( uint size )
/********************************************/
{
    int i;
    int nop_type;

    /* emit
     - nothing ... for BSS
     - x'00'   ... for DATA
     - nops    ... for CODE
     */

    /* v2.04: no output if nothing has been written yet */
    if( CurrSeg->e.seginfo->written == FALSE ) {

        SetCurrOffset( CurrSeg, size, TRUE, TRUE );

    } else if( CurrSeg->e.seginfo->segtype != SEGTYPE_CODE ) {

        FillDataBytes( 0x00, size ); /* just output nulls */

    } else {
        /* output appropriate NOP type instructions to fill in the gap */

        while( size > NopLists[ ModuleInfo.Ofssize ][0] ) {
            for( i = 1; i <= NopLists[ ModuleInfo.Ofssize ][0]; i++ ) {
                OutputByte( NopLists[ ModuleInfo.Ofssize ][i] );
            }
            size -= NopLists[ ModuleInfo.Ofssize ][0];
        }
        if( size == 0 ) return;

        i=1; /* here i is the index into the NOP table */
        for( nop_type = NopLists[ ModuleInfo.Ofssize ][0]; nop_type > size ; nop_type-- ) {
            i+=nop_type;
        }
        /* i now is the index of the 1st part of the NOP that we want */
        for( ; nop_type > 0; nop_type--,i++ ) {
            OutputByte( NopLists[ ModuleInfo.Ofssize ][i] );
        }
    }
}

/* align current offset to value ( alignment is 2^value ) */

void AlignCurrOffset( int value )
/*******************************/
{
    int seg_align;
    int alignment = (1 << value);
    unsigned int CurrAddr;

    CurrAddr = GetCurrOffset();
    seg_align = CurrAddr % alignment;
    if( seg_align ) {
        alignment -= seg_align;
        fill_in_objfile_space( alignment );
    }
}

ret_code AlignDirective( int i, struct asm_tok tokenarray[] )
/***********************************************************/
{
    int_32 align_val;
    int seg_align;
    struct expr opndx;
    uint_32 CurrAddr;
    char buffer[32];

    DebugMsg1(("AlignDirective enter\n"));

    switch( tokenarray[i].tokval ) {
    case T_ALIGN:
        i++;
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
            return( ERROR );
        if ( opndx.kind == EXPR_CONST ) {
            int power;
            align_val = opndx.value;
            /* check that the parm is a power of 2 */
            for( power = 1; power < align_val; power <<= 1 );
            if( power != align_val ) {
                EmitError( POWER_OF_2 );
                return( ERROR );
            }
        } else if ( opndx.kind == EXPR_EMPTY ) { /* ALIGN without argument? */
            /* v2.03: special STRUCT handling was missing */
            if ( CurrStruct )
                align_val = CurrStruct->e.structinfo->alignment;
            else
                align_val = GetCurrSegAlign();
        } else {
            EmitError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        break;
    case T_EVEN:
        align_val = 2;
        i++;
        break;
    }
    if ( tokenarray[i].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    /* ALIGN/EVEN inside a STRUCT definition? */
    if ( CurrStruct )
        return( AlignInStruct( align_val ));

#if FASTPASS
    if ( StoreState == FALSE )
        FStoreLine(0);
#endif
    seg_align = GetCurrSegAlign(); /* # of bytes */
    if( seg_align <= 0 ) {
        EmitError( MUST_BE_IN_SEGMENT_BLOCK );
        return( ERROR );
    }
    if( align_val > seg_align ) {
        if ( Parse_Pass == PASS_1 )
            EmitWarn( 1, ALIGN_TOO_HIGH, myltoa( align_val, buffer, 10, FALSE, FALSE ) );
        //return( ERROR ); /* v2.0: don't exit */
    }
    /* v2.04: added, Skip backpatching after ALIGN occured */
    if ( Parse_Pass == PASS_1 && CurrSeg && CurrSeg->e.seginfo->FixupListHead )
        CurrSeg->e.seginfo->FixupListHead->orgoccured = TRUE;
    /* find out how many bytes past alignment we are & add the remainder */
    /* store temp. value */
    CurrAddr = GetCurrOffset();
    seg_align = CurrAddr % align_val;
    if( seg_align ) {
        align_val -= seg_align;
        fill_in_objfile_space( align_val );
    }
    if ( CurrFile[LST] ) {
        LstWrite( LSTTYPE_DATA, CurrAddr, NULL );
    }
    DebugMsg1(("AlignDirective exit\n"));
    return( NOT_ERROR );
}
