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
* Description:  expression evaluator.
*
****************************************************************************/

#include <stddef.h>

#include "globals.h"
#include "parser.h"
#include "reswords.h"
#include "expreval.h"
#include "segment.h"
#include "proc.h"
#include "assume.h"
#include "input.h"
#include "tokenize.h"
#include "types.h"
#include "labels.h"
#include "data.h"

#define ALIAS_IN_EXPR 1 /* allow alias names in expression */

#define UNARY_PLUSMINUS 0
#define BINARY_PLUSMINUS 1

/* activate if a detailed error location is needed and -d6 cant be used */
#if 0
#define ERRLOC( i ) printf("Error at %s.%u: %u >%s< >%s<\n", __FILE__, __LINE__, i, ModuleInfo.tokenarray[i].string_ptr, ModuleInfo.tokenarray[0].tokpos )
//#undef DebugMsg1
//#define DebugMsg1( x ) printf x
#else
#define ERRLOC( i )
#endif

static int op_sq_bracket_level;
#ifdef DEBUG_OUT
static int evallvl = 0;
#endif

/* error msg flag.
 * 1=display error messages.
 * for historical reasons, it's also used to detect
 * "EQU mode" (error_msg = 0).
 */
static char error_msg;
static char eflags;

static struct asym *thissym; /* helper symbol for THIS operator */
static struct asym *nullstruct; /* used for T_DOT if there's no current type */
static struct asym *nullmbr; /* used for T_DOT if struct is a forward ref */

enum process_flag {
    PROC_BRACKET, /* evaluate until the end or a closing bracket is found */
    PROC_OPERAND  /* stop if an operator with lower precedence is found */
};

enum labelsize {
    LS_SHORT  = 0xFF01, /* it's documented, but can a label be "short"? */
    LS_NEAR16 = 0xFF02,
    LS_NEAR32 = 0xFF04,
    LS_FAR16  = 0xFF05,
    LS_FAR32  = 0xFF06
};

static ret_code evaluate( struct expr *, int *, struct asm_tok[], int, enum process_flag );

void EmitConstError( const struct expr *opnd )
/********************************************/
{
    if ( opnd->hlvalue != 0 )
        EmitErr( CONSTANT_VALUE_TOO_LARGE_EX, opnd->hlvalue, opnd->value64 );
    else
        EmitErr( CONSTANT_VALUE_TOO_LARGE, opnd->value64 );
    return;
}

static void init_expr( struct expr *opnd )
/****************************************/
{
    opnd->value    = 0;
    opnd->hvalue   = 0;
    opnd->hlvalue  = 0;
    opnd->quoted_string   = NULL;
    opnd->base_reg = NULL;
    opnd->idx_reg  = NULL;
    opnd->label_tok = NULL;
    opnd->override = NULL;
    opnd->instr    = EMPTY;
    opnd->kind     = EXPR_EMPTY;
    opnd->mem_type = MT_EMPTY;
    opnd->scale    = 0;
    opnd->Ofssize  = USE_EMPTY;
    opnd->flags1   = 0;
    opnd->sym      = NULL;
    opnd->mbr      = NULL;
    opnd->type     = NULL;
}

static void TokenAssign( struct expr *opnd1, const struct expr *opnd2 )
/*********************************************************************/
{
#if 1
    /* note that offsetof() is used. This means, don't change position
     of field <type> in expr! */
    memcpy( opnd1, opnd2, offsetof( struct expr, type ) );
#else
    opnd1->llvalue  = opnd2->llvalue;
    opnd1->hlvalue  = opnd2->hlvalue;
    opnd1->quoted_string   = opnd2->quoted_string; /* probably useless */
    opnd1->base_reg = opnd2->base_reg;
    opnd1->idx_reg  = opnd2->idx_reg;
    opnd1->label_tok = opnd2->label_tok;
    opnd1->override = opnd2->override;
    opnd1->instr    = opnd2->instr;
    opnd1->kind     = opnd2->kind;
    opnd1->mem_type = opnd2->mem_type;
    opnd1->scale    = opnd2->scale;
    opnd1->Ofssize  = opnd2->Ofssize;
    opnd1->flags1   = opnd2->flags1;
    opnd1->sym      = opnd2->sym;
    opnd1->mbr      = opnd2->mbr;
//  opnd1->type     = opnd2->type;
#endif
}

//#define BRACKET_PRECEDENCE 1
//#define PTR_PRECEDENCE     4
//#define PLUS_PRECEDENCE    9
#define CMP_PRECEDENCE    10

static int get_precedence( const struct asm_tok *item )
/*****************************************************/
{
    /* The following table is taken verbatim from MASM 6.1 Programmer's Guide,
     * page 14, Table 1.3.

     * 1            (), []
     * 2            LENGTH, SIZE, WIDTH, MASK, LENGTHOF, SIZEOF
     * 3            . (structure-field-name operator)
     * 4            : (segment override operator), PTR
     * 5            LROFFSET, OFFSET, SEG, THIS, TYPE
     * 6            HIGH, HIGHWORD, LOW, LOWWORD
     * 7            +, - (unary)
     * 8            *, /, MOD, SHL, SHR
     * 9            +, - (binary)
     * 10           EQ, NE, LT, LE, GT, GE
     * 11           NOT
     * 12           AND
     * 13           OR, XOR
     * 14           OPATTR, SHORT, .TYPE

     * The following table appears in QuickHelp online documentation for
     * both MASM 6.0 and 6.1. It's slightly different!

     * 1            LENGTH, SIZE, WIDTH, MASK
     * 2            (), []
     * 3            . (structure-field-name operator)
     * 4            : (segment override operator), PTR
     * 5            THIS, OFFSET, SEG, TYPE
     * 6            HIGH, LOW
     * 7            +, - (unary)
     * 8            *, /, MOD, SHL, SHR
     * 9            +, - (binary)
     * 10           EQ, NE, LT, LE, GT, GE
     * 11           NOT
     * 12           AND
     * 13           OR, XOR
     * 14           SHORT, OPATTR, .TYPE, ADDR

     * japheth: the first table is the prefered one. Reasons:
     * - () and [] must be first.
     * - it contains operators SIZEOF, LENGTHOF, HIGHWORD, LOWWORD, LROFFSET
     * - ADDR is no operator for expressions. It's exclusively used inside
     *   INVOKE directive.

     * However, what's wrong in both tables is the precedence of
     * the dot operator: Actually for both JWasm and Wasm the dot precedence
     * is 2 and LENGTH, SIZE, ... have precedence 3 instead.

     * Precedence of operator TYPE was 5 in original Wasm source. It has
     * been changed to 4, as described in the Masm docs. This allows syntax
     * "TYPE DWORD ptr xxx"

     * v2.02: another case which is problematic:
     *     mov al,BYTE PTR CS:[]
     * Since PTR and ':' have the very same priority, the evaluator will
     * first calculate 'BYTE PTR CS'. This is invalid, but didn't matter
     * prior to v2.02 because register coercion was never checked for
     * plausibility. Solution: priority of ':' is changed from 4 to 3.
     */

    switch( item->token ) {
    case T_UNARY_OPERATOR:
    case T_BINARY_OPERATOR:
        return( item->precedence );
    case T_OP_BRACKET:
    case T_OP_SQ_BRACKET:
        /* v2.08: with -Zm, the priority of [] and (), if
         * used as binary operator, is 9 (like binary +/-).
         * test cases: mov ax,+5[bx]
         *             mov ax,-5[bx]
         */
        //return( 1 );
        return( ModuleInfo.m510 ? 9 : 1 );
    case T_DOT:
        return( 2 );
    case T_COLON:
        //return( 4 );
        return( 3 ); /* changed for v2.02 */
    case '*':
    case '/':
        return( 8 );
    case '+':
    case '-':
        return( item->specval ? 9 : 7 );
    }
    /* shouldn't happen! */
    DebugMsg(("get_precedence: unexpected operator=%s\n", item->string_ptr));
    EmitErr( SYNTAX_ERROR_EX, item->string_ptr );
    return( ERROR );
}

#if 0
static bool is_operator( enum tok_type tt )
/*****************************************/
/* determine if token is an operator */
{
    /* T_OP_BRACKET and above: "(,[,],},:,.,+,-,*,/" */
    /* rest: T_REG, T_STYPE, T_RES_ID, T_ID, T_STRING,
     * T_NUM, T_FLOAT, T_BAD_NUM, T_DBL_COLON, T_PERCENT
     */
    return( tt >= T_OP_BRACKET || tt == T_UNARY_OPERATOR || tt == T_BINARY_OPERATOR );
}

static bool is_unary_op( enum tok_type tt )
/*****************************************/
/* determine if token is an unary operator */
{
    return( tt == T_OP_BRACKET || tt == T_OP_SQ_BRACKET || tt ==  '+' || tt == '-' || tt == T_UNARY_OPERATOR );
}

#else
#define is_operator( tt ) ( tt >= T_OP_BRACKET || tt == T_UNARY_OPERATOR || tt == T_BINARY_OPERATOR )
#define is_unary_op( tt ) ( tt == T_OP_BRACKET || tt == T_OP_SQ_BRACKET || tt ==  '+' || tt == '-' || tt == T_UNARY_OPERATOR )
#endif

/* get value for types
 * NEAR, FAR and PROC are handled slightly differently:
 * the HIBYTE is set to 0xFF, and PROC depends on the memory model
 */
static unsigned int GetTypeSize( enum memtype mem_type, int Ofssize )
/*******************************************************************/
{
    if ( (mem_type & MT_SPECIAL) == 0 )
        return( ( mem_type & MT_SIZE_MASK ) + 1 );
    switch ( mem_type ) {
    case MT_NEAR:
        switch ( Ofssize ) {
        case USE16: return( 2 );
        case USE32: return( 4 );
        }
        return (0xFF00 | ( 2 << ModuleInfo.Ofssize ) ) ;
    case MT_FAR:
        switch ( Ofssize ) {
        case USE16:  return( 4 );
        case USE32:  return( 6 );
        }
        return (0xFF00 | ( ( 2 << ModuleInfo.Ofssize ) + 2 ) );
    }
    /* shouldn't happen */
    return( 0 );
}

static void CEmitError( int msg )
/*******************************/
{
    if ( error_msg )
        EmitError( msg );
}

#if AMD64_SUPPORT
static uint_64 GetRecordMask( struct dsym *record )
#else
static uint_32 GetRecordMask( struct dsym *record )
#endif
/*************************************************/
{
#if AMD64_SUPPORT
    uint_64 mask = 0;
#else
    uint_32 mask = 0;
#endif
    int i;
    struct field_item *fl;

    for ( fl = record->e.structinfo->head; fl; fl = fl->next ) {
        struct asym *sym = fl->sym;
        for ( i = sym->offset ;i < sym->offset + sym->total_size; i++ )
            mask |= 1 << i;
    }
    return( mask );
}

/* v2.06: the value of number strings is now evaluated here.
 * Prior to v2.06, it was evaluated in the tokenizer and the
 * value was stored in the token string buffer. Since the content
 * of the token buffer is no longer destroyed when macros or
 * generated code is run, the old strategy needed too much space.
 */

void myatoi128( const char *src, uint_64 dst[], int base, int size )
/******************************************************************/
{
    uint_32             val;
    unsigned            len;
    const char          *end = src + size;
    uint_16             *px;

    dst[0] = 0;
    dst[1] = 0;
    while( src < end ) {
        val = ( *src <= '9' ? *src - '0' : ( *src | 0x20 ) - 'a' + 10 );
        px = (uint_16 *)dst;
        for ( len = ( 2 * sizeof( uint_64 ) ) >> 1; len; len-- ) {
            val += (uint_32)*px * base;
            *(px++) = val;
            val >>= 16;
        };
        src++;
    }
    return;
}

/* get an operand. operands are:
 * - integer constant      : EXPR_CONST
 * - quoted string         : EXPR_CONST
 * - register              : EXPR_REG (indirect = 1/0)
 * - user identifier (T_ID): EXPR_ADDR | EXPR_CONST
 * - reserved ID (T_RES_ID): EXPR_CONST ( EXPR_ADDR if id=FLAT )
 * - float constant        : EXPR_FLOAT
 *
 * valid user identifiers are
 * - TYPE ( struct/union, typedef, record )
 * - STRUCT FIELD (also bitfield)
 * - variable (internal, external, stack ) or constant (EQU, '=')
 * valid reserved IDs are types (BYTE, WORD, ... ) and FLAT
 */
static ret_code get_operand( struct expr *opnd, int *idx, struct asm_tok tokenarray[] )
/*************************************************************************************/
{
    char        *tmp;
    struct asym *sym;
    int         i = *idx;
    int         j;
    char        labelbuff[16];/* for anonymous labels */

    DebugMsg1(("get_operand(start=%u >%s<) enter, memtype=%Xh\n", i, tokenarray[i].tokpos, opnd->mem_type ));
    switch( tokenarray[i].token ) {
    case T_NUM:
        DebugMsg1(("get_operand: T_NUM (%s, base=%u, len=%u)\n", tokenarray[i].string_ptr, tokenarray[i].numbase, tokenarray[i].itemlen ));
        opnd->kind = EXPR_CONST;
        myatoi128( tokenarray[i].string_ptr, &opnd->llvalue, tokenarray[i].numbase, tokenarray[i].itemlen );
        //opnd->llvalue = tokenarray[i].value64;
        //opnd->hlvalue = ( tokenarray[i].numflg == NF_NULL ? 0 : *(uint_64 *)( tokenarray[i].string_ptr - sizeof(uint_64) ) );
        break;
    case T_STRING:
        DebugMsg1(("get_operand: T_STRING (%s), size=%u\n", tokenarray[i].string_ptr, tokenarray[i].stringlen ));
        /* string enclosed in <> or {} are rejected since v1.94! */
        if ( tokenarray[i].string_delim != '"' && tokenarray[i].string_delim != '\'') {
            if ( opnd->is_opattr ) /* OPATTR operator accepts anything! */
                break;
            if ( error_msg )
                /* v2.0: display a comprehensible error msg if a quote is missing */
                if ( tokenarray[i].string_delim == NULLC &&
                    ( *tokenarray[i].string_ptr == '"' || *tokenarray[i].string_ptr == '\'' ))
                    EmitErr( MISSING_QUOTATION_MARK_IN_STRING );
                else
                    EmitErr( UNEXPECTED_LITERAL_FOUND_IN_EXPRESSION, tokenarray[i].tokpos );
            return( ERROR );
        }
        opnd->kind = EXPR_CONST;
        opnd->quoted_string = &tokenarray[i];
        //opnd->value = 0;
        tmp = tokenarray[i].string_ptr + 1; /* skip the quote */

        /* v2.06: use max. 16 bytes to create the "value".
         * Prior to 2.06, max 8 bytes were used for 64-bit and
         * max 4 bytes were used for 16-/32-bit.
         */
        j = ( tokenarray[i].stringlen > sizeof( opnd->chararray ) ? sizeof( opnd->chararray ) : tokenarray[i].stringlen );
        for( ; j; j-- )
            opnd->chararray[j-1] = *tmp++;
        break;
    case T_REG:
        DebugMsg1(( "get_operand: T_REG (%s)\n", tokenarray[i].string_ptr ));
        opnd->kind = EXPR_REG;
        opnd->base_reg = &tokenarray[i];
        j = tokenarray[i].tokval;
#if 1
        /* this check was previously done in the parser.
         Check if the register needs an extensions (which are bit masks).
         If no, then check if the cpu is sufficient.
         */
        if( ( ( GetCpuSp( j ) & P_EXT_MASK ) &&
            (( GetCpuSp( j ) & ModuleInfo.curr_cpu & P_EXT_MASK) == 0) ||
              ( ModuleInfo.curr_cpu & P_CPU_MASK ) < ( GetCpuSp( j ) & P_CPU_MASK ) ) ) {
            CEmitError( INSTRUCTION_OR_REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
            return( ERROR );
        }
#endif
        if( op_sq_bracket_level > 0 ) {
            /* a valid index register? */
            if ( GetSflagsSp( j ) & SFR_IREG ) {
                opnd->indirect = TRUE;
                opnd->assumecheck = TRUE;

                /* v2.06: in strict Masm mode, a base/index register must not
                 * be preceded by "<segreg|segment>:" INSIDE the brackets!
                 */
                /* v2.08: test moved to colon_op() */
                //if ( Options.strict_masm_compat && tokenarray[i-1].token == T_COLON )
                //    CEmitError( INVALID_USE_OF_REGISTER );

            } else if ( GetValueSp( j ) & OP_SR ) {
                /* a segment register inside square brackets is only
                 * accepted by Masm if it is the segment part of an
                 * address (mov ax,[bx+cs:label])!
                 */
                if( tokenarray[i+1].token != T_COLON ) {
                    CEmitError( INVALID_USE_OF_REGISTER );
                    return( ERROR );
                }
            } else {
                CEmitError( MUST_BE_INDEX_OR_BASE_REGISTER );
                return( ERROR );
            }
#if 0 /* v2.08: now done in calculate() for [] and () op */
        } else if( j == T_ST ) {

            struct expr sti;
            char cl_bracket;

            /* read st(i), put i into idx_reg */
            i++;
            switch( tokenarray[i].token ) {
            case T_OP_BRACKET:    /* syntax st(0) */
            case T_OP_SQ_BRACKET: /* syntax st[0] */
                /* v2.06: added: skip the opening bracket! This ensures that
                 * evaluate() will definitely stop at the closing bracket.
                 * The type of the closing bracket must be checked then! */
                *start = i+1;
                init_expr( &sti );
                if( evaluate( &sti, start, tokenarray, end, PROC_BRACKET ) == ERROR ) {
                    return( ERROR );
                }
                cl_bracket = ( tokenarray[i].token == T_OP_BRACKET ? T_CL_BRACKET : T_CL_SQ_BRACKET );
                if( tokenarray[*start].token != cl_bracket ) {
                    CEmitError( MISSING_RIGHT_PARENTHESIS_IN_EXPRESSION );
                    return( ERROR );
                }
                if( sti.kind != EXPR_CONST ) {
                    CEmitError( CONSTANT_OPERAND_EXPECTED );
                    return( ERROR );
                }
                opnd->st_idx = sti.value;
                break;
            default:
                opnd->st_idx = 0; /* st = st(0) */
                break;
            }
#endif
        }
        break;
    case T_ID:
        tmp = tokenarray[i].string_ptr;
        if ( opnd->type ) {
            DebugMsg1(("get_operand(%s): ID, type=%s\n", tokenarray[i].string_ptr, opnd->type->name ));
            opnd->value = 0;
            if ( opnd->type == nullstruct )
                sym = NULL;
            else {
                sym = SearchNameInStruct( opnd->type, tmp, &opnd->uvalue, 0 );
                DebugMsg1(("get_operand(%s): SearchNameInStruct(%s)=%X, offset=%u\n",
                           tokenarray[i].string_ptr, opnd->type->name, sym, opnd->uvalue));
            }
            if ( sym == NULL ) {
                sym = SymSearch( tmp );
                if ( sym ) {
                    /*
                     * skip a type specifier matching the data item's type
                     * that's something like "<item>.<type>.<member>"
                     */
                    if ( sym->state == SYM_TYPE ) {
                        /* superfluous type spec? */
                        /* v2.7: "if" added.
                         * Masm accepts a different type spec if the "assumed"
                         * type is undefined */
                        if ( sym == opnd->type || opnd->type->isdefined == FALSE )
                            ; //opnd->sym = sym;
                        else
                            sym = NULL;
                    } else if ( ModuleInfo.oldstructs &&
                               ( sym->state == SYM_STRUCT_FIELD ||
                                sym->state == SYM_EXTERNAL || /* v2.01: added */
                                /* v2.05: changed */
                                //( sym->state == SYM_INTERNAL && sym->mem_type == MT_ABS ) ) )
                                sym->state == SYM_INTERNAL ) )
                        //opnd->sym = sym;
                        ;
                    else {
                        /* fixme: clear sym?
                         * if the symbol is not a type, it's an error which can
                         * be detected in pass 1 already. dot_op() will emit
                         * 'struct field expected' if sym isn't cleared.
                         */
                        if ( opnd->type != nullstruct )
                            sym = NULL;
                    }
                }
            }
        } else {
            DebugMsg1(("get_operand(%s): ID\n", tokenarray[i].string_ptr ));
            /* ensure anonym labels are uppercase */
            /* v2.06: changed. Previously member 'string_ptr' was used to
             * store the anonymous label, but one cannot safely assume that
             * there's enough free space for a larger symbol name! It (partly)
             * worked by accident, because @F/@B usually are the last tokens
             * in a line [ but see: .if ( eax == @F && ecx == 2 ) ].
             */
            if ( *tmp == '@' && *(tmp+2 ) == NULLC ) {
                if ( *(tmp+1) == 'b' || *(tmp+1 ) == 'B' )
                    tmp = GetAnonymousLabel( labelbuff, 0 );
                else if (*(tmp+1) == 'f' || *(tmp+1 ) == 'F' )
                    tmp = GetAnonymousLabel( labelbuff, 1 );
            }
            sym = SymSearch( tmp );
        }
        if ( sym == NULL ||
            sym->state == SYM_UNDEFINED ||
#if ALIAS_IN_EXPR == 0
            sym->state == SYM_ALIAS || /* v2.04: added */
#endif
            sym->state == SYM_MACRO ||
            sym->state == SYM_TMACRO ) {

            /* for OPATTR, anything is ok */
            if ( opnd->is_opattr ) {
                DebugMsg1(( "get_operand(%s): OPATTR, symbol invalid\n", tokenarray[i].string_ptr ));
                opnd->kind = EXPR_UNDEF;
                break;
            }
            /* if it is EQU (then error_msg is FALSE), don't display an error,
             but return ERROR */
            if ( error_msg == FALSE ) {
                DebugMsg1(("get_operand(%s): EQU, symbol invalid\n", tokenarray[i].string_ptr));
                return( ERROR );
            }
            if ( sym && ( sym->state == SYM_MACRO ||
#if ALIAS_IN_EXPR == 0
                         sym->state == SYM_ALIAS || /* v2.04: added */
#endif
                         sym->state == SYM_TMACRO ) ) {
                DebugMsg1(("get_operand(%s): symbol is macro/textmacro/alias!\n", tokenarray[i].string_ptr));
                EmitErr( INVALID_SYMBOL_TYPE_IN_EXPRESSION, sym->name );
                return( ERROR );
            }
            //if( Parse_Pass == PASS_1 ) {
            if( Parse_Pass == PASS_1 ) {
                /* if symbol wasn't found, assume it is a forward ref! */
                if ( sym == NULL ) {
                    if ( opnd->type == NULL && !( eflags & EXPF_NOLCREATE ) ) { /* added v1.95 */
                        sym = SymLookup( tmp );
                        sym->state = SYM_UNDEFINED;
                        sym_add_table( &SymTables[TAB_UNDEF], (struct dsym *)sym ); /* add UNDEFINED */
                        DebugMsg1(("get_operand(%s): symbol not (yet) defined, CurrProc=%s\n", tmp, CurrProc ? CurrProc->sym.name : "NULL" ));
                        /* v2.08: if changed */
                    // } else if ( opnd->type == NULL || opnd->type != nullstruct ) {
                    } else if ( opnd->type == NULL || opnd->type->typekind != TYPE_NONE ) {
                        /* no struct or struct is known and defined */
                        DebugMsg(("get_operand(%s): symbol error (type=%s kind=%u)\n", tmp, opnd->type ? opnd->type->name : "NULL", opnd->type ? opnd->type->typekind : 0 ));
                        if ( opnd->type && *opnd->type->name )
                            EmitErr( MEMBER_NOT_DEFINED, opnd->type->name, tmp );
                        else
                            EmitErr( SYMBOL_NOT_DEFINED, tmp );
                        return( ERROR );
                    } else {
                        /* forward reference to a struct.
                         * In these cases, assume everything is ok.
                         */
                        if ( !nullmbr ) {
                            nullmbr = SymAlloc( "" );
                        }
                        DebugMsg(("get_operand(%s): forward reference to a struct (using nullmbr)\n", tmp ));
                        /* "break" because nullmbr has state SYM_UNDEFINED */
                        opnd->mbr = nullmbr;
                        opnd->kind = EXPR_CONST;
                        break;
                    }
                }
            } else {
                DebugMsg1(("get_operand(%s): symbol %s not defined, pass > 1, curr proc=>%s<, \n", tokenarray[i].string_ptr, tmp, CurrProc ? CurrProc->sym.name : "NULL" ));
                if ( opnd->type && *opnd->type->name ) {
                    EmitErr( MEMBER_NOT_DEFINED, opnd->type->name, tmp );
                } else {
                    EmitErr( SYMBOL_NOT_DEFINED, *(tmp+1) == '&' ? "@@" : tmp );
                }
                return( ERROR );
            }
#if ALIAS_IN_EXPR /* v2.04b: added */
        } else if ( sym->state == SYM_ALIAS ) {
            /* ALIAS symbols are not really useable in expressions.
             * The alias' substitute symbol is, however.
             */
            sym = sym->substitute; /* can't be NULL */
#endif
        }
        /* set default values */
        sym->used = TRUE;
        DebugMsg1(("get_operand(%s): sym->state=%u type=%X ofs=%X memtype=%Xh total_size=%u defined=%u\n",
                  tokenarray[i].string_ptr, sym->state, sym->type, sym->offset, sym->mem_type, sym->total_size, sym->isdefined ));
        switch ( sym->state ) {
        case SYM_TYPE: /* STRUCT, UNION, RECORD, TYPEDEF */
            if ( ((struct dsym *)sym)->e.structinfo->isOpen == FALSE ) {
                opnd->kind = EXPR_CONST;
                //opnd->mem_type = MT_ABS;
                opnd->mem_type = sym->mem_type;
                opnd->is_type = TRUE;
                DebugMsg1(("get_operand(%s): symbol.kind=%u (STRUCT/UNION/TYPEDEF/RECORD)\n", sym->name, sym->typekind ));

                /* v2.08: if() removed. This was an old hack. */
                //if ( tokenarray[i-1].token != T_DOT && tokenarray[i+1].token != T_DOT )
                /* v2.06: the default value for RECORD types is the mask value */
                if ( sym->typekind == TYPE_RECORD ) {
#if AMD64_SUPPORT
                    opnd->llvalue = GetRecordMask( (struct dsym *)sym );
#else
                    opnd->value = GetRecordMask( (struct dsym *)sym );
#endif
                } else
                    opnd->value = sym->total_size;

            } else {
                DebugMsg1(("get_operand(%s): struct/union definition isn't closed!\n", sym->name ));
                /* a valid constant should be returned if
                 1. the struct is open      AND
                 2. it's not an EQU operand
                 the number isn't used then (except if it's the first DUP operand)
                 */
                if ( error_msg == FALSE )
                    opnd->kind = EXPR_UNDEF;
                else {
                    opnd->kind = EXPR_CONST;
                    /* v2.07: MT_ABS is obsolete */
                    //opnd->mem_type = MT_ABS;
                    opnd->value = -1;
                    opnd->is_type = TRUE;
                }
            }
            /* skip "alias" types. Obsolete? */
            for ( ; sym->type; sym = sym->type );
            opnd->type = sym;
            break;
        case SYM_STRUCT_FIELD:
            DebugMsg1(("get_operand(%s): structure field, ofs=%Xh\n", sym->name, sym->offset ));

            /* opnd->value might have been set by SearchNameInStruct() already! */
            opnd->value += sym->offset;
            opnd->kind = EXPR_CONST;
            opnd->mbr = sym;
            /* skip "alias" types (probably obsolete by now!) */
            for ( ; sym->type; sym = sym->type );
            opnd->mem_type = sym->mem_type;
            /*
             * check if the member field has arbitrary type.
             * If yes, set the <type> member!
             * It's probably better to handle this case in PrepareOp()
             * or in dot_op().
             */
            if ( sym->state == SYM_TYPE && sym->typekind != TYPE_TYPEDEF ) {
                opnd->type = sym;
            } else {
                opnd->type = NULL; /* added v1.96 */
            }
            DebugMsg1(("get_operand: mem_type=%Xh type=%s\n", opnd->mem_type, opnd->type ? opnd->type->name : "NULL" ));
            break;
        default: /* SYM_INTERNAL, SYM_EXTERNAL, SYM_SEG, SYM_GRP, SYM_STACK */
            opnd->kind = EXPR_ADDR;
            /* call internal function (@Line, ... ) */
            if ( sym->predefined && sym->sfunc_ptr )
                sym->sfunc_ptr( sym );
            //if( opnd->sym->mem_type == MT_ABS ) {
            if( sym->state == SYM_INTERNAL && sym->segment == NULL ) {
                opnd->kind = EXPR_CONST;
                opnd->uvalue = sym->uvalue;
                opnd->hvalue = sym->value3264;
                DebugMsg1(("get_operand(%s): equate hval=%Xh, lval=%Xh\n", sym->name, opnd->hvalue, opnd->uvalue ));
                opnd->mem_type = sym->mem_type;
                /* don't set the symbol reference, it isn't a label */
            } else if( sym->state == SYM_EXTERNAL &&
                      sym->mem_type == MT_EMPTY &&
                      sym->iscomm == FALSE ) {
                /* type remains EXPR_ADDR, to force fixup creation */
                opnd->mem_type = sym->mem_type;
                opnd->abs = TRUE;
                opnd->sym = sym;
            } else {
                opnd->label_tok = &tokenarray[i];
                /* a variable with arbitrary type? */
                /* v2.05: added check for MT_EMPTY */
                //if( opnd->sym->type ) { 
                if( sym->type && sym->type->mem_type != MT_EMPTY ) {
                    /* skip "alias" types */
                    /* v2.05: obsolete */
                    //for ( sym2 = opnd->sym; sym2->type; sym2 = sym2->type );
                    //opnd->mem_type = sym2->mem_type;
                    opnd->mem_type = sym->type->mem_type;
                } else {
                    opnd->mem_type = sym->mem_type;
                }
                /* since there is no fixup for auto variables, the "offset"
                 must be stored in the <value> field */
                if ( sym->state == SYM_STACK ) {
                    opnd->llvalue = sym->offset;
                    opnd->indirect = TRUE;
                }
                opnd->sym = sym;
            }
            break;
        }
        break;
    case T_STYPE:
        DebugMsg1(("get_operand: T_STYPE, >%s<, value=%X\n", tokenarray[i].string_ptr, tokenarray[i].tokval));
        opnd->kind = EXPR_CONST;
        /* for types, return the size as numeric constant */
        /* fixme: mem_type should be set only when used as first arg of PTR op! */
        opnd->mem_type = GetMemtypeSp( tokenarray[i].tokval );
        opnd->Ofssize = GetSflagsSp( tokenarray[i].tokval );
        opnd->value = GetTypeSize( opnd->mem_type, opnd->Ofssize );
        opnd->is_type = TRUE;
        opnd->type = NULL; /* v2.08: added */
        break;
    case T_RES_ID:
        DebugMsg1(("get_operand: T_RES_ID, >%s<, value=%X\n", tokenarray[i].string_ptr, tokenarray[i].tokval));
        if ( tokenarray[i].tokval == T_FLAT ) {
            if ( error_msg ) { /* don't define FLAT group in EQU expression! */
                /* v2.08 cpu check added */
                if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) {
                    EmitError( INSTRUCTION_OR_REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
                    return( ERROR );
                }
                DefineFlatGroup();
            }
            if ( !( opnd->sym = &ModuleInfo.flat_grp->sym ) )
                return( ERROR );

            opnd->label_tok = &tokenarray[i];
            opnd->kind = EXPR_ADDR;

        } else {
            if( error_msg )
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
        break;
    case T_FLOAT: /* v2.05 */
        opnd->kind = EXPR_FLOAT;
        opnd->float_tok = &tokenarray[i];
        //opnd->ftype = ( tokenarray[i].floattype != 0 );
        break;
    //case T_CL_BRACKET:
    //case T_CL_SQ_BRACKET:
    default:
        DebugMsg1(("get_operand, default: i=%d, string=%s\n", i, tokenarray[i].string_ptr));
        if ( opnd->is_opattr ) {    /* for OPATTR, allow any operand */
            if ( tokenarray[i].token == T_FINAL ||
                tokenarray[i].token == T_CL_BRACKET ||
                tokenarray[i].token == T_CL_SQ_BRACKET ) /* don't go beyond T_FINAL, ) or ] ! */
                return( NOT_ERROR );
            break;
        }
        if( error_msg ) {
            if ( tokenarray[i].token == T_BAD_NUM )
                /* Masm complains even if in EQU-mode */
                EmitErr( NONDIGIT_IN_NUMBER, tokenarray[i].string_ptr );
            else if ( tokenarray[i].token == T_COLON )
                EmitError( SYNTAX_ERROR_UNEXPECTED_COLON );
            else if ( is_operator( tokenarray[i].token ) ) {
                EmitError( OPERAND_EXPECTED ); ERRLOC(*i);
            } else
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
        }
        return( ERROR );
    }
    (*idx)++;
    DebugMsg1(("get_operand exit, ok, value=%" I64X_SPEC " hvalue=%" I64X_SPEC " mem_type=%Xh abs=%u string=%s type=>%s<\n",
              opnd->llvalue, opnd->hlvalue, opnd->mem_type, opnd->abs, opnd->quoted_string ? opnd->quoted_string->string_ptr : "NULL", opnd->type ? opnd->type->name : "NULL" ));
    return( NOT_ERROR );
}

#if 0
static bool check_same( struct expr *opnd1, struct expr *opnd2, enum exprtype kind )
/**********************************************************************************/
/* Check if both tok_1 and tok_2 equal type */
{
    if( opnd1->kind == kind &&
        opnd2->kind == kind ) {
        return( TRUE );
    } else {
        return( FALSE );
    }
}
#else
#define check_same( first, second, KIND ) (first->kind == KIND && second->kind == KIND )
#endif

static bool check_both( const struct expr *opnd1, const struct expr *opnd2, enum exprtype type1, enum exprtype type2 )
/********************************************************************************************************************/
/* Check if tok_1 == type1 and tok_2 == type2 or vice versa */
{
    if( opnd1->kind == type1 && opnd2->kind == type2 )
        return( TRUE );
    if( opnd1->kind == type2 && opnd2->kind == type1 )
        return( TRUE );
    return( FALSE );
}

static ret_code index_connect( struct expr *opnd1, const struct expr *opnd2 )
/***************************************************************************/
/* Connects the register lists. called by plus_op() and dot_op() */
{
    /* move opnd2.base to either opnd1.base or opnd1.idx */
    if ( opnd2->base_reg != NULL ) {
        if ( opnd1->base_reg == NULL )
            opnd1->base_reg = opnd2->base_reg;
        else if ( opnd1->idx_reg == NULL ) {
            opnd1->idx_reg = opnd2->base_reg;
        } else {
            CEmitError( MULTIPLE_INDEX_REGISTERS_NOT_ALLOWED );
            return( ERROR );
        }
        opnd1->indirect = 1;
    }
    /* move opnd2.idx to either opnd1.base or opnd1.index */
    if( opnd2->idx_reg != NULL ) {
        //if ( opnd2->scale == 0 && opnd1->base_reg == NULL ) {
        //    opnd1->base_reg = opnd2->idx_reg;
        //} else if ( opnd1->idx_reg == NULL ) {
        if ( opnd1->idx_reg == NULL ) {
            opnd1->idx_reg = opnd2->idx_reg;
            opnd1->scale = opnd2->scale;
        } else {
            CEmitError( MULTIPLE_INDEX_REGISTERS_NOT_ALLOWED );
            return( ERROR );
        }
        opnd1->indirect = 1;
    }
    return( NOT_ERROR );
}

/* convert an address operand to a const operand if possible.
 * called for '*', '/', '+', '-' operators.
 */

static void MakeConst( struct expr *opnd )
/****************************************/
{
    if( opnd->kind != EXPR_ADDR )
        return;

    if( opnd->sym ) {
        if ( Parse_Pass > PASS_1 )
            return;
        /* added for v1.94: if the evaluator assumed an address because
         * the label wasn't defined yet, then negate this. Also, an
         * EXTERNDEF:ABS is to be accepted.
         * v2.07: if the "not yet defined" label was an argument of
         * an (OFFSET) operator, do NOT change the type!
         */
        if ( ( opnd->sym->state == SYM_UNDEFINED && opnd->instr == EMPTY ) ||
            ( opnd->sym->state == SYM_EXTERNAL && opnd->sym->weak == TRUE && opnd->abs == TRUE ) )
            ;
        else
            return;
        /* assume a value != 0 to avoid problems with div */
        opnd->value = 1;
    }

    opnd->label_tok = NULL;
    if( opnd->mbr != NULL ) {
        if( opnd->mbr->state == SYM_STRUCT_FIELD ) {
        } else if( opnd->mbr->state == SYM_TYPE ) {
            opnd->value += opnd->mbr->total_size;
            opnd->mbr = NULL;
        } else {
            return;
        }
    }
    if( opnd->base_reg != NULL )
        return;
    if( opnd->idx_reg  != NULL )
        return;
    if( opnd->override != NULL )
        return;
    opnd->instr = EMPTY;
    opnd->kind = EXPR_CONST;
    opnd->indirect = FALSE;
    opnd->explicit = FALSE;
    opnd->mem_type = MT_EMPTY;
}

/* used by EQ, NE, GT, GE, LE, LT if item is a direct address
 */

static ret_code MakeConst2( struct expr *opnd1, struct expr *opnd2 )
/******************************************************************/
{

    if ( opnd1->sym->state == SYM_EXTERNAL ) {
        if ( error_msg )
            EmitErr( INVALID_USE_OF_EXTERNAL_SYMBOL, opnd1->sym->name );
        return( ERROR );
    } else if ( ( opnd1->sym->segment != opnd2->sym->segment &&
                 /* v2.07: ignore segments if at least one label is a fwd ref */
                 opnd1->sym->state != SYM_UNDEFINED &&
                 opnd2->sym->state != SYM_UNDEFINED ) ||
               opnd2->sym->state == SYM_EXTERNAL ) {
        CEmitError( OPERANDS_MUST_BE_IN_SAME_SEGMENT );
        return( ERROR );
    }
    opnd1->kind = EXPR_CONST;
    opnd1->value += opnd1->sym->offset;
    opnd2->kind = EXPR_CONST;
    opnd2->value += opnd2->sym->offset;
    return( NOT_ERROR );
}

static ret_code ConstError( struct expr *opnd1, struct expr *opnd2 )
/******************************************************************/
{
    if ( opnd1->is_opattr )
        return( NOT_ERROR );
    if ( opnd1->kind == EXPR_FLOAT || opnd2->kind == EXPR_FLOAT )
        CEmitError( REAL_OR_BCD_NUMBER_NOT_ALLOWED );
    else
        CEmitError( CONSTANT_EXPECTED );
    return( ERROR );
}

/* used by + and - binary operators */

static void fix_struct_value( struct expr *opnd )
/***********************************************/
{
    if( opnd->mbr && ( opnd->mbr->state == SYM_TYPE ) ) {
        opnd->value += opnd->mbr->total_size;
        opnd->mbr = NULL;
    }
}

static int check_direct_reg( const struct expr *opnd1, const struct expr *opnd2 )
/*******************************************************************************/
{
    if( ( opnd1->kind == EXPR_REG ) && ( opnd1->indirect == FALSE )
        || ( opnd2->kind == EXPR_REG ) && ( opnd2->indirect == FALSE ) ) {
        return( ERROR );
    }
    return( NOT_ERROR );
}

static unsigned GetSizeValue( struct asym *sym )
/**********************************************/
{
    if ( sym->mem_type == MT_PTR )
        return( SizeFromMemtype( sym->isfar ? MT_FAR : MT_NEAR, sym->Ofssize, sym->type ) );
    return( SizeFromMemtype( sym->mem_type, sym->Ofssize, sym->type ) );
}

static unsigned IsOffset( struct expr *opnd )
/*******************************************/
{
    if ( opnd->mem_type == MT_EMPTY )
        if ( opnd->instr == T_OFFSET ||
#if IMAGERELSUPP
            opnd->instr == T_IMAGEREL ||
#endif
#if SECTIONRELSUPP
            opnd->instr == T_SECTIONREL ||
#endif
            opnd->instr == T_LROFFSET )
            return( 1 );
    return( 0 );
}

static void invalid_operand( char *oprtr, char *operand )
/*******************************************************/
{
    if ( error_msg )
        EmitErr( INVALID_OPERAND_FOR_OPERATOR, _strupr( oprtr), operand );
}

/* operators
 * LENGTH:    number of items of first initializer
 * SIZE:      size in bytes of first initializer
 * LENGTHOF:  number of elements in an array
 * SIZEOF:    size in bytes of item (array/struct)
 *
 * these operators accept structure fields, stack variables and data labels.
 * the old SIZE and LENGTH ops also accept code labels (memtype NEAR/FAR).
 * in Masm, symbolic constants (defined with EQU or =) are
 * also accepted, but no plain numbers!?
 */

static ret_code sizlen_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/*********************************************************************************************************/
{
    opnd1->kind = EXPR_CONST;

    DebugMsg1(("sizlen_op(%s): sym=%X, mbr=%X, type=>%s<\n", GetResWName( oper, NULL ),
               opnd2->sym, opnd2->mbr, opnd2->type ? opnd2->type->name : "NULL" ));

    if ( sym ) {
        if ( sym->state == SYM_STRUCT_FIELD || sym->state == SYM_STACK )
            ;
        else if ( sym->state == SYM_UNDEFINED && Parse_Pass == PASS_1 )
            ;
        else if ( ( sym->state == SYM_EXTERNAL ||
                 sym->state == SYM_INTERNAL) &&
                 //sym->mem_type != MT_ABS &&
                 sym->mem_type != MT_EMPTY &&
                 sym->mem_type != MT_PROC &&
                 sym->mem_type != MT_FAR &&
                 sym->mem_type != MT_NEAR )
            ;
        else if ( sym->state == SYM_GRP || sym->state == SYM_SEG ) {
            CEmitError( EXPECTED_DATA_LABEL );
            return( ERROR );
        } else if ( oper == T_SIZE || oper == T_LENGTH )
            ;
        else {
            CEmitError( EXPECTED_DATA_LABEL );
            return( ERROR );
        }
    }

    switch( oper ) {
    case T_LENGTH:
#if 1
        /* data items and struct fields have a "first" count.
         * OTOH, procedure locals have none, although they may be arrays.
         */
        opnd1->value = ( sym->state != SYM_STACK && sym->isarray ) ? sym->first_length : 1;
#else
        if( opnd2->kind == EXPR_CONST ) {
            opnd1->value = opnd2->mbr->first_length ? opnd2->mbr->first_length : 1;
            /* v2.05: first_length no longer set for SYM_STACK */
            //} else if ( sym->state == SYM_EXTERNAL || ( sym->state == SYM_INTERNAL && sym->isproc ) ) {
        } else if ( sym->state == SYM_EXTERNAL || sym->state == SYM_STACK || ( sym->state == SYM_INTERNAL && sym->isproc ) ) {
            opnd1->value = 1;
        } else if( sym->mem_type == MT_EMPTY ) {
            opnd1->value = 0;
        } else {
            opnd1->value = sym->first_length ? sym->first_length : 1;
        }
#endif
        break;
    case T_LENGTHOF:
        /* LENGTHOF needs either a data label or a structure field */
        /* a TYPE (structure, typedef) is invalid */
        if( opnd2->kind == EXPR_CONST ) {
            opnd1->value = opnd2->mbr->total_length;
        } else if( sym->state == SYM_UNDEFINED && Parse_Pass == PASS_1 ) {
            opnd1->value = sym->total_length;
        } else if ( sym->state == SYM_EXTERNAL && sym->iscomm == FALSE ) {
            /* for externals other than COMM, total_length field is used otherwise */
            opnd1->value = 1;
        } else {
            opnd1->value = sym->total_length;
        }
        break;
    case T_SIZE:
        /* if it is a TYPE, first_size isn't set. then use
         * total_size.
         * v2.04: first_size is no longer set for SYM_STACK.
         */
        if( sym == NULL ) {
            opnd1->value = opnd2->value;
        } else if ( sym->isdata ) {
            opnd1->value = sym->first_size;
        } else if( sym->state == SYM_TYPE ) {
            opnd1->value = sym->total_size;
        } else if( sym->state == SYM_STACK ) {
            opnd1->value = GetSizeValue( sym );
        } else if( sym->mem_type == MT_NEAR ) {
            opnd1->value = GetSymOfssize( sym ) ? LS_NEAR32 : LS_NEAR16;
        } else if( sym->mem_type == MT_FAR ) {
            opnd1->value = GetSymOfssize( sym ) ? LS_FAR32 : LS_FAR16;
        } else {
            opnd1->value = GetSizeValue( sym );
        }
        DebugMsg1(("sizlen_op(SIZE): result=%u [symbol %s, first_size=%u]\n", opnd1->value, sym ? sym->name : "NULL", sym ? sym->first_size : 0 ));
        break;
    case T_SIZEOF:
#ifdef DEBUG_OUT
        if (sym)
            DebugMsg1(("sizlen_op(sizeof): symbol %s, state=%u, size=%u\n", sym->name, sym->state, sym->total_size ));
        else if ( opnd2->is_type && opnd2->type )
            DebugMsg1(("sizlen_op(sizeof): symbol %s (TYPE), opnd2.value=%u\n", opnd2->type->name, opnd2->value ));
        else
            DebugMsg1(("sizlen_op(sizeof): symbol NULL, opnd2.value=%u\n", opnd2->value ));
#endif
        /* if sym = NULL then operand is a type constant */
        if ( sym == NULL ) {
            /* v2.06: default value of RECORD types is the mask! */
            if ( opnd2->is_type && opnd2->type && opnd2->type->typekind == TYPE_RECORD )
                opnd1->value = opnd2->type->total_size;
            else
                opnd1->value = opnd2->value;
#if 1 /* v2.05: don't use total_size for externals anymore! */
        } else if ( sym->state == SYM_EXTERNAL && sym->iscomm == FALSE ) {
            opnd1->value = GetSizeValue( sym );
            if ( sym->iscomm == TRUE )
                opnd1->value *= sym->total_length;
#endif
        } else
            opnd1->value = sym->total_size;
        break;
    }
    return( NOT_ERROR );
}

/* TYPE operator */

static ret_code type_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/*******************************************************************************************************/
{
    DebugMsg1(("type_op: argument kind=%u memtype=%X sym=%s type=%s instr=%d istype=%u explicit=%u\n",
               opnd2->kind,
               opnd2->mem_type,
               sym ? sym->name : "NULL",
               opnd2->type ? opnd2->type->name : "NULL",
               opnd2->instr,
               opnd2->is_type,
               opnd2->explicit ));
    opnd1->kind = EXPR_CONST;
    /* TYPE accepts arrays/structs/unions */
    if( opnd2->instr != EMPTY ) {
        if ( opnd2->sym ) {
            switch ( opnd2->instr ) {
            case T_LOW:
            case T_HIGH:
                opnd1->value = 1;
                break;
            case T_LOWWORD:
            case T_HIGHWORD:
                //case T_SEG: /* masm returns 0 for TYPE SEG <label>! */
                opnd1->value = 2;
                break;
#if LOHI32
            case T_LOW32:
            case T_HIGH32:
                opnd1->value = 4;
                break;
#endif
            case T_OFFSET:
            case T_LROFFSET:
#if SECTIONRELSUPP
            case T_SECTIONREL: /* masm returns 0 for TYPE SECTIONREL <label>! */
#endif
#if IMAGERELSUPP
            case T_IMAGEREL: /* masm returns 0 for TYPE IMAGEREL <label>! */
#endif
                opnd1->value = 2 << GetSymOfssize( opnd2->sym );
                opnd1->is_type = TRUE; /* v2.03: added */
                break;
            }
        }
    } else if ( sym == NULL ) {
        /* for types, return total_size */
        if ( opnd2->is_type == TRUE ) {
            /* v2.06: default value of RECORD types is the mask! */
            if ( opnd2->type && opnd2->type->typekind == TYPE_RECORD )
                opnd2->value = opnd2->type->total_size;
            //opnd1->value = opnd2->value;
            TokenAssign( opnd1, opnd2 );
            /* v2.05: added, (type procptr) ptr <reg>
             * the type probably should be moved generally,
             * but this variant is the one used by INVOKE, other
             * usages are virtually irrelevant.
             */
            if ( opnd2->mem_type == MT_PROC )
                opnd1->type = opnd2->type;
        } else if ( opnd2->kind == EXPR_REG && opnd2->indirect == FALSE ) {
            opnd1->value = SizeFromRegister( opnd2->base_reg->tokval );
#if 1 /* v2.03: added */
            opnd1->is_type = TRUE;
            if ( opnd1->mem_type == MT_EMPTY )
                MemtypeFromSize( opnd1->value, &opnd1->mem_type );
#endif
#if 0 /* Masm returns 0 for TYPE <segment_register> */
            /* if it is a segment register, use default word size */
            if ( opnd1->value == 0 )
                opnd1->value = Use32 ? 4 : 2;
#endif
        //} else if ( opnd2->explicit ) { /* v2.05: changed */
        } else if ( opnd2->mem_type != MT_EMPTY ) {
            opnd1->value = SizeFromMemtype( opnd2->mem_type, ModuleInfo.Ofssize, opnd2->type );
            opnd1->is_type = TRUE; /* v2.04: added */
            opnd1->mem_type = opnd2->mem_type; /* v2.04: added */
        } else /* it is a number or EXPR_REG + indirect */
            opnd1->value = 0;
#if 0
    } else if ( sym->state == SYM_TYPE ) {
        TokenAssign( opnd1, opnd2 );
        opnd1->type = sym;
#endif
        //} else if( sym->mem_type == MT_TYPE ) { /* v2.04: check for explicit */
    } else if( sym->mem_type == MT_TYPE && opnd2->explicit == FALSE ) {
        opnd1->value = sym->type->total_size;
#if 1 /* v2.03: added */
        opnd1->is_type = TRUE;
        if ( opnd1->mem_type == MT_EMPTY )
            opnd1->mem_type = opnd2->mem_type;
#endif
    } else {
#if 1 /* v2.03: added */
        opnd1->is_type = TRUE;
        if ( opnd1->mem_type == MT_EMPTY )
            opnd1->mem_type = opnd2->mem_type;
#endif
        /* v2.05: stack vars pointer types? */
        if ( sym->mem_type == MT_PTR )
            opnd1->value = SizeFromMemtype( sym->isfar ? MT_FAR : MT_NEAR, sym->Ofssize, NULL );
        else if( sym->mem_type == MT_NEAR )
            opnd1->value = GetSymOfssize( sym ) ? LS_NEAR32 : LS_NEAR16;
        else if( sym->mem_type == MT_FAR )
            opnd1->value = GetSymOfssize( sym ) ? LS_FAR32 : LS_FAR16;
        else
            opnd1->value = SizeFromMemtype( opnd2->mem_type, GetSymOfssize( sym ), sym->type );
    }
    DebugMsg1(("type_op: result value=%u type=%s\n", opnd1->value, opnd1->type ? opnd1->type->name : "NULL" ));
    return( NOT_ERROR );
}

/* v2.08: changed plain hex numbers to enum */
enum opattr_bits {
    OPATTR_CODELABEL = 0x01,
    OPATTR_DATALABEL = 0x02,  /* memory variable, has relocatable data label */
    OPATTR_IMMEDIATE = 0x04,  /* immediate value */
    OPATTR_DIRECTMEM = 0x08,  /* uses direct memory addressing */
    OPATTR_REGISTER  = 0x10,  /* is a register value */
    OPATTR_DEFINED   = 0x20,  /* no reference to undefined label */
    OPATTR_SSREL     = 0x40,  /* is relative to SS */
    OPATTR_EXTRNREF  = 0x80,  /* references external label */
    OPATTR_LANG_MASK = 0x700,
};

/*
 * T_DOT_TYPE: implement .TYPE as an alias for OPATTR
 * T_OPATTR:
 */
static ret_code opattr_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/*********************************************************************************************************/
{

    DebugMsg1(("opattr_op: arg kind=%u memtype=%X sym=%s\n",
               opnd2->kind, opnd2->mem_type,
               opnd2->sym ? opnd2->sym->name : "NULL" ));
    opnd1->kind = EXPR_CONST;
    opnd1->sym = NULL;  /* clear symbol in case it is undef */
    opnd1->value = 0;
    opnd1->mem_type = MT_EMPTY;
    opnd1->is_opattr = FALSE; /* v2: added */

    if ( opnd2->kind == EXPR_EMPTY )
        return( NOT_ERROR );

    /* bit 0: code label (near|far)? */
    if ( opnd2->kind == EXPR_ADDR ) {
        if ( opnd2->sym && opnd2->sym->state != SYM_STACK &&
            ( opnd2->mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS )
            opnd1->value |= OPATTR_CODELABEL;

        if ( IsOffset( opnd2 ) &&
            opnd2->sym &&
            ( opnd2->sym->mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS )
            opnd1->value |= OPATTR_CODELABEL;

        /* bit 1: memory variable, relocatable data label? */
        if ( opnd2->sym &&
            (( opnd2->sym->mem_type == MT_TYPE ||
              ( opnd2->mem_type & MT_SPECIAL ) == 0 ) ||
             ( opnd2->mem_type == MT_EMPTY &&
              ( opnd2->sym->mem_type & MT_SPECIAL ) == 0 )))
            opnd1->value |= OPATTR_DATALABEL;
    }
    /* kind==EXPR_ADDR is not reliably set for indirect register addressing! */
    if ( opnd2->indirect )
        opnd1->value |= OPATTR_DATALABEL;


    /* bit 2: immediate value? */
    if ( opnd2->kind == EXPR_CONST ||
        ( opnd2->kind == EXPR_ADDR &&
         opnd2->indirect == FALSE &&
         (( opnd2->mem_type == MT_EMPTY && IsOffset(opnd2) ) ||
          //( opnd2->mem_type == MT_ABS ) ||  /* v2.06: added (abs. external) */
          ( opnd2->mem_type == MT_EMPTY ) ||  /* v2.06: added (abs. external) */
          (( opnd2->mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS )) &&
         ( opnd2->sym->state == SYM_INTERNAL ||
          opnd2->sym->state == SYM_EXTERNAL ) ) )
        opnd1->value |= OPATTR_IMMEDIATE;

    /* bit 3: uses direct memory addressing?
     */
    if ( opnd2->kind == EXPR_ADDR &&
        opnd2->indirect == FALSE &&
        //opnd2->base_reg == NULL &&
        (( opnd2->mem_type == MT_EMPTY && opnd2->instr == EMPTY ) ||
         ( opnd2->mem_type == MT_TYPE ) || /* v2.05: added */
         (( opnd2->mem_type & MT_SPECIAL ) == 0 ) ||
         opnd2->mem_type == MT_PTR ) &&
        (opnd2->sym == NULL ||
         opnd2->sym->state == SYM_INTERNAL ||
         opnd2->sym->state == SYM_EXTERNAL ) )
        opnd1->value |= OPATTR_DIRECTMEM;

    if ( opnd2->kind == EXPR_REG && opnd2->indirect == FALSE )
        opnd1->value |= OPATTR_REGISTER;

    //if ( opnd2->kind != EXPR_UNDEF && ( opnd2->sym == 0 || opnd2->sym->isdefined == TRUE ) )
    if ( opnd2->kind != EXPR_UNDEF && opnd2->kind != EXPR_FLOAT && ( opnd2->sym == 0 || opnd2->sym->isdefined == TRUE ) )
        opnd1->value |= OPATTR_DEFINED; 

    if ( opnd2->sym && opnd2->sym->state == SYM_STACK ||
        ( opnd2->indirect == TRUE &&
         opnd2->base_reg != NULL &&
         ( opnd2->base_reg->tokval == T_ESP ||
          opnd2->base_reg->tokval == T_EBP ||
          opnd2->base_reg->tokval == T_BP ) ) )
        opnd1->value |= OPATTR_SSREL;

    if ( opnd2->sym && opnd2->sym->state == SYM_EXTERNAL )
        opnd1->value |= OPATTR_EXTRNREF;

    if ( oper == T_OPATTR )
        if ( opnd2->sym )
            opnd1->value |= opnd2->sym->langtype << 8;

    DebugMsg1(("opattr_op returns %Xh\n", opnd1->value));
    return( NOT_ERROR );
}

static ret_code short_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/********************************************************************************************************/
{
    if ( opnd2->kind != EXPR_ADDR ||
        ( opnd2->mem_type != MT_EMPTY &&
         opnd2->mem_type != MT_NEAR &&
         opnd2->mem_type != MT_FAR &&
         opnd2->mem_type != MT_PROC ) ) {
        CEmitError( EXPRESSION_MUST_BE_A_CODE_ADDRESS );
        return( ERROR );
    }
    TokenAssign( opnd1, opnd2 );
    opnd1->instr = oper;
    return( NOT_ERROR );
}

static ret_code seg_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/******************************************************************************************************/
{
    if ( opnd2->sym->state == SYM_STACK || opnd2->abs ) {
        CEmitError( OPERAND_MUST_BE_RELOCATABLE );
        return( ERROR );
    }
    TokenAssign( opnd1, opnd2 );
    opnd1->instr = oper;
    if ( opnd1->mbr ) /* v2.08: set value more selectively */
        opnd1->value = 0;    /* v2.07: added ( SEG <member> ) */
    opnd1->mem_type = MT_EMPTY; /* v2.04a */
    return( NOT_ERROR );
}

/* handles offset operators:
 * OFFSET, LROFFSEG, IMAGEREL, SECTIONREL
 */

static ret_code offset_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/*********************************************************************************************************/
{
    if ( oper == T_OFFSET ) {
        /* if operand is a constant value, skip OFFSET operator */
        if ( opnd2->kind == EXPR_CONST ) {
            TokenAssign( opnd1, opnd2 );
            return( NOT_ERROR );
        }
    }
    if ( (sym && sym->state == SYM_GRP) || opnd2->instr == T_SEG ) {
        invalid_operand( GetResWName( oper, NULL ), name );
        return( ERROR );
    }
    /* offset operator accepts types, but returns always 0 */
    if ( opnd2->is_type )
        opnd2->value = 0;

    TokenAssign( opnd1, opnd2 );
    opnd1->instr = oper;

    if ( opnd2->indirect ) {
        /* Masm v5.1 allows indirect operands, but Masm v6 with -Zm
         * won't accept it.
         */
        invalid_operand( GetResWName( oper, NULL ), name );
        return( ERROR );
    }
    /* skip memory type of operand, just address is needed */
    //opnd1->mem_type = MT_NEAR;
    opnd1->mem_type = MT_EMPTY;
    /* clear overrides ("offset SEG:xxx") */
    /* v2.01: override information is important for fixup creation!
     * the reason why it was cleared probably was to avoid creation
     * of a segment prefix. This case is now handled in the parser.
     */
    // opnd1->override = NULL;
    return( NOT_ERROR );
}

static ret_code lowword_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/**********************************************************************************************************/
{
    TokenAssign( opnd1, opnd2 );
    if ( opnd2->kind == EXPR_ADDR && opnd2->instr != T_SEG ) {
        opnd1->instr = T_LOWWORD;
        //opnd1->mem_type = MT_WORD; /* v2.05 */
        opnd1->mem_type = MT_EMPTY;
    }
    opnd1->llvalue &= 0xffff;
    return( NOT_ERROR );
}

static ret_code highword_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/***********************************************************************************************************/
{
    TokenAssign( opnd1, opnd2 );
    if ( opnd2->kind == EXPR_ADDR && opnd2->instr != T_SEG ) {
        opnd1->instr = T_HIGHWORD;
        //opnd1->mem_type = MT_WORD; /* v2.05 */
        opnd1->mem_type = MT_EMPTY;
    }
    opnd1->value = opnd1->value >> 16;
    return( NOT_ERROR );
}

static ret_code low_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/******************************************************************************************************/
{
    TokenAssign( opnd1, opnd2 );
    if ( opnd2->kind == EXPR_ADDR && opnd2->instr != T_SEG ) {
        /* LOW works for OMF/BIN only */
        /* v2.07: don't check any format-specific rules in the
         * expression evaluator!
         */
#if 0
        if ( Options.output_format != OFORMAT_OMF &&
            Options.output_format != OFORMAT_BIN && opnd2->sym ) {
            if ( error_msg )
                EmitErr( SYMBOL_TYPE_CONFLICT, opnd2->sym->name );
            return( ERROR );
        }
#endif
        opnd1->instr = T_LOW;
        opnd1->mem_type = MT_EMPTY;
    }
    opnd1->llvalue &= 0xff;
    return( NOT_ERROR );
}

static ret_code high_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/*******************************************************************************************************/
{
    TokenAssign( opnd1, opnd2 );
    if ( opnd2->kind == EXPR_ADDR && opnd2->instr != T_SEG ) {
        /* v2.07: don't check any format-specific rules in the
         * expression evaluator!
         */
#if 0
        if ( Options.output_format != OFORMAT_OMF &&
            Options.output_format != OFORMAT_BIN && opnd2->sym ) {
            if ( error_msg )
                EmitErr( SYMBOL_TYPE_CONFLICT, opnd2->sym->name );
            return( ERROR );
        }
#endif
        opnd1->instr = T_HIGH;
        opnd1->mem_type = MT_EMPTY;
    }
    opnd1->value = opnd1->value >> 8;
    opnd1->llvalue &= 0xff;
    return( NOT_ERROR );
}

#if LOHI32

static ret_code low32_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/********************************************************************************************************/
{
    /* v2.06: added support for double constants */
    if ( opnd2->kind == EXPR_FLOAT ) {
        if ( Options.strict_masm_compat )
            return( ConstError( opnd1, opnd2 ) );
        atofloat( &opnd2->llvalue, opnd2->float_tok->string_ptr, sizeof( opnd2->llvalue), opnd2->negative, opnd2->float_tok->floattype );
        opnd2->kind = EXPR_CONST;
        opnd2->float_tok = NULL;
    }
    TokenAssign( opnd1, opnd2 );
    if ( opnd2->kind == EXPR_ADDR && opnd2->instr != T_SEG ) {
        opnd1->instr = T_LOW32;
        opnd1->mem_type = MT_DWORD;
    }
    opnd1->llvalue &= 0xffffffff;
    return( NOT_ERROR );
}

static ret_code high32_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/*********************************************************************************************************/
{
    /* v2.06: added support for double constants */
    if ( opnd2->kind == EXPR_FLOAT ) {
        if ( Options.strict_masm_compat )
            return( ConstError( opnd1, opnd2 ) );
        atofloat( &opnd2->llvalue, opnd2->float_tok->string_ptr, sizeof( opnd2->llvalue), opnd2->negative, opnd2->float_tok->floattype );
        opnd2->kind = EXPR_CONST;
        opnd2->float_tok = NULL;
    }
    TokenAssign( opnd1, opnd2 );
    if ( opnd2->kind == EXPR_ADDR && opnd2->instr != T_SEG ) {
        opnd1->instr = T_HIGH32;
        opnd1->mem_type = MT_DWORD;
    }
    opnd1->llvalue = opnd1->llvalue >> 32;
    return( NOT_ERROR );
}

#endif

static ret_code this_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/*******************************************************************************************************/
{
    if ( opnd2->is_type == FALSE ) {
        CEmitError( INVALID_TYPE_EXPRESSION );
        return( ERROR );
    }
    /* v2.06: won't work inside structs */
    if ( CurrStruct ) {
        CEmitError( MUST_BE_IN_SEGMENT_BLOCK );
        return( ERROR );
    }
    /* v2.06: won't work outside segments */
    if ( CurrSeg == NULL ) {
        EmitError( MUST_BE_IN_SEGMENT_BLOCK ); /* error even in EQU! */
        return( ERROR );
    }

    if ( thissym == NULL ) {
        thissym = SymAlloc( "" );
        /* fixme: set thissym->variable? */
        thissym->state = SYM_INTERNAL;
        thissym->isdefined = TRUE;
    }

    opnd1->kind = EXPR_ADDR;
    thissym->mem_type = opnd2->mem_type;
    if ( opnd2->sym && opnd2->sym->mem_type == MT_TYPE )
        thissym->type = opnd2->sym->type;
    opnd1->sym  = thissym;
    SetSymSegOfs( thissym );
    opnd1->mem_type = thissym->mem_type;
    return( NOT_ERROR );
}

/* WIDTH and MASK operators */

static ret_code wimask_op( int oper, struct expr *opnd1, struct expr *opnd2, struct asym *sym, char *name )
/*********************************************************************************************************/
{
    /* additional check needed if operand is a type */
    if ( opnd2->is_type ) {
        sym = opnd2->type;
        if (sym->typekind != TYPE_RECORD ) {
            CEmitError( OPERAND_MUST_BE_RECORD );
            return( ERROR );
        }
    } else if ( opnd2->kind == EXPR_CONST ) {
        sym = opnd2->mbr;
    } else {
        sym = opnd2->sym;
    }
    if ( oper == T_MASK ) {
        int i;
        opnd1->value = 0;
        if ( opnd2->is_type ) { /* get mask of the RECORD? */
#if AMD64_SUPPORT
            opnd1->llvalue = GetRecordMask( (struct dsym *)sym );
#else
            opnd1->value = GetRecordMask( (struct dsym *)sym );
#endif
        } else { /* get mask of the bitfield */
            for ( i = sym->offset ;i < sym->offset + sym->total_size; i++ )
#if AMD64_SUPPORT
#if defined(LLONG_MAX) || defined(__GNUC__) || defined(__TINYC__)
                opnd1->llvalue |= 1ULL << i;
#else
                opnd1->llvalue |= 1i64 << i;
#endif
#else
                opnd1->value |= 1 << i;
#endif
        }
    } else {
        if ( opnd2->is_type ) { /* get width of the RECORD? */
            struct dsym *dir = (struct dsym *)sym;
            struct field_item *fl;
            for ( fl = dir->e.structinfo->head; fl; fl = fl->next )
                opnd1->value += fl->sym->total_size;
        } else
            opnd1->value = sym->total_size;
    }
    opnd1->kind = EXPR_CONST;
    return( NOT_ERROR );
}

#define  res(token, function) function ,
static ret_code (* const unaryop[])( int, struct expr *, struct expr *, struct asym *, char * ) = {
#include "unaryop.h"
};
#undef res

static ret_code plus_op( struct expr *opnd1, struct expr *opnd2 )
/***************************************************************/
{
    DebugMsg1(("plus_op: memtype=%Xh-%Xh value=%d-%d mbr=%s-%s type=%s-%s\n",
               opnd1->mem_type, opnd2->mem_type, 
               opnd1->value, opnd2->value,
               opnd1->mbr ? opnd1->mbr->name : "NULL",
               opnd2->mbr ? opnd2->mbr->name : "NULL",
               opnd1->type ? opnd1->type->name : "NULL",
               opnd2->type ? opnd2->type->name : "NULL" ));
    /*
     * The formats allowed are (registers inside [] only!):
     *        constant + constant  CONST-CONST
     *        constant + address   CONST-ADDR
     *        register + constant  ADDR-CONST
     *        address + register   ADDR-ADDR
     *        register + register  ADDR-ADDR
     *        address  + address   ADDR-ADDR
     */

    if( check_direct_reg( opnd1, opnd2 ) == ERROR ) {
        DebugMsg(("plus_op: error direct register\n" ));
        CEmitError( INVALID_USE_OF_REGISTER );
        return( ERROR );
    }
    /* v2.08: remove EXPR_REG variants */
    if ( opnd1->kind == EXPR_REG )
        opnd1->kind = EXPR_ADDR;
    if ( opnd2->kind == EXPR_REG )
        opnd2->kind = EXPR_ADDR;

    /* v2.07: don't allow multiple overrides */
    if ( opnd2->override ) {
        if ( opnd1->override ) {
            /* v2.07a: both T_REG or both T_ID is rejected */
            if ( opnd1->override->token == opnd2->override->token ) {
                DebugMsg(("plus_op: multiple overrides\n" ));
                CEmitError( MULTIPLE_OVERRIDES );
                return( ERROR );
            }
        }
        opnd1->override = opnd2->override;
    }

    if( check_same( opnd1, opnd2, EXPR_CONST ) ) {

        opnd1->llvalue += opnd2->llvalue;

    } else if( check_same( opnd1, opnd2, EXPR_ADDR ) ) {

        DebugMsg1(("plus_op: ADDR - ADDR\n" ));
        fix_struct_value( opnd1 );
        fix_struct_value( opnd2 );
        if ( index_connect( opnd1, opnd2 ) == ERROR )
            return( ERROR );
        if( opnd2->sym != NULL ) {
            /* two relocatable labels not allowed */
            /* v2.05: changed */
            //if ( ( opnd1->sym != NULL ) && ( Parse_Pass > PASS_1 || error_msg == FALSE ) ) {
            if ( opnd1->sym != NULL &&
                opnd1->sym->state != SYM_UNDEFINED &&
                opnd2->sym->state != SYM_UNDEFINED ) {
                DebugMsg(("plus_op: two relocatable labels: %s - %s \n", opnd1->sym->name, opnd2->sym->name ));
                CEmitError( CANNOT_ADD_TWO_RELOCATABLE_LABELS );
                return( ERROR );
            }
            opnd1->label_tok = opnd2->label_tok;
            opnd1->sym = opnd2->sym;
            /* v2.05: added */
            if ( opnd1->mem_type == MT_EMPTY )
                opnd1->mem_type = opnd2->mem_type;
        }
        opnd1->llvalue += opnd2->llvalue;
        /* v2.08: added, test case [ecx+ebx.<struc>].<mbr> */
        if ( opnd2->type )
            opnd1->type = opnd2->type;

    } else if( check_both( opnd1, opnd2, EXPR_CONST, EXPR_ADDR ) ) {

        DebugMsg1(("plus_op: CONST - ADDR\n" ));
        if( opnd1->kind == EXPR_CONST ) {
            opnd2->llvalue += opnd1->llvalue;
            opnd2->indirect |= opnd1->indirect;

            if( opnd1->explicit == TRUE ) {
                opnd2->explicit = TRUE;
                opnd2->mem_type = opnd1->mem_type;
            } else if ( opnd2->mem_type == MT_EMPTY )
                opnd2->mem_type = opnd1->mem_type;

            /* v2.05: added. See dotop2.asm, "mov eax, v2.f1[ebx*2]" */
            if ( opnd2->mbr == NULL )
                opnd2->mbr = opnd1->mbr;

            /* v2.08: added, test case [4+ebx.<struc>].<mbr> */
            if ( opnd2->type )
                opnd1->type = opnd2->type; /* set <type> in op1! */

            TokenAssign( opnd1, opnd2 );

        } else {
            opnd1->llvalue += opnd2->llvalue;
            /* v2.04: added. to make this case behave like
             * the CONST - REG case (see below).
             */
            /* v2.08: changed, test case [reg+struct] */
            //if ( opnd1->mem_type == MT_EMPTY )
            if ( opnd1->mem_type == MT_EMPTY && opnd2->is_type == FALSE )
                opnd1->mem_type = opnd2->mem_type;
        }
        fix_struct_value( opnd1 );
    } else {
        DebugMsg(("plus_op: error, unexpected format: %u - %u\n", opnd1->kind, opnd2->kind ));
        return( ConstError( opnd1, opnd2 ) );
    }
    return( NOT_ERROR );
}

static ret_code minus_op( struct expr *opnd1, struct expr *opnd2 )
/****************************************************************/
{
    struct asym      *sym;
    /*
     * The only formats allowed are:
     *        constant - constant
     *         address - constant       ( only in this order )
     *         address - address
     *        register - constant       ( only inside [] and in this
     *                                    order )
     */

    DebugMsg1(("minus_op: types tok1=%u, tok2=%u\n", opnd1->type, opnd2->type ));

    if( check_direct_reg( opnd1, opnd2 ) == ERROR ) {
        DebugMsg(("minus_op: error direct register\n"));
        CEmitError( INVALID_USE_OF_REGISTER );
        return( ERROR );
    }

    /* added for v1.94. It's related to the change done in MakeConst()!
     */
    if ( opnd1->kind == EXPR_ADDR &&
        opnd2->kind == EXPR_ADDR &&
        opnd2->sym &&
        opnd2->sym->state == SYM_UNDEFINED &&
        error_msg != FALSE )
        ; /* don't convert token2 to a constant! */
    else
        MakeConst( opnd2 );

    if( check_same( opnd1, opnd2, EXPR_CONST ) ) {

        DebugMsg1(("minus_op: CONST-CONST\n" ));
        opnd1->llvalue -= opnd2->llvalue;

    } else if( opnd1->kind == EXPR_ADDR &&
              opnd2->kind == EXPR_CONST ) {

        DebugMsg1(("minus_op: ADDR-CONST\n" ));
        opnd1->llvalue -= opnd2->llvalue;
        fix_struct_value( opnd1 );

    } else if( check_same( opnd1, opnd2, EXPR_ADDR ) ){

        DebugMsg1(("minus_op: ADDR-ADDR\n" ));
        fix_struct_value( opnd1 );
        fix_struct_value( opnd2 );
        if( opnd2->base_reg != NULL || opnd2->idx_reg != NULL ) {
            CEmitError( INVALID_USE_OF_REGISTER );
            DebugMsg(("minus_op error 2\n"));
            return( ERROR );
        }
        if( opnd2->label_tok == NULL ) {
            /* v2.06c: do 64-bit arithmetic (more rigid test in data.c) */
            //opnd1->value -= opnd2->value;
            opnd1->value64 -= opnd2->value64;
            opnd1->indirect |= opnd2->indirect;
        } else {
            if( opnd1->label_tok == NULL || opnd1->sym == NULL || opnd2->sym == NULL ) {
                /* v2.05: error msg changed */
                //CEmitError( SYNTAX_ERROR );
                CEmitError( OPERAND_MUST_BE_RELOCATABLE );
                DebugMsg(("minus_op error 3\n"));
                return( ERROR );
            }
            /* handle first operand */
            sym = opnd1->sym;
#if 0 /* v2.05: can't happen */
            //if( Parse_Pass > PASS_1 && sym->defined == FALSE ) {
            if( Parse_Pass > PASS_1 && sym->state == SYM_UNDEFINED ) {
                if( error_msg )
                    EmitErr( LABEL_NOT_DEFINED, sym->name );
                DebugMsg(("minus_op error 4\n"));
                return( ERROR );
            }
#endif
            opnd1->value += sym->offset;

            /* handle second operand */
            sym = opnd2->sym;
            if( Parse_Pass > PASS_1 ) {
#if 0 /* v2.05: can't happen */
                if( sym->state == SYM_UNDEFINED ) {
                    if( error_msg )
                        EmitErr( LABEL_NOT_DEFINED, sym->name );
                    DebugMsg(("minus_op error 5\n"));
                    return( ERROR );
                }
#endif
                /* if symbol is external, error - unless it's the same symbol */
                if ( ( sym->state == SYM_EXTERNAL ||
                     opnd1->sym->state == SYM_EXTERNAL) &&
                    sym != opnd1->sym ) {
                    if ( error_msg )
                        EmitErr(INVALID_USE_OF_EXTERNAL_SYMBOL, opnd1->sym->name );
                    DebugMsg(("minus_op error 6\n"));
                    return( ERROR );
                }
                /* check if the 2 offsets belong to the same segment */
                if ( sym->segment != opnd1->sym->segment ) {
                    CEmitError( OPERANDS_MUST_BE_IN_SAME_SEGMENT );
                    DebugMsg(("minus_op error 7\n"));
                    return( ERROR );
                }
            }
            /* v2.05: if at least one label is undefined, assume result=1 */
            if ( opnd1->sym->state == SYM_UNDEFINED ||
                opnd2->sym->state == SYM_UNDEFINED )
                opnd1->value = 1;
            else {
                /* v2.06c: do 64-bit arithmetic (more rigid test in data.c) */
                //opnd1->value -= sym->offset;
                //opnd1->value -= opnd2->value;
                opnd1->value64 -= sym->offset;
                opnd1->value64 -= opnd2->value64;
            }
            opnd1->label_tok = NULL;
            opnd1->sym = NULL;
            if( opnd1->base_reg == NULL && opnd1->idx_reg == NULL ) {

                if( opnd1->instr == T_OFFSET && opnd2->instr == T_OFFSET )
                    opnd1->instr = EMPTY;

                opnd1->kind = EXPR_CONST;
                /* the type changes from address to constant.
                 */
                opnd1->indirect = FALSE;
            } else {
                DebugMsg1(("minus_op, exit, ADDR, base=%X, idx=%X\n", opnd1->base_reg, opnd1->idx_reg ));
                opnd1->kind = EXPR_ADDR;
                opnd1->indirect |= opnd2->indirect;
            }
            opnd1->explicit = FALSE;
            opnd1->mem_type = MT_EMPTY;
        }

    } else if( opnd1->kind == EXPR_REG &&
              opnd2->kind == EXPR_CONST ) {

        opnd1->llvalue = -1 * opnd2->llvalue;
        opnd1->indirect |= opnd2->indirect;
        opnd1->kind = EXPR_ADDR;

    } else {
        DebugMsg(("minus_op, exit, error: types tok1=%u, tok2=%u\n", opnd1->type, opnd2->type ));
        return( ConstError( opnd1, opnd2 ) );
    }
    return( NOT_ERROR );
}

static ret_code dot_op( struct expr *opnd1, struct expr *opnd2 )
/**************************************************************/
{
    /* this code needs cleanup! some stuff is obsolete. */

    DebugMsg1(("dot_op: op1-op2 sym=%s-%s type=%s-%s mbr=%s-%s\n",
               opnd1->sym  ? opnd1->sym->name  : "NULL",
               opnd2->sym  ? opnd2->sym->name  : "NULL",
               opnd1->type ? opnd1->type->name : "NULL",
               opnd2->type ? opnd2->type->name : "NULL",
               opnd1->mbr  ? opnd1->mbr->name  : "NULL",
               opnd2->mbr  ? opnd2->mbr->name  : "NULL" ));

    /*
     * The formats allowed are:
     *        [register]      . (type) constant
     *        label           . (type) constant
     *        (type) constant . (type) constant
     *
     * with OPTION OLDSTRUCTS:
     *        [register]      . address
     *        address         . address
     */

    if( check_direct_reg( opnd1, opnd2 ) == ERROR ) {
        DebugMsg(("dot_op: error direct register\n"));
        CEmitError( INVALID_USE_OF_REGISTER );
        return( ERROR );
    }

    /* v2.08: remove EXPR_REG variants */
    if ( opnd1->kind == EXPR_REG )
        opnd1->kind = EXPR_ADDR;
    if ( opnd2->kind == EXPR_REG )
        opnd2->kind = EXPR_ADDR;

#if 1 /* fixme: to be removed */
    if ( opnd2->sym && opnd2->sym->state == SYM_UNDEFINED ) {
        if ( !nullstruct )
            nullstruct = CreateTypeSymbol( NULL, "", FALSE );
        opnd2->type = nullstruct;
        opnd2->sym = NULL;
        opnd2->kind = EXPR_CONST;
    }
#endif

    if( check_same( opnd1, opnd2, EXPR_ADDR ) ) {

        DebugMsg1(("dot_op, ADDR - ADDR, t1-t2 memtype=%X-%X sym=%s-%s\n",
                   opnd1->mem_type, opnd2->mem_type,
                   opnd1->sym  ? opnd1->sym->name  : "NULL",
                   opnd2->sym  ? opnd2->sym->name  : "NULL" ));

#if 1 /* v2.05: error */
        if ( opnd2->mbr == NULL && !ModuleInfo.oldstructs ) {
            DebugMsg(("dot_op: error, mbr 2 is NULL\n"));
            CEmitError( STRUCTURE_FIELD_EXPECTED );
            return( ERROR );
        }
#endif
        if ( index_connect( opnd1, opnd2 ) == ERROR )
            return( ERROR );

        if( opnd2->sym != NULL ) {
            if( opnd1->sym != NULL &&
                opnd1->sym->state != SYM_UNDEFINED &&
                opnd2->sym->state != SYM_UNDEFINED ) {
                DebugMsg(("dot_op: error, two relocatable labels: %s - %s \n", opnd1->sym->name, opnd2->sym->name ));
                CEmitError( CANNOT_ADD_TWO_RELOCATABLE_LABELS );
                return( ERROR );
            }
            opnd1->label_tok = opnd2->label_tok;
            opnd1->sym = opnd2->sym;
        }
        if( opnd2->mbr != NULL ) {
            opnd1->mbr = opnd2->mbr;
        }
        opnd1->value += opnd2->value;
        if( opnd1->explicit == FALSE ) {
            opnd1->mem_type = opnd2->mem_type;
        }
        if ( opnd2->type )
            opnd1->type = opnd2->type;

    } else if( ( opnd1->kind == EXPR_CONST ) && ( opnd2->kind == EXPR_ADDR ) ) {

        DebugMsg1(("dot_op, CONST - ADDR: t1-t2 memtype=%Xh-%Xh istype=%u-%u\n",
                   opnd1->mem_type, opnd2->mem_type, opnd1->is_type, opnd2->is_type ));
        /* v2.08 added (copied from branch EXPR_ADDR-EXPR_REG )*/
        if ( opnd1->is_type && opnd1->type ) {
            opnd2->assumecheck = FALSE;
            opnd1->llvalue = 0;  /* v2.08: this was previously done in get_operand() */
        }
#if 1 /* v2.05: error */
        /* <structname>.<member>[<index_reg>] is ALWAYS ok! */
        if ( ( !ModuleInfo.oldstructs ) && ( opnd1->is_type == FALSE && opnd1->mbr == NULL ) )
            CEmitError( STRUCTURE_FIELD_EXPECTED );
#endif
        /* for TYPE.xxx, return offset instead of size */
        if ( opnd1->mbr && opnd1->mbr->state == SYM_TYPE )
            opnd1->llvalue = opnd1->mbr->offset;
        opnd2->indirect |= opnd1->indirect;
        opnd2->llvalue += opnd1->llvalue;
        DebugMsg1(("dot_op, CONST - ADDR, t1.type=%X (%s), t2.type=%X (%s)\n",
                   opnd1->type,
                   opnd1->type ? opnd1->type->name : "",
                   opnd2->type,
                   opnd2->type ? opnd2->type->name : "" ));
        /* v2.06: added. test case: INVOKE struct.mbr[edx] ( mbr has a type ) */
        if ( opnd2->mbr )
            opnd1->type = opnd2->type;
        TokenAssign( opnd1, opnd2 );

    } else if( ( opnd1->kind == EXPR_ADDR ) && ( opnd2->kind == EXPR_CONST ) ) {

        DebugMsg1(("dot_op, ADDR - CONST: t1-t2 memtype=%Xh-%Xh t1.explicit=%u\n",
                   opnd1->mem_type, opnd2->mem_type, opnd1->explicit ));

        /* v2.08: changed to catch [ebx].<num> or [ebx].<simple type> */
        //if ( (!ModuleInfo.oldstructs) && opnd2->type == NULL && opnd2->mbr == NULL ) {
        if ( (!ModuleInfo.oldstructs) && ( opnd2->type == NULL || opnd2->is_type == FALSE ) && opnd2->mbr == NULL ) {
            DebugMsg(("dot_op: error, constant or simple type after dot\n"));
            CEmitError( STRUCTURE_FIELD_EXPECTED );
            return( ERROR );
        }

        /* v2.08 added (copied from branch EXPR_ADDR-EXPR_REG )*/
        if ( opnd2->is_type && opnd2->type ) {
            opnd1->assumecheck = FALSE;
            opnd2->llvalue = 0;  /* v2.08: this was previously done in get_operand() */
        }
        /* for [var].TYPE | STRUCT_FIELD, use offset instead of size */
        if ( opnd2->mbr && opnd2->mbr->state == SYM_TYPE )
            opnd2->llvalue = opnd2->mbr->offset;
        opnd1->llvalue += opnd2->llvalue;
        opnd1->mem_type = opnd2->mem_type; /* v2.08: now always done */
        if( opnd2->mbr != NULL ) {
            opnd1->mbr = opnd2->mbr;
#if 0 /* v2.07 */
            /* temp. disabled in v1.95, test case:
             * mov eax,(<struct> ptr [ebx]).F1
             * however: mov ax, word ptr var[bx].F1 ???
             * the condition can't be disabled. Instead the PTR
             * operator must NOT set the explicit flag if the
             * first operand is a structure.
             */
            if( opnd1->explicit == FALSE )
#endif
                //opnd1->mem_type = opnd2->mem_type; /* v2.08: obsolete */
        }

        DebugMsg1(("dot_op, ADDR - CONST, t1.type=%X (%s), t2.type=%X (%s)\n",
                   opnd1->type,
                   opnd1->type ? opnd1->type->name : "",
                   opnd2->type,
                   opnd2->type ? opnd2->type->name : "" ));
#if 0 /* v1.96 */
        if ( opnd2->type )
#endif
            opnd1->type = opnd2->type;

    } else if ( opnd1->kind == EXPR_CONST && opnd2->kind == EXPR_CONST ) {

        DebugMsg1(("dot_op, CONST - CONST, t1-t2 value=%u-%u, memtype=%Xh-%Xh istype=%u-%u\n",
                   opnd1->value, opnd2->value, opnd1->mem_type, opnd2->mem_type, opnd1->is_type, opnd2->is_type));
        if ( opnd2->mbr == NULL && !ModuleInfo.oldstructs ) {
            DebugMsg(("dot_op: error, opnd2.mbr=NULL\n" ));
            CEmitError( STRUCTURE_FIELD_EXPECTED );
            return( ERROR );
        }
        if ( opnd1->type != NULL ) {
            /*
             * v2.06: the token1 value must NOT be ignored if the token is a
             * struct member: mov ax, [offset] <struct>.<mbr>.<mbr>
             */
            if ( opnd1->mbr != NULL )
                opnd1->llvalue += opnd2->llvalue;
            else {
                /* old token is a type - the value (=size) is ignored then. */
                opnd1->llvalue = opnd2->llvalue;
            }
            opnd1->mbr = opnd2->mbr;
            /* v2.0: copy mem_type (test case: mov ds:[<struct>.<mbr>], 123) */
            opnd1->mem_type = opnd2->mem_type;
            /* v2.05: removed, it's still a type constant */
            //opnd1->is_type = FALSE;
            opnd1->is_type = opnd2->is_type;
            /* either clear <type> or use the renewed one */
            if ( opnd1->type != opnd2->type )
                opnd1->type = opnd2->type;
            else
                opnd1->type = NULL;
        } else {
            /* old token is NOT a type */
            /* most likely a number or an MT_ABS symbol! */
            /* so the TOTAL of both constants is required */
            opnd1->llvalue += opnd2->llvalue;
            opnd1->mbr = opnd2->mbr;
            opnd1->mem_type = opnd2->mem_type;
        }
    } else {
        DebugMsg(("dot_op: error, unknown type combination, opnd1->type=%u, opnd2->type=%u\n", opnd1->type, opnd2->type ));
        CEmitError( STRUCTURE_FIELD_EXPECTED );
        return( ERROR );
    }
    return( NOT_ERROR );
}

static ret_code colon_op( struct expr *opnd1, struct expr *opnd2 )
/****************************************************************/
{
    int_32              temp;
    struct asym         *sym;
    /*
     * The only formats allowed are:
     *     seg_reg : const
     *     seg_reg : address
     *     seg_label : const
     *     seg_label : address
     *     ( seg_label = segment or group symbol )
     *     inside square brackets, seg_reg : register is not accepted
     *     if Masm-syntax is on.
     */
    DebugMsg1(("colon_op: t1-t2 kind=%u-%u type=%s-%s is_type=%u-%u\n",
               opnd1->kind, opnd2->kind,
               opnd1->type ? opnd1->type->name : "NULL",
               opnd2->type ? opnd2->type->name : "NULL",
               opnd1->is_type, opnd2->is_type ));
    if( opnd2->override != NULL ) {
        /* v2.07a: was too rigid */
        if ( ( opnd1->kind == EXPR_REG && opnd2->override->token == T_REG ) ||
            ( opnd1->kind == EXPR_ADDR && opnd2->override->token == T_ID ) ) {
            CEmitError( MULTIPLE_OVERRIDES );
            DebugMsg(("colon_op: multiple override=%s\n", opnd2->override->string_ptr ));
            return( ERROR );
        }
    }
    switch ( opnd2->kind ) {
    case EXPR_REG:
        /* v2.05: register as second operand must be enclosed in [] */
        if ( opnd2->indirect == FALSE ) {
            CEmitError( INVALID_USE_OF_REGISTER );
            return( ERROR );
        }
        break;
    case EXPR_FLOAT:
        CEmitError( REAL_OR_BCD_NUMBER_NOT_ALLOWED );
        return( ERROR );
    }

    if( opnd1->kind == EXPR_REG ) {

        /* the item before the ':' must be a single register */
        if( opnd1->idx_reg != NULL ) {
            CEmitError( INVALID_USE_OF_REGISTER );
            DebugMsg(("colon_op error 2\n"));
            return( ERROR );
        }
        /* segment override inside bracket not allowed with -Zne  */
        /* v2.08: test moved here from get_operand() */
        if ( Options.strict_masm_compat ) {
            CEmitError( INVALID_USE_OF_REGISTER );
            return( ERROR );
        }
        /* make sure first operand is a segment register */
        temp = opnd1->base_reg->tokval;
        if ( ( GetValueSp( temp ) & OP_SR ) == 0 ) {
            CEmitError( SEGMENT_GROUP_OR_SEGREG_EXPECTED );
            return( ERROR );
        }

        opnd2->override = opnd1->base_reg;
        opnd2->indirect |= opnd1->indirect;

        if ( opnd2->kind == EXPR_CONST ) {
            opnd2->kind = EXPR_ADDR;
            /* v2.05: type flag cleared HERE, not in dot_op()
             * v2.05rc17 problem: mov es:byte ptr <var>,0
             * so the flag isn't cleared at all now.
             */
            //opnd2->is_type = FALSE;
        }

        if( opnd1->explicit ) {
            opnd2->explicit = opnd1->explicit;
            opnd2->mem_type = opnd1->mem_type;
            opnd2->Ofssize  = opnd1->Ofssize;
        }
        TokenAssign( opnd1, opnd2 );

        /*
         * currently the <type> token isn't copied by
         * TokenAssign (which is probably just for historical reasons).
         * So copy it manually!
         * v1.95: only copy if it is != NULL!
         * Testcase: (<type> ptr DS:[0]).<struct_field> ...
         * In this case the DS:[] will clear the <type>, as a result
         * the dot operator won't have a valid assume and the code fails.
         */
        if ( opnd2->type )
            opnd1->type = opnd2->type;

    } else if( opnd1->kind == EXPR_ADDR &&
              /* opnd2->kind == EXPR_ADDR && */
              opnd1->override == NULL &&
              opnd1->instr == EMPTY &&
              opnd1->value == 0 &&
              opnd1->sym &&
              opnd1->base_reg == NULL &&
              opnd1->idx_reg == NULL ) {

        sym = opnd1->sym;

        if( sym->state == SYM_GRP || sym->state == SYM_SEG ) {
            opnd2->kind = EXPR_ADDR;
            opnd2->override = opnd1->label_tok;
            opnd2->indirect |= opnd1->indirect;
            if( opnd1->explicit ) {
                opnd2->explicit = opnd1->explicit;
                opnd2->mem_type = opnd1->mem_type;
                opnd2->Ofssize  = opnd1->Ofssize;
            }
            TokenAssign( opnd1, opnd2 );
            opnd1->type = opnd2->type;

        } else if( Parse_Pass > PASS_1 || sym->state != SYM_UNDEFINED ) {
            CEmitError( SEGMENT_GROUP_OR_SEGREG_EXPECTED );
            DebugMsg(("colon_op error 4\n"));
            return( ERROR );
        }
    } else {
        CEmitError( SEGMENT_GROUP_OR_SEGREG_EXPECTED );
        DebugMsg(("colon_op error 5\n"));
        return( ERROR );
    }
    return( NOT_ERROR );
}

static ret_code positive_op( struct expr *opnd1, struct expr *opnd2 )
/*******************************************************************/
{
    DebugMsg1(("positive_op: value=%" I64X_SPEC " high=%" I64X_SPEC "\n", opnd2->llvalue, opnd2->hlvalue ));
    /*
     * The formats allowed are:
     *        + constant
     *        + float
     * v2.06: unlike the other operators unary + will
     * handle 128-bit values (needed for TBYTE integers)
     */

    MakeConst( opnd2 );
    if( opnd2->kind == EXPR_CONST ) {
        opnd1->kind = EXPR_CONST;
        opnd1->llvalue = opnd2->llvalue;
        opnd1->hlvalue = opnd2->hlvalue; /* v2.06: added */
    } else if( opnd2->kind == EXPR_FLOAT ) {
        opnd1->kind = EXPR_FLOAT;
        opnd1->float_tok = opnd2->float_tok;
        opnd1->negative = opnd2->negative;
    } else {
        CEmitError( CONSTANT_EXPECTED );
        DebugMsg(("positive_op: error 1\n"));
        return( ERROR );
    }
    return( NOT_ERROR );
}

static ret_code negative_op( struct expr *opnd1, struct expr *opnd2 )
/*******************************************************************/
{
    DebugMsg1(("negative_op: value=%" I64X_SPEC " high=%" I64X_SPEC "\n", opnd2->llvalue, opnd2->hlvalue ));
    /*
     * The formats allowed are:
     *        - constant
     *        - float
     */

    MakeConst( opnd2 );
    if( opnd2->kind == EXPR_CONST ) {
        opnd1->kind = EXPR_CONST;
        opnd1->llvalue = -opnd2->llvalue;
        /* v2.06: the unary '-' operator is to work with
         * magnitudes > 64-bit. Current implementation is
         * a bit hackish.
         */
        if ( opnd2->hlvalue )
            opnd1->hlvalue = -opnd2->hlvalue - 1;
        opnd1->negative = 1 - opnd2->negative;
    } else if( opnd2->kind == EXPR_FLOAT ) {
        opnd1->kind = EXPR_FLOAT;
        opnd1->float_tok = opnd2->float_tok;
        opnd1->negative = 1 - opnd2->negative;
    } else {
        CEmitError( CONSTANT_EXPECTED );
        DebugMsg(("negative_op: error 1\n"));
        return( ERROR );
    }
    return( NOT_ERROR );
}

/* v2.07: moved out from get_operand, case T_REG
 * this function is now called from calculate() only.
 */
static void CheckAssume( struct expr *opnd )
/******************************************/
{
    struct asym *sym = NULL;

    /* "index" has higher priority than "base" */
    if ( opnd->idx_reg )
        sym = GetStdAssumeEx( opnd->idx_reg->bytval );
    if (!sym && opnd->base_reg )
        sym = GetStdAssumeEx( opnd->base_reg->bytval );
    if ( sym ) {
        DebugMsg1(( "CheckAssume(%s, type=>%s<, mbr=>%s<): assume=%s [memtype=%X isptr=%u type=%s target_type=%s ptr_memt=%X]\n",
                   GetResWName( ( opnd->idx_reg ? opnd->idx_reg->tokval : opnd->base_reg->tokval ), NULL ),
                   opnd->type ? opnd->type->name : "NULL",
                   opnd->mbr ? opnd->mbr->name : "NULL",
                   sym->name, sym->mem_type, sym->is_ptr,
                   sym->type ? sym->type->name : "NULL",
                   sym->target_type ? sym->target_type->name : "NULL",
                   sym->ptr_memtype ));
        /* v2.08: skip ASSUMEd type if type or mbr is set */
        //if ( opnd->type || opnd->mbr )
        //    return;
        /* skip "alias" types */
        /* v2.05: obsolete */
        //for ( ; sym->type; sym = sym->type );
        /* v2.05: new */
        if ( sym->mem_type == MT_TYPE )
            opnd->type = sym->type;
        else if ( sym->is_ptr ) {
            opnd->type = sym->target_type;
            if ( sym->target_type )
                opnd->mem_type = sym->target_type->mem_type;
            else
                opnd->mem_type = sym->ptr_memtype;
        }
    }
}

/* get floating-point register index */

static ret_code check_streg( struct expr *opnd1, struct expr *opnd2 )
/*******************************************************************/
{
    if ( opnd1->scale > 0 ) {
        CEmitError( INVALID_USE_OF_REGISTER );
        return( ERROR );
    }
    opnd1->scale++; /* make sure there's only ONE bracket pair */
    if ( opnd2->kind != EXPR_CONST ) {
        CEmitError( INVALID_COPROCESSOR_REGISTER );
        return( ERROR );
    }
    opnd1->st_idx = opnd2->value;
    return( NOT_ERROR );
}

static ret_code calculate( struct expr *opnd1, struct expr *opnd2, const struct asm_tok *oper )
/*********************************************************************************************/
/* Performs operation <oper> with operands <opnd1> and <opnd2>.
 * the result will be returned in <opnd1>.
 * <oper> points to the item in tokenarray[] that contains the operator.
 * possible operators:
 *  T_OP_BRACKET       is (virtually) an alias for '+'
 *  T_OP_SQ_BRACKET    is (virtually) an alias for '+'
 *  '+' (unary + binary )
 *  '-' (unary + binary )
 *  '*'
 *  '/'
 *  T_DOT
 *  T_COLON
 *  T_BINARY_OPERATOR ( PTR, MOD, GE, GT, LE, GT, EQ, NE, also AND, OR, XOR, SHL, SHR )
 *  T_UNARY_OPERATOR ( OFFSET, SHORT, ... , also NOT )
 *
 * to be done: the code can be simplified because currently
 *             expression type is set to EXPR_REG when a
 *             register was found - even if it is inside [].
 *             A reg inside [] should ALWAYS give EXPR_ADDR!
 */
{
    int_32              temp;
    struct asym         *sym;
    char                *name;

    /* avoid to use the <string> member once it's part of an expression!
     * the <value> member is the one to be used then.
     * test case: db "a"+80h
     * v2.08: first: this is too early; second: the current operand is opnd2.
     * third: the space is also used by float_tok member, which cannot be cleared.
     * probably the best solution - at calculate()'s end:
     * if ( opnd1->kind == EXPR_CONST ) opnd1->quoted_string = NULL;
     */
    opnd1->quoted_string = NULL;

#if 0
    /* v2.06: Arithmetic is 64-bit only ( no need to care about REAL10,
     * because value of floating-points aren't stored in opndx.value ).
     * However, TBYTE integers will cause error if they won't fit in
     * 8 bytes - incompatible, since Masm remains silent in this case.
     */
    if ( opnd2->hlvalue != 0 ) {
        if ( error_msg )
            EmitConstError( opnd2 );
        return( ERROR );
    }
#endif
    switch( oper->token ) {
    case T_OP_SQ_BRACKET:
        /* v2.07: the ASSUMEs are now checked only when operator [] is done.
         * this is compatible with Masm:
         *   assume ebx:ptr <struct>
         *   mov eax, [ebx.<member>]             ;is to fail
         *   mov eax, [ebx.<struct>.<member>]    ;is to be ok
         * previously both variants were accepted by jwasm.
         */
        if ( opnd2->assumecheck == TRUE ) {
            opnd2->assumecheck = FALSE;   /* check ONE time only! */
            CheckAssume( opnd2 );
        }

        if ( opnd1->kind == EXPR_EMPTY ) {
            DebugMsg1(("calculate(%s): single item\n", oper->string_ptr ));
            TokenAssign( opnd1, opnd2 );
            opnd1->type = opnd2->type;
            if ( opnd1->is_type && opnd1->kind == EXPR_CONST )
                opnd1->is_type = 0;
            break;
        }

        /* v2.03: make JWasm reject syntax variants
         * "mov eax, DWORD [EBX]"
         * "mov eax, DWORD [var_name]"
         * variants still valid:
         * "mov eax, DWORD [WORD]"
         * "mov eax, DWORD [4]"
         * "mov eax, [DWORD][EBX]"
         */
        /* v2.08: structure/union names are ok: mov eax, S1[ebx] */
        //if ( opnd1->is_type == TRUE &&
        if ( opnd1->is_type == TRUE && opnd1->type == NULL &&
            (opnd2->kind == EXPR_ADDR || opnd2->kind == EXPR_REG ) ) {
            DebugMsg(("calculate(%s): incompatible usage of (simple) type\n", oper->string_ptr ));
            CEmitError( SYNTAX_ERROR_IN_EXPRESSION );
            return( ERROR );
        }

        /* v2.08: moved here from get_operand() */
        if ( opnd1->base_reg && opnd1->base_reg->tokval == T_ST )
            return( check_streg( opnd1, opnd2 ) );

#ifdef DEBUG_OUT
        if ( plus_op( opnd1, opnd2 ) == ERROR )
            return( ERROR );
        break;
#else
        return( plus_op( opnd1, opnd2 ) );
#endif
    case T_OP_BRACKET:

        if ( opnd1->kind == EXPR_EMPTY ) {
            DebugMsg1(("calculate(%s): single item\n", oper->string_ptr ));
            TokenAssign( opnd1, opnd2 );
            opnd1->type = opnd2->type;
            break;
        }
        /* v2.03: make JWasm reject syntax variants
         * "mov eax, DWORD (<label>)"
         */
        if ( opnd1->is_type == TRUE && opnd2->kind == EXPR_ADDR ) {
            DebugMsg(("calculate(%s): incompatible usage of (simple) type\n", oper->string_ptr ));
            CEmitError( SYNTAX_ERROR_IN_EXPRESSION );
            return( ERROR );
        }

        /* v2.08: moved here from get_operand() */
        if ( opnd1->base_reg && opnd1->base_reg->tokval == T_ST )
            return( check_streg( opnd1, opnd2 ) );

        DebugMsg1(("calculate(%s): calling plus_op()\n", oper->string_ptr ));
#ifdef DEBUG_OUT
        if ( plus_op( opnd1, opnd2 ) == ERROR )
            return( ERROR );
        break;
#else
        return( plus_op( opnd1, opnd2 ) );
#endif
    case '+':
        if ( oper->specval == UNARY_PLUSMINUS ) /* unary op? */
            return( positive_op( opnd1, opnd2 ) );
#ifdef DEBUG_OUT
        if ( plus_op( opnd1, opnd2 ) == ERROR )
            return( ERROR );
        break;
#else
        return( plus_op( opnd1, opnd2 ) );
#endif
    case '-':
        if ( oper->specval == UNARY_PLUSMINUS ) /* unary op? */
            return( negative_op( opnd1, opnd2 ) );
#ifdef DEBUG_OUT
        if ( minus_op( opnd1, opnd2 ) == ERROR )
            return( ERROR );
        break;
#else
        return( minus_op( opnd1, opnd2 ) );
#endif
    case T_DOT:
#ifdef DEBUG_OUT
        if ( dot_op( opnd1, opnd2 ) == ERROR )
            return( ERROR );
        break;
#else
        return( dot_op( opnd1, opnd2 ) );
#endif
    case T_COLON:
#ifdef DEBUG_OUT
        if ( colon_op( opnd1, opnd2 ) == ERROR )
            return( ERROR );
        break;
#else
        return( colon_op( opnd1, opnd2 ) );
#endif
    case '*':
        /*
         * The only formats allowed are:
         *        constant * constant
         *        register * scaling factor ( 1, 2, 4 or 8 )
         *                   386 only
         */
        DebugMsg1(("calculate(*): kind=%u-%u value=%" I64d_SPEC "-%" I64d_SPEC " mbr=%X-%X\n",
                   opnd1->kind,    opnd2->kind,
                   opnd1->value64, opnd2->value64,
                   opnd1->mbr,     opnd2->mbr ));

        MakeConst( opnd1 );
        MakeConst( opnd2 );

        if( check_same( opnd1, opnd2, EXPR_CONST ) ) {
            opnd1->llvalue *= opnd2->llvalue;
        } else if( check_both( opnd1, opnd2, EXPR_REG, EXPR_CONST ) ) {
            if( check_direct_reg( opnd1, opnd2 ) == ERROR ) {
                DebugMsg(("calculate(*) error direct register\n"));
                CEmitError( INVALID_USE_OF_REGISTER );
                return( ERROR );
            }
            /* scaling factor */
            if( opnd2->kind == EXPR_REG ) {
                /* scale * reg */
                opnd1->idx_reg = opnd2->base_reg;
                opnd1->scale = opnd1->value;
                opnd1->value = 0;
                //opnd2->base_reg = NULL;
            } else {
                /* reg * scale */
                opnd1->idx_reg = opnd1->base_reg;
                opnd1->scale = opnd2->value;
            }
            /* v2.08: check 0 (the default value) here */
            if ( opnd1->scale == 0 ) {
                CEmitError( SCALE_FACTOR_MUST_BE_1_2_4_OR_8 );
                return( ERROR );
            }

            opnd1->base_reg = NULL;
            opnd1->indirect |= opnd2->indirect;
            opnd1->kind = EXPR_ADDR;
        } else {
            DebugMsg(("calculate(*) error 2\n"));
            return( ConstError( opnd1, opnd2 ) );
        }
        break;
    case '/':
        /*
         * The only formats allowed are:
         *        constant / constant
         */
        DebugMsg1(("calculate(/): t1-t2 kind %u-%u values %" I64d_SPEC "-%" I64d_SPEC "\n",
                   opnd1->kind,    opnd2->kind,
                   opnd1->value64, opnd2->value64 ));
        MakeConst( opnd1 );
        MakeConst( opnd2 );

        if( check_same( opnd1, opnd2, EXPR_CONST ) == FALSE ) {
            DebugMsg(("calculate(/) error 1\n"));
            return( ConstError( opnd1, opnd2 ) );
        }

        if ( opnd2->llvalue == 0 ) {
            CEmitError( DIVIDE_BY_ZERO_IN_EXPR );
            DebugMsg(("calculate(/) error 2\n"));
            return( ERROR );
        }

        opnd1->value64 /= opnd2->value64;
        break;
    case T_BINARY_OPERATOR:
        DebugMsg1(("calculate(%s [T_BINARY_OPERATOR] ): t1-t2 kind %d-%d memtype %X-%X sym %s-%s type %s-%s\n",
                   oper->string_ptr,
                   opnd1->kind, opnd2->kind,
                   opnd1->mem_type, opnd2->mem_type,
                   opnd1->sym  ? opnd1->sym->name  : "NULL",
                   opnd2->sym  ? opnd2->sym->name  : "NULL",
                   opnd1->type ? opnd1->type->name : "NULL",
                   opnd2->type ? opnd2->type->name : "NULL" ));

        if ( oper->tokval == T_PTR ) {
            if ( opnd1->is_type == FALSE ) {
                if ( opnd1->sym && opnd1->sym->state == SYM_UNDEFINED ) {
                    CreateTypeSymbol( opnd1->sym, NULL, TRUE );
                    opnd1->type = opnd1->sym;
                    opnd1->sym = NULL;
                    opnd1->is_type = TRUE;
                } else {
                    DebugMsg(("calculate(PTR), error 1: t1 is_type == FALSE\n"));
                    CEmitError( INVALID_TYPE_EXPRESSION );
                    return( ERROR );
                }
            }
            opnd2->explicit = TRUE;
            /* v2.02: if operand is a register, make sure
             * that invalid combinations ("DWORD PTR AX") are flagged.
             */
            if ( opnd2->kind == EXPR_REG && opnd2->indirect == FALSE ) {
                temp = opnd2->base_reg->tokval;
                /* for segment registers, both size 2 and 4 is ok.*/
                if ( GetValueSp( temp ) & OP_SR ) {
                    if ( opnd1->value != 2 && opnd1->value != 4 ) {
                        CEmitError( INVALID_USE_OF_REGISTER );
                        return( ERROR );
                    }
                } else if ( opnd1->value != SizeFromRegister( temp ) ) {
                    CEmitError( INVALID_USE_OF_REGISTER );
                    return( ERROR );
                }
            } else if ( opnd2->kind == EXPR_FLOAT ) {
                if ( !( opnd1->mem_type & MT_FLOAT ) ) {
                    CEmitError( REAL_OR_BCD_NUMBER_NOT_ALLOWED );
                    return( ERROR );
                }
            }
            opnd2->mem_type = opnd1->mem_type;
            opnd2->Ofssize  = opnd1->Ofssize;
            /* v2.04: added */
            if ( opnd2->is_type )
                opnd2->value  = opnd1->value;
            if ( opnd1->override != NULL ) {
                if ( opnd2->override == NULL )
                    opnd2->override = opnd1->override;
                opnd2->kind = EXPR_ADDR;
            }
            //if ( opnd1->mbr )
            //    opnd2->mbr = opnd1->mbr;
            //if ( opnd1->sym )
            //    opnd2->sym = opnd1->sym;
            //opnd2->instr = opnd1->instr;
            TokenAssign( opnd1, opnd2 );
            break;
        }

        MakeConst( opnd1 );
        MakeConst( opnd2 );

        if ( check_same( opnd1, opnd2, EXPR_CONST ) )
            ;
        /* if it's EQ, NE, LE [, ...], operands may be either constants
         or relocatable labels */
        else if ( oper->precedence == CMP_PRECEDENCE &&
                 opnd1->kind != EXPR_CONST ) {
            if ( opnd1->kind == EXPR_ADDR && opnd1->indirect == FALSE && opnd1->sym )
                if ( opnd2->kind == EXPR_ADDR && opnd2->indirect == FALSE && opnd2->sym ) {
                    if ( MakeConst2( opnd1, opnd2 ) == ERROR ) {
                        DebugMsg(("calculate(%s) error 1\n", oper->string_ptr ));
                        return( ERROR );
                    }
                } else {
                    CEmitError( OPERAND_MUST_BE_RELOCATABLE );
                    DebugMsg(("calculate(%s) error 2, token2.kind=%u indirect=%u sym=%s\n",
                              oper->string_ptr, opnd2->kind, opnd2->indirect,
                              opnd2->sym ? opnd2->sym->name : "NULL" ));
                    return( ERROR );
                }
            else {
                CEmitError( CONSTANT_OR_RELOCATABLE_LABEL_EXPECTED );
                DebugMsg(("calculate(%s) error 3\n", oper->string_ptr ));
                return( ERROR );
            }
        } else {
            DebugMsg(("calculate(%s) error 4\n", oper->string_ptr ));
            return( ConstError( opnd1, opnd2 ) );
        }

        DebugMsg1(("calculate(%s): values=%I64u/%I64u types=%u/%u memtypes=%X/%X\n", oper->string_ptr,
                   opnd1->value64, opnd2->value64, opnd1->is_type, opnd2->is_type, opnd1->mem_type, opnd2->mem_type  ));
        switch( oper->tokval ) {
        case T_EQ:
#if 1 /* v2.03: added */
            /* if both operands are types, do a more strict comparison! */
            if ( opnd1->is_type && opnd2->is_type ) {
                opnd1->value64 = ( ((opnd1->value64 == opnd2->value64) &&
                ( opnd1->mem_type == opnd2->mem_type )) ? -1:0 );
            } else
#endif
            opnd1->value64 = ( opnd1->value64 == opnd2->value64 ? -1:0 );
            break;
        case T_NE:
#if 1 /* v2.03: added */
            /* if both operands are types, do a more strict comparison! */
            if ( opnd1->is_type && opnd2->is_type ) {
                opnd1->value64 = ( ((opnd1->value64 != opnd2->value64) ||
                ( opnd1->mem_type != opnd2->mem_type )) ? -1:0 );
            } else
#endif
            opnd1->value64 = ( opnd1->value64 != opnd2->value64 ? -1:0 );
            break;
        case T_LT:
            opnd1->value64 = ( opnd1->value64 <  opnd2->value64 ? -1:0 );
            break;
        case T_LE:
            opnd1->value64 = ( opnd1->value64 <= opnd2->value64 ? -1:0 );
            break;
        case T_GT:
            opnd1->value64 = ( opnd1->value64 >  opnd2->value64 ? -1:0 );
            break;
        case T_GE:
            opnd1->value64 = ( opnd1->value64 >= opnd2->value64 ? -1:0 );
            break;
        case T_MOD:
            if ( opnd2->llvalue == 0 ) {
                CEmitError( DIVIDE_BY_ZERO_IN_EXPR );
                return( ERROR );
            } else
                opnd1->llvalue %= opnd2->llvalue;
            break;
        case T_SHL:
            /* v2.04: check for shift count < 0 */
            DebugMsg1(("calculate(SHL): value=%" I64X_SPEC " << %lu (max=%u)\n", opnd1->llvalue, opnd2->value, 8 * sizeof( opnd1->llvalue ) ));
            if ( opnd2->value < 0 )
                CEmitError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            else if ( opnd2->value >= ( 8 * sizeof( opnd1->llvalue ) ) )
                opnd1->llvalue = 0;
            else
                opnd1->llvalue = opnd1->llvalue << opnd2->value;
            /* v2.01: result is 64-bit only if mode is USE64 */
            /* v2.06: for -Zm only. This is not entirely correct,
             * since Masm v6x also does 32-bit shifts, but since v2.06
             * JWasm intends to behave like Masm v8+.
             * Might be better to implement OPTION EXPR16|32|64.
             */
            //if ( ModuleInfo.Ofssize <= USE32 ) {
            if ( ModuleInfo.m510 ) {
                opnd1->hvalue = 0;
                opnd1->hlvalue = 0;
            }
            break;
        case T_SHR:
            /* Masm v8 works with unsigned 64-bit,
             * Masm v6 masks shift count with 0x3F.
             * v2.04: does behave like Masm v8+.
             * there is a problem with some compilers if shift
             * count is >= 64. So in this case the result is zeroed manually
             */
#if 0
            if ( opnd1->hvalue == -1 ) {
                opnd1->hvalue = 0;
                opnd1->hlvalue = 0;
            }
#endif
            /* v2.04: check for shift count < 0 */
            if ( opnd2->value < 0 )
                CEmitError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            else if ( opnd2->value >= ( 8 * sizeof( opnd1->llvalue ) ) )
                opnd1->llvalue = 0;
            else
                opnd1->llvalue = opnd1->llvalue >> opnd2->value;
            break;
        case T_AND:
            opnd1->llvalue &= opnd2->llvalue;
            break;
        case T_OR:
            opnd1->llvalue |= opnd2->llvalue;
            break;
        case T_XOR:
            opnd1->llvalue ^= opnd2->llvalue;
            break;
        }
        break; /* end case T_BINARY_OPERATOR */
    case T_UNARY_OPERATOR:
        DebugMsg1(("calculate(%s [T_UNARY_OPERATOR]): opnd2 kind=%u sym=%s mbr=%s type=%s memtype=%X is_type=%u indirect=%u\n",
                   oper->string_ptr,
                   opnd2->kind,
                   opnd2->sym ? opnd2->sym->name : "NULL",
                   opnd2->mbr ? opnd2->mbr->name : "NULL",
                   opnd2->type ? opnd2->type->name : "NULL",
                   opnd2->mem_type, opnd2->is_type, opnd2->indirect ));
        /* NOT is an instruction and hence has no valid
         * value to be returned by GetValueSp() or GetSflagsSp()!
         */
        if( oper->tokval == T_NOT ) {
            MakeConst( opnd2 );
            if( opnd2->kind != EXPR_CONST ) {
                CEmitError( CONSTANT_OPERAND_EXPECTED );
                DebugMsg(("calculate(%s) error 1\n", oper->string_ptr ));
                return( ERROR );
            }
            TokenAssign( opnd1, opnd2 );
            opnd1->llvalue = ~(opnd2->llvalue);
            break;
        }

        /* operator         accepts
         ----------------------------------------------
         SIZEOF/SIZE        label, type, struct field
         LENGTHOF/LENGTH    label, struct field
         TYPE               label, type, struct field, register, number
         LOW                constant, label (OMF+BIN only)
         HIGH               constant, label (OMF+BIN only)
         LOWWORD            constant, label
         HIGHWORD           constant
         LOW32              constant, label, float
         HIGH32             constant, float
         THIS               type
         OPATTR/.TYPE       label, type, struct field, register, number
         SHORT              label
         SEG                label
         OFFSET/LROFFSET    label, struct field, number
         IMAGEREL           label
         SECTIONREL         label
         WIDTH/MASK         bitfields or RECORD type
         */

        temp = GetValueSp( oper->tokval );

        sym = opnd2->sym;
        if( opnd2->mbr != NULL )
            sym = opnd2->mbr;

        /* for error displays, get the position of the operand that
         * caused the trouble.
         */
        if ( opnd2->instr != EMPTY )
            name = oper->tokpos + strlen( oper->string_ptr ) + 1;
        else if ( sym )
            name = sym->name;
        else if ( opnd2->base_reg != NULL && opnd2->indirect == FALSE )
            name = opnd2->base_reg->string_ptr;
        else
            name = oper->tokpos + strlen( oper->string_ptr ) + 1;

        switch ( opnd2->kind ) {
        case EXPR_CONST:
            /* v2.05: conditions "struct-field" and "istype" exchanged */
            /* is item a struct field? */
            if ( opnd2->mbr != NULL && opnd2->mbr->state != SYM_TYPE ) {
                if ( opnd2->mbr->mem_type == MT_BITS ) { /* bitfield? */
                    if ( ( temp & AT_BF ) == 0 ) {
                        invalid_operand( oper->string_ptr, name );
                        return( ERROR );
                    }
                } else {
                    if ( ( temp & AT_FIELD ) == 0 ) {
                        invalid_operand( oper->string_ptr, name );
                        return( ERROR );
                    }
                }
            } else if ( opnd2->is_type ) { /* is item a type? */
                if ( ( temp & AT_TYPE ) == 0 ) {
                    invalid_operand( oper->string_ptr, name );
                    return( ERROR );
                }
            } else { /*  or is it a number? */
                if ( ( temp & AT_NUM ) == 0 ) {
                    invalid_operand( oper->string_ptr, name );
                    return( ERROR );
                }
            }
            break;
        case EXPR_ADDR:
            /* an indirect memory operand? (not an auto variable) */
            if ( opnd2->indirect == TRUE && opnd2->sym == NULL ) {
                if ( ( temp & AT_IND ) == 0 ) {
                    invalid_operand( oper->string_ptr, name );
                    return( ERROR );
                }
            } else {
                if ( ( temp & AT_LABEL ) == 0 ) {
                    invalid_operand( oper->string_ptr, name );
                    return( ERROR );
                }
            }
#if 0 /* v2.08: this if() obsolete? */
            if( opnd2->instr != EMPTY ) {
                /* if instr is set, it's not a full address */
                switch ( oper->tokval ) {
                case T_LOW:
                case T_HIGH:
                case T_LOWWORD:
                case T_HIGHWORD:
#if LOHI32
                case T_LOW32:
                case T_HIGH32:
#endif
                case T_TYPE:
                case T_OPATTR:
                case T_DOT_TYPE:
                case T_OFFSET: /* v2.08: added, to allow OFFSET OFFSET <addr> */
                    break;
                default:
                    /* remaining: OFFSET, LROFFSET, IMAGEREL, SECTIONREL, SEG,
                     * SHORT
                     * THIS (won't set opnd.instr)
                     * (SIZE, SIZEOF, LENGTH, LENGHTOF, MASK, WIDTH) -> EXPR_CONST
                     *
                     */
                    CEmitError( LABEL_EXPECTED );
                    DebugMsg(("calculate %s error 2\n", oper->string_ptr ));
                    return( ERROR );
                }
            }
#endif
            break;
        case EXPR_REG:
            if ( ( temp & AT_REG ) == 0 ) {
                invalid_operand( oper->string_ptr, name );
                return( ERROR );
            }
            break;
        case EXPR_FLOAT: /* v2.05: added */
            if ( ( temp & AT_FLOAT ) == 0 ) {
                DebugMsg(("calculate %s 'float' error\n", oper->string_ptr ));
                CEmitError( REAL_OR_BCD_NUMBER_NOT_ALLOWED );
                return( ERROR );
            }
            break;
        }
#ifdef DEBUG_OUT
        if ( unaryop[ GetSflagsSp( oper->tokval ) ]( oper->tokval, opnd1, opnd2, sym, name ) == ERROR )
            return( ERROR );
        break;
#else
        return( unaryop[ GetSflagsSp( oper->tokval ) ]( oper->tokval, opnd1, opnd2, sym, name ) );
#endif
    //case T_RES_ID:
    default: /* shouldn't happen */
        DebugMsg(("calculate(%s): unknown operator\n", oper->string_ptr ));
        if ( error_msg )
            EmitErr( SYNTAX_ERROR_EX, oper->string_ptr );
        return( ERROR );
    } /* end switch( oper->token ) */

#ifdef DEBUG_OUT
    if ( opnd1->hlvalue ) {
        DebugMsg1(("calculate(%s) exit, ok, value=0x%" I64X_SPEC "_%016" I64X_SPEC " memtype=0x%X indirect=%u type=>%s<\n",
                   oper->string_ptr,
                   opnd1->hlvalue, opnd1->llvalue,
                   opnd1->mem_type,
                   opnd1->indirect, opnd1->type ? opnd1->type->name : "NULL" ));
    } else if ( opnd1->hvalue ) {
        DebugMsg1(("calculate(%s) exit, ok, value=%" I64d_SPEC"(0x%" I64X_SPEC") memtype=0x%X indirect=%u type=>%s<\n",
                   oper->string_ptr,
                   opnd1->llvalue, opnd1->llvalue,
                   opnd1->mem_type,
                   opnd1->indirect, opnd1->type ? opnd1->type->name : "NULL" ));
    } else {
        DebugMsg1(("calculate(%s) exit, ok, value=%d(0x%X) memtype=0x%X ind=%u exp=%u type=%s mbr=%s\n",
                   oper->string_ptr,
                   opnd1->value, opnd1->value,
                   opnd1->mem_type,
                   opnd1->indirect, opnd1->explicit,
                   opnd1->type ? opnd1->type->name : "NULL",
                   opnd1->mbr ? opnd1->mbr->name : "NULL" ));
    }
#endif
    return( NOT_ERROR );
}

/* this code runs BEFORE the - right - operand of an operator is read */

static void PrepareOp( struct expr *opnd, const struct expr *old, const struct asm_tok *oper )
/********************************************************************************************/
{
    opnd->is_opattr = old->is_opattr;

    switch ( oper->token ) {
    case T_DOT:
        DebugMsg(("PrepareOp: DOT operator found, old.sym=%X, old.type=%s, expr=%s\n", old->sym, (old->type ? old->type->name : "NULL" ), oper->tokpos + strlen( oper->string_ptr ) ));
        if ( old->type ) {
            DebugMsg1(("PrepareOp: implicit type: %s\n", old->type->name));
            opnd->type = old->type;
        //} else if ( old->sym && old->sym->mem_type == MT_TYPE ) {
        } else if ( old->sym && old->sym->mem_type == MT_TYPE && old->instr == EMPTY ) {
            DebugMsg1(("PrepareOp: label %s, implicit type: %s\n", old->sym->name, old->sym->type->name));
            for ( opnd->type = old->sym->type; opnd->type->type; opnd->type = opnd->type->type );

        /* v2.07: changed */
        //} else if ( !ModuleInfo.oldstructs ) {
        /* v2.08: reverted, replaced by changes in dot_op() and get_operand(), case T_STYPE */
        //} else if ( old->sym && old->sym->mem_type == MT_EMPTY && !ModuleInfo.oldstructs ) {
        } else if ( !ModuleInfo.oldstructs ) {
            if ( !nullstruct ) {
                nullstruct = CreateTypeSymbol( NULL, "", FALSE );
                //nullstruct->typekind = TYPE_STRUCT;
            }
            DebugMsg1(("PrepareOp: implicit type: <nullstruct>\n" ));
            opnd->type = nullstruct;
            /* a - probably unnecessary - hack */
            //opnd->type->type = old->sym;
        }
        break;
    case T_UNARY_OPERATOR:
        switch ( oper->tokval ) {
        case T_OPATTR:
        case T_DOT_TYPE:
            DebugMsg(("PrepareOp: OPATTR operator found, old.sym=%X, old.type=%s, expr=%s\n",
                      old->sym, (old->type ? old->type->name : "NULL" ), oper->tokpos + strlen( oper->string_ptr ) ));
            opnd->is_opattr = TRUE;
        }
        break;
    }
}

static ret_code OperErr( int i, struct asm_tok tokenarray[] )
/***********************************************************/
{
    if ( error_msg ) {
        if ( tokenarray[i].token <= T_BAD_NUM ) {
            EmitError( MISSING_OPERATOR_IN_EXPRESSION ); ERRLOC(i);
        } else
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
    }
    return( ERROR );
}

#define IsCurrToken( tok )  ( tokenarray[*i].token == tok )

static ret_code evaluate( struct expr *opnd1, int *i, struct asm_tok tokenarray[], int end, enum process_flag proc_flag )
/***********************************************************************************************************************/
{
    DebugMsg1(("%u evaluate(i=%d, end=%d, flags=%X) enter [opnd1: kind=%d type=%s]\n",
               ++evallvl, *i, end, proc_flag, opnd1->kind, opnd1->type ? opnd1->type->name : "NULL" ));

    /* v2.07: this function has been "simplified".
     * it's ensured now that if any operator is involved
     * - including () and [] - then calculate() will be called.
     */

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /* Look at first token, which may be an unary operator or an operand */
    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    if ( opnd1->kind == EXPR_EMPTY ) {
        if( is_unary_op( tokenarray[*i].token ) ) {
            DebugMsg1(("evaluate: unary operator detected: %s [value=%u]\n", tokenarray[*i].string_ptr, tokenarray[*i].tokval));
        } else {
            if( get_operand( opnd1, i, tokenarray ) == ERROR ) {
                DebugMsg(("%u evaluate exit 7, error (get_operand() failed), i=%d\n", evallvl--, *i ));
                return( ERROR );
            }
            /* if is the expression is a single item, then exit quickly! */
            if( *i >= end || IsCurrToken( T_CL_BRACKET ) || IsCurrToken( T_CL_SQ_BRACKET ) ) {
                DebugMsg1(("%u evaluate exit, ok, kind=%u value=%d string=%X memtype=%Xh indirect=%u ofssize=%d type=%X\n",
                           evallvl--, opnd1->kind,      opnd1->value,
                           opnd1->quoted_string,    opnd1->mem_type,
                           opnd1->indirect,  opnd1->Ofssize, opnd1->type ));
                return( NOT_ERROR );
            }
            /* Read the operator. Must be binary or open bracket */
            if ( tokenarray[*i].token == '+' || tokenarray[*i].token == '-' )
                tokenarray[*i].specval = BINARY_PLUSMINUS;
            else if( !is_operator( tokenarray[*i].token ) || tokenarray[*i].token == T_UNARY_OPERATOR ) {
                DebugMsg(("%u evaluate: unexpected token at idx=%u, token=%X >%s<\n", evallvl--, *i, tokenarray[*i].token, tokenarray[*i].tokpos ));
                return( OperErr( *i, tokenarray ) );
            }
        }
    }

    /* now handle operators. */
    do {
        int curr_operator;
        struct expr opnd2;

        curr_operator = *i;
        DebugMsg1(("%u evaluate loop enter, operator index=%u ('%s'), opnd1->sym=%X, type=%s\n",
                  evallvl, curr_operator, tokenarray[curr_operator].string_ptr, opnd1->sym, (opnd1->type ? opnd1->type->name : "NULL") ));
        (*i)++;

        init_expr( &opnd2 );
        PrepareOp( &opnd2, opnd1, &tokenarray[curr_operator] );

        /* read the (next) operand */

        if( tokenarray[curr_operator].token == T_OP_BRACKET ||
           tokenarray[curr_operator].token == T_OP_SQ_BRACKET ) {
            int exp_token = T_CL_BRACKET;
            if( tokenarray[curr_operator].token == T_OP_SQ_BRACKET ) {
                op_sq_bracket_level++;
                exp_token = T_CL_SQ_BRACKET;
            }
            if( evaluate( &opnd2, i, tokenarray, end, PROC_BRACKET ) == ERROR ) {
                DebugMsg(("%u evaluate exit 12, error, i=%d\n", evallvl--, *i ));
                return( ERROR );
            }
            if( !IsCurrToken( exp_token ) ) {
                CEmitError( MISSING_RIGHT_PARENTHESIS_IN_EXPRESSION );
                DebugMsg(("%u evaluate exit 11, error, i=%d\n", evallvl--, *i ));
                return( ERROR );
            }
            (*i)++;
            if ( tokenarray[curr_operator].token == T_OP_SQ_BRACKET )
                op_sq_bracket_level--;

        } else if( is_unary_op( tokenarray[*i].token ) ) {
            if( evaluate( &opnd2, i, tokenarray, end, PROC_OPERAND ) == ERROR ) {
                DebugMsg(("%u evaluate exit 14, error, i=%d\n", evallvl--, *i ));
                return( ERROR );
            }
        } else {
            if( get_operand( &opnd2, i, tokenarray ) == ERROR ) {
                DebugMsg(("%u evaluate exit 16, error, i=%d\n", evallvl--, *i ));
                return( ERROR );
            }
        }

        /* Look at the next operator and compare its priority with the
         current one. Continue to do so until an operator with a higher
         priority is found.
         */

        while( *i < end && !IsCurrToken( T_CL_BRACKET) && !IsCurrToken( T_CL_SQ_BRACKET) ) {

            if ( tokenarray[*i].token == '+' || tokenarray[*i].token == '-' )
                tokenarray[*i].specval = BINARY_PLUSMINUS;
            else if( !is_operator( tokenarray[*i].token ) || tokenarray[*i].token == T_UNARY_OPERATOR ) {
                DebugMsg(("%u evaluate: unexpected token at %u, token=%X >%s<\n", evallvl--, *i, tokenarray[*i].token, tokenarray[*i].tokpos ));
                return( OperErr( *i, tokenarray ) );
            }
            if( get_precedence( &tokenarray[*i] ) < get_precedence( &tokenarray[curr_operator] ) ) {
                if( evaluate( &opnd2, i, tokenarray, end, PROC_OPERAND ) == ERROR ) {
                    DebugMsg(("%u evaluate exit 18, error, i=%d\n", evallvl--, *i ));
                    return( ERROR );
                }
            } else {
                break;
            }
        }

        if( calculate( opnd1, &opnd2, &tokenarray[curr_operator] ) == ERROR ) {
            DebugMsg(("%u evaluate exit 19, error, i=%d\n", evallvl--, *i ));
            return( ERROR );
        }

    } while ( ( proc_flag == PROC_BRACKET &&
                *i < end &&
                !IsCurrToken( T_CL_BRACKET ) &&
                !IsCurrToken( T_CL_SQ_BRACKET ) ) );

#ifdef DEBUG_OUT
    if ( opnd1->hvalue != -1 && opnd1->hvalue != 0 ) {
        DebugMsg1(("%u evaluate exit, ok, value=%" I64d_SPEC "(0x%" I64X_SPEC") kind=%u string=%s memtype=0x%X indirect=%u type=>%s<\n",
                  evallvl--, opnd1->llvalue, opnd1->llvalue,
                  opnd1->kind,
                  opnd1->quoted_string ? opnd1->quoted_string->string_ptr : "NULL",
                  opnd1->mem_type,
                  opnd1->indirect, opnd1->type ? opnd1->type->name : "NULL" ));
    } else {
        DebugMsg1(("%u evaluate exit, ok, value=%ld(0x%" FX32 ") kind=%u string=%s memtype=0x%X ind=%u exp=%u ofssiz=%u type=>%s<\n",
                  evallvl--, opnd1->value, opnd1->value,
                  opnd1->kind,
                  opnd1->quoted_string ? opnd1->quoted_string->string_ptr : "NULL",
                  opnd1->mem_type,
                  opnd1->indirect, opnd1->explicit, opnd1->Ofssize,
                  opnd1->type ? opnd1->type->name : "NULL" ));
    }
#endif
    return( NOT_ERROR );
}

static bool is_expr_item( struct asm_tok *item )
/**********************************************/
/* Check if a token is a valid part of an expression.
 * chars + - * / . : [] and () are operators.
 * also done here:
 * T_INSTRUCTION  SHL, SHR, AND, OR, XOR changed to T_BINARY_OPERATOR
 * T_INSTRUCTION  NOT                    changed to T_UNARY_OPERATOR
 * T_DIRECTIVE    PROC                   changed to T_STYPE
 * for the new operators the precedence is set.
 * DUP, comma or other instructions or directives terminate the expression.
 */
{
    switch( item->token ) {
    case T_INSTRUCTION:
        switch( item->tokval ) {
        case T_SHL:
        case T_SHR:
            item->token = T_BINARY_OPERATOR;
            item->precedence = 8;
            return( TRUE );
        case T_NOT:
            item->token = T_UNARY_OPERATOR;
            item->precedence = 11;
            return( TRUE );
        case T_AND:
            item->token = T_BINARY_OPERATOR;
            item->precedence = 12;
            return( TRUE );
        case T_OR:
        case T_XOR:
            item->token = T_BINARY_OPERATOR;
            item->precedence = 13;
            return( TRUE );
        }
        return( FALSE );
    case T_RES_ID:
        if ( item->tokval == T_DUP ) /* DUP must terminate the expression */
            return( FALSE );
        break;
    case T_DIRECTIVE:
        /* PROC is converted to a type */
        if ( item->tokval == T_PROC ) {
            item->token = T_STYPE;
            /* v2.06: avoid to use ST_PROC */
            //item->bytval = ST_PROC;
            item->tokval = ( ( SIZE_CODEPTR & ( 1 << ModuleInfo.model ) ) ? T_FAR : T_NEAR );
            return( TRUE );
        }
        /* fall through. Other directives will end the expression */
    case T_COMMA:
    //case T_FLOAT: /* v2.05: floats are now handled */
    //case T_QUESTION_MARK: /* v2.08: no need to be handled here */
        return( FALSE );
    }
    return( TRUE );
}

/* evaluate an operand
 * start_tok: index of first token of expression
 * end_tok:   index of last  token of expression
 */
ret_code EvalOperand( int *start_tok, struct asm_tok tokenarray[], int end_tok, struct expr *result, uint_8 flags )
/*****************************************************************************************************************/
{
    int         i;

    DebugMsg1(("EvalOperand(start=%u, end=%u, flags=%X) enter: >%s<\n", *start_tok, end_tok, flags, tokenarray[*start_tok].tokpos ));

    init_expr( result );

    for( i = *start_tok; ( i < end_tok ) && is_expr_item( &tokenarray[i] ); i++ );
    if ( i == *start_tok )
        return( NOT_ERROR );

    op_sq_bracket_level = 0;
    error_msg = !(flags & EXPF_NOERRMSG);
    eflags = flags;
    return ( evaluate( result, start_tok, tokenarray, i, PROC_BRACKET ) );
}

/* global init (called once for each module) */

void ExprEvalInit( void )
/***********************/
{
    thissym = NULL;
    nullstruct = NULL;
    nullmbr = NULL;
}
