/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  handles EQU and '=' directives.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "expreval.h"
#include "equate.h"
#include "tokenize.h"
#include "macro.h"
#include "fastpass.h"
#include "listing.h"
#include "input.h"

extern void myatoi128( const char *, uint_64[], int, int );

#if defined(LLONG_MAX) || defined(__GNUC__) || defined(__TINYC__)
/* gcc needs suffixes if the constants won't fit in long type */
const int_64 maxintvalues[] = { 0x00000000ffffffffULL, 0x00000000ffffffffULL,
#if AMD64_SUPPORT
0x7fffffffffffffffULL
#endif
};
const int_64 minintvalues[] = { 0xffffffff00000000ULL, 0xffffffff00000000ULL,
#if AMD64_SUPPORT
0x8000000000000000ULL
#endif
};
#else
/* the "i64" suffixes shouldn't be necessary, but sometimes it's needed (OCC) */
const int_64 maxintvalues[] = { 0x00000000ffffffffi64, 0x00000000ffffffffi64,
#if AMD64_SUPPORT
0x7fffffffffffffffi64
#endif
};
const int_64 minintvalues[] = { 0xffffffff00000000i64, 0xffffffff00000000i64,
#if AMD64_SUPPORT
0x8000000000000000i64
#endif
};
#endif

/* set the value of a constant (EQU) or an assembly time variable (=) */

static void SetValue( struct asym *sym, struct expr *opndx )
/**********************************************************/
{
    sym->isequate = TRUE;
    sym->state = SYM_INTERNAL;
    sym->isdefined = TRUE;
    if ( opndx->kind == EXPR_CONST ) {
        /* v2.07: use expression's memtype */
        //sym->mem_type = MT_ABS;
        sym->mem_type = opndx->mem_type;
        sym->uvalue = opndx->uvalue;
        sym->value3264 = opndx->hvalue;
        sym->segment = NULL;
        sym->isproc = FALSE;
    } else {
        sym->isproc = opndx->sym->isproc;
        /* for a PROC alias, copy the procinfo extension! */
        if ( sym->isproc ) {
            struct dsym *dir = (struct dsym *)sym;
            dir->e.procinfo = ((struct dsym *)opndx->sym)->e.procinfo;
        }
        sym->mem_type = opndx->mem_type;
        /* v2.01: allow equates of variables with arbitrary type.
         * Currently the expression evaluator sets opndx.mem_type
         * to the mem_type of the type (i.e. QWORD for a struct with size 8),
         * which is a bad idea in this case. So the original mem_type of the
         * label is used instead.
         */
        if ( opndx->sym->mem_type == MT_TYPE && opndx->explicit == FALSE ) {
            sym->mem_type = opndx->sym->mem_type;
            sym->type = opndx->sym->type;
        }
        sym->offset = opndx->sym->offset + opndx->value;
        sym->segment = opndx->sym->segment;
    }
    return;
}

/* the '=' directive defines an assembly time variable.
 * this can only be a number, with or without "type".
 */

static struct asym *CreateAssemblyTimeVariable( struct asm_tok tokenarray[] )
/***************************************************************************/
{
    struct asym         *sym;
    const char          *name = tokenarray[0].string_ptr;
    int                 i = 2;
    struct expr         opndx;

    DebugMsg1(( "CreateAssemblyTimeVariable(%s) enter\n", name ));

    /* v2.08: for plain numbers ALWAYS avoid to call evaluator */
    if ( tokenarray[2].token == T_NUM &&
        //tokenarray[3].token == T_FINAL &&
        //tokenarray[2].itemlen <= 8 ) {
        tokenarray[3].token == T_FINAL ) {
        //opndx.llvalue = tokenarray[2].value64;
        //opndx.llvalue = *(uint_64 *)(tokenarray[2].string_ptr - sizeof(uint_64) );
        myatoi128( tokenarray[i].string_ptr, &opndx.llvalue, tokenarray[i].numbase, tokenarray[i].itemlen );
    check_number:
        opndx.kind = EXPR_CONST;
        opndx.mem_type = MT_EMPTY; /* v2.07: added */
        /* v2.08: check added. the number must be 32-bit */
        if ( opndx.hlvalue != 0 ||
            opndx.value64 < minintvalues[ModuleInfo.Ofssize] ||
            opndx.value64 > maxintvalues[ModuleInfo.Ofssize] ) {
            EmitConstError( &opndx );
            return( NULL );
        }
    } else {
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
            return( NULL );
        if( tokenarray[i].token != T_FINAL ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( NULL );
        }
        /* a relocatable item (EXPR_ADDR) is accepted if it is guess of the
         * expression evaluator, which encountered a forward ref */
        if( opndx.kind != EXPR_CONST &&
           ( opndx.kind != EXPR_ADDR ||
            opndx.indirect == TRUE ||
            ( opndx.sym != NULL && opndx.sym->state != SYM_INTERNAL && opndx.sym->state != SYM_UNDEFINED ) ) ) {
            DebugMsg(( "CreateAssemblyTimeVariable(%s) kind=%u sym=%p state=%u\n", name, opndx.kind, opndx.sym, opndx.sym ? opndx.sym->state : 0 ));
            EmitError( CONSTANT_EXPECTED );
            return( NULL );
        }

        /* v2.08: accept any result that fits in 64-bits from expression evaluator */
        if ( opndx.hlvalue != 0 ) {
            EmitConstError( &opndx );
            return( NULL );
        }
        /* for quoted strings, the same restrictions as for plain numbers apply */
        if ( opndx.quoted_string )
            goto check_number;
    }

    sym = SymSearch( name );
    if( sym == NULL || sym->state == SYM_UNDEFINED ) {
        if( sym == NULL ) {
            sym = SymCreate( name );
        } else {
            sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
        }
        //sym->variable  = TRUE;
    //} else if ( sym->state == SYM_EXTERNAL && sym->weak == TRUE && sym->mem_type == MT_ABS ) {
    } else if ( sym->state == SYM_EXTERNAL && sym->weak == TRUE && sym->mem_type == MT_EMPTY ) {
        sym_ext2int( sym );
        //sym->variable  = TRUE;
    } else if ( sym->state != SYM_INTERNAL ||
               ( sym->variable == FALSE &&
                ( opndx.uvalue != sym->uvalue || opndx.hvalue != sym->value3264 ) ) ) {
        EmitErr( SYMBOL_REDEFINITION, sym->name );
        return( NULL );
    }
#if FASTPASS
    /* v2.04a regression in v2.04. Do not save the variable when it
     * is defined the first time */
    if ( StoreState && sym->saved == FALSE && sym->isdefined == TRUE ) {
        SaveVariableState( sym );
    }
#endif
    sym->variable = TRUE;
    SetValue( sym, &opndx );
    DebugMsg1(( "CreateAssemblyTimeVariable(%s) memtype=%Xh value=%d\n", name, sym->mem_type, sym->value ));
    return( sym );
}

/* CreateVariable().
 * define an assembly time variable directly without using the token buffer.
 * this is used for some internally generated variables (SIZESTR, INSTR, @Cpu)
 * NO listing is written! The value is ensured to be max 16-bit wide.
 */
struct asym *CreateVariable( const char *name, int value )
/********************************************************/
{
    struct asym      *sym;

    DebugMsg1(( "CreateVariable(%s, %d ) enter\n", name, value ));

    sym = SymSearch( name );
    if( sym == NULL ) {
        sym = SymCreate( name );
#if FASTPASS
        sym->saved = FALSE;
#endif
    } else if ( sym->state == SYM_UNDEFINED ) {
        sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
    } else if ( sym->isequate == FALSE ) {
        EmitErr( SYMBOL_REDEFINITION, name );
        return( NULL );
    }
#if FASTPASS
    if ( StoreState && sym->saved == FALSE ) {
        SaveVariableState( sym );
    }
#endif
    sym->isdefined  = TRUE;
    sym->state    = SYM_INTERNAL;
    //sym->mem_type = MT_ABS;
    sym->variable = TRUE;
    sym->value    = value;
    sym->isequate = TRUE;
    return( sym );
}

/*
 * CreateConstant()
 * this is the worker behind EQU.
 * EQU may define 3 different types of equates:
 * - numeric constants (with or without "type")
 * - relocatable items ( aliases )
 * - text macros
 * the argument may be
 * - an expression which can be evaluated to a number or address
 * - a text literal (enclosed in <>)
 * - anything. This will also become a text literal.
 */

struct asym *CreateConstant( struct asm_tok tokenarray[] )
/********************************************************/
{
    struct asym         *sym;
    const char          *name = tokenarray[0].string_ptr;
    int                 i = 2;
    ret_code            rc;
    char                *p;
    bool                cmpvalue = FALSE;
    struct expr         opnd;
    char                argbuffer[MAX_LINE_LEN];

    DebugMsg1(( "CreateConstant(%s) enter\n", name ));

    sym = SymSearch( name );

    if( sym == NULL ||
       sym->state == SYM_UNDEFINED ||
       ( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) ) {
        /* It's a "new" equate.
         * wait with definition until type of equate is clear
         */
    } else if( sym->state == SYM_TMACRO ) {

        return ( SetTextMacro( tokenarray, sym, name, tokenarray[2].tokpos ) );

    } else if( sym->isequate == FALSE ) {

        DebugMsg1(( "CreateConstant(%s) state=%u, mem_type=%Xh, value=%" FX32 ", symbol redefinition\n", name, sym->state, sym->mem_type, sym->value));
        EmitErr( SYMBOL_REDEFINITION, name );
        return( NULL );

    } else {
        if ( sym->asmpass == ( Parse_Pass & 0xFF ) )
            cmpvalue = TRUE;
        sym->asmpass = Parse_Pass;
    }

    /* try to evaluate the expression */

    if ( tokenarray[2].token == T_STRING && tokenarray[2].string_delim == '<' ) {

        /* the simplest case: value is a literal. define a text macro! */
        if ( tokenarray[3].token != T_FINAL ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[3].string_ptr );
            return( NULL );
        }
        return ( SetTextMacro( tokenarray, sym, name, tokenarray[2].string_ptr ) );

    } else if ( tokenarray[2].token == T_NUM && Token_Count == 3 ) {

        p = tokenarray[2].string_ptr;
    do_single_number:
        /* value is a plain number. it will be accepted only if it fits into 32-bits.
         * Else a text macro is created.
         */
        myatoi128( tokenarray[2].string_ptr, &opnd.llvalue, tokenarray[2].numbase, tokenarray[2].itemlen );
    check_single_number:
        opnd.instr = EMPTY;
        opnd.kind = EXPR_CONST;
        opnd.mem_type = MT_EMPTY; /* v2.07: added */
        opnd.flags1 = 0;
        /* v2.08: does it fit in 32-bits */
        if ( opnd.hlvalue == 0 && opnd.value64 >= minintvalues[ModuleInfo.Ofssize] &&
            opnd.value64 <= maxintvalues[ModuleInfo.Ofssize] ) {
            rc = NOT_ERROR;
            DebugMsg1(( "CreateConstant(%s): simple numeric value=%" I64d_SPEC "\n", name, opnd.value64 ));
            i++;
        } else
            return ( SetTextMacro( tokenarray, sym, name, p ) );

    } else {
        p = tokenarray[2].tokpos;
        if ( Parse_Pass == PASS_1 ) {
            /* if the expression cannot be evaluated to a numeric value,
             * it's to become a text macro. The value of this macro will be
             * the original (unexpanded!) line - that's why it has to be
             * saved here to argbuffer[].
             */
            strcpy( argbuffer, p );
            DebugMsg1(("CreateConstant(%s): before ExpandLineItems: >%s<\n", name, p ));
            /* expand EQU argument (macro functions won't be expanded!) */
            if ( ExpandLineItems( p, 2, tokenarray, FALSE, TRUE ) )
                /* v2.08: if expansion result is a plain number, handle is specifically.
                 * this is necessary because values of expressions may be 64-bit now.
                 */
                p = argbuffer; /* ensure that p points to unexpanded source */
                if ( tokenarray[2].token == T_NUM && Token_Count == 3 ) {
                    goto do_single_number;
                }
            DebugMsg1(("CreateConstant(%s): after ExpandLineItems: >%s<\n", name, p ));
        }
        rc = EvalOperand( &i, tokenarray, Token_Count, &opnd, EXPF_NOERRMSG | EXPF_NOLCREATE );

        /* v2.08: if it's a quoted string, handle it like a plain number */
        if ( opnd.quoted_string ) {
            goto check_single_number;
        }

        /* check here if last token has been reached? */
    }
    /* what is an acceptable 'number' for EQU?
     * 1. a numeric value - if magnitude is <= 64 (or 32, if it's a plain number)
     *    This includes struct fields.
     * 2. an address - if it is direct and doesn't contain an external reference.
     * Anything else will be stored as a text macro.
     * v2.04: large parts rewritten.
     */
    if ( rc != ERROR &&
        tokenarray[i].token == T_FINAL &&
        ( ( opnd.kind == EXPR_CONST && opnd.hlvalue == 0 ) || /* magnitude <= 64 bits? */
         ( opnd.kind == EXPR_ADDR && opnd.indirect == FALSE &&
          opnd.sym != NULL &&
          //opnd.sym->state != SYM_EXTERNAL ) ) && /* SYM_SEG, SYM_GROUP are also not ok */
          opnd.sym->state == SYM_INTERNAL ) ) &&
        ( opnd.instr == EMPTY ) ) {

        if ( !sym ) {
            sym = SymCreate( name );
            sym->asmpass = Parse_Pass;
        } else if ( sym->state == SYM_UNDEFINED ) {
            sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
        } else if ( sym->state == SYM_EXTERNAL ) {
            sym_ext2int( sym );
        } else if ( cmpvalue ) {
            if ( opnd.kind == EXPR_CONST ) {
                /* for 64bit, it may be necessary to check 64bit value! */
                /* v2.08: always compare 64-bit values */
                //if ( sym->value != opnd.value ) {
                if ( sym->value != opnd.value || sym->value3264 != opnd.hvalue ) {
                    DebugMsg(("CreateConstant(%s), CONST value changed: old=%X, new=%X\n", name, sym->offset, opnd.value ));
                    EmitErr( SYMBOL_REDEFINITION, name );
                    return( NULL );
                }
            } else if ( opnd.kind == EXPR_ADDR ) {
                if ( ( sym->offset != ( opnd.sym->offset + opnd.value ) ) || ( sym->segment != opnd.sym->segment ) ) {
                    DebugMsg(("CreateConstant(%s), ADDR value changed: old=%X, new ofs+val=%X+%X\n", name, sym->offset, opnd.sym->offset, opnd.value));
                    EmitErr( SYMBOL_REDEFINITION, name );
                    return( NULL );
                }
            }
        }
        /* change from alias to number is ok if value (=offset) won't change!
         * memtype must not be checked!
         */
        //if ( opnd.kind == EXPR_CONST ) {
        //    if ( sym->mem_type != MT_ABS && sym->mem_type != MT_EMPTY ) {
        //        EmitErr( SYMBOL_REDEFINITION, name );
        //        return( NULL );
        //    }
        //}
        sym->variable = FALSE;
        SetValue( sym, &opnd );
        DebugMsg1(("CreateConstant(%s): memtype=%Xh value=%" I64X_SPEC " isproc=%u variable=%u\n", name, sym->mem_type, sym->value, sym->value3264, sym->isproc, sym->variable ));
        return( sym );
    }
    DebugMsg1(("CreateConstant(%s): calling SetTextMacro() [MI.Ofssize=%u]\n", name, ModuleInfo.Ofssize ));
    return ( SetTextMacro( tokenarray, sym, name, argbuffer ) );
}

/* EQU and '=' directives */

ret_code EquDirective( int i, struct asm_tok tokenarray[] )
/*********************************************************/
{
    struct asym *sym;

    if( tokenarray[0].token != T_ID ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
        return( ERROR );
    }
    if ( sym = ( ( tokenarray[i].dirtype == DRT_EQUALSGN ) ? CreateAssemblyTimeVariable( tokenarray ) : CreateConstant( tokenarray ) ) ) {
        if ( ModuleInfo.list == TRUE ) {
            LstWrite( LSTTYPE_EQUATE, 0, sym );
        }
        return( NOT_ERROR );
    }
    return( ERROR );
}

