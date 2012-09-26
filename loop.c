/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  Implements FOR/IRP, FORC/IRPC, REPEAT/REPT, WHILE
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "input.h"
#include "equate.h"
#include "expreval.h"
#include "tokenize.h"
#include "macro.h"
#include "listing.h"
#include "reswords.h"

#define USELOCALMAC 1 /* 1=create the macro onto the stack */

ret_code LoopDirective( int i, struct asm_tok tokenarray[] )
/**********************************************************/
{
    int directive = tokenarray[i].tokval;
    int arg_loc;
    int len;
    //bool first = TRUE;
    char *parmstring;
    char *ptr;
    struct dsym *macro;
    bool is_exitm;
    char first;
    struct expr opndx;
#if USELOCALMAC
    struct macro_info macinfo;
    struct dsym tmpmacro;
#endif
#ifdef DEBUG_OUT
    uint_32 count = 0;
#endif
    /* v2.08: use myalloca() to get space to store the argument */
    //char line[MAX_LINE_LEN];
    char buffer[4];

    DebugMsg1(("LoopDirective(%s) enter\n", GetResWName( directive, NULL ) ));

    i++; /* skip directive */
    if ( ModuleInfo.list == TRUE ) LstWriteSrcLine();

    switch ( directive ) {
    case T_WHILE:
        arg_loc = i;
        /* no break */
    case T_REPT:
    case T_REPEAT:
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) { /* syntax <REPEAT|WHILE 'A'> is valid! */
            /* the expression is "critical", that is, no forward
             * referenced symbols may be used here!
             */
            if ( opndx.sym && opndx.sym->state == SYM_UNDEFINED )
                EmitErr( SYMBOL_NOT_DEFINED, opndx.sym->name );
            else
                EmitError( CONSTANT_EXPECTED );
            opndx.kind = EXPR_CONST;
            opndx.value = 0;
        }
        if( tokenarray[i].token != T_FINAL ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
            return( ERROR );
        }
        break;
    default: /* FOR, FORC, IRP, IRPC */
        /* get the formal parameter and the argument list */
        /* the format parameter will become a macro parameter, so it can
         * be a simple T_ID, but also an instruction or something else.
         * v2.02: And it can begin with a '.'!
         */
        if( tokenarray[i].token == T_FINAL ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i-1].tokpos );
            return( ERROR );
        }
        /* v2.02: allow parameter name to begin with a '.' */
        //c = *tokenarray[i].string_ptr;
        //if( ( is_valid_id_char(c) == FALSE ) || ( isdigit(c) == TRUE ) ) {
        if( is_valid_id_first_char( *tokenarray[i].string_ptr ) == FALSE ) {
            DebugMsg(( "LoopDirective(FOR/FORC): token %s is not a valid parameter name\n", tokenarray[i].string_ptr ));
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
            return( ERROR );
        }
        arg_loc = i;
        i++;

        if( directive == T_FORC || directive == T_IRPC ) {
            if( tokenarray[i].token != T_COMMA ) {
                EmitError( EXPECTING_COMMA );
                return( ERROR );
            }
            i++;
            /* FORC/IRPC accepts anything as "argument list", even nothing! */
            if( tokenarray[i].token == T_STRING && tokenarray[i].string_delim == '<' ) {
                parmstring = myalloca( tokenarray[i].stringlen + 1 );
                //GetLiteralValue( parmstring, tokenarray[i].string_ptr );
                memcpy( parmstring, tokenarray[i].string_ptr, tokenarray[i].stringlen + 1 );
                /* v2.02: if there's additional stuff behind the <> literal,
                 * it's an error!
                 */
                if ( tokenarray[i+1].token != T_FINAL )
                    EmitErr( SYNTAX_ERROR_EX, tokenarray[i+1].tokpos );
            } else {
                char *ptr2;
                ptr = tokenarray[i].tokpos;
                ptr2 = ptr;
                /* this is what Masm does: use the string until a space
                 * is detected. Anything beyond the space is ignored.
                 */
                while ( *ptr2 && ( isspace( *ptr2 ) == FALSE ) )
                    ptr2++;
                len = ptr2 - ptr;
                parmstring = myalloca( len + 1 );
                memcpy( parmstring, ptr, len );
                *(parmstring+len) = NULLC;
            }
        } else {
            /* for FOR/IRP, skip everything between the name and the comma!
             * these items will be stored as (first) macro parameter.
             * for example, valid syntax is:
             * FOR xxx,<a, ...>
             * FOR xxx:REQ,<a, ...>
             */
            while ( tokenarray[i].token != T_FINAL && tokenarray[i].token != T_COMMA )
                i++;
            if( tokenarray[i].token != T_COMMA ) {
                EmitError( EXPECTING_COMMA );
                return( ERROR );
            }
            i++;
            /* FOR/IRP accepts a literal enclosed in <> only */
            if( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
                return( ERROR );
            }
            /* v2.03: also ensure that the literal is the last item */
            if( tokenarray[i+1].token != T_FINAL ) {
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i+1].tokpos );
                return( ERROR );
            }
            /* v2.08: use myalloca() instead of a fixed-length buffer.
             * the loop directives are often nested, they call RunMacro()
             * and hence should be careful with stack usage because of JWASMR!
             */
            parmstring = myalloca( tokenarray[i].stringlen + 1 );
            /* v2.0: use GetLiteralValue() instead of memcpy!!! */
            //memcpy( line, tokenarray[i].string_ptr, tokenarray[i].stringlen + 1 );
            GetLiteralValue( parmstring, tokenarray[i].string_ptr );
            DebugMsg1(("LoopDirective(FOR): param string >%s<\n", parmstring));
        }
        /* to run StoreMacro(), tokenarray must be setup correctly. */
        /* clear contents beginning with the comma! */
        i--;
        tokenarray[i].token = T_FINAL;
        Token_Count = i;
        i = arg_loc;
    }

    /* now make a temporary macro */
#if USELOCALMAC
    macro = &tmpmacro;
    memset( &tmpmacro, 0, sizeof(tmpmacro) );
    tmpmacro.sym.name = "";
    tmpmacro.e.macroinfo = &macinfo;
    memset( &macinfo, 0, sizeof(macinfo) );
#else
    macro = CreateMacro( "" ); /* won't work currently, since a name is needed */
#endif
    macro->e.macroinfo->srcfile = get_curr_srcfile();

#if DEBUG_OUT
    if ( directive ==  T_WHILE )
        tmpmacro.sym.name = "<WHILE>";
    else if ( directive == T_REPEAT || directive == T_REPT )
        tmpmacro.sym.name = "<REPT>";
    else if ( directive == T_FORC || directive == T_IRPC )
        tmpmacro.sym.name = "<FORC>";
    else
        tmpmacro.sym.name = "<FOR>";
#endif

    DebugMsg1(("LoopDirective(%s): calling StoreMacro\n", GetResWName( directive, NULL )));
    if( StoreMacro( macro, i, tokenarray, TRUE ) == ERROR ) {
#if USELOCALMAC
        ReleaseMacroData( macro );
#else
        SymFree( (struct asym *)macro );
#endif
        return( ERROR );
    }
    /* EXITM is allowed inside a macro loop.
     * This doesn't make the loop a macro function, reset the bit!
     */
    macro->sym.isfunc = FALSE;

    /* now run the just created macro in a loop */

    /* don't run the macro if there are no lines (macroinfo->data == NULL)!
     * this isn't exactly what Masm does; an empty 'WHILE 1'
     * will loop "forever" in Masm,
     */
    if ( macro->e.macroinfo->data ) /* added in v2.01 */
    switch ( directive ) {
    case T_REPEAT:
    case T_REPT:
        /* negative repeat counts are accepted and are treated like 0 */
        for ( ; macro->sym.value < opndx.value; macro->sym.value++ ) {
            //RunMacro( macro, "", NULL, FALSE, &is_exitm );
            RunMacro( macro, Token_Count, tokenarray, NULL, -1, &is_exitm );
            if ( is_exitm )
                break;
            DebugMsg1(("LoopDirective REPT: iteration=%" FU32 "\n", ++count ));
            //first = FALSE;
        }
        break;
    case T_WHILE:
        while ( opndx.kind == EXPR_CONST && opndx.value != 0 ) {
            DebugMsg1(("LoopDirective WHILE: cnt=%u\n", count++ ));
            //RunMacro( macro, "", NULL, FALSE, &is_exitm );
            RunMacro( macro, Token_Count, tokenarray, NULL, -1, &is_exitm );
            if ( is_exitm )
                break;
            i = arg_loc;
            if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
                break;
            macro->sym.value++;
        }
        break;
    case T_FORC:
    case T_IRPC:
        i = Token_Count + 1;
        tokenarray[i].token = T_STRING;
        tokenarray[i].tokpos = buffer;
        tokenarray[i].string_ptr = buffer;
        tokenarray[i+1].token = T_FINAL;
        buffer[2] = NULLC;
        Token_Count = i + 1;
        for( ptr = parmstring; *ptr; macro->sym.value++ ) {
#if 0
            /* v1.96: '"' and '\'' added, '!' removed */
            //if (*ptr == '<' || *ptr == '>' || *ptr == '%' || *ptr == '"' || *ptr == '\'')
            if (*ptr == '<' || *ptr == '>' )
                *ptr2++ = '!';
            else if (*ptr == '!' ) { /* v1.96: handling of ! changed */
                *ptr2++ = *ptr++;
                if ( *ptr == NULLC )
                    ptr = "\t";  /* make sure there's something != NULLC */
            }
            *ptr2++ = *ptr++;
            *ptr2 = NULLC;
#else
            tokenarray[i].string_delim = NULLC; /* use <>-literal (undelimited string has problems with spaces) */
            if ( *ptr == '!' ) {
                buffer[0] = *ptr++;
                buffer[1] = *ptr++;
                tokenarray[i].stringlen = 2;
                tokenarray[i+1].tokpos = buffer+2;
            } else if ( isspace( *ptr ) ) {
                tokenarray[i].string_delim = '<';
                buffer[0] = *ptr++;
                tokenarray[i].stringlen = 1;
                tokenarray[i+1].tokpos = buffer+1;
            } else {
                buffer[0] = *ptr++;
                tokenarray[i].stringlen = 1;
                tokenarray[i+1].tokpos = buffer+1;
                buffer[1] = NULLC;
            }
#endif
            RunMacro( macro, i, tokenarray, NULL, -1, &is_exitm );
            if ( is_exitm )
                break;
            //first = FALSE;
            DebugMsg1(("LoopDirective FORC: call RunMacro(), cnt=%" FU32 ", param=>%s<\n", count++, buffer ));
        }
        break;
    default: /* T_FOR, T_IRP */
        i = Token_Count + 1;
        Token_Count = Tokenize( parmstring, i, tokenarray, TOK_RESCAN | TOK_NOCURLBRACES );
        /* a FOR/IRP parameter can be a macro function call */
        /* that's why the macro calls must be run synchronously */
        /* v2.05: reset an optional VARARG attribute for the macro
         * parameter.
         * take care of a trailing comma, this is to make another
         * RunMacro() call with a "blank" argument.
         */
        macro->sym.mac_vararg = FALSE;
        for( first = TRUE; i < Token_Count; macro->sym.value++ ) {
            int j;
            int cnt;
            int old_token;
            if ( first == FALSE )
                i++; /* skip comma */
            for ( j = i, cnt=0; j < Token_Count; j++ ) {
                if ( tokenarray[j].token == T_OP_BRACKET )
                    cnt++;
                else if ( tokenarray[j].token == T_CL_BRACKET )
                    cnt--;
                if ( cnt == 0 && tokenarray[j].token == T_COMMA )
                    break;
            }
            old_token = tokenarray[j].token;
            tokenarray[j].token = T_FINAL;
            if ( j > i ) {
                while ( isspace( *( tokenarray[j].tokpos - 1 ) ) )
                    tokenarray[j].tokpos--;
                *tokenarray[j].tokpos = NULLC;
            }
            DebugMsg1(("LoopDirective FOR: cnt=%" FU32 ", calling RunMacro( param=>%s< )\n", count++, tokenarray[i].tokpos ));
            //len = RunMacro( macro, ptr, NULL, FALSE, &is_exitm );
            i = RunMacro( macro, i, tokenarray, NULL, -1, &is_exitm );
            tokenarray[j].token = old_token;
            if ( i < 0 || is_exitm )
                break;
            first = FALSE;
            //i++;
        }
    }
#if USELOCALMAC
    ReleaseMacroData( macro );
#else
    SymFree( (struct asym *)macro );
#endif
    DebugMsg1(("LoopDirective(%s) exit\n", GetResWName( directive, NULL ) ));
    return( NOT_ERROR );
}
