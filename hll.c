/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  implements hll directives:
*               .IF, .WHILE, .REPEAT, .ELSE, .ELSEIF, .ENDIF,
*               .ENDW, .UNTIL, .UNTILCXZ, .BREAK, .CONTINUE.
*               also handles C operators:
*               ==, !=, >=, <=, >, <, &&, ||, &, !,
*               and flags:
*               ZERO?, CARRY?, SIGN?, PARITY?, OVERFLOW?
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "equate.h"
#include "labels.h"
#include "input.h"
#include "expreval.h"
#include "types.h"
#include "hll.h"
#include "segment.h"
#include "listing.h"

#define LABELSIZE 8

#define LABELSGLOBAL 0 /* make the generated labels global */

#if LABELSGLOBAL
#define LABELQUAL "::"
#else
#define LABELQUAL ":"
#endif

#define LSTART 0      /* loop start label         */
#define LTEST  1      /* continue, test for exit  */
#define LEXIT  2      /* loop exit label          */

#ifdef DEBUG_OUT
#define EOLCHAR '^'  /* this allows better displays */
#define EOLSTR  "^"
#else
#define EOLCHAR '\n' /* line termination char in generated source */
#define EOLSTR  "\n"
#endif

enum hll_cmd {
    HLL_IF,
    HLL_WHILE,
    HLL_REPEAT,
    HLL_BREAK  /* .IF behind .BREAK or .CONTINUE */
};

enum hll_flags {
    HLLF_ELSEOCCURED = 0x01,
};


/* item for .IF, .WHILE, .REPEAT, ... */
struct hll_item {
    struct hll_item     *next;
    uint_32             labels[3];
    char                *condlines;     /* for .WHILE: lines to add after test */
#ifdef __WATCOMC__
    enum hll_cmd        cmd;            /* start cmd (IF, WHILE, REPEAT) */
    enum hll_flags      flags;          /* v2.08: added */
#else
    char                cmd;
    char                flags;
#endif
};

/* v2.08: struct added */
struct hll_opnd {
    char    *lastjmp;
    uint_32 lasttruelabel; /* v2.08: new member */
};

static ret_code GetExpression( struct hll_item *hll, int *i, struct asm_tok[], int ilabel, bool is_true, char *buffer, struct hll_opnd *, struct expr *opndx );

/* c binary ops */

enum c_bop {
    COP_NONE,
    COP_EQ,   /* == */
    COP_NE,   /* != */
    COP_GT,   /* >  */
    COP_LT,   /* <  */
    COP_GE,   /* >= */
    COP_LE,   /* <= */
    COP_AND,  /* && */
    COP_OR,   /* || */
    COP_ANDB, /* &  */
    COP_NEG,  /* !  */
    COP_ZERO, /* ZERO?   not really a valid C operator */
    COP_CARRY,/* CARRY?  not really a valid C operator */
    COP_SIGN, /* SIGN?   not really a valid C operator */
    COP_PARITY,  /* PARITY?   not really a valid C operator */
    COP_OVERFLOW /* OVERFLOW? not really a valid C operator */
};

/* must be in same order as in enum c_bop COP_ZERO .. COP_OVERFLOW */
static const char flaginstr[] = {
    'z',  'c',  's',  'p',  'o'
};

/* in Masm, there's a nesting level limit of 20. In JWasm, there's
 * currently no limit.
 */
static struct hll_item     *HllStack; /* for .WHILE, .IF, .REPEAT */
/* v2.06: <struct hll>-items made reuseable */
static struct hll_item     *HllFree; /* stack of free <struct hll>-items */

#ifdef DEBUG_OUT
static int cntAlloc = 0;
static int cntReused = 0;
static int cntCond = 0;
static int cntCondBytes = 0;
static int evallvl;
#endif

static uint_32 GetHllLabel( void )
/********************************/
{
    return ( ++ModuleInfo.hll_label );
}

/* get a C binary operator from the token stream.
 * there is a problem with the '<' because it is a "string delimiter"
 * which Tokenize() usually is to remove.
 * There has been a hack implemented in Tokenize() so that it won't touch the
 * '<' if .IF, .ELSEIF, .WHILE, .UNTIL, .UNTILCXZ or .BREAK/.CONTINUE has been
 * detected
 */
static enum c_bop GetCOp( struct asm_tok *item )
/**********************************************/
{
    int size;
    enum c_bop rc;
    char *p = item->string_ptr;

    size = ( item->token == T_STRING ? item->stringlen : 0 );

    if ( size == 2 ) {
        if ( *p == '=' && *(p+1) == '=' )
            rc = COP_EQ;
        else if ( *p == '!' && *(p+1) == '=' )
            rc = COP_NE;
        else if ( *p == '>' && *(p+1) == '=' )
            rc = COP_GE;
        else if ( *p == '<' && *(p+1) == '=' )
            rc = COP_LE;
        else if ( *p == '&' && *(p+1) == '&' )
            rc = COP_AND;
        else if ( *p == '|' && *(p+1) == '|' )
            rc = COP_OR;
        else
            return( COP_NONE );
    } else if ( size == 1 ) {
        if ( *p == '>' )
            rc = COP_GT;
        else if ( *p == '<' )
            rc = COP_LT;
        else if ( *p == '&' )
            rc = COP_ANDB;
        else if ( *p == '!' )
            rc = COP_NEG;
        else
            return( COP_NONE );
    } else {
        if ( item->token != T_ID )
            return( COP_NONE );
        /* a valid "flag" string must end with a question mark */
        size = strlen( p );
        if ( *(p+size-1) != '?' )
            return( COP_NONE );
        if ( size == 5 && ( 0 == _memicmp( p, "ZERO", 4 ) ) )
            rc = COP_ZERO;
        else if ( size == 6 && ( 0 == _memicmp( p, "CARRY", 5 ) ) )
            rc = COP_CARRY;
        else if ( size == 5 && ( 0 == _memicmp( p, "SIGN", 4 ) ) )
            rc = COP_SIGN;
        else if ( size == 7 && ( 0 == _memicmp( p, "PARITY", 6 ) ) )
            rc = COP_PARITY;
        else if ( size == 9 && ( 0 == _memicmp( p, "OVERFLOW", 8 ) ) )
            rc = COP_OVERFLOW;
        else
            return( COP_NONE );
    }
    return( rc );
}

/* render an instruction */

static char *RenderInstr( char *p, char *instr, struct expr *op, int start1, int end1, int start2, int end2, struct asm_tok tokenarray[] )
/****************************************************************************************************************************************/
{
    int i;
#ifdef DEBUG_OUT
    char *old = p;
#endif
    i = strlen( instr );
    /* copy the instruction */
    memcpy( p, instr, i );
    p += i;
    /* copy the first operand's tokens */
    /* v2.06: use tokpos instead of string_ptr */
    //for ( ; start1 < end1; start1++ ) {
    //    *p++ = ' ';
    //    strcpy( p, tokenarray[start1].string_ptr );
    //    p += strlen( p );
    //}
    *p++ = ' ';
    i = tokenarray[end1].tokpos - tokenarray[start1].tokpos;
    memcpy( p, tokenarray[start1].tokpos, i );
    p += i;
    if ( start2 != EMPTY ) {
        *p++ = ',';
        /* copy the second operand's tokens */
        /* v2.06: use tokpos instead of string_ptr */
        //for ( ; start2 < end2; start2++ ) {
        //    *p++ = ' ';
        //    strcpy( p, tokenarray[start2].string_ptr );
        //    p += strlen( p );
        //}
        *p++ = ' ';
        i = tokenarray[end2].tokpos - tokenarray[start2].tokpos;
        memcpy( p, tokenarray[start2].tokpos, i );
        p += i;
    } else if ( end2 != EMPTY ) {
        p += sprintf( p, ", %d", end2 );
    }
    *p++ = EOLCHAR;
    *p = NULLC;
    DebugMsg1(("%u RenderInstr(%s)=>%s<\n", evallvl, instr, old ));
    return( p );
}

/* render a Jcc instruction */

static char *RenderJcc( char *p, char cc, int neg, char *label )
/**************************************************************/
{
#ifdef DEBUG_OUT
    char *old = p;
#endif
    /* create the jump opcode: j[n]cc */
    *p++ = 'j';
    if ( neg )
        *p++ = 'n';
    *p++ = cc;
    if ( neg == FALSE )
        *p++ = ' '; /* make sure there's room for the inverse jmp */

    *p++ = ' ';
    strcpy( p, label );
    p += strlen( p );
    *p++ = EOLCHAR;
    *p = NULLC;
    DebugMsg1(("%u RenderJcc()=>%s<\n", evallvl, old ));
    return( p );
}

/* a "token" in a C expression actually is a set of ASM tokens */

static ret_code HllGetToken( struct hll_item *hll, int *i, struct asm_tok tokenarray[], bool is_true, struct expr *opndx )
/************************************************************************************************************************/
{
    int end_tok;

    /* scan for the next C operator in the token array.
     because the ASM evaluator will report an error if such a thing
     is found */
    for ( end_tok = *i; end_tok < Token_Count; end_tok++ ) {
        if ( ( GetCOp( &tokenarray[end_tok] ) ) != COP_NONE )
            break;
    }
    opndx->kind = EXPR_EMPTY;
    if ( end_tok > *i )
        if ( ERROR == EvalOperand( i, tokenarray, end_tok, opndx, 0 ) )
            return( ERROR );
    return( NOT_ERROR );
}

static char *GetLabelStr( int_32 label, char *buff )
/**************************************************/
{
    sprintf( buff, "@C%04X", label );
    return( buff );
}

static uint_32 GetLabel( struct hll_item *hll, int index )
/********************************************************/
{
    if ( hll->labels[index] > 0 )
        return( hll->labels[index] );
    /* the test label may not exist due to optimization */
    if ( index == LTEST )
        return( hll->labels[LSTART] );
    return( 0 ); /* shouldn't happen */
}

/* a "simple" expression is
 * 1. two tokens, coupled with a <cmp> operator: == != >= <= > <
 * 2. two tokens, coupled with a "&" operator
 * 3. unary operator "!" + one token
 * 4. one token (short form for "<token> != 0")
 */
static ret_code GetSimpleExpression( struct hll_item *hll, int *i, struct asm_tok tokenarray[], int ilabel, bool is_true, char *buffer, struct hll_opnd *hllop, struct expr *opndx )
/**********************************************************************************************************************************************************************/
{
    //struct expr opndx;
    struct expr op2;
    enum c_bop op;
    //int size;
    char instr;
    int op1_pos;
    int op1_end;
    int op2_pos;
    int op2_end;
    char label[16];
    char *p;
    bool issigned;
    bool neg;

    DebugMsg1(("%u GetSimpleExpression(>%.32s< buf=>%s<) enter\n", evallvl, tokenarray[*i].tokpos, buffer ));

    while ( tokenarray[*i].string_ptr[0] == '!' && tokenarray[*i].string_ptr[1] == '\0' ) {
        (*i)++; //GetCOp( i );
        is_true = 1 - is_true;
    }

    op1_pos = *i;
    /* the problem with '()' is that is might enclose just a standard Masm
     * expression or a "hll" expression. The first case is to be handled 
     * entirely by the expression evaluator, while the latter case is to be
     * handled HERE!
     */
    if ( tokenarray[*i].token == T_OP_BRACKET ) {
        int brcnt;
        int j;
        for ( brcnt = 1, j = *i + 1; tokenarray[j].token != T_FINAL; j++ ) {
            if ( tokenarray[j].token == T_OP_BRACKET )
                brcnt++;
            else if ( tokenarray[j].token == T_CL_BRACKET ) {
                brcnt--;
                if ( brcnt == 0 ) /* a standard Masm expression? */
                    break;
            } else if ( ( GetCOp( &tokenarray[j] )) != COP_NONE )
                break;
        }
        if ( brcnt ) {
            (*i)++;
            DebugMsg1(("%u GetSimpleExpression: calling GetExpression, i=%u\n", evallvl, *i));
            if ( ERROR == GetExpression( hll, i, tokenarray, ilabel, is_true, buffer, hllop, opndx ) )
                return( ERROR );

            if ( tokenarray[*i].token != T_CL_BRACKET ) {
                //if (( tokenarray[*i].token == T_FINAL ) || ( tokenarray[*i].token == T_CL_BRACKET ))
                DebugMsg(( "GetSimpleExpression: expected ')', found: %s\n", tokenarray[*i].string_ptr ));
                EmitError( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE );
                return( ERROR );
            }
            (*i)++;
            return( NOT_ERROR );
        }
    }

    if ( ERROR == HllGetToken( hll, i, tokenarray, is_true, opndx ) )
        return ( ERROR );

    op1_end = *i;

    if ( ( op = GetCOp( &tokenarray[*i] ) ) != COP_NONE )
        (*i)++;

    if ( op == COP_AND || op == COP_OR ) {
        *i = op1_end;
        if ( opndx->kind == EXPR_EMPTY )
            return( NOT_ERROR );
        op = COP_NONE;
    }

    GetLabelStr( GetLabel( hll, ilabel ), label );

    DebugMsg1(("%u GetSimpleExpression: EvalOperand ok, kind=%X, i=%u [%s]\n", evallvl, opndx->kind, *i, tokenarray[*i].tokpos ));

    if ( opndx->kind == EXPR_EMPTY ) {
        /* no valid ASM expression detected. check for some special ops */
        /* COP_ZERO, COP_CARRY, COP_SIGN, COP_PARITY, COP_OVERFLOW */
        if ( op >= COP_ZERO ) {
            p = buffer;
            hllop->lastjmp = p;
            RenderJcc( p, flaginstr[ op - COP_ZERO ], !is_true, label );
            return( NOT_ERROR );
        }
        if ( hll->condlines )
            return( NOT_ERROR );

        EmitError( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE );
        return( NOT_ERROR );
    }

    if ( ( opndx->kind != EXPR_CONST ) && ( opndx->kind != EXPR_ADDR ) && ( opndx->kind != EXPR_REG ) )
        return( ERROR );

    op2_pos = *i;

    if ( op != COP_NONE ) {
        if ( ERROR == HllGetToken( hll, i, tokenarray, is_true, &op2 ) ) {
            return( ERROR );
        }
        DebugMsg1(("%u GetSimpleExpression: EvalOperand 2 ok, type=%X, i=%u [%s]\n", evallvl, op2.type, *i, tokenarray[*i].tokpos));
        if ( op2.kind != EXPR_CONST && op2.kind != EXPR_ADDR && op2.kind != EXPR_REG ) {
            DebugMsg(("GetSimpleExpression: syntax error, op2.kind=%u\n", op2.kind ));
            EmitError( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE );
            return( ERROR );
        }
    }
    op2_end = *i;

    /* now generate ASM code for expression */

    buffer[0] = NULLC;
    switch ( op ) {
    case COP_EQ:
    case COP_NE:
    case COP_GT:
    case COP_LT:
    case COP_GE:
    case COP_LE:
        /* optimisation: generate 'or EAX,EAX' instead of 'cmp EAX,0' */
        if ( Options.masm_compat_gencode &&
            ( op == COP_EQ || op == COP_NE ) &&
            opndx->kind == EXPR_REG &&
            opndx->indirect == FALSE &&
            op2.kind == EXPR_CONST &&
            op2.value == 0 ) {
            p = RenderInstr( buffer, "or", opndx, op1_pos, op1_end, op1_pos, op1_end, tokenarray );
        } else {
            p = RenderInstr( buffer, "cmp", opndx, op1_pos, op1_end, op2_pos, op2_end, tokenarray );
        }

        hllop->lastjmp = p;

        if ( IS_SIGNED( opndx->mem_type ) || IS_SIGNED( op2.mem_type ) )
            issigned = TRUE;
        else
            issigned = FALSE;

        switch ( op ) {
        case COP_EQ:
            instr = 'z';
            neg = !is_true;
            break;
        case COP_NE:
            instr = 'z';
            neg = is_true;
            break;
        case COP_GT:
            instr = ( issigned ? 'g' : 'a' );
            neg = !is_true;
            break;
        case COP_LT:
            instr = ( issigned ? 'l' : 'b' );
            neg = !is_true;
            break;
        case COP_GE:
            instr = ( issigned ? 'l' : 'b' );
            neg = is_true;
            break;
        case COP_LE:
            instr = ( issigned ? 'g' : 'a' );
            neg = is_true;
            break;
        }
        RenderJcc( p, instr, neg, label );
        break;
    case COP_ANDB:
        p = RenderInstr( buffer, "test", opndx, op1_pos, op1_end, op2_pos, op2_end, tokenarray );
        hllop->lastjmp = p;
        RenderJcc( p, 'e', is_true, label );
        break;
    case COP_NONE:
        switch ( opndx->kind ) {
        case EXPR_REG:
            if ( opndx->indirect == FALSE ) {
                p = RenderInstr( buffer, "and", opndx, op1_pos, op1_end, op1_pos, op1_end, tokenarray );
                hllop->lastjmp = p;
                RenderJcc( p, 'z', is_true, label );
                break;
            }
            /* no break */
        case EXPR_ADDR:
            p = RenderInstr( buffer, "cmp", opndx, op1_pos, op1_end, EMPTY, 0, tokenarray );
            hllop->lastjmp = p;
            RenderJcc( p, 'z', is_true, label );
            break;
        case EXPR_CONST:
#if 0
            /* v2.05: string constant is allowed! */
            if ( opndx->string != NULL ) {
                EmitError( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE );
                return ( ERROR );
            }
#endif
            hllop->lastjmp = buffer;
            if ( ( is_true == TRUE && opndx->value ) ||
                ( is_true == FALSE && opndx->value == 0 ) )
                sprintf( buffer, "jmp %s" EOLSTR, label );
            else
                strcpy( buffer, " " EOLSTR ); /* make sure there is a char */
            break;
        }
    }
    return( NOT_ERROR );
}

static void InvertJmp( char *p )
/******************************/
{
    if ( *p == 'e' || *p == 'z' || *p == 'c' || *p == 's' || *p == 'p' || *p == 'o' ) {
        *(p+1) = *p;
        *p = 'n';
        return;
    } else if ( *p == 'n' ) {
        *p = *(p+1);
        *(p+1) = ' ';
        return;
    } else if ( *p == 'a' ) {
        *p++ = 'b';
    } else if ( *p == 'b' ) {
        *p++ = 'a';
    } else if ( *p == 'g' ) {
        *p++ = 'l';
    } else if ( *p == 'l' ) {
        *p++ = 'g';
    } else
        return;

    if ( *p == 'e' )
        *p = ' ';
    else
        *p = 'e';
    return;
}

/* todo: if more than 0xFFFF labels are needed,
 * it may happen that length of nlabel > length of olabel!
 * then the simple memcpy() below won't work!
 */

static void ReplaceLabel( char *p, uint_32 olabel, uint_32 nlabel )
/*****************************************************************/
{
    char oldlbl[16];
    char newlbl[16];
    int i;

    GetLabelStr( olabel, oldlbl );
    GetLabelStr( nlabel, newlbl );

    i = strlen( newlbl );

    DebugMsg1(("%u ReplaceLabel(%s->%s, >%s<)\n", evallvl, oldlbl, newlbl, p ));
    while ( p = strstr( p, oldlbl ) ) {
        memcpy( p, newlbl, i );
        p = p  + i;
    }
}

/* operator &&, which has the second lowest precedence, is handled here */

static ret_code GetAndExpression( struct hll_item *hll, int *i, struct asm_tok tokenarray[], int ilabel, bool is_true, char *buffer, struct hll_opnd *hllop, struct expr *opndx )
/***********************************************************************************************************************************************************************/
{
    enum c_bop op;
    char *ptr = buffer;
    uint_32 truelabel = 0;
    //char buff[16];
    //char *nlabel;
    //char *olabel;

    DebugMsg1(("%u GetAndExpression(>%.32s< buf=>%s<) enter\n", evallvl, tokenarray[*i].tokpos, buffer ));

    while (1) {
        ptr += strlen( ptr );
        if ( ERROR == GetSimpleExpression( hll, i, tokenarray, ilabel, is_true, ptr, hllop, opndx ) )
            return( ERROR );
        op = GetCOp( &tokenarray[*i] );
        if ( op != COP_AND )
            break;
        (*i)++;
        DebugMsg1(("%u GetAndExpression: && found, is_true=%u, lastjmp=%s\n", evallvl, is_true, hllop->lastjmp ? hllop->lastjmp : "NULL" ));
        /* v2.02: query is_true var instead of cmd field!
         * this is important if the '!' operator was used.
         */
        //if ( hll->cmd == HLL_WHILE || hll->cmd == HLL_BREAK ) {
        if ( is_true ) {
            /* todo: please describe what's done here and why! */
            if ( hllop->lastjmp ) {
                char *p = hllop->lastjmp;
                InvertJmp( p+1 );         /* step 1 */
                if ( truelabel == 0 )     /* step 2 */
                    truelabel = GetHllLabel();
                DebugMsg1(("%u GetAndExpression: jmp inverted >%s<\n", evallvl, hllop->lastjmp ));
                ReplaceLabel( buffer, GetLabel( hll, ilabel ), truelabel );
                hllop->lastjmp = NULL;
            }
        }
        hllop->lasttruelabel = 0; /* v2.08 */
    };

    if ( truelabel > 0 ) {
        ptr += strlen( ptr );
        GetLabelStr( truelabel, ptr );
        strcat( ptr, LABELQUAL );
        strcat( ptr, EOLSTR );
        DebugMsg1(("%u GetAndExpression: label added >%s<\n", evallvl, ptr ));
        hllop->lastjmp = NULL;
    }
    return( NOT_ERROR );
}

/* operator ||, which has the lowest precedence, is handled here */

static ret_code GetExpression( struct hll_item *hll, int *i, struct asm_tok tokenarray[], int ilabel, bool is_true, char *buffer, struct hll_opnd *hllop, struct expr *opndx )
/********************************************************************************************************************************************************************/
{
    enum c_bop op;
    char *ptr = buffer;
    uint_32 truelabel = 0;

    DebugMsg1(("%u GetExpression(>%.32s< buf=>%s<) enter\n", ++evallvl, tokenarray[*i].tokpos, buffer ));

    /* v2.08: structure changed from for(;;) to while() to increase
     * readability and - optionally - handle the second operand differently
     * than the first.
     */

    if ( ERROR == GetAndExpression( hll, i, tokenarray, ilabel, is_true, ptr, hllop, opndx ) ) {
        DebugMsg1(("%u GetExpression exit, error\n", evallvl-- ));
        return( ERROR );
    }
    while ( COP_OR == (op = GetCOp( &tokenarray[*i] ))) {

        uint_32 nlabel;
        uint_32 olabel;
        char buff[16];

        /* the generated code of last simple expression has to be modified
         1. the last jump must be inverted
         2. a "is_true" label must be created (it's used to jump "behind" the expr)
         3. create a new label
         4. the current "false" label must be generated

         if it is a .REPEAT, step 4 is slightly more difficult, since the "false"
         label is already "gone":
         4a. create a new label
         4b. replace the "false" label in the generated code by the new label
         */

        (*i)++;
        DebugMsg1(("%u GetExpression: || found, is_true=%u, lastjmp=%s\n", evallvl, is_true, hllop->lastjmp ? hllop->lastjmp : "NULL" ));

        /* v2.02: query is_true var instead of cmd field!
         * this is important if the '!' operator was used.
         */
        //if (*lastjmp && ( hll->cmd != HLL_BREAK ) && ( hll->cmd != HLL_WHILE ) ) {
        if ( hllop->lastjmp && is_true == FALSE ) {
            char *p = hllop->lastjmp;
            InvertJmp(p+1);        /* step 1 */
            p += 4;                /* skip jcc */
            if ( truelabel == 0 )  /* step 2 */
                truelabel = GetHllLabel();
            GetLabelStr( truelabel, p );
            strcat( p, EOLSTR );
            /* v2.08: if-block added */
            if ( hllop->lasttruelabel )
                ReplaceLabel( ptr, hllop->lasttruelabel, truelabel );
            DebugMsg1(("%u GetExpression: jmp inverted, dest changed >%s<\n", evallvl, ptr ));
            hllop->lastjmp = NULL;

            nlabel = GetHllLabel();  /* step 3 */
            olabel = GetLabel( hll, ilabel );
            if ( hll->cmd == HLL_REPEAT ) {
                ReplaceLabel( buffer, olabel, nlabel );
                sprintf( ptr + strlen( ptr ), "%s" LABELQUAL EOLSTR, GetLabelStr( nlabel, buff ) );
            } else {
                sprintf( ptr + strlen( ptr ), "%s" LABELQUAL EOLSTR, GetLabelStr( olabel, buff ) );
                ReplaceLabel( buffer, olabel, nlabel );
            }
            DebugMsg1(("%u GetExpression: dest changed, label added>%s<\n", evallvl, ptr ));
        }
        ptr += strlen( ptr );
        hllop->lasttruelabel = 0; /* v2.08 */
        if ( ERROR == GetAndExpression( hll, i, tokenarray, ilabel, is_true, ptr, hllop, opndx ) ) {
            DebugMsg1(("%u GetExpression exit, error\n", evallvl-- ));
            return( ERROR );
        }
    }
    if ( truelabel > 0 ) {
        /* v2.08: this is needed, but ober-hackish. to be improved... */
        if ( hllop->lastjmp && hllop->lasttruelabel ) {
            DebugMsg1(("%u GetExpression: suppressed ReplaceLabel %u -> %u, lastjmp=%s\n", evallvl, hllop->lasttruelabel, truelabel, hllop->lastjmp ));
            ReplaceLabel( ptr, hllop->lasttruelabel, truelabel );
            *(strchr(hllop->lastjmp, EOLCHAR ) + 1 ) = NULLC;
        }
        ptr += strlen( ptr );
        GetLabelStr( truelabel, ptr );
        strcat( ptr, LABELQUAL );
        strcat( ptr, EOLSTR );
        DebugMsg1(("%u GetExpression: label added >%s<\n", evallvl, ptr ));
        hllop->lasttruelabel = truelabel; /* v2.08 */
    }
    DebugMsg1(("%u GetExpression exit\n", evallvl-- ));
    return( NOT_ERROR );
}

/* update hll->condlines */

static ret_code WriteExprSrc( struct hll_item *hll, char *buffer )
/****************************************************************/
{
    int size;
    int size2;
    char *p;

    size = strlen( buffer ) + 1;
    if ( hll->condlines ) {
        size2 = strlen( hll->condlines ) + 1;
        size += size2;
    }
    p = LclAlloc( size );
#ifdef DEBUG_OUT
    cntCond++;
    cntCondBytes += size;
#endif
    if ( hll->condlines ) {
        memcpy( p, hll->condlines, size2 );
        p += size2;
        strchr( p, EOLCHAR );
        strcpy( p + 1, buffer );
    } else
        strcpy( p, buffer );

    LclFree( hll->condlines );
    hll->condlines = p;
    return( NOT_ERROR );
}

/*
 * evaluate the C like boolean expression found in HLL structs
 * like .IF, .ELSEIF, .WHILE, .UNTIL and .UNTILCXZ
 * might return multiple lines (strings separated by EOLCHAR)
 * - i = index for tokenarray[] where expression starts. Is restricted
 *       to one source line (till T_FINAL)
 * - label: label to jump to if expression is <is_true>!
 * is_true:
 *   .IF:       FALSE
 *   .ELSEIF:   FALSE
 *   .WHILE:    TRUE
 *   .UNTIL:    FALSE
 *   .UNTILCXZ: FALSE
 *   .BREAK .IF:TRUE
 *   .CONT .IF: TRUE
 */


static ret_code EvaluateHllExpression( struct hll_item *hll, int *i, struct asm_tok tokenarray[], int ilabel, bool is_true )
/**************************************************************************************************************************/
{
    struct hll_opnd hllop = {NULL,0};
    struct expr opndx;
    char buffer[MAX_LINE_LEN*2];

    DebugMsg1(("EvaluateHllExpression enter\n"));

    buffer[0] = NULLC;
    if ( ERROR == GetExpression( hll, i, tokenarray, ilabel, is_true, buffer, &hllop, &opndx ) )
        return( ERROR );
    if ( buffer[0] ) {
        WriteExprSrc( hll, buffer );
    }
    if ( hll->condlines != NULL && *hll->condlines == EOLCHAR ) {
        EmitError( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE );
        return( ERROR );
    }
    return( NOT_ERROR );
}

/* write ASM test lines */

static ret_code HllPushTestLines( struct hll_item *hll )
/******************************************************/
{
    char *p = hll->condlines;
    char *p2;
    char buffer[MAX_LINE_LEN];

    DebugMsg1(("HllPushTestLines enter\n"));
    if ( !p )
        return( ERROR );

    while ( *p ) {
        if (*p == ' ') p++; /* there might be lines with 1 ' ' only! */
        for ( p2=buffer; *p && ( *p != EOLCHAR );)
            *p2++ = *p++;
        *p2 = NULLC;
        if ( *p == EOLCHAR )
            p++;
        if ( *buffer )
            AddLineQueue( buffer );
    }
    LclFree( hll->condlines );
    hll->condlines = NULL;


    return( NOT_ERROR );
}

/* for .UNTILCXZ: check if expression is simple enough.
 * what's acceptable is ONE condition, and just operators == and !=
 * Constants (0 or != 0) are also accepted.
 */

static ret_code CheckCXZLines( struct hll_item *hll )
/***************************************************/
{
    int lines = 0;
    int i;
    bool NL = TRUE;
    char *p = hll->condlines;

    for (; *p; p++ ) {
        if ( *p == EOLCHAR ) {
            NL = TRUE;
            lines++;
        } else if ( NL ) {
            NL = FALSE;
            if ( *p == 'j' ) {
                p++;
                /* v2.06: rewritten. case ".untilcxz 1" still has problems */
                if ( *p == 'm' && lines == 0 ) {
                    p--;
                    i = strlen( p );
                    while ( i ) {
                        *(p+2+i) = *(p+i);
                        i--;
                    }
                    memcpy( p,"loope",5 );
                } else if ( lines == 1 && ( *p == 'z' || (*p == 'n' && *(p+1) == 'z') ) ) {
                    p--;
                    i = strlen( p );
                    while ( i ) {
                        *(p+3+i) = *(p+i);
                        i--;
                    }
                    memcpy( p,"loop",4 );
                } else
                    return( ERROR );
            }
        }
    }
    if ( lines > 2 )
        return( ERROR );
    return( NOT_ERROR );
}

/* Start a .IF, .WHILE, .REPEAT item */

ret_code HllStartDir( int i, struct asm_tok tokenarray[] )
/********************************************************/
{
    struct hll_item      *hll;
    ret_code             rc = NOT_ERROR;
    int                  cmd = tokenarray[i].tokval;
    char buff[16];

    DebugMsg1(("HllStartDir(%s) enter\n", tokenarray[i].string_ptr ));

    i++; /* skip directive */

    /* v2.06: is there an item on the free stack? */
    if ( HllFree ) {
        hll = HllFree;
#ifdef DEBUG_OUT
        cntReused++;
#endif
    } else {
        hll = LclAlloc( sizeof( struct hll_item ) );
#ifdef DEBUG_OUT
        cntAlloc++;
#endif
    }

    /* create labels which are always needed */
    /* for .IF -.ENDIF without .ELSE no LEXIT-label is needed. */

    hll->labels[LSTART] = 0;
    hll->labels[LTEST] = GetHllLabel();
    hll->labels[LEXIT] = 0;

    hll->condlines = NULL;

    /* structure for .IF .ELSE .ENDIF
     *    cond jump to LTEST-label
     *    ...
     *    jmp LEXIT
     *  LTEST:
     *    ...
     *  LEXIT:

     * structure for .IF .ELSEIF
     *    cond jump to LTEST
     *    ...
     *    jmp LEXIT
     *  LTEST:
     *    cond jump to (new) LTEST
     *    ...
     *    jmp LEXIT
     *  LTEST:
     *    ...

     * structure for .WHILE and .REPEAT:
     *   jmp LTEST (for .WHILE only)
     * LSTART:
     *   ...
     * LTEST: (jumped to by .continue)
     *   test end condition, cond jump to LSTART label
     * LEXIT: (jumped to by .break)
     */
    NewLineQueue();

    switch ( cmd ) {
    case T_DOT_IF:
        hll->cmd = HLL_IF;
        hll->flags = 0;
        /* get the C-style expression, convert to ASM code lines */
        if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LTEST, FALSE ) ) {
            rc = ERROR;
            break;
        }
        HllPushTestLines( hll );
#if 1
        /* if no lines have been created, the LTEST label isn't needed */
        if ( !is_linequeue_populated() ) {
            hll->labels[LTEST] = 0;
        }
#endif
        break;
    case T_DOT_WHILE:
    case T_DOT_REPEAT:
        /* create the label to loop start */
        hll->labels[LSTART] = GetHllLabel();
        hll->labels[LEXIT] = GetHllLabel();
        if ( cmd == T_DOT_WHILE ) {
            hll->cmd = HLL_WHILE;
            if ( tokenarray[i].token != T_FINAL ) {
                if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LSTART, TRUE ) ) {
                    rc = ERROR;
                }
            } else
                hll->condlines = "";
            /* create a jump to second label */
            /* optimisation: if second label is just a jump, dont jump! */
            if ( hll->condlines && _memicmp( hll->condlines, "jmp", 3 ) ) {
                AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LTEST], buff ) );
            } else {
                hll->labels[LTEST] = 0;
            }
        } else {
            hll->cmd = HLL_REPEAT;
        }
        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LSTART], buff ) );
        break;
    }

    if ( tokenarray[i].token != T_FINAL && rc == NOT_ERROR ) {
        //LclFree( hll->condlines );
        DebugMsg(("HllStartDir: unexpected token %u [%s]\n", tokenarray[i].token, tokenarray[i].string_ptr ));
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        rc = ERROR;
        //return( ERROR ); /* v2.08: continue and parse the line queue */
    }
    /* v2.06: remove the item from the free stack */
    if ( hll == HllFree )
        HllFree = hll->next;
    hll->next = HllStack;
    HllStack = hll;

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );

    if ( is_linequeue_populated() ) /* might be NULL! (".if 1") */
        RunLineQueue();

    return( rc );
}

/* End a .IF, .WHILE, .REPEAT item
 * that is: .ENDIF, .ENDW, .UNTIL and .UNTILCXZ are handled here
 */
ret_code HllEndDir( int i, struct asm_tok tokenarray[] )
/******************************************************/
{
    //struct asym       *sym;
    struct hll_item     *hll;
    ret_code            rc = NOT_ERROR;
    int                 cmd = tokenarray[i].tokval;
    char buff[16];

    DebugMsg1(("HllEndDir(%s) enter\n", tokenarray[i].string_ptr ));

    if ( HllStack == NULL ) {
        DebugMsg(("HllEndDir: hll stack is empty\n"));
        EmitError( DIRECTIVE_MUST_BE_IN_CONTROL_BLOCK );
        return( ERROR );
    }

    hll = HllStack;
    HllStack = hll->next;
    /* v2.06: move the item to the free stack */
    hll->next = HllFree;
    HllFree = hll;

    NewLineQueue();

    switch ( cmd ) {
    case T_DOT_ENDIF:
        if ( hll->cmd != HLL_IF ) {
            DebugMsg(("HllEndDir: no .IF on the hll stack\n"));
            EmitErr( BLOCK_NESTING_ERROR, tokenarray[i].string_ptr );
            return( ERROR );
        }
        /* if a test label isn't created yet, create it */
        if ( hll->labels[LTEST] > 0 ) {
            AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LTEST], buff ) );
        }
        /* create the exit label if it exists */
        if ( hll->labels[LEXIT] > 0 ) {
            AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LEXIT], buff ) );
        }
        i++;
        break;
    case T_DOT_ENDW:
        if ( hll->cmd != HLL_WHILE ) {
            DebugMsg(("HllEndDir: no .WHILE on the hll stack\n"));
            EmitErr( BLOCK_NESTING_ERROR, tokenarray[i].string_ptr );
            return( ERROR );
        }
        /* create test label  */
        if ( hll->labels[LTEST] > 0 ) {
            AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LTEST], buff ) );
            DebugMsg(("HllEndDir: created: %s" LABELQUAL "\n", GetLabelStr( hll->labels[LTEST], buff ) ));
        }
        HllPushTestLines( hll );

        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LEXIT], buff ) );
        i++;
        break;
    case T_DOT_UNTILCXZ:
        if ( hll->cmd != HLL_REPEAT ) {
            DebugMsg(("HllEndDir: no .REPEAT on the hll stack\n"));
            EmitErr( BLOCK_NESTING_ERROR, tokenarray[i].string_ptr );
            return( ERROR );
        }
        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LTEST], buff ) );

        i++;
        /* read in optional (simple) expression */
        if ( tokenarray[i].token != T_FINAL ) {
            if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LSTART, FALSE ) ) {
                rc = ERROR;
                break;
            }
            if ( CheckCXZLines( hll ) == ERROR ) {
                EmitError( EXPR_TOO_COMPLEX_FOR_UNTILCXZ );
                rc = ERROR;
                break;
            }
            /* write condition lines */
            HllPushTestLines( hll );
        } else {
            AddLineQueueX( " loop %s", GetLabelStr( hll->labels[LSTART], buff ) );
        }
        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LEXIT], buff ) );
        break;
    case T_DOT_UNTIL:
        if ( hll->cmd != HLL_REPEAT ) {
            DebugMsg(("HllEndDir: no .REPEAT on the hll stack\n"));
            EmitErr( BLOCK_NESTING_ERROR, tokenarray[i].string_ptr );
            return( ERROR );
        }
        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LTEST], buff ) );

        i++;
        /* read in (optional) expression */
        /* if expression is missing, just generate nothing */
        if ( tokenarray[i].token != T_FINAL ) {
            if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LSTART, FALSE ) ) {
                rc = ERROR;
                break;
            }
            /* write condition lines */
            HllPushTestLines( hll );
        }
#if 0
        AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LSTART], buff ) );
#endif

        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LEXIT], buff ) );
        break;
    }

    LclFree( hll->condlines );

    if ( tokenarray[i].token != T_FINAL && rc == NOT_ERROR ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        rc = ERROR;
    }
    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );

    if ( is_linequeue_populated() )
        if ( rc == NOT_ERROR )
            RunLineQueue();
        else
            DeleteLineQueue();

    return( rc );
}

/* Exit current .IF, .WHILE, .REPEAT item
 * that is: .ELSE, .ELSEIF, .CONTINUE and .BREAK are handled here
 */
ret_code HllExitDir( int i, struct asm_tok tokenarray[] )
/*******************************************************/
{
    //int               level;
    //struct asym       *sym;
    struct hll_item     *hll;
    ret_code            rc = NOT_ERROR;
    char                *savedlines;
    enum hll_cmd        savedcmd;
    int                 cmd = tokenarray[i].tokval;
    char buff[16];

    DebugMsg1(("HllExitDir(%s) enter\n", tokenarray[i].string_ptr ));

    hll = HllStack;

    if ( hll == NULL ) {
        DebugMsg(("HllExitDir stack error\n"));
        EmitError( DIRECTIVE_MUST_BE_IN_CONTROL_BLOCK );
        return( ERROR );
    }

    NewLineQueue();

    switch ( cmd ) {
    case T_DOT_ELSE:
    case T_DOT_ELSEIF:
        if ( hll->cmd != HLL_IF ) {
            DebugMsg(("HllExitDir(%s): labels[LTEST]=%X\n", tokenarray[i].string_ptr, hll->labels[LTEST]));
            EmitErr( BLOCK_NESTING_ERROR, tokenarray[i].string_ptr );
            return( ERROR );
        }
        /* v2.08: check for multiple ELSE clauses */
        if ( hll->flags & HLLF_ELSEOCCURED ) {
            EmitError( DOT_ELSE_CLAUSE_ALREADY_OCCURED_IN_THIS_DOT_IF_BLOCK );
            return( ERROR );
        }

        /* the "labels[LEXIT]" label is only needed if an .ELSE branch exists.
         That's why it is created delayed.
         */
        if ( hll->labels[LEXIT] == 0 )
            hll->labels[LEXIT] = GetHllLabel();

        AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LEXIT], buff ) );
        if ( hll->labels[LTEST] > 0 ) {
            AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LTEST], buff ) );
            hll->labels[LTEST] = 0;
        }
        i++;
        if ( cmd == T_DOT_ELSEIF ) {
            /* create new labels[LTEST] label */
            hll->labels[LTEST] = GetHllLabel();
            if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LTEST, FALSE ) ) {
                rc = ERROR;
                break;
            }
            HllPushTestLines( hll );
        } else
            hll->flags |= HLLF_ELSEOCCURED;

        break;
    case T_DOT_BREAK:
    case T_DOT_CONTINUE:
        for ( ; hll && hll->cmd == HLL_IF; hll = hll->next );
        if ( hll == NULL ) {
            EmitError( DIRECTIVE_MUST_BE_IN_CONTROL_BLOCK );
            return( ERROR );
        }
        /* .BREAK .IF ... or .CONTINUE .IF ? */
        i++;
        if ( tokenarray[i].token != T_FINAL ) {
            if ( tokenarray[i].token == T_DIRECTIVE && tokenarray[i].tokval == T_DOT_IF ) {
                savedlines = hll->condlines;
                savedcmd = hll->cmd;
                hll->condlines = NULL;
                hll->cmd = HLL_BREAK;
                i++;
                if ( cmd == T_DOT_BREAK ) {
                    if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LEXIT, TRUE ) ) {
                        return( ERROR );
                    }
                } else { /* T_DOT_CONTINUE */
                    if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LTEST, TRUE ) ) {
                        return( ERROR );
                    }
                }
                HllPushTestLines( hll );
                LclFree( hll->condlines );
                hll->condlines = savedlines;
                hll->cmd = savedcmd;
            }
        } else {
            if ( cmd == T_DOT_BREAK ) {
                AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LEXIT], buff ) );
            } else {
                if ( hll->labels[LTEST] > 0 )
                    AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LTEST], buff ) );
                else
                    AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LSTART], buff ) );
            }
        }
        break;
    }
    if ( tokenarray[i].token != T_FINAL && rc == NOT_ERROR ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        rc = ERROR;
    }

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );

    if ( rc == NOT_ERROR )
        RunLineQueue();
    else
        DeleteLineQueue();

    return( rc );
}

/* check if an hll block has been left open */

void HllCheckOpen( void )
/***********************/
{
    if ( HllStack ) {
        //EmitErr( BLOCK_NESTING_ERROR, ".if-.repeat-.while" );
        EmitErr( UNMATCHED_BLOCK_NESTING, ".if-.repeat-.while" );
    }
    DebugMsg1(("HllCheckOpen: allocated items:%u, reused items:%u, cond-blocks/bytes:%u/%u\n", cntAlloc, cntReused, cntCond, cntCondBytes ));
}

/* HllInit() is called for each pass */

void HllInit( int pass )
/**********************/
{
    if ( pass == PASS_1 )
        HllFree = NULL;

    HllStack = NULL; /* empty stack of open hll directives */
    ModuleInfo.hll_label = 0; /* init hll label counter */
#ifdef DEBUG_OUT
    evallvl = 0;
#endif
    return;
}
