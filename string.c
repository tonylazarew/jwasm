/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  string macro processing routines.
*
* functions:
* - CatStrDir    handle CATSTR   directive ( also TEXTEQU )
* - SetTextMacro handle EQU      directive if expression is text
* - SubStrDir    handle SUBSTR   directive
* - SizeStrDir   handle SIZESTR  directive
* - InStrDir     handle INSTR    directive
* - CatStrFunc   handle @CatStr  function
* - SubStrFunc   handle @SubStr  function
* - SizeStrFunc  handle @SizeStr function
* - InStrFunc    handle @InStr   function
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "expreval.h"
#include "equate.h"
#include "input.h"
#include "tokenize.h"
#include "macro.h"
#include "condasm.h"
#include "fastpass.h"
#include "listing.h"

#ifdef DEBUG_OUT
static uint_32 catstrcnt;
static uint_32 substrcnt;
static uint_32 sizstrcnt;
static uint_32 instrcnt;
static uint_32 equcnt;
#endif

/* generic parameter names. In case the parameter name is
 * displayed in an error message ("required parameter %s missing")
 * v2.05: obsolete
 */
//static const char * parmnames[] = {"p1","p2","p3"};

void TextItemError( struct asm_tok *item )
/****************************************/
{
    if ( item->token == T_STRING && *item->string_ptr == '<' ) {
        EmitError( MISSING_ANGLE_BRACKET_OR_BRACE_IN_LITERAL );
        return;
    }
    /* v2.05: better error msg if (text) symbol isn't defined */
    if ( item->token == T_ID ) {
        struct asym *sym = SymSearch( item->string_ptr );
        if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
            EmitErr( SYMBOL_NOT_DEFINED, item->string_ptr );
            return;
        }
    }
    EmitError( TEXT_ITEM_REQUIRED );
    return;
}

/* CATSTR directive.
 * defines a text equate
 * syntax <name> CATSTR [<string>,...]
 * TEXTEQU is an alias for CATSTR
 */

ret_code CatStrDir( int i, struct asm_tok tokenarray[] )
/******************************************************/
{
    struct asym *sym;
    int count;
    char *p;
    /* struct expr opndx; */

#ifdef DEBUG_OUT
    catstrcnt++;
#endif
    DebugMsg1(("CatStrDir(%u) enter\n", i ));

#if 0 /* can't happen */
    /* syntax must be <id> CATSTR textitem[,textitem,...] */
    if ( i != 1 ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    if ( tokenarray[0].token != T_ID ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
        return( ERROR );
    }
#endif
    i++; /* go past CATSTR/TEXTEQU */

    /* v2.08: don't copy to temp buffer */
    //*StringBufferEnd = NULLC;
    /* check correct syntax and length of items */
    for ( count = 0; i < Token_Count; ) {
        DebugMsg1(("CatStrDir(%s): item[%u]=%s delim=0x%x\n", tokenarray[0].string_ptr, i, tokenarray[i].string_ptr, tokenarray[i].string_delim ));
        if ( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
            DebugMsg(("CatStrDir: error, not a <>-literal: %s\n", tokenarray[i].tokpos ));
            TextItemError( &tokenarray[i] );
            return( ERROR );
        }
        /* v2.08: using tokenarray.stringlen is not quite correct, since some chars
         * are stored in 2 bytes (!) */
        if ( ( count + tokenarray[i].stringlen ) >= MAX_LINE_LEN ) {
            DebugMsg(("CatStrDir: error, literal too long: %u + %u >= %u\n", count, tokenarray[i].stringlen, MAX_LINE_LEN ));
            EmitError( STRING_OR_TEXT_LITERAL_TOO_LONG );
            return( ERROR );
        }
        /* v2.08: don't copy to temp buffer */
        //strcpy( StringBufferEnd + count, tokenarray[i].string_ptr );
        count = count + tokenarray[i].stringlen;
        i++;
        if ( ( tokenarray[i].token != T_COMMA ) &&
            ( tokenarray[i].token != T_FINAL ) ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
        i++;
    }

    sym = SymSearch( tokenarray[0].string_ptr );
    if ( sym == NULL ) {
        sym = SymCreate( tokenarray[0].string_ptr );
        DebugMsg1(( "CatStrDir: new symbol %s created\n", sym->name));
    } else if( sym->state == SYM_UNDEFINED ) {
        /* v2.01: symbol has been used already. Using
         * a textmacro before it has been defined is
         * somewhat problematic.
         */
        sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
#if FASTPASS
        SkipSavedState(); /* further passes must be FULL! */
#endif
        EmitWarn( 2, TEXT_MACRO_USED_PRIOR_TO_DEFINITION, sym->name );
    } else if( sym->state != SYM_TMACRO ) {
        /* it is defined as something else, get out */
        DebugMsg(( "CatStrDir(%s) exit, symbol redefinition\n", sym->name));
        EmitErr( SYMBOL_REDEFINITION, sym->name );
        return( ERROR );
    }


    sym->state = SYM_TMACRO;
    sym->isdefined = TRUE;
#if FASTMEM==0
    if ( sym->string_ptr )
        LclFree( sym->string_ptr );
    sym->string_ptr = (char *)LclAlloc( count + 1 );
#else
    /* v2.08: reuse string space if fastmem is on */
    if ( sym->total_size < ( count+1 ) ) {
        LclFree( sym->string_ptr ); /* is a noop if fastmem is on */
        sym->string_ptr = (char *)LclAlloc( count + 1 );
        sym->total_size = count + 1;
    }
#endif
    /* v2.08: don't use temp buffer */
    //memcpy( sym->string_ptr, StringBufferEnd, count + 1 );
    for ( i = 2, p = sym->string_ptr; i < Token_Count; i += 2 ) {
        memcpy( p, tokenarray[i].string_ptr, tokenarray[i].stringlen );
        p += tokenarray[i].stringlen;
    }
    *p = NULLC;
    DebugMsg1(("CatStrDir(%s) (new) value: >%s<\n", sym->name, sym->string_ptr ));

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_EQUATE, 0, sym );

    return( NOT_ERROR );
}

/*
 * used by EQU if the value to be assigned to a symbol is text.
 */
struct asym *SetTextMacro( struct asm_tok tokenarray[], struct asym *sym, const char *name, const char *value )
/*************************************************************************************************************/
{
    int count;

#ifdef DEBUG_OUT
    equcnt++;
#endif
#if 0 /* FASTPASS */
    /* there's no need to set the value if FASTPASS is on, because
     * the input are just preprocessed lines.
     * this check is probably obsolete by now, since it won't be called
     * ever if pass > 1. Also, even with fastpass it's sometimes necessary
     * to do a full second pass, including source preprocessing.
     */
    if ( Parse_Pass != PASS_1 )
        return( sym );
#endif

    if ( sym == NULL )
        sym = SymCreate( name );
    else if ( sym->state == SYM_UNDEFINED ) {
        sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
#if FASTPASS
        /* the text macro was referenced before being defined.
         * this is valid usage, but it requires a full second pass.
         * just simply deactivate the fastpass feature for this module!
         */
        SkipSavedState();
#endif
        EmitWarn( 2, TEXT_MACRO_USED_PRIOR_TO_DEFINITION, sym->name );
    } else if ( sym->state != SYM_TMACRO ) {
        EmitErr( SYMBOL_REDEFINITION, name );
        return( NULL );
    }

    sym->state = SYM_TMACRO;
    sym->isdefined = TRUE;

    if ( tokenarray[2].token == T_STRING && tokenarray[2].string_delim == '<' && tokenarray[3].token == T_FINAL ) {
        value = tokenarray[2].string_ptr;
        count = tokenarray[2].stringlen;
    } else {
        char *p = StringBufferEnd;
        /*
         the original source is used, since the tokenizer has
         deleted some information. it's important to double '!' found inside
         the string.
         */
        //while ( isspace( *value ) ) value++; /* probably obsolete */
        count = strlen( value );
        /* skip trailing spaces */
        if ( count ) {
            for ( ; count; count-- )
                if ( isspace( *( value + count - 1 ) ) == FALSE )
                    break;
        }
        for ( ; count; count-- ) {
            if ( *value == '!' || *value == '<' || *value == '>' )
                *p++ = '!';
            *p++ = *value++;
        }
        *p = NULLC;
        value = StringBufferEnd;
        count = p - StringBufferEnd;
    }
#if FASTMEM==0
    if ( sym->string_ptr )
        LclFree( sym->string_ptr );
    sym->string_ptr = (char *)LclAlloc( count + 1 );
#else
    if ( sym->total_size < ( count + 1 ) ) {
        LclFree( sym->string_ptr ); /* is a noop if fastmem is on */
        sym->string_ptr = (char *)LclAlloc( count + 1 );
        sym->total_size = count + 1;
    }
#endif
    memcpy( sym->string_ptr, value, count + 1 );

    DebugMsg1(( "SetTextMacro(%s): value is >%s<, exit\n", sym->name, sym->string_ptr ));
    return( sym );
}

/* create a (predefined) text macro.
 * used to create @code, @data, ...
 */
struct asym *AddPredefinedText( const char *name, const char *value )
/*******************************************************************/
{
    struct asym *sym;

    DebugMsg1(("AddPredefinedText(%s): >%s<\n", name, value ));
    /* v2.08: ignore previous setting */
    if ( NULL == ( sym = SymSearch( name ) ) )
        sym = SymCreate( name );
    sym->state = SYM_TMACRO;
    sym->isdefined = TRUE;
    sym->predefined = TRUE;
    sym->string_ptr = (char *)value;
    /* to ensure that a new buffer is used if the string is modified */
    sym->total_size = 0;
    return( sym );
}

/* SubStr()
 * defines a text equate.
 * syntax: name SUBSTR <string>, pos [, size]
 */
ret_code SubStrDir( int i, struct asm_tok tokenarray[] )
/******************************************************/
{
    struct asym         *sym;
    char                *name;
    char                *p;
    char                *newvalue;
    int                 pos;
    int                 size;
    int                 cnt;
    bool                chksize;
    struct expr         opndx;

    DebugMsg1(("SubStrDir entry\n"));
#ifdef DEBUG_OUT
    substrcnt++;
#endif

    /* at least 5 items are needed
     * 0  1      2      3 4    5   6
     * ID SUBSTR SRC_ID , POS [, LENGTH]
     */
#if 0 /* can't happen */
    if ( i != 1 ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    if ( tokenarray[0].token != T_ID ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
        return( ERROR );
    }
#endif
    name = tokenarray[0].string_ptr;

    i++; /* go past SUBSTR */

    /* third item must be a string */

    if ( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
        DebugMsg(("SubStrDir: error, no text item\n"));
        TextItemError( &tokenarray[i] );
        return( ERROR );
    }
    p = tokenarray[i].string_ptr;
    i++;
    DebugMsg1(("SubStrDir(%s): src=>%s<\n", name, p));

    if ( tokenarray[i].token != T_COMMA ) {
        EmitError( EXPECTING_COMMA );
        return( ERROR );
    }
    i++;

    /* get pos, must be a numeric value and > 0 */

    if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR ) {
        DebugMsg(("SubStrDir(%s): invalid pos value\n", name));
        return( ERROR );
    }

    /* v2.04: "string" constant allowed as second argument */
    //if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
    if ( opndx.kind != EXPR_CONST ) {
        DebugMsg(("SubStrDir(%s): pos value is not a constant\n", name));
        EmitError( CONSTANT_EXPECTED );
        return( ERROR );
    }

    /* pos is expected to be 1-based */
    pos = opndx.value;
    if ( pos <= 0 ) {
        EmitError( POSITIVE_VALUE_EXPECTED );
        return( ERROR );
    }
    if ( tokenarray[i].token != T_FINAL ) {
        if ( tokenarray[i].token != T_COMMA ) {
            EmitError( EXPECTING_COMMA );
            return( ERROR );
        }
        i++;
        /* get size, must be a constant */
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR ) {
            DebugMsg(("SubStrDir(%s): invalid size value\n", name));
            return( ERROR );
        }
        /* v2.04: string constant ok */
        //if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
        if ( opndx.kind != EXPR_CONST ) {
            DebugMsg(("SubStrDir(%s): size value is not a constant\n", name));
            EmitError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        size = opndx.value;
        if ( tokenarray[i].token != T_FINAL ) {
            DebugMsg(("SubStrDir(%s): additional items found\n", name));
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
        if ( size < 0 ) {
            EmitError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            return( ERROR );
        }
        chksize = TRUE;
    } else {
        size = -1;
        chksize = FALSE;
    }

    cnt = pos;
    /* position p to start of substring */
    for ( pos--; pos > 0 && *p ; pos--, p++ )
        if ( *p == '!' && *(p+1) != NULLC )
            p++;

    if ( *p == NULLC ) {
        EmitErr( INDEX_PAST_END_OF_STRING, cnt );
        return( ERROR );
    }

    if ( *p == '!' && *(p+1) != NULLC )
        p++;

    for ( newvalue = p, cnt = size; *p && cnt; cnt--, p++ )
        if ( *p == '!' && *(p+1) != NULLC )
            p++;

    /* v2.04: check added */
    if ( chksize && cnt ) {
        EmitError( COUNT_VALUE_TOO_LARGE );
        return( ERROR );
    }

    sym = SymSearch( name );

    /* if we've never seen it before, put it in */
    if( sym == NULL ) {
        sym = SymCreate( name );
    } else if( sym->state == SYM_UNDEFINED ) {
        /* it was referenced before being defined. This is
         * a bad idea for preprocessor text items, because it
         * will require a full second pass!
         */
        sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
#if FASTPASS
        SkipSavedState();
        EmitWarn( 2, TEXT_MACRO_USED_PRIOR_TO_DEFINITION, sym->name );
#endif
    } else if( sym->state != SYM_TMACRO ) {
        /* it is defined as something incompatible, get out */
        DebugMsg(( "SubStrDir(%s) error, incompatible type\n", sym->name));
        EmitErr( SYMBOL_REDEFINITION, sym->name );
        return( ERROR );
    }

    sym->state = SYM_TMACRO;
    sym->isdefined = TRUE;

    size = p - newvalue;
    p = newvalue;

#if FASTMEM==0
    if ( sym->string_ptr )
        LclFree( sym->string_ptr );
    sym->string_ptr = (char *)LclAlloc( size + 1 );
#else
    if ( sym->total_size < ( size + 1 ) ) {
        LclFree( sym->string_ptr );
        sym->string_ptr = LclAlloc ( size + 1 );
        sym->total_size = size + 1;
    }
#endif
    memcpy( sym->string_ptr, p, size );
    *(sym->string_ptr + size) = NULLC;
    DebugMsg1(("SubStrDir(%s): result=>%s<\n", sym->name, sym->string_ptr ));

    LstWrite( LSTTYPE_EQUATE, 0, sym );

    return( NOT_ERROR );
}

/* SizeStr()
 * defines a numeric variable which contains size of a string
 */
ret_code SizeStrDir( int i, struct asm_tok tokenarray[] )
/*******************************************************/
{
    struct asym *sym;
    int sizestr;

    DebugMsg1(("SizeStrDir entry\n"));
#ifdef DEBUG_OUT
    sizstrcnt++;
#endif

    if ( i != 1 ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
#if 0 /* this is checked in ParseLine() */
    if ( tokenarray[0].token != T_ID ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
        return( ERROR );
    }
#endif
    if ( tokenarray[2].token != T_STRING || tokenarray[2].string_delim != '<' ) {
        TextItemError( &tokenarray[2] );
        return( ERROR );
    }
    if ( Token_Count > 3 ) {
        DebugMsg(("SizeStrDir: syntax error, name=%s, Token_Count=%u\n", tokenarray[0].string_ptr, Token_Count));
        EmitErr( SYNTAX_ERROR_EX, tokenarray[3].string_ptr );
        return( ERROR );
    }

    sizestr = GetLiteralValue( StringBufferEnd, tokenarray[2].string_ptr );

    if ( sym = CreateVariable( tokenarray[0].string_ptr, sizestr ) ) {
        DebugMsg1(("SizeStrDir(%s) exit, value=%u\n", tokenarray[0].string_ptr, sizestr));
        LstWrite( LSTTYPE_EQUATE, 0, sym );
        return( NOT_ERROR );
    }
    return( ERROR );

}

/* InStr()
 * defines a numeric variable which contains position of substring.
 * syntax:
 * name INSTR [pos,]string, substr
 */
ret_code InStrDir( int i, struct asm_tok tokenarray[] )
/*****************************************************/
{
    struct asym *sym;
    int sizestr;
    int j;
    /* int commas; */
    char *src;
    char *p;
    char *q;
    char *string1;
    struct expr opndx;
    int start = 1;
    int strpos;

    DebugMsg1(("InStrDir entry\n"));
#ifdef DEBUG_OUT
    instrcnt++;
#endif

    if ( i != 1) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
#if 0 /* this is checked in ParseLine() */
    if ( tokenarray[0].token != T_ID ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
        return( ERROR );
    }
#endif

    i++; /* go past INSTR */

    if ( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) {
            EmitError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        start = opndx.value;
        if ( start <= 0 ) {
            /* v2.05: don't change the value. if it's invalid, the result
             * is to be 0. Emit a level 3 warning instead.
             */
            //start = 1;
            EmitWarn( 3, POSITIVE_VALUE_EXPECTED );
        }
        if ( tokenarray[i].token != T_COMMA ) {
            EmitError( EXPECTING_COMMA );
            return( ERROR );
        }
        i++; /* skip comma */
    }

    if ( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
        TextItemError( &tokenarray[i] );
        return( ERROR );
    }

    /* to compare the strings, the "visible" format is needed, since
     * the possible '!' operators inside the strings is optional and
     * must be ignored.
     */
    src = StringBufferEnd;
    sizestr = GetLiteralValue( src, tokenarray[i].string_ptr );
    DebugMsg1(("InStrDir: first string >%s< \n", src ));

    if ( start > sizestr ) {
        EmitErr( INDEX_PAST_END_OF_STRING, start );
        return( ERROR );
    }
    p = src + start - 1;

    i++;
    if ( tokenarray[i].token != T_COMMA ) {
        EmitError( EXPECTING_COMMA );
        return( ERROR );
    }
    i++;

    if ( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
        TextItemError( &tokenarray[i] );
        return( ERROR );
    }
    q = GetAlignedPointer( src, sizestr );
    j = GetLiteralValue( q, tokenarray[i].string_ptr );
    DebugMsg1(("InStrDir: second string >%s< \n", q ));
    i++;
    if ( tokenarray[i].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    strpos = 0;
    /* v2.05: check for start > 0 added */
    /* v2.08: check for j > 0 added */
    if ( ( start > 0 ) && ( sizestr >= j ) && j && ( string1 = strstr( p, q ) ))
        strpos = string1 - src + 1;

    if ( sym = CreateVariable( tokenarray[0].string_ptr, strpos ) ) {
        DebugMsg1(("InStrDir(%s) exit, value=%u\n", tokenarray[0].string_ptr, strpos));
        LstWrite( LSTTYPE_EQUATE, 0, sym );
        return ( NOT_ERROR );
    }
    return( ERROR );
}

/* internal @CatStr macro function */

static ret_code CatStrFunc( struct macro_instance *mi, char *buffer, struct asm_tok tokenarray[] )
/************************************************************************************************/
{
#ifdef DEBUG_OUT
    int cnt = 0;
#endif
    int i;
    char *p;

    DebugMsg1(("@CatStr( %s )\n", mi->parm_array[0] ? mi->parm_array[0] : "NULL" ));

    for ( p = mi->parm_array[0]; mi->parmcnt; mi->parmcnt-- ) {
        DebugMsg1(("@CatStr.%u: >%s<\n", cnt++, p ));
        i = strlen( p );
        memcpy( buffer, p, i );
        p = GetAlignedPointer( p, i );
        buffer += i;
    }
    *buffer = NULLC;
    return( NOT_ERROR );
}

/* convert string to a number */

static ret_code GetNumber( char *string, int *pi, struct asm_tok tokenarray[] )
/*****************************************************************************/
{
    struct expr opndx;
    int i;
    int last;

    last = Tokenize( string, Token_Count+1, tokenarray, TOK_RESCAN );
    i = Token_Count+1;
    if( EvalOperand( &i, tokenarray, last, &opndx, 0 ) == ERROR ) {
        return( ERROR );
    }
    if( opndx.kind != EXPR_CONST || opndx.quoted_string != NULL || tokenarray[i].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, string );
        return( ERROR );
    }
    *pi = opndx.value;
    return( NOT_ERROR );
}

/* internal @InStr macro function
 * the result is returned as string in current radix
 */
static ret_code InStrFunc( struct macro_instance *mi, char *buffer, struct asm_tok tokenarray[] )
/***********************************************************************************************/
{
    int pos = 1;
    char *p;
    uint_32 found;

    DebugMsg1(("@InStr( %s, %s, %s)\n",
              mi->parm_array[0] ? mi->parm_array[0] : "",
              mi->parm_array[1] ? mi->parm_array[1] : "",
              mi->parm_array[2] ? mi->parm_array[2] : "" ));

    /* init buffer with "0" */
    *buffer = '0';
    *(buffer+1) = NULLC;

    if ( mi->parm_array[0] ) {
        if ( GetNumber( mi->parm_array[0], &pos, tokenarray ) == ERROR )
            return( ERROR );
        if ( pos == 0 )
            pos++;
    }

    if ( pos > strlen( mi->parm_array[1] ) ) {
        EmitErr( INDEX_PAST_END_OF_STRING, pos );
        return( ERROR );
    }
    /* v2.08: if() added, empty searchstr is to return 0 */
    if ( *(mi->parm_array[2]) != NULLC ) {
        p = strstr( mi->parm_array[1] + pos - 1, mi->parm_array[2] );
        if ( p ) {
            found = p - mi->parm_array[1] + 1;
            myltoa( found, buffer, ModuleInfo.radix, FALSE, TRUE );
        }
    }

    DebugMsg1(( "@InStrFunc()=>%s<\n", buffer ));

    return( NOT_ERROR );
}

/* internal @SizeStr macro function
 * the result is returned as string in current radix
 */
static ret_code SizeStrFunc( struct macro_instance *mi, char *buffer, struct asm_tok tokenarray[] )
/*************************************************************************************************/
{
    DebugMsg1(("@SizeStr(%s)\n", mi->parm_array[0] ? mi->parm_array[0] : "" ));
    if ( mi->parm_array[0] )
        myltoa( strlen( mi->parm_array[0] ), buffer, ModuleInfo. radix, FALSE, TRUE );
    else {
        buffer[0] = '0';
        buffer[1] = NULLC;
    }
    return( NOT_ERROR );
}

/* internal @SubStr macro function */

static ret_code SubStrFunc( struct macro_instance *mi, char *buffer, struct asm_tok tokenarray[] )
/************************************************************************************************/
{
    int pos;
    int size;
    char *src = mi->parm_array[0];

    DebugMsg1(("@SubStr( %s, %s, %s)\n",
              src ? src : "",
              mi->parm_array[1] ? mi->parm_array[1] : "",
              mi->parm_array[2] ? mi->parm_array[2] : "" ));

    if ( GetNumber( mi->parm_array[1], &pos, tokenarray ) == ERROR )
        return( ERROR );

    if (pos <= 0)
        pos = 1;

    size = strlen( src );
    if ( pos > size ) {
        EmitErr( INDEX_PAST_END_OF_STRING, pos );
        return( ERROR );
    }

    if ( mi->parm_array[2] ) {
        if ( GetNumber( mi->parm_array[2], &size, tokenarray ) == ERROR )
            return( ERROR );
        if ( size < 0 ) {
            EmitError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            return( ERROR );
        }
    } else {
        size = size - pos + 1;
    }

    for( src = src+pos-1 ; size && *src ; size-- )
        *buffer++ = *src++;

    *buffer = NULLC;

    if ( size ) {
        EmitError( COUNT_VALUE_TOO_LARGE );
        return( ERROR );
    }

    return( NOT_ERROR );
}

/* string macro initialization
 * this proc is called once per module
 */
void StringInit( void )
/*********************/
{
    int i;
    struct dsym *macro;

    DebugMsg(( "StringInit() enter\n" ));

#ifdef DEBUG_OUT
    catstrcnt = 0;
    substrcnt = 0;
    sizstrcnt = 0;
    instrcnt = 0;
    equcnt = 0;
#endif

    /* add @CatStr() macro func */

    macro = CreateMacro( "@CatStr" );
    macro->sym.isdefined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = CatStrFunc;
    macro->sym.isfunc = TRUE;
    /* v2.08: @CatStr() changed to VARARG */
    macro->sym.mac_vararg = TRUE;
    macro->e.macroinfo->parmcnt = 1;
    macro->e.macroinfo->parmlist = LclAlloc( sizeof( struct mparm_list ) * 1 );
    macro->e.macroinfo->parmlist[0].deflt = NULL;
    macro->e.macroinfo->parmlist[0].required = FALSE;

    /* add @InStr() macro func */

    macro = CreateMacro( "@InStr" );
    macro->sym.isdefined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = InStrFunc;
    macro->sym.isfunc = TRUE;
    macro->e.macroinfo->parmcnt = 3;
    macro->e.macroinfo->autoexp = 1; /* param 1 (pos) is expanded */
    macro->e.macroinfo->parmlist = LclAlloc(sizeof( struct mparm_list) * 3);
    for (i = 0; i < 3; i++) {
        macro->e.macroinfo->parmlist[i].deflt = NULL;
        //macro->e.macroinfo->parmlist[i].label = parmnames[i];
        macro->e.macroinfo->parmlist[i].required = (i != 0);
    }

    /* add @SizeStr() macro func */

    macro = CreateMacro( "@SizeStr" );
    macro->sym.isdefined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = SizeStrFunc;
    macro->sym.isfunc = TRUE;
    macro->e.macroinfo->parmcnt = 1;
    macro->e.macroinfo->parmlist = LclAlloc(sizeof( struct mparm_list));
    macro->e.macroinfo->parmlist[0].deflt = NULL;
    //macro->e.macroinfo->parmlist[0].label = parmnames[0];
    /* macro->e.macroinfo->parmlist[0].required = TRUE; */
    /* the string parameter is NOT required, '@SizeStr()' is valid */
    macro->e.macroinfo->parmlist[0].required = FALSE;

    /* add @SubStr() macro func */

    macro = CreateMacro( "@SubStr" );
    macro->sym.isdefined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = SubStrFunc;
    macro->sym.isfunc = TRUE;
    macro->e.macroinfo->parmcnt = 3;
    macro->e.macroinfo->autoexp = 2 + 4; /* param 2 (pos) and 3 (size) are expanded */
    macro->e.macroinfo->parmlist = LclAlloc(sizeof( struct mparm_list) * 3);
    for (i = 0; i < 3; i++) {
        macro->e.macroinfo->parmlist[i].deflt = NULL;
        //macro->e.macroinfo->parmlist[i].label = parmnames[i];
        macro->e.macroinfo->parmlist[i].required = (i < 2);
    }

    return;
}
#ifdef DEBUG_OUT
void StringFini( void )
/*********************/
{
    if ( Options.quiet == FALSE )
        printf("invokation CATSTR=%u SUBSTR=%u SIZESTR=%u INSTR=%u EQU(text)=%u\n", catstrcnt, substrcnt, sizstrcnt, instrcnt, equcnt );
}
#endif
