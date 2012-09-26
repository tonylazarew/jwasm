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
* Description:  data definition. handles
*               - directives DB,DW,DD,...
*               - predefined types (BYTE, WORD, DWORD, ...)
*               - arbitrary types
*
****************************************************************************/

#include <ctype.h>
#include <float.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "expreval.h"
#include "input.h"
#include "tbyte.h"
#include "fixup.h"
#include "listing.h"
#include "segment.h"
#include "types.h"
#include "fastpass.h"
#include "tokenize.h"
#include "macro.h"
#include "omf.h"
#include "myassert.h"

//#ifndef min
//#define min(x,y) (((x) < (y)) ? (x) : (y))
//#endif

#if !defined(__GNUC__) && !defined(__POCC__)
#define tolower(c) ((c >= 'A' && c <= 'Z') ? c | 0x20 : c )
#endif

extern ret_code segm_override( const struct expr *, struct code_info * );
extern struct asym *SegOverride;
extern const char szNull[];

/* initialize an array inside a structure
 * if there are no brackets, the next comma, '>' or '}' will terminate
 *
 * valid initialization are:
 * - an expression, might contain DUP or a string enclosed in quotes.
 * - a literal enclosed in <> or {} which then is supposed to contain
 *   single items.
 */
static ret_code InitializeArray( const struct field_item *f, int *pi, struct asm_tok tokenarray[] )
/*************************************************************************************************/
{
    //int  count;
    //char *ptr;
    //char bArray = FALSE;
    int  oldofs;
    int  i = *pi;
    int  j;
    int  lvl;
    char c;

    DebugMsg1(("InitializeArray( %s ) enter, items=%lu, type=%s, tokenpos=%s\n", f->sym->name, f->sym->total_length, f->initializer, tokenarray[i].tokpos ));

    /* empty the line queue to update the current offset */
    if ( is_linequeue_populated() ) {
        RunLineQueueEx();
    }
    oldofs = GetCurrOffset();
    DebugMsg1(("InitializeArray(%s): current offset=%X\n", f->sym->name, oldofs ));

    /* If current item is a literal enclosed in <> or {}, just use this
     * item. Else, use all items until a comma or EOL is found.
     * Prior to v2.04, EvalOperand() was called in the latter case. This
     * was an error, because text macros might be found in the
     * expression - which makes the expression evaluator complain.
     */

    if ( tokenarray[i].token != T_STRING ||
         ( tokenarray[i].string_delim != '<' &&
           tokenarray[i].string_delim != '{' )) {
        DebugMsg1(("InitializeArray( %s ): i=%u token=%s\n", f->sym->name, i, tokenarray[i].string_ptr ));
        /* copy items until a comma or EOL is found. */
        for( j = i, lvl = 0; tokenarray[j].token != T_FINAL; j++ ) {
            if ( tokenarray[j].token == T_OP_BRACKET )
                lvl++;
            else if ( tokenarray[j].token == T_CL_BRACKET )
                lvl--;
#if 0
            /* v2.05: a "single item" should cause an error.
             * however, this can't be detected here, because the item
             * may be a text macro. This is to be fixed yet.
             */
            else if ( tokenarray[j].token == T_RES_ID && tokenarray[j].tokval == T_DUP )
                bArray = TRUE;
            else if ( tokenarray[j].token == T_STRING &&
                     ( tokenarray[j].string_delim == '"' ||
                     tokenarray[j].string_delim == '\'') &&
                     (f->sym->mem_type & MT_SPECIAL_MASK) == 0 )
                bArray = TRUE;
#endif
            else if ( lvl == 0 && tokenarray[j].token == T_COMMA )
                break;
        }
        lvl = tokenarray[j].tokpos - tokenarray[i].tokpos;
#if 0
        if ( bArray == FALSE )
            EmitErr( INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM, tokenarray[i].tokpos );
#endif
        *pi = j;
        /* v2.07: accept an "empty" quoted string as array initializer for byte arrays */
        if ( lvl == 2 &&
            f->sym->total_size == f->sym->total_length &&
            ( tokenarray[i].string_delim == '"' || tokenarray[i].string_delim == '\'' ) )
            ;
        else {
            //memcpy( buffer, tokenarray[i].tokpos, lvl );
            //buffer[lvl] = NULLC;
            c = *tokenarray[j].tokpos;
            *tokenarray[j].tokpos = NULLC;
            AddLineQueueX( "%s %s", f->initializer, tokenarray[i].tokpos );
            *tokenarray[j].tokpos = c;
            RunLineQueueEx();
        }
    } else {

        /* initializer is a literal */

        (*pi)++;

        /* if the string is empty, use the default initializer */
        if ( tokenarray[i].stringlen == 0 )
            AddLineQueueX( "%s %s", f->initializer, f->value );
        else
            AddLineQueueX( "%s %s", f->initializer, tokenarray[i].string_ptr );

        RunLineQueueEx();
    }

    /* the generated line has been assembled and the true size
     * of the array can be calculated now (may be 0).
     */

    j = GetCurrOffset() - oldofs ;
    DebugMsg1(("InitializeArray(%s): new offset=%X\n", f->sym->name, j + oldofs ));

    if ( j > f->sym->total_size ) {
        DebugMsg1(("InitializeArray(%s): error, j=%u total_size=%u\n", f->sym->name, j, f->sym->total_size ));
        EmitErr( TOO_MANY_INITIAL_VALUES_FOR_ARRAY, tokenarray[i].tokpos );
    } else if ( j < f->sym->total_size ) {
        char *filler = "0";
        DebugMsg1(("InitializeArray: remaining bytes=%lu\n", f->sym->total_size - j ));
        /* v2.07: if element size is 1 and a string is used as initial value,
         * pad array with spaces!
         */
        if ( f->sym->total_size == f->sym->total_length &&
            f->value && ( *f->value == '"' || *f->value == '\'' ) )
            filler = "' '";
        AddLineQueueX( "db %u dup (%s)",
                      f->sym->total_size - j,
                      CurrSeg && CurrSeg->e.seginfo->segtype != SEGTYPE_BSS ? filler : "?" );
    }

    DebugMsg1(("InitializeArray(%s) exit, curr ofs=%X\n", f->sym->name, GetCurrOffset() ));
    return( NOT_ERROR );
}

#define EXPINIT 0

/* initialize a STRUCT/UNION/RECORD data item
 * index:    index of start item of initializer string
 * symtype:  type of data item
 * embedded: is != NULL if proc is called recursively

 * currently this proc emits ASM lines with simple types
 * to actually "fill" the structure.

 * Since this function may be reentered, it's necessary to save/restore
 * global variable Token_Count.
 */
static ret_code InitStructuredVar( int index, struct asm_tok tokenarray[], const struct dsym *symtype, struct asym *embedded )
/****************************************************************************************************************************/
{
    //char            *ptr;
    struct field_item *f;
    int_32          nextofs;
    int             i;
    int             old_tokencount = Token_Count;
    char            *old_stringbufferend = StringBufferEnd;
    int             lvl;
#if AMD64_SUPPORT
    uint_64         dwRecInit;
#else
    uint_32         dwRecInit;
#endif
    bool            is_record_set;
    struct expr     opndx;

#ifdef DEBUG_OUT
#if FASTPASS
    if ( Parse_Pass > PASS_1 && UseSavedState ) {
        DebugMsg(("InitStructuredVar(%s): unexpectedly called in pass %u!!!!!\n", symtype->sym.name, Parse_Pass+1 ));
    }
#endif
    DebugMsg1(("InitStructuredVar(%s) enter, total=%" FU32 "/%" FU32 ", init=>%s<, embedded=%s, alignm=%u\n",
              symtype->sym.name, symtype->sym.total_size, symtype->sym.total_length, tokenarray[index].string_ptr, embedded ? embedded->name : "NULL", symtype->e.structinfo->alignment ));
#endif

    if ( tokenarray[index].token == T_STRING ) {
        /* v2.08: no special handling of {}-literals anymore */
        if ( tokenarray[index].string_delim != '<' &&
            tokenarray[index].string_delim != '{' ) {
            EmitError( MISSING_ANGLE_BRACKET_OR_BRACE_IN_LITERAL );
            return( ERROR );
        }
        i = Token_Count + 1;
        Token_Count = Tokenize( tokenarray[index].string_ptr, i, tokenarray, TOK_RESCAN );
        /* once Token_Count has been modified, don't exit without
         * restoring this value!
         */
        index++;
#if EXPINIT
        /* v2.05: expand the initialization string HERE!
         * the problem is that the string may contain macro function calls.
         * and it's crucial at what time the macro is evaluated.
         *
         * this doesn't fully work yet, but should be changed eventually.
         */
        ExpandLineItems( ptr, i, tokenarray, FALSE, FALSE );
#endif
    } else if ( embedded &&
                ( tokenarray[index].token == T_COMMA ||
                 tokenarray[index].token == T_FINAL)) {
        i = Token_Count;
    } else {
        EmitErr( INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM, embedded ? embedded->name : "" );
        return( ERROR );
    }

    /*
     * If embedded == NULL, then push the line queue.
     * If embedded != NULL, InitStructuredVar was directly reentered
     * due to an embedded structure/union within the structure. Then
     * RunLineQueue() is NOT called and therefore the queue needn't be
     * pushed.
     */
    if ( embedded == NULL ) {
        NewLineQueue();
    }

    if ( symtype->sym.typekind == TYPE_RECORD ) {
        dwRecInit = 0;
        is_record_set = FALSE;
    }

    ModuleInfo.StructInit++;
    /* scan the STRUCT/UNION/RECORD's members */
    for( f = symtype->e.structinfo->head; f != NULL; f = f->next ) {

        DebugMsg1(("InitStructuredVar field=%s ofs=%" FU32 " total_size=%" FU32 " total_length=%" FU32 " initializer=%s default=>%s<\n",
                  f->sym->name,
                  f->sym->offset,
                  f->sym->total_size,
                  f->sym->total_length,
                  f->initializer ? f->initializer : "NULL",
                  f->value ));

        /* is it a RECORD field? */
        if ( f->sym->mem_type == MT_BITS ) {
            if ( tokenarray[i].token == T_COMMA || tokenarray[i].token == T_FINAL ) {
                if ( f->value ) {
                    int j = Token_Count + 1;
                    int max_item = Tokenize( f->value, j, tokenarray, TOK_RESCAN );
                    EvalOperand( &j, tokenarray, max_item, &opndx, 0 );
                    is_record_set = TRUE;
                } else {
                    opndx.value = 0;
                    opndx.kind = EXPR_CONST;
                    opndx.quoted_string = NULL;
                }
            } else {
                EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 );
                is_record_set = TRUE;
            }
            if ( opndx.kind != EXPR_CONST || opndx.quoted_string != NULL )
                EmitError( CONSTANT_EXPECTED );
            if ( f->sym->total_size < 32 ) {
                uint_32 dwMax = (1 << f->sym->total_size);
                if ( opndx.value >= dwMax )
                    EmitErr( INITIALIZER_MAGNITUDE_TOO_LARGE, opndx.sym ? opndx.sym->name : "" );
            }
#if AMD64_SUPPORT
            dwRecInit |= opndx.llvalue << f->sym->offset;
#else
            dwRecInit |= opndx.value << f->sym->offset;
#endif

        } else if ( f->initializer == NULL ) {  /* embedded struct? */

            InitStructuredVar( i, tokenarray, (struct dsym *)f->sym->type, f->sym );
            if ( tokenarray[i].token == T_STRING )
                i++;

        } else if ( f->sym->isarray &&
                    tokenarray[i].token != T_FINAL &&
                    tokenarray[i].token != T_COMMA ) {
            if ( ERROR == InitializeArray( f, &i, tokenarray ) )
                break;

        } else if ( f->sym->total_size == f->sym->total_length &&
                   tokenarray[i].token == T_STRING &&
                   tokenarray[i].stringlen > 1 &&
                   ( tokenarray[i].string_delim == '"' ||
                    tokenarray[i].string_delim == '\'' ) ) {
            /* v2.07: it's a byte type, but no array, string initializer must have true length 1 */
            EmitError( STRING_OR_TEXT_LITERAL_TOO_LONG );
            i++;
        } else {
            if ( tokenarray[i].token == T_FINAL || tokenarray[i].token == T_COMMA ) {
                AddLineQueueX( "%s %s", f->initializer, f->value );
            } else {
                char c;
                int j = i;
                /* ignore commas enclosed in () */
                /* might be a good idea to check the items:
                 * if DUP is found, call InitializeArray()
                 */
                for ( lvl = 0, c = 0; tokenarray[i].token != T_FINAL; i++ ) {
                    if ( tokenarray[i].token == T_OP_BRACKET )
                        lvl++;
                    else if ( tokenarray[i].token == T_CL_BRACKET )
                        lvl--;
                    else if ( lvl == 0 && tokenarray[i].token == T_COMMA )
                        break;
                    else if ( tokenarray[i].token == T_RES_ID && tokenarray[i].tokval == T_DUP )
                        c++; /* v2.08: check added */
                }
                if ( c ) {
                    EmitErr( INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM, tokenarray[j].tokpos );
                    goto next_member;
                }
                c = *tokenarray[i].tokpos;
                *tokenarray[i].tokpos = NULLC;
                AddLineQueueX( "%s %s", f->initializer, tokenarray[j].tokpos );
                *tokenarray[i].tokpos = c;
            }
        }
    next_member:
        /* Add padding bytes if necessary (never inside RECORDS!).
         * f->next == NULL : it's the last field of the struct/union/record
         */
        if ( symtype->sym.typekind != TYPE_RECORD ) {
            if ( f->next == NULL || symtype->sym.typekind == TYPE_UNION )
                nextofs = symtype->sym.total_size;
            else
                nextofs = f->next->sym->offset;

            if ( f->sym->offset + f->sym->total_size < nextofs ) {
                DebugMsg1(("InitStructuredVar: padding, field=%s ofs=%" FX32 " total=%" FX32 " nextofs=%" FX32 "\n",
                          f->sym->name, f->sym->offset, f->sym->total_size, nextofs ));
                AddLineQueueX( "db %u dup (?) ;padding",
                        nextofs - (f->sym->offset + f->sym->total_size) );
            }
        }
        /* for a union, just the first field is initialized */
        if ( symtype->sym.typekind == TYPE_UNION )
            break;

        if ( f->next != NULL ) {

            if ( tokenarray[i].token != T_FINAL )
                if ( tokenarray[i].token == T_COMMA )
                    i++;
                else {
                    EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
                    while ( tokenarray[i].token != T_FINAL && tokenarray[i].token != T_COMMA)
                        i++;
                }
        }
    }  /* end for */

    if ( symtype->sym.typekind == TYPE_RECORD ) {
        short ddir;
        switch ( symtype->sym.mem_type ) {
        case MT_BYTE: ddir = T_DB; break;
        case MT_WORD: ddir = T_DW; break;
#if AMD64_SUPPORT
        case MT_QWORD: ddir = T_DQ; break;
#endif
        default:      ddir = T_DD;
        }
#if AMD64_SUPPORT
        /* AddLineQueueX() can't handle 64-bit numbers */
        if ( ddir == T_DQ ) {
            char buffer[32];
            sprintf( buffer, "%0" I64x_SPEC "h", dwRecInit );
            AddLineQueueX( is_record_set ? "%r %s" : "%r ?", ddir, buffer );
        } else
            AddLineQueueX( is_record_set ? "%r 0%xh" : "%r ?", ddir, (uint_32)dwRecInit );
#else
        AddLineQueueX( is_record_set ? "%r 0%xh" : "%r ?", ddir, dwRecInit );
#endif
    }

    if ( tokenarray[i].token != T_FINAL ) {
        DebugMsg1(("InitStructuredVar(%s): error, i=%u token=%s\n", symtype->sym.name, i, tokenarray[i].string_ptr ));
        EmitErr( TOO_MANY_INITIAL_VALUES_FOR_STRUCTURE, tokenarray[i].tokpos );
    }

    /* restore token status before RunLineQueueEx(), to minimize mem usage */
    Token_Count = old_tokencount;
    StringBufferEnd = old_stringbufferend;

    /* now run the generated code if embedded is NULL ... and
     if at least one line has been generated */

    if ( ( embedded == NULL ) && is_linequeue_populated() )
        RunLineQueueEx();

    ModuleInfo.StructInit--;

    DebugMsg1(("InitStructuredVar(%s) exit, current ofs=%" FX32 "\n", symtype->sym.name, GetCurrOffset() ));

    return( NOT_ERROR );
}

/*
 * convert a string into little endian format - ( LSB 1st, LSW 1st ... etc ).
 * <len> is the TYPE, may be 2,4,6,8,10?,16
 */
static char *little_endian( const char *src, unsigned len )
/*********************************************************/
{
    /* v2.06: input and output buffer must be different! */
    char *dst = StringBufferEnd;

    for( ; len > 1; dst++, src++, len-- ) {
        len--;
        *dst = *(src + len);
        *(dst + len) = *src;
    }
    if ( len )
        *dst = *src;

    return( StringBufferEnd );
}

#if 0 /* changed in v2.03 */
static void OutputDataBytes( const unsigned char *p, int len )
/************************************************************/
{
    for( ; len; len-- )
        OutputByte( *p++ );
}
#else
#define OutputDataBytes( x, y ) OutputBytes( x, y, NULL )
#endif

/* convert hex numbers with "real number designator" to binary.
 * the tokenizer stores just the string.
 */

static void hex2float( void *out, const char *inp, int size )
/***********************************************************/
{
    int val;
    int i;
    int j;
    char *p = out;
    const char *char_ptr = inp;

    memset( (char *)out, 0, size );
    for ( i = 0;
         *char_ptr != NULLC && *char_ptr != 'r';
          char_ptr++, i++ ) {
        if( *char_ptr <= '9' )
            val = *char_ptr - '0';
        else
            val = tolower( *char_ptr ) - 'a' + 10;
        for ( j = 0, p = out; j < size; j++ ) {
            val += *p * 16;
            *(p++) = val;
            val >>= 8;
        }
    }
}

void atofloat( void *out, const char *inp, unsigned size, bool negative, uint_8 ftype )
/*************************************************************************************/
{
    //const char *inp;
    double  double_value;
    float   float_value;

    /* v2.04: accept and handle 'real number designator' */
    if ( ftype ) {
        hex2float( out, inp, size );
    } else {
        switch ( size ) {
        case 4:
            double_value = strtod( inp, NULL );
            /* v2.06: check added */
            if ( double_value > FLT_MAX )
                EmitErr( MAGNITUDE_TOO_LARGE_FOR_SPECIFIED_SIZE );
            if( negative )
                double_value *= -1;
            float_value = double_value;
            *(float *)out = float_value;
            break;
        case 8:
            double_value = strtod( inp, NULL );
            if( negative )
                double_value *= -1;
            *(double *)out = double_value;
            break;
        case 10:
            strtotb( inp, (struct TB_LD *)out, negative );
            break;
        default:
            /* sizes != 4,8 or 10 aren't accepted.
             * Masm ignores silently, JWasm also unless -W4 is set.
             */
            if ( Parse_Pass == PASS_1 )
                EmitWarn( 4, FP_INITIALIZER_IGNORED );
            memset( (char *)out, 0, size );
        }
    }
    return;
}

static void output_float( const struct expr *opnd, unsigned size )
/****************************************************************/
{
    /* v2.07: buffer extended to max size of a data item (=32).
     * test case: XMMWORD REAL10 ptr 1.0
     */
    //char buffer[12];
    char buffer[32];

    if ( opnd->mem_type != MT_EMPTY ) {
        memset( buffer, 0, sizeof( buffer ) );
        atofloat( buffer, opnd->float_tok->string_ptr, SizeFromMemtype( opnd->mem_type, USE_EMPTY, NULL ), opnd->negative, opnd->float_tok->floattype );
    } else {
        atofloat( buffer, opnd->float_tok->string_ptr, size, opnd->negative, opnd->float_tok->floattype );
    }
    OutputDataBytes( buffer, size );
    return;
}

/* update a symbols's total_length (operator LENGTHOF) and
 * first_length (operator LENGTH) fields.
 * Called in Pass 1 only.
 */

static void update_sizes( struct asym *sym, bool first, uint_32 size )
/********************************************************************/
{
    sym->total_length++;
    sym->total_size += size;
    if( first ) {
        sym->first_length++;
        sym->first_size += size;
    }
}

/*
 * initialize a data item or structure field;
 * - sym:
 *   for data items: label (may be NULL)
 *   for STRUCT declarations: field/member (is never NULL)
 * - start_pos: pointer to token array index [in/out]
 * - tokenarray[]: token array
 * - struct_sym: type of label/field if item is a STRUCT/UNION/RECORD, otherwise NULL.
 * - no_of_bytes: size of type
 * - dup: array size if called by DUP operator, otherwise 1
 * - struct_field: 1=inside a STRUCT declaration
 *
 * the symbol will have its 'isarray' flag set if any of the following is true:
 * 1. at least 2 items separated by a comma are used for initialization
 * 2. the DUP operator occures
 * 3. item size is 1 and a quoted string with len > 1 is used as initializer
 */

static ret_code data_item( int *start_pos, struct asm_tok tokenarray[], struct asym *sym, const struct dsym *struct_sym, uint_32 no_of_bytes, uint_32 dup, bool struct_field, bool float_initializer, bool first )
/****************************************************************************************************************************************************************************************************************/
{
    int                 i;
    int                 string_len;
#if FASTPASS
    bool                firstitem = TRUE;
#endif
    bool                initwarn = FALSE;
    //unsigned int        count;
    uint_8              *pchar;
    char                tmp;
    enum fixup_types    fixup_type;
    struct fixup        *fixup;
    struct expr         opndx;

    DebugMsg1(("data_item( label=%s, type=%s, pos=%d, size=%" FU32 ", dup=%" FX32 "h, struct=%u ) enter\n",
               sym ? sym->name : "NULL",
               struct_sym ? struct_sym->sym.name : "NULL",
               *start_pos, no_of_bytes, dup, struct_field ));

    if ( sym )
        sym->isdata = TRUE;

    for ( ; dup; dup-- ) {
    i = *start_pos;
next_item:
    /* since v1.94, the expression evaluator won't handle strings
     * enclosed in <> or {}. That is, in previous versions syntax
     * "mov eax,<1>" was accepted, now it's rejected.
     */
    if ( tokenarray[i].token == T_STRING && ( tokenarray[i].string_delim == '<'  || tokenarray[i].string_delim == '{' ) ) {
        if( struct_sym != NULL ) {
            DebugMsg1(("data_item(%s): literal/brace found: >%s<, struct_field=%u, no_of_bytes=%" FU32 ", curr_ofs=%" FX32 "\n",
                       struct_sym->sym.name, tokenarray[i].string_ptr, struct_field, no_of_bytes, GetCurrOffset()));
            /* it's either a real data item - then struct_field is FALSE -
             or a structure FIELD of arbitrary type */
            if( struct_field == FALSE ) {
#if FASTPASS
                DebugMsg1(("data_item: calling InitStructuredVar(%s, %u), firstitem=%u\n", struct_sym->sym.name, i, firstitem ));
                if ( Parse_Pass == PASS_1 ) {
                    /* v2.05: severe error: ignore firstitem if first == 0! */
                    //if ( firstitem ) {
                    if ( first && firstitem ) {
                        /* "remove" the current line" */
                        firstitem = FALSE;
                        *LineStoreCurr->line = ';';
                        LstWriteSrcLine();
                        /* CurrSource holds the source line, which is not
                         * to reach pass 2
                         */
                        *tokenarray[0].tokpos = ';';  /* v2.08: was CurrSource */
                    }
                    if ( sym && sym->first_length == 0 ) {
                        sprintf( StringBufferEnd, "%s label %s", sym->name, struct_sym->sym.name );
                        StoreLine( StringBufferEnd, list_pos, 0 );
                        if ( Options.preprocessor_stdout == TRUE )
                            WritePreprocessedLine( StringBufferEnd );
                    }
                }
#endif
                if ( InitStructuredVar( i, tokenarray, struct_sym, NULL ) == ERROR )
                    return( ERROR );
            }

            if( sym && Parse_Pass == PASS_1 )
                update_sizes( sym, first, no_of_bytes );
            i++;
            goto item_done;
        }
#if 0 /* just let EvalOperand() emit 'Unexpected literal...' error */
        else {
            DebugMsg(("data_item: invalid initializer for structure >%s<\n", tokenarray[i].tokpos ));
            //EmitError( INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM );
            EmitErr( INVALID_DATA_INITIALIZER );
            return( ERROR );
        }
#endif
    }

    if ( tokenarray[i].token == T_QUESTION_MARK )
        opndx.kind = EXPR_EMPTY;
    else
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
            return( ERROR );

    //DebugMsg(("data_item, EvalOperand() returned, opndx.kind=%u\n", opndx.kind ));

    /* handle DUP operator */

    if ( tokenarray[i].token == T_RES_ID && tokenarray[i].tokval == T_DUP ) {
        /* v2.03: db 'AB' dup (0) is valid syntax! */
        //if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
        if ( opndx.kind != EXPR_CONST ) {
            DebugMsg(("data_item, error, opndx.kind=%u, opndx.string=%X\n", opndx.kind, opndx.quoted_string));
            EmitError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        /* max dup is 0x7fffffff */
        if ( opndx.value < 0 ) {
            EmitError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            return( ERROR );
        }
        i++;
        if( tokenarray[i].token != T_OP_BRACKET ) {
            DebugMsg(("data_item error, missing '('\n"));
            EmitErr( EXPECTED, "(" );
            return( ERROR );
        }
        i++;

        if ( sym )
            sym->isarray = TRUE;

        if ( opndx.value == 0 ) {
            int level = 1;
            for ( ; tokenarray[i].token != T_FINAL; i++ ) {
                if ( tokenarray[i].token == T_OP_BRACKET )
                    level++;
                else if ( tokenarray[i].token == T_CL_BRACKET )
                    level--;
                if ( level == 0 )
                    break;
            }
        } else {
            DebugMsg1(("data_item(%s): op DUP, count=%" FX32 "h, calling data_item()\n", sym ? sym->name : "NULL", opndx.uvalue ));
            if ( data_item( &i, tokenarray, sym, struct_sym, no_of_bytes, opndx.uvalue, struct_field, float_initializer, first ) == ERROR ) {
                DebugMsg(("data_item(%s): op DUP, count=%" FX32 "h, returned with error\n", sym ? sym->name : "NULL", opndx.uvalue ));
                return( ERROR );
            }
        }
        if( tokenarray[i].token != T_CL_BRACKET ) {
            DebugMsg(("data_item: error 'missing ')', exit\n"));
            EmitErr( EXPECTED, ")" );
            return( ERROR );
        }
        i++;
    } else {
        /* a STRUCT/UNION/RECORD data item needs a literal as initializer */
        /* v2.06: changed */
        //if( struct_sym != NULL && struct_field == FALSE ) {
        if( struct_sym != NULL && struct_sym->sym.typekind != TYPE_TYPEDEF ) {
            EmitErr( STRUCTURE_IMPROPERLY_INITIALIZED, struct_sym->sym.name );
            return( ERROR );
        }

        /* handle '?' */
        if ( opndx.kind == EXPR_EMPTY && tokenarray[i].token == T_QUESTION_MARK ) {
            DebugMsg1(("data_item: ? found, curr_ofs=%X\n", GetCurrOffset()));
            opndx.uvalue = no_of_bytes;
            /* tiny optimization for uninitialized arrays */
            if ( tokenarray[i+1].token != T_COMMA && i == *start_pos ) {
                opndx.uvalue *= dup;
                if( sym && Parse_Pass == PASS_1 ) {
                    sym->total_length += dup;
                    sym->total_size += opndx.uvalue;
                    if( first ) {
                        sym->first_length += dup;
                        sym->first_size += opndx.uvalue;
                    }
                }
                dup = 1; /* force loop exit */
            } else {
                if( sym && Parse_Pass == PASS_1 )
                    update_sizes( sym, first, opndx.uvalue );
            }
            if( !struct_field ) {
                SetCurrOffset( CurrSeg, opndx.uvalue, TRUE, TRUE );
            }
            i++;
            goto item_done;
        }

        /* warn about initialized data in BSS/AT segments */
        if ( Parse_Pass == PASS_2 &&
            struct_field == FALSE  &&
            // CurrSeg != NULL &&  /* this is already ensured to be true */
            (CurrSeg->e.seginfo->segtype == SEGTYPE_BSS ||
             CurrSeg->e.seginfo->segtype == SEGTYPE_ABS) &&
            initwarn == FALSE ) {
            EmitWarn( 2,
                    INITIALIZED_DATA_NOT_SUPPORTED_IN_SEGMENT,
                    (CurrSeg->e.seginfo->segtype == SEGTYPE_BSS) ? "BSS" : "AT" );
            initwarn = TRUE;
        };
        switch( opndx.kind ) {
        case EXPR_EMPTY:
            DebugMsg(("data_item.EMPTY: idx=%u, tokenarray.token=%X\n", i, tokenarray[i].token));
            if ( tokenarray[i].token != T_FINAL )
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            else
                EmitError( SYNTAX_ERROR );
            return( ERROR );
        case EXPR_FLOAT:
            DebugMsg1(("data_item.FLOAT: >%s<, struct_field=%u, no_of_bytes=%u, curr_ofs=%X\n",
                       opndx.float_tok->string_ptr, struct_field, no_of_bytes, GetCurrOffset()));
            if (!struct_field)
                output_float( &opndx, no_of_bytes );
            if( sym && Parse_Pass == PASS_1 ) {
                update_sizes( sym, first, no_of_bytes );
            }
            break;
        case EXPR_CONST:
            if ( float_initializer ) {
                EmitError( MUST_USE_FLOAT_INITIALIZER );
                return( ERROR );
            }

            /* a string returned by the evaluator (enclosed in quotes!)? */

            if ( opndx.quoted_string ) {
                DebugMsg1(("data_item.CONST: string found: >%s<, struct_field=%u, no_of_bytes=%u, curr_ofs=%X\n", opndx.quoted_string, struct_field, no_of_bytes, GetCurrOffset()));
                pchar = (uint_8 *)opndx.quoted_string->string_ptr + 1;
                string_len = opndx.quoted_string->stringlen; /* this is the length without quotes */

                /* v2.07: check for empty string for ALL types */
                if ( string_len == 0 ) {
                    if ( struct_field ) {
                        /* no error for struct field */
                        /* v2.07: don't modify string_len! Instead
                         * mark field as array!
                         */
                        //string_len = 1;
                        sym->isarray = TRUE;
                    } else {
                        EmitError( EMPTY_STRING ); /* MASM doesn't like "" */
                        return( ERROR );
                    }
                }
                /* a string is only regarded as an array if item size is 1 */
                /* else it is regarded as ONE item */
                if( no_of_bytes != 1 ) {
                    if( string_len > no_of_bytes ) {
                        EmitError( INITIALIZER_OUT_OF_RANGE );
                        return( ERROR );
                    }
                }

                if( sym && Parse_Pass == PASS_1 && string_len > 0 ) {
                    update_sizes( sym, first, no_of_bytes );
                    if ( no_of_bytes == 1 && string_len > 1 ) {
                        int j;
                        sym->isarray = TRUE; /* v2.07: added */
                        for ( j = string_len-1; j; j-- )
                            update_sizes( sym, FALSE, no_of_bytes );
                    }
                }

                if( !struct_field ) {
                    /* anything bigger than a byte must be stored in little-endian
                     * format -- LSB first */
                    if ( string_len > 1 && no_of_bytes > 1 )
                        pchar = little_endian( (const char *)pchar, string_len );
                    OutputDataBytes( pchar, string_len );
                    if ( no_of_bytes > string_len )
                        FillDataBytes( 0, no_of_bytes - string_len );
                }
            } else {
                /* it's NOT a string */
                DebugMsg1(("data_item.CONST: const found, value=%" FX32 "h, no_of_bytes=%u, curr_ofs=%" FX32 "\n", opndx.value, no_of_bytes, GetCurrOffset()));
                if( !struct_field ) {
                    /* the evaluator cannot handle types with size > 16.
                     * so if a simple type is larger (can't happen currently),
                     * clear anything which is above.
                     */
                    if ( no_of_bytes > 16 ) {
                        OutputDataBytes( opndx.chararray, 16 );
                        tmp = ( opndx.chararray[15] < 0x80 ? 0 : 0xFF );
                        FillDataBytes( tmp, no_of_bytes - 16 );
                    } else {
                        /* v2.06: TBYTE/OWORD/XMMWORD: extend a negative value to 16-byte */
                        if ( no_of_bytes > sizeof( int_64 ) ) {
                            if ( opndx.negative && opndx.value64 < 0 && opndx.hlvalue == 0 )
                                opndx.hlvalue = -1;
                        }
                        OutputDataBytes( opndx.chararray, no_of_bytes );
                        /* check that there's no significant data left
                         * which hasn't been emitted.
                         */
                        /* v2.06: rewritten, now more rigid and checks
                         * 1-8 and 10 bytes instead of just 1-4.
                         */
                        if ( no_of_bytes <= sizeof( int_64 ) ) {
                            tmp = ( opndx.chararray[7] < 0x80 ? 0 : 0xFF );
                            memset( opndx.chararray, tmp, no_of_bytes );
                            if ( opndx.llvalue != 0 && opndx.llvalue != -1 ) {
                                DebugMsg(("data_item.CONST: error, unhandled data is %" I64X_SPEC "_%016" I64X_SPEC "\n", opndx.hlvalue, opndx.llvalue));
                                EmitErr( INITIALIZER_MAGNITUDE_TOO_LARGE, opndx.sym ? opndx.sym->name : "" );
                                return( ERROR );
                            }
                        } else if ( no_of_bytes == 10 ) {
                            //if ( opndx.hlvalue > 0xffff ) {
                            if ( opndx.hlvalue > 0xffff && opndx.hlvalue < -0xffff ) {
                                EmitErr( INITIALIZER_MAGNITUDE_TOO_LARGE, opndx.sym ? opndx.sym->name : "" );
                                return( ERROR );
                            }
                        }
                    }
                }
                if( sym && Parse_Pass == PASS_1 ) {
                    update_sizes( sym, first, no_of_bytes );
                }
            }
            break;
        case EXPR_ADDR:
            /* since a fixup will be created, 8 bytes is max */
            if ( no_of_bytes > sizeof(uint_64) ) {
                EmitError( INVALID_DATA_INITIALIZER );
                break;
            }
#if AMD64_SUPPORT
            if ( ModuleInfo.Ofssize != USE64 )
#endif
                if ( opndx.hvalue && ( opndx.hvalue != -1 || opndx.value >= 0 ) ) {
                    /* v2.05: compared to Masm, the above is too restrictive.
                     * the line below might be better.
                     */
                    //if ( opndx.hvalue != 0 && ( opndx.hvalue != -1 || opndx.value == 0 ) ) {
                    DebugMsg(("data_item.ADDR: displacement doesn't fit in 32 bits: %" I64X_SPEC "\n", opndx.value64 ));
                    EmitConstError( &opndx );
                    return( ERROR );
                }

            /* indirect addresses (incl. stack variables) are invalid */
            if ( opndx.indirect == TRUE ) {
                DebugMsg(("data_item.ADDR: error, indirect=%u, sym=%X\n", opndx.indirect, opndx.sym ));
                EmitError( INVALID_USE_OF_REGISTER );
                break;
            }
            if ( float_initializer ) {
                DebugMsg(("data_item.ADDR: error, float_initializer=%u\n", float_initializer ));
                EmitError( MUST_USE_FLOAT_INITIALIZER );
                break;
            }

            if( sym && Parse_Pass == PASS_1 ) {
                update_sizes( sym, first, no_of_bytes );
            }
            /* for STRUCT fields, don't emit anything! */
            if ( struct_field ) {
                break;
            }

            /* determine what type of fixup is to be created */

            switch ( opndx.instr ) {
            case T_SEG:
                if ( no_of_bytes < 2 ) {
                    DebugMsg(("data_item.ADDR: error, a SEG wont fit in a BYTE\n" ));
                    EmitError( MAGNITUDE_TOO_LARGE_FOR_SPECIFIED_SIZE );
                }
                fixup_type = FIX_SEG;
                break;
            case T_OFFSET:
                switch ( no_of_bytes ) {
                case 1:
                    DebugMsg(("data_item.ADDR: error, a offset wont fit in a BYTE\n" ));
                    EmitError( OFFSET_MAGNITUDE_TOO_LARGE );
                    fixup_type = FIX_OFF8;
                    break;
                case 2:
                    fixup_type = FIX_OFF16;
                    break;
#if AMD64_SUPPORT
                case 8:
                    if ( ModuleInfo.Ofssize == USE64 ) {
                        fixup_type = FIX_OFF64;
                        break;
                    }
#endif
                default:
                    if ( opndx.sym && ( GetSymOfssize(opndx.sym) == USE16 ) )
                        fixup_type = FIX_OFF16;
                    else
                        fixup_type = FIX_OFF32;
                    break;
                }
                break;
#if IMAGERELSUPP
            case T_IMAGEREL:
                if ( no_of_bytes < sizeof(uint_32) ) {
                    DebugMsg(("data_item.ADDR: IMAGEREL, error, size=%u (should be 4)\n", no_of_bytes ));
                    EmitError( OFFSET_MAGNITUDE_TOO_LARGE );
                }
                fixup_type = FIX_OFF32_IMGREL;
                break;
#endif
#if SECTIONRELSUPP
            case T_SECTIONREL:
                if ( no_of_bytes < sizeof(uint_32) ) {
                    DebugMsg(("data_item.ADDR: SECTIONREL, error, size=%u (should be 4)\n", no_of_bytes ));
                    EmitError( OFFSET_MAGNITUDE_TOO_LARGE );
                }
                fixup_type = FIX_OFF32_SECREL;
                break;
#endif
            case T_LOW:
                fixup_type = FIX_OFF8; /* OMF, BIN + GNU-ELF only */
                break;
            case T_HIGH:
                DebugMsg(("data_item.ADDR: HIGH detected\n"));
                fixup_type = FIX_HIBYTE; /* OMF only */
                break;
            case T_LOWWORD:
                fixup_type = FIX_OFF16;
                if ( no_of_bytes < 2 ) {
                    EmitError( MAGNITUDE_TOO_LARGE_FOR_SPECIFIED_SIZE );
                    break;
                }
                break;
#if LOHI32
            case T_HIGH32:
                /* no break */
#endif
            case T_HIGHWORD:
                fixup_type = FIX_VOID;
                EmitError( CONSTANT_EXPECTED );
                break;
#if LOHI32
            case T_LOW32:
                fixup_type = FIX_OFF32;
                if ( no_of_bytes < 4 ) {
                    EmitError( MAGNITUDE_TOO_LARGE_FOR_SPECIFIED_SIZE );
                    break;
                }
                break;
#endif
            default:
                /* size < 2 can work with T_LOW|T_HIGH operator only */
                if ( no_of_bytes < 2 ) {
                    /* forward reference? */
                    if ( Parse_Pass == PASS_1 && opndx.sym && opndx.sym->state == SYM_UNDEFINED )
                        ;
                    else {
                        /* v2.08: accept 1-byte absolute externals */
                        if ( opndx.sym && opndx.sym->state == SYM_EXTERNAL && opndx.abs == TRUE ) {
                        } else {
                            DebugMsg(("data_item.ADDR: error, no of bytes=%u\n", no_of_bytes ));
                            EmitError( MAGNITUDE_TOO_LARGE_FOR_SPECIFIED_SIZE );
                        }
                        fixup_type = FIX_OFF8;
                        break;
                    }
                }
                /* if the symbol references a segment or group,
                 then generate a segment fixup.
                 */
                if ( opndx.sym && (opndx.sym->state == SYM_SEG || opndx.sym->state == SYM_GRP ) ) {
                    fixup_type = FIX_SEG;
                    break;
                }

                switch ( no_of_bytes ) {
                case 2:
                    /* accept "near16" override, else complain
                     * if symbol's offset is 32bit */
                    /* v2.06: if condition changed */
                    //if ( opndx.explicit == TRUE && opndx.mem_type == MT_NEAR && opndx.Ofssize == USE16 )
                    if ( opndx.explicit == TRUE ) {
                        if ( SizeFromMemtype( opndx.mem_type, opndx.Ofssize, opndx.type ) > no_of_bytes ) {
                            DebugMsg(("data_item.ADDR: error, memtype %X wont fit in a WORD\n", opndx.mem_type));
                            EmitErr( INITIALIZER_MAGNITUDE_TOO_LARGE, opndx.sym ? opndx.sym->name : "" );
                        };
                    } else if ( opndx.sym && opndx.sym->state == SYM_EXTERNAL && opndx.abs == TRUE ) {
                        /* v2.07a: accept ABSolute externals (regression in v2.07) */
                    } else if ( opndx.sym &&
                             opndx.sym->state != SYM_UNDEFINED &&
                             ( GetSymOfssize(opndx.sym) > USE16 ) ) {
                        DebugMsg(("data_item.ADDR: error, a 32bit offset (%s) wont fit in a WORD\n", opndx.sym->name));
                        EmitErr( INITIALIZER_MAGNITUDE_TOO_LARGE, opndx.sym ? opndx.sym->name : "" );
                    }
                    fixup_type = FIX_OFF16;
                    break;
                case 4:
                    /* masm generates:
                     * off32 if curr segment is 32bit,
                     * ptr16 if curr segment is 16bit,
                     * and ignores type overrides.
                     * if it's a NEAR external, size is 16, and
                     * format isn't OMF, error 'symbol type conflict'
                     * is displayed
                     */
                    if ( opndx.explicit == TRUE ) {
                        if ( opndx.mem_type == MT_FAR ) {
                            if ( opndx.Ofssize != USE_EMPTY && opndx.Ofssize != USE16 ) {
                                DebugMsg(("data_item.ADDR: error, FAR32 won't fit in a DWORD\n" ));
                                EmitErr( INITIALIZER_MAGNITUDE_TOO_LARGE, opndx.sym ? opndx.sym->name : "" );
                            }
                            fixup_type = FIX_PTR16;
                        } else if ( opndx.mem_type == MT_NEAR ) {
                            if ( opndx.Ofssize == USE16 )
                                fixup_type = FIX_OFF16;
                            else if ( opndx.sym && ( GetSymOfssize( opndx.sym ) == USE16 ) )
                                fixup_type = FIX_OFF16;
                            else
                                fixup_type = FIX_OFF32;
                        }
                    } else {
                        /* what's done if code size is 16 is Masm-compatible.
                         * It's not very smart, however.
                         * A better strategy is to choose fixup type depending
                         * on the symbol's offset size.
                         */
                        //if ( opndx.sym && ( GetSymOfssize( opndx.sym ) == USE16 ) )
                        if ( ModuleInfo.Ofssize == USE16 )
#if COFF_SUPPORT || ELF_SUPPORT
                            if ( opndx.mem_type == MT_NEAR &&
                                ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
                                 || Options.output_format == OFORMAT_ELF
#endif
                                )) {
                                fixup_type = FIX_OFF16;
                                EmitErr( SYMBOL_TYPE_CONFLICT, sym->name );
                            } else
#endif
                                fixup_type = FIX_PTR16;
                        else
                            fixup_type = FIX_OFF32;
                    }
                    break;
                case 6:
                    /* Masm generates a PTR32 fixup in OMF!
                     * and a DIR32 fixup in COFF.
                     */
                    /* COFF/ELF has no far fixups */
#if COFF_SUPPORT || ELF_SUPPORT
                    if ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
                        || Options.output_format == OFORMAT_ELF
#endif
                       ) {
                        fixup_type = FIX_OFF32;
                    } else
#endif
                        fixup_type = FIX_PTR32;
                    break;
                default:
                    /* Masm generates
                     * off32 if curr segment is 32bit
                     * ptr16 if curr segment is 16bit
                     * JWasm additionally accepts a FAR32 PTR override
                     * and generates a ptr32 fixup then */
                    if ( opndx.explicit == TRUE && opndx.mem_type == MT_FAR && opndx.Ofssize == USE32 )
                        fixup_type = FIX_PTR32;
                    else if( ModuleInfo.Ofssize == USE32 )
                        fixup_type = FIX_OFF32;
#if AMD64_SUPPORT
                    else if( ModuleInfo.Ofssize == USE64 )
                        fixup_type = FIX_OFF64;
#endif
                    else
                        fixup_type = FIX_PTR16;
                }
                break;
            }
            /* v2.07: fixup type check moved here */
            if ( ( 1 << fixup_type ) & ModuleInfo.fmtopt->invalid_fixup_type ) {
                EmitErr( UNSUPPORTED_FIXUP_TYPE,
                       ModuleInfo.fmtopt->formatname,
                       opndx.sym ? opndx.sym->name : szNull );
                return( ERROR );
            }
            fixup = NULL;
            if ( write_to_file ) {
                /* there might be a segment override:
                 * a segment, a group or a segment register.
                 * Init var SegOverride, it's used inside find_frame()
                 */
                SegOverride = NULL;
                segm_override( &opndx, NULL );

                /* set global vars Frame and Frame_Datum */
                /* opndx.sym may be NULL, then SegOverride is set. */
                if ( ModuleInfo.offsettype == OT_SEGMENT &&
                    ( opndx.instr == T_OFFSET || opndx.instr == T_SEG ))
                    find_frame2( opndx.sym );
                else
                    find_frame( opndx.sym );
                /* uses Frame and Frame_Datum  */
                fixup = CreateFixup( opndx.sym, fixup_type, OPTJ_NONE );
                //store_fixup( fixup, &opndx.value ); /* may fail, but ignore error! */
            }
            OutputBytes( (unsigned char *)&opndx.value, no_of_bytes, fixup );
            break;
        case EXPR_REG:
            EmitError( INVALID_USE_OF_REGISTER );
            break;
        default: /* unknown opndx.kind, shouldn't happen */
            DebugMsg(("data_item: error, opndx.kind=%u\n", opndx.kind ));
            EmitError( SYNTAX_ERROR );
            return( ERROR );
        } /* end switch (opndx.kind */
    }
item_done:
    if( tokenarray[i].token == T_COMMA ) {
        i++;
        if ( tokenarray[i].token != T_FINAL &&
            tokenarray[i].token != T_CL_BRACKET ) {
            first = FALSE;
            if ( sym )
                sym->isarray = TRUE;
            goto next_item;
        }
    }

    } /* end for */

    *start_pos = i;
    DebugMsg1(("data_item: exit, no error, i=%d\n", i));
    return( NOT_ERROR );
}

static ret_code checktypes( const struct asym *sym, enum memtype mem_type, const struct dsym *struct_sym )
/********************************************************************************************************/
{
    /* for EXTERNDEF, check type changes */
    if ( sym->mem_type != MT_EMPTY ) {
        enum memtype mem_type2 = sym->mem_type;
        const struct asym *tmp;
        /* skip alias types */
        tmp = (struct asym *)struct_sym;
        while ( mem_type == MT_TYPE ) {
            mem_type = tmp->mem_type;
            tmp = tmp->type;
        }
        tmp = sym;
        while ( mem_type2 == MT_TYPE ) {
            mem_type2 = tmp->mem_type;
            tmp = tmp->type;
        }
        if ( mem_type2 != mem_type ) {
            DebugMsg(("checktypes: memtype conflict: %u - %u\n", mem_type2, mem_type ));
            EmitErr( SYMBOL_TYPE_CONFLICT, sym->name );
            return( ERROR );
        }
    }
    return( NOT_ERROR );
}
/*
 * parse data initialization assembly line:
 * [label] simple|arbitrary type initializer,...
 * i: pos of directive (DB,DW,...) or of type (predef or userdef)
 * type_sym: userdef type or NULL
 */

ret_code data_dir( int i, struct asm_tok tokenarray[], struct dsym *type_sym )
/****************************************************************************/
{
    uint_32             no_of_bytes;
    struct asym         *sym = NULL;
    uint                old_offset;
    uint                currofs; /* for LST output */
    enum memtype        mem_type;
    bool                float_initializer = FALSE;
    int                 idx;
    char                *name;

    DebugMsg1(("data_dir( i=%u, type=%s ) enter\n", i, type_sym ? type_sym->sym.name : "NULL" ));

    /* v2.05: the previous test in parser.c wasn't fool-proved */
    if ( i > 1 && ModuleInfo.m510 == FALSE ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    if( tokenarray[i+1].token == T_FINAL ) {
        DebugMsg(("data_dir: missing initializer\n"));
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
        return( ERROR );
    }

    /* set values for mem_type and no_of_bytes */
    if ( type_sym ) {
        /* if the parser found a TYPE id, type_sym is != NULL */
        //DebugMsg1(("data_dir: arbitrary type %s, calling SymSearch\n", type_sym->sym.name ));
        //type_sym = SymSearch( tokenarray[i].string_ptr );
        mem_type = MT_TYPE;
        if ( type_sym->sym.typekind == TYPE_STRUCT &&
             ((struct dsym *)type_sym)->e.structinfo->OrgInside == TRUE ) {
            EmitError( STRUCT_CANNOT_BE_INSTANCED );
            return( ERROR );
        }
        no_of_bytes = type_sym->sym.total_size;
        if ( no_of_bytes == 0 ) {
            DebugMsg(("data_dir: size of arbitrary type is 0!\n"));
            /* a void type is not valid */
            if ( type_sym->sym.typekind == TYPE_TYPEDEF ) {
                EmitErr( INVALID_TYPE_FOR_DATA_DECLARATION, type_sym->sym.name );
                return( ERROR );
            }
        }
    } else {
        /* it's either a type or a data directive. For types, the index
         into the simpletype table is in <bytval>, for data directives
         the index is found in <sflags>.
         * v2.06: SimpleType is obsolete. Use token index directly!
         */

        if ( tokenarray[i].token == T_STYPE ) {
            //idx = tokenarray[i].bytval;
            idx = tokenarray[i].tokval;
        } else if ( tokenarray[i].token == T_DIRECTIVE &&
                   ( tokenarray[i].dirtype == DRT_DATADIR )) {
            idx = GetSflagsSp( tokenarray[i].tokval );
        } else {
            EmitErr( INVALID_TYPE_FOR_DATA_DECLARATION, tokenarray[i].string_ptr );
            return( ERROR );
        }
        /* types NEAR[16|32], FAR[16|32] and PROC are invalid here */
        mem_type = GetMemtypeSp( idx );
        //if ( ( SimpleType[idx].mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS ) {
        if ( ( mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
        //mem_type = SimpleType[idx].mem_type;
        no_of_bytes = (mem_type & MT_SIZE_MASK) + 1;
        if ( mem_type & MT_FLOAT )
            float_initializer = TRUE;

    }

    /* if i == 1, there's a (data) label at pos 0 */
    name = (( i == 1 ) ? tokenarray[0].string_ptr : NULL );

    /* in a struct declaration? */
    if( CurrStruct != NULL ) {

        /* structure parsing is done in the first pass only */
        if( Parse_Pass == PASS_1 ) {

            /* current offset isn't necessarily the fields start offset */
            //currofs = CurrStruct->sym.offset;

            if (!(sym = AddFieldToStruct( i, tokenarray, name, mem_type, type_sym, no_of_bytes ))) {
                return ( ERROR );
            }
#if FASTPASS
            if ( StoreState ) FStoreLine(0);
#endif
            currofs = sym->offset;
            DebugMsg1(("data_dir: %s, AddFieldToStruct called, ofs=%d\n", sym->name, sym->offset ));
        } else { /* v2.04: else branch added */
            sym = CurrStruct->e.structinfo->tail->sym;
            currofs = sym->offset;
            CurrStruct->e.structinfo->tail = CurrStruct->e.structinfo->tail->next;
        }

    } else {

        if( CurrSeg == NULL ) {
            EmitError( MUST_BE_IN_SEGMENT_BLOCK );
            return( ERROR );
        }

        FStoreLine(0);

        if ( ModuleInfo.CommentDataInCode )
            omf_OutSelect( TRUE );

        if ( ModuleInfo.list ) {
            currofs = GetCurrOffset();
        }

        /* is a label accociated with the data definition? */
        if( name ) {
            /* get/create the label. */
            DebugMsg1(("data_dir: calling SymLookup(%s)\n", name ));
            sym = SymLookup( name );
            if( sym == NULL ) {
                DebugMsg(("data_dir exit, error: invalid label name\n"));
                return( ERROR );
            }

            if( Parse_Pass == PASS_1 ) {
                /* if it's an EXTERNDEF, remove the external info */
                if( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) {
                    //if ( checktypes( sym, mem_type, type_sym ) == ERROR )
                    //    return( ERROR );
                    /* v2.0: display error and continue! */
                    checktypes( sym, mem_type, type_sym );
                    sym_ext2int( sym );
                    sym->total_size = 0;
                    sym->total_length = 0;
                    sym->first_length = 0;
                } else if( sym->state == SYM_UNDEFINED ) {
                    sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
                    sym->state = SYM_INTERNAL;
#if 1
                    /* accept a symbol "redefinition" if addresses and types
                     * do match.
                     */
                } else if ( sym->state == SYM_INTERNAL &&
                           CurrSeg &&
                           sym->segment == (struct asym *)CurrSeg &&
                           sym->offset == GetCurrOffset() ) {
                    if ( checktypes( sym, mem_type, type_sym ) == ERROR )
                        return( ERROR );
                    goto label_defined; /* don't relink the label */
#endif
                } else {
                    DebugMsg(("data_dir: exit 5 with error\n"));
                    EmitErr( SYMBOL_ALREADY_DEFINED, sym->name );
                    return( ERROR );
                }
                /* add the label to the linked list attached to curr segment */
                /* this allows to reduce the number of passes (see Fixup.c) */
                ((struct dsym *)sym)->next = (struct dsym *)CurrSeg->e.seginfo->labels;
                CurrSeg->e.seginfo->labels = sym;

            } else {
                old_offset = sym->offset;
            }
        label_defined:
            SetSymSegOfs( sym );
            if( Parse_Pass != PASS_1 && sym->offset != old_offset ) {
#ifdef DEBUG_OUT
                if ( !ModuleInfo.PhaseError )
                    DebugMsg(("data_dir: Phase error, pass %u, sym >%s< first time, %X != %X\n", Parse_Pass+1, sym->name, sym->offset, old_offset));
#endif
                ModuleInfo.PhaseError = TRUE;
            }
            sym->isdefined = TRUE;
            sym->mem_type = mem_type;
            sym->type = (struct asym *)type_sym;
            /* backpatch for data items? Yes, if the item is defined
             * in a code segment then its offset may change!
             */
            BackPatch( sym );
        }

        if ( type_sym ) {
            while ( type_sym->sym.mem_type == MT_TYPE )
                type_sym = (struct dsym *)type_sym->sym.type;
            /* if it is just a type alias, skip the arbitrary type */
            if ( type_sym->sym.typekind == TYPE_TYPEDEF )
                type_sym = NULL;
        }

    }

    i++;
    if ( data_item( &i, tokenarray, sym, type_sym, no_of_bytes, 1, CurrStruct != NULL, float_initializer, TRUE ) == ERROR ) {
        DebugMsg(("data_dir: error in data_item()\n"));
        return( ERROR );
    }

    if ( tokenarray[i].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
        return( ERROR );
    }

    /* v2.06: update struct size after ALL items have been processed */
    if ( CurrStruct )
        UpdateStructSize( sym );

    if ( ModuleInfo.list )
        LstWrite( CurrStruct ? LSTTYPE_STRUCT : LSTTYPE_DATA, currofs, sym );

    /**/myassert( CurrStruct || CurrSeg );
    DebugMsg1(("data_dir: exit, no error, label=%s, is_array=%u Curr%s.ofs=%X\n",
               sym ? sym->name : "NULL",
               sym ? sym->isarray : 0,
               CurrStruct ? "Struct" : "Seg",
               CurrStruct ? CurrStruct->sym.offset : CurrSeg->sym.offset ));
    return( NOT_ERROR );
}

