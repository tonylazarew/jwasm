/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:
* function                directive
*--------------------------------------------------
* EchoDirective()         ECHO
* IncludeLibDirective()   INCLUDELIB
* IncBinDirective()       INCBIN
* AliasDirective()        ALIAS
* NameDirective()         NAME
* RadixDirective()        .RADIX
* SegOrderDirective()     .DOSSEG, .SEQ, .ALPHA
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "segment.h"
#include "input.h"
#include "tokenize.h"
#include "expreval.h"
#include "types.h"
#include "fastpass.h"
#include "listing.h"
#include "omf.h"
#include "macro.h"

#include "myassert.h"

#define  res(token, function) extern ret_code function( int, struct asm_tok[] );
#include "dirtype.h"
#undef res

/* table of function addresses for directives */
#define  res(token, function) function ,
ret_code (* const directive[])( int, struct asm_tok[] ) = {
#include "dirtype.h"
};
#undef res

/* should never be called */
ret_code StubDir( int i, struct asm_tok tokenarray[] ){ return( ERROR ); }

/* handle ECHO directive.
 * displays text on the console
 */
ret_code EchoDirective( int i, struct asm_tok tokenarray[] )
/**********************************************************/
{
    if ( Parse_Pass == PASS_1 ) /* display in pass 1 only */
        if ( Options.preprocessor_stdout == FALSE ) { /* don't print to stdout if -EP is on! */
            printf( "%s\n", tokenarray[i+1].tokpos );
        }
    return( NOT_ERROR );
}

static char *IncludeLibrary( const char *name )
/*********************************************/
{
    struct qnode *q;
    char *node;

    /* old approach, <= 1.91: add lib name to global namespace */
    /* new approach, >= 1.92: check lib table, if entry is missing, add it */
    /* Masm doesn't map cases for the paths. So if there is
     * includelib <kernel32.lib>
     * includelib <KERNEL32.LIB>
     * then 2 defaultlib entries are added. If this is to be changed for
     * JWasm, activate the _stricmp() below.
     */
    for ( q = ModuleInfo.g.LibQueue.head; q ; q = q->next ) {
        //if ( _stricmp( dir->sym.name, name) == 0)
        if ( strcmp( q->elmt, name ) == 0 )
            return( (char *)q->elmt );
    }
    node = LclAlloc( strlen( name ) + 1 );
    strcpy( node, name );
    QAddItem( &ModuleInfo.g.LibQueue, node );
    return( node );
}

#if FASTMEM==0

/* release the lib queue ( created by INCLUDELIB directive ) */

void FreeLibQueue( void )
/***********************/
{
    struct qnode *curr;
    struct qnode *next;
    for( curr = ModuleInfo.g.LibQueue.head; curr; curr = next ) {
        next = curr->next;
        LclFree( (void *)curr->elmt );
        LclFree( curr );
    }
}
#endif

/* directive INCLUDELIB */

ret_code IncludeLibDirective( int i, struct asm_tok tokenarray[] )
/****************************************************************/
{
    char *name;
    //struct asym *sym;

    if ( Parse_Pass != PASS_1 ) /* do all work in pass 1 */
        return( NOT_ERROR );
    i++; /* skip the directive */
    /* v2.03: library name may be just a "number" */
    //if ( tokenarray[i].token == T_FINAL || tokenarray[i].token == T_NUM ) {
    if ( tokenarray[i].token == T_FINAL ) {
        /* v2.05: Masm doesn't complain if there's no name, so emit a warning only! */
        //EmitError( LIBRARY_NAME_MISSING );
        //return( ERROR );
        EmitWarn( 2, LIBRARY_NAME_MISSING );
    }

    if ( tokenarray[i].token == T_STRING && tokenarray[i].string_delim == '<' ) {
        if ( tokenarray[i+1].token != T_FINAL ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i+1].tokpos );
            return( ERROR );
        }
        /* v2.08: use GetLiteralValue() */
        //name = tokenarray[i].string_ptr;
        name = StringBufferEnd;
        GetLiteralValue( name, tokenarray[i].string_ptr );
    } else {
        char *p;
        /* regard "everything" behind INCLUDELIB as the library name */
        name = tokenarray[i].tokpos;
        /* remove trailing white spaces */
        for ( p = tokenarray[Token_Count].tokpos - 1; p > name && isspace( *p ); *p = NULLC, p-- );
    }

    IncludeLibrary( name );
    return( NOT_ERROR );
}

#if INCBINSUPP

/* INCBIN directive */

ret_code IncBinDirective( int i, struct asm_tok tokenarray[] )
/************************************************************/
{
    FILE *file;
    //int size;
    uint_32 fileoffset = 0; /* fixme: should be uint_64 */
    uint_32 sizemax = -1;
    struct expr opndx;

    DebugMsg(("IncBinDirective enter\n"));

    i++; /* skip the directive */
    /* v2.03: file name may be just a "number" */
    //if ( tokenarray[i].token == T_FINAL || tokenarray[i].token == T_NUM ) {
    if ( tokenarray[i].token == T_FINAL ) {
        EmitError( EXPECTED_FILE_NAME );
        return( ERROR );
    }

    if ( tokenarray[i].token == T_STRING ) {

        /* v2.08: use string buffer to avoid buffer overflow if string is > _MAX_PATH */
        if ( tokenarray[i].string_delim == '"' || tokenarray[i].string_delim == '\'' ) {
            memcpy( StringBufferEnd, tokenarray[i].string_ptr+1, tokenarray[i].stringlen );
            StringBufferEnd[tokenarray[i].stringlen] = NULLC;
        } else if ( tokenarray[i].string_delim == '<' ) {
            /* v2.08: use GetLiteralValue() instead of strncpy() */
            GetLiteralValue( StringBufferEnd, tokenarray[i].string_ptr );
        } else {
            EmitError( FILENAME_MUST_BE_ENCLOSED_IN_QUOTES_OR_BRACKETS );
            return( ERROR );
        }
    } else {
        EmitError( FILENAME_MUST_BE_ENCLOSED_IN_QUOTES_OR_BRACKETS );
        return( ERROR );
    }
    i++;
    if ( tokenarray[i].token == T_COMMA ) {
        i++;
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
            return( ERROR );
        if ( opndx.kind == EXPR_CONST ) {
            fileoffset = opndx.value;
        } else if ( opndx.kind != EXPR_EMPTY ) {
            EmitError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if ( tokenarray[i].token == T_COMMA ) {
            i++;
            if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
                return( ERROR );
            if ( opndx.kind == EXPR_CONST ) {
                sizemax = opndx.value;
            } else if ( opndx.kind != EXPR_EMPTY ) {
                EmitError( CONSTANT_EXPECTED );
                return( ERROR );
            }
        }
    }
    if ( tokenarray[i].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
        return( ERROR );
    }

    if( CurrSeg == NULL ) {
        EmitError( MUST_BE_IN_SEGMENT_BLOCK );
        return( ERROR );
    }

    /* v2.04: tell assembler that data is emitted */
    if ( ModuleInfo.CommentDataInCode )
        omf_OutSelect( TRUE );

    DebugMsg1(("IncBinDirective: filename=%s, offset=%" FU32 ", size=%" FU32 "\n", StringBufferEnd, fileoffset, sizemax ));

    /* try to open the file */
    if ( InputQueueFile( StringBufferEnd, &file ) == NOT_ERROR ) {
        /* transfer file content to the current segment. */
        if ( fileoffset )
            fseek( file, fileoffset, SEEK_SET );  /* fixme: use fseek64() */
        for( ; sizemax; sizemax-- ) {
            int ch = fgetc( file );
            if ( ( ch == EOF ) && feof( file ) )
                break;
            OutputByte( ch );
        }
        fclose( file );
    }

    return( NOT_ERROR );
}
#endif

/* Alias directive.
 * Masm syntax is:
 *   'ALIAS <alias_name> = <substitute_name>'
 * which looks somewhat strange if compared to other Masm directives.
 * (OW Wasm syntax is 'alias_name ALIAS substitute_name', which is
 * what one might have expected for Masm as well).
 *
 * <alias_name> is a global name and must be unique (that is, NOT be
 * defined elsewhere in the source!
 * <substitute_name> is the name which is defined in the source.
 * For COFF and ELF, this name MUST be defined somewhere as
 * external or public!
 */

ret_code AliasDirective( int i, struct asm_tok tokenarray[] )
/***********************************************************/
{
    //char *tmp;
    struct asym *sym;
    char *subst;

    i++; /* go past ALIAS */

    if ( tokenarray[i].token != T_STRING ||
        tokenarray[i].string_delim != '<' ) {
        DebugMsg(("AliasDirective: first argument is not a literal: %s\n", tokenarray[i].string_ptr ));
        EmitError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }

    /* check syntax. note that '=' is T_DIRECTIVE && DRT_EQUALSGN */
    if ( tokenarray[i+1].token != T_DIRECTIVE ||
        //tokenarray[i+1].tokval != T_EQU ||
        tokenarray[i+1].dirtype != DRT_EQUALSGN ) {
        DebugMsg(("AliasDirective: syntax error: %s\n", tokenarray[i+1].string_ptr ));
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i+1].string_ptr );
        return( ERROR );
    }

    if ( tokenarray[i+2].token != T_STRING ||
        tokenarray[i+2].string_delim != '<' )  {
        DebugMsg(("AliasDirective: second argument is not a literal: %s\n", tokenarray[i+2].string_ptr ));
        EmitError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }
    subst = tokenarray[i+2].string_ptr;

    if ( tokenarray[i+3].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i+3].string_ptr );
        return( ERROR );
    }

    /* make sure <alias_name> isn't defined elsewhere */
    sym = SymSearch( tokenarray[i].string_ptr );
    if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
        struct asym *sym2;
        /* v2.04b: adjusted to new field <substitute> */
        sym2 = SymSearch( subst );
        if ( sym2 == NULL ) {
            sym2 = SymCreate( subst );
            sym2->state = SYM_UNDEFINED;
            sym_add_table( &SymTables[TAB_UNDEF], (struct dsym *)sym2 );
        } else if ( sym2->state != SYM_UNDEFINED &&
                   sym2->state != SYM_INTERNAL &&
                   sym2->state != SYM_EXTERNAL ) {
            EmitErr( MUST_BE_PUBLIC_OR_EXTERNAL, subst );
            return( ERROR );
        }
        if ( sym == NULL )
            sym = SymCreate( tokenarray[i].string_ptr );
        else
            sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );

        sym->state = SYM_ALIAS;
        sym->substitute = sym2;
        sym_add_table( &SymTables[TAB_ALIAS], (struct dsym *)sym ); /* add ALIAS */
        return( NOT_ERROR );
    }
    if ( sym->state != SYM_ALIAS || ( strcmp( sym->substitute->name, subst ) != 0 )) {
        DebugMsg(("AliasDirective: symbol redefinition\n"));
        EmitErr( SYMBOL_REDEFINITION, sym->name );
        return( ERROR );
    }
#if COFF_SUPPORT || ELF_SUPPORT
    /* for COFF+ELF, make sure <actual_name> is "global" (EXTERNAL or
     * public INTERNAL). For OMF, there's no check at all. */
    if ( Parse_Pass != PASS_1 ) {
        if ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
             || Options.output_format == OFORMAT_ELF
#endif
           ) {
            if ( sym->substitute->state == SYM_UNDEFINED ) {
                EmitErr( SYMBOL_NOT_DEFINED, subst );
                return( ERROR );
            } else if ( sym->substitute->state != SYM_EXTERNAL &&
                       ( sym->substitute->state != SYM_INTERNAL || sym->substitute->public == FALSE ) ) {
                EmitErr( MUST_BE_PUBLIC_OR_EXTERNAL, subst );
                return( ERROR );
            }
        }
    }
#endif
    return( NOT_ERROR );
}

/* the NAME directive is ignored in Masm v6 */

ret_code NameDirective( int i, struct asm_tok tokenarray[] )
/**********************************************************/
{
    if( Parse_Pass != PASS_1 )
        return( NOT_ERROR );
    /* if a module name is set with -nm, ignore NAME directive! */
    /* v2.08: removed, since Options.names isn't touched at all */
    //if( Options.names[OPTN_MODULE_NAME] != NULL )
    //    return( NOT_ERROR );

    i++; /* skip directive */

    /* improper use of NAME is difficult to see since it is a nop
     therefore some syntax checks are implemented:
     - no 'name' structs, unions, records, typedefs!
     - no 'name' struct fields!
     - no 'name' segments!
     - no 'name:' label!
     */
    if ( CurrStruct != NULL ||
        ( tokenarray[i].token == T_DIRECTIVE &&
         ( tokenarray[i].tokval == T_SEGMENT ||
          tokenarray[i].tokval == T_STRUCT  ||
          tokenarray[i].tokval == T_STRUC   ||
          tokenarray[i].tokval == T_UNION   ||
          tokenarray[i].tokval == T_TYPEDEF ||
          tokenarray[i].tokval == T_RECORD)) ||
         tokenarray[i].token == T_COLON ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i-1].tokpos );
        return( ERROR );
    }

    /* don't touch Option fields! if anything at all, ModuleInfo.name may be modified.
     * However, since the directive is ignored by Masm, nothing is done.
     */
//  strncpy( ModuleInfo.name, tokenarray[i].string_ptr, sizeof( ModuleInfo.name ) );
//  ModuleInfo.name[ sizeof( ModuleInfo.name ) - 1] = NULLC;
//  DebugMsg(("NameDirective: set name to >%s<\n", ModuleInfo.name ));
    DebugMsg(("NameDirective: ignored name >%s<\n", tokenarray[i].string_ptr ));
    return( NOT_ERROR );
}

/* .RADIX directive, value must be between 2 .. 16 */

ret_code RadixDirective( int i, struct asm_tok tokenarray[] )
/***********************************************************/
{
    uint_8          oldradix;
    struct expr     opndx;

    /* to get the .radix parameter, enforce radix 10 and retokenize! */
    oldradix = ModuleInfo.radix;
    ModuleInfo.radix = 10;
    i++; /* skip directive token */
    Tokenize( tokenarray[i].tokpos, i, tokenarray, TOK_RESCAN );
    ModuleInfo.radix = oldradix;
    if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR ) {
        return( ERROR );
    }

    if ( opndx.kind != EXPR_CONST ) {
        EmitError( CONSTANT_EXPECTED );
        return( ERROR );
    }
    if ( tokenarray[i].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
        return( ERROR );
    }
    if ( opndx.value > 16 || opndx.value < 2 || opndx.hvalue != 0 ) {
        EmitError( INVALID_RADIX_TAG );
        return( ERROR );
    }

    ModuleInfo.radix = opndx.value;
    DebugMsg(("RadixDirective: new radix=%u\n", ModuleInfo.radix ));

    return( NOT_ERROR );
}

/* DOSSEG, .DOSSEG, .ALPHA, .SEQ directives */

ret_code SegOrderDirective( int i, struct asm_tok tokenarray[] )
/**************************************************************/
{
    if ( tokenarray[i+1].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i+1].tokpos );
        return( ERROR );
    }
#if COFF_SUPPORT || ELF_SUPPORT
    if ( Options.output_format != OFORMAT_OMF &&
        Options.output_format != OFORMAT_BIN ) {
        if ( Parse_Pass == PASS_1 )
            EmitWarn( 2, DIRECTIVE_IGNORED_FOR_COFF, tokenarray[i].string_ptr );
    } else {
#endif
#if 1 /* v2.05 */
        ModuleInfo.segorder = GetSflagsSp( tokenarray[i].tokval );
#else
        switch( tokenarray[i].tokval ) {
        case T_DOT_DOSSEG:
        case T_DOSSEG:    ModuleInfo.segorder = SEGORDER_DOSSEG;  break;
        case T_DOT_ALPHA: ModuleInfo.segorder = SEGORDER_ALPHA;   break;
        default:          ModuleInfo.segorder = SEGORDER_SEQ;     break;
        }
#endif
#if COFF_SUPPORT || ELF_SUPPORT
    }
#endif
    return( NOT_ERROR );
}
