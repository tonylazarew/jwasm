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
* Description:  Diagnostics routines: (fatal) errors, warnings, notes
*
****************************************************************************/

#include <stdarg.h>
#include <ctype.h>
#include <setjmp.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "input.h"
#include "tokenize.h"
#include "macro.h"
#include "msgtext.h"
#include "listing.h"
#include "segment.h"

extern void             print_source_nesting_structure( void );
extern char             banner_printed;
extern jmp_buf          jmpenv;

//static bool             Errfile_Written;
//static void             PrtMsg( int severity, int msgnum, va_list args1, va_list args2 );
//void                    PutMsg( FILE *fp, int severity, int msgnum, va_list args );

#ifdef DEBUG_OUT
void DoDebugMsg( const char *format, ... )
/****************************************/
{
    va_list args;
    if( !Options.debug ) return;

    if( ModuleInfo.cref == FALSE && CurrFName[ASM] != NULL )
        return;

    va_start( args, format );
    vprintf( format, args );
    va_end( args );
#if 0
    fflush( stdout );
#endif
}
void DoDebugMsg1( const char *format, ... )
/****************************************/
{
    va_list args;
    char buffer[MAX_LINE_LEN];

    if( !Options.debug ) return;

    if( ModuleInfo.cref == FALSE ) return;

    printf( "%lu%s. ", LineNumber, GetTopLine( buffer ) );

    va_start( args, format );
    vprintf( format, args );
    va_end( args );
#if 0
    fflush( stdout );
#endif
}
#endif

void PutMsg( FILE *fp, int severity, int msgnum, va_list args )
/*************************************************************/
{
    int             i,j;
    char            *type;
    char            *pMsg;
    char            buffer[MAX_LINE_LEN+128];

    if( fp != NULL ) {

        if ( severity && ( j = GetCurrSrcPos( buffer ) ) ) {
            fwrite( buffer, 1, j, fp );
        }
        pMsg = MsgGetEx( msgnum );
        switch (severity ) {
        case 1:  type = MsgGetEx( MSG_FATAL_PREFIX );   break;
        case 2:  type = MsgGetEx( MSG_ERROR_PREFIX );   break;
        case 4:  type = MsgGetEx( MSG_WARNING_PREFIX ); break;
        default:  type = NULL; i = 0; break;
        }
        if ( type )
            i = sprintf( buffer, "%s A%4u: ", type, severity * 1000 + msgnum );
        i += vsprintf( buffer+i, pMsg, args );
        //buffer[i] = NULLC;

        fwrite( buffer, 1, i, fp );
        fwrite( "\n", 1, 1, fp );

        /* if in Pass 1, add the error msg to the listing */
        if ( CurrFile[LST] &&
             severity &&
             Parse_Pass == PASS_1 &&
             fp == CurrFile[ERR] ) {
            LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), 0 );
            /* size of "blank" prefix to be explained! */
            LstPrintf( "                           %s", buffer );
            LstNL();
        }
    }
}

static void PrtMsg( int severity, int msgnum, va_list args1, va_list args2 )
/**************************************************************************/
{
#ifndef __SW_BD
    if( !banner_printed )
        write_logo();
#endif
    /* open .err file if not already open and a name is given */
    if( CurrFile[ERR] == NULL && CurrFName[ERR] != NULL ) {
        CurrFile[ERR] = fopen( CurrFName[ERR], "w" );
        if( CurrFile[ERR] == NULL ) {
            /* v2.06: no fatal error anymore if error file cannot be written */
            char *p = CurrFName[ERR];
            CurrFName[ERR] = NULL; /* set to NULL before EmitErr()! */
            Options.no_error_disp = FALSE; /* disable -eq! */
            EmitErr( CANNOT_OPEN_FILE, p, ErrnoStr() );
            LclFree( p );
        }
    }

    /* v2.05: new option -eq */
    if ( Options.no_error_disp == FALSE ) {
        PutMsg( errout, severity, msgnum, args1 );
        fflush( errout );                       /* 27-feb-90 */
    }
    if( CurrFile[ERR] ) {
        //Errfile_Written = TRUE;
        PutMsg( CurrFile[ERR], severity, msgnum, args2 );
    }
}

/* notes: "included by", "macro called from", ... */

void PrintNote( int msgnum, ... )
/*******************************/
{
    va_list args1, args2;

    va_start( args1, msgnum );
    va_start( args2, msgnum );

    PrtMsg( 0, msgnum, args1, args2 );
    va_end( args1 );
    va_end( args2 );
}

void EmitErr( int msgnum, ... )
/*****************************/
{
    va_list args1, args2;

#ifdef DEBUG_OUT
    printf( "%s\n", ModuleInfo.tokenarray ? ModuleInfo.tokenarray[0].tokpos : "" );
#endif
    va_start( args1, msgnum );
    va_start( args2, msgnum );
    PrtMsg( 2, msgnum, args1, args2 );
    va_end( args1 );
    va_end( args2 );
    ModuleInfo.g.error_count++;
    write_to_file = FALSE;
    print_source_nesting_structure();
    if( Options.error_limit != -1  &&  ModuleInfo.g.error_count == Options.error_limit+1 )
        Fatal( TOO_MANY_ERRORS );
}

void EmitError( int msgnum )
/**************************/
{
    EmitErr( msgnum );
}

void EmitWarn( int level, int msgnum, ... )
/*****************************************/
{
    va_list args1, args2;

    if( level <= Options.warning_level ) {
#ifdef DEBUG_OUT
        printf( "%s\n", ModuleInfo.tokenarray ? ModuleInfo.tokenarray[0].tokpos : "" );
#endif
        va_start( args1, msgnum );
        va_start( args2, msgnum );
        if( !Options.warning_error ) {
            PrtMsg( 4, msgnum, args1, args2 );
            ModuleInfo.g.warning_count++;
        } else {
            PrtMsg( 2, msgnum, args1, args2 );
            ModuleInfo.g.error_count++;
        }
        va_end( args1 );
        va_end( args2 );
        print_source_nesting_structure();
    }
}

char *ErrnoStr( void )
/********************/
{
    static char buffer[32];
    return( ( errno == ENOENT ) ? "ENOENT" : myltoa( errno, buffer, 10, FALSE, FALSE ) );
}

/* fatal error (out of memory, unable to open files for write, ...)
 * don't use functions which need to alloc memory here!
 * v2.08: do not exit(), just a longjmp() into AssembleModule().
 */
void Fatal( unsigned msg, ... )
/*****************************/
{
    va_list     args;

    va_start( args, msg );
    PutMsg( errout, 1, msg, args );
    va_end( args );

    ModuleInfo.g.error_count++;
    //write_to_file = FALSE;

    /* setjmp() has been called in AssembleModule().
     * if a fatal error happens outside of this function, longjmp()
     * is NOT to be used ( virtually may happen for "out of memory" only)
     */
    if ( CurrFName[ASM] )
        longjmp( jmpenv, 2 );

    exit(1);
}

#if 0
void SeekError( void )
/********************/
{
    DebugMsg(("SeekError occured\n"));
    Fatal( FILE_SEEK_ERROR, CurrFName[OBJ], errno );
};
#endif

void WriteError( void )
/*********************/
{
    DebugMsg(("WriteError occured\n"));
    Fatal( FILE_WRITE_ERROR, CurrFName[OBJ], errno );
};

#ifndef NDEBUG

int InternalError( const char *file, unsigned line )
/**************************************************/
/* it's used by myassert() function in debug version */
{
    char buffer[MAX_LINE_LEN];
    DebugMsg(("InternalError enter\n"));
    ModuleInfo.g.error_count++;
    GetCurrSrcPos( buffer );
    fprintf( errout, "%s", buffer );
    fprintf( errout, MsgGetEx( INTERNAL_ERROR ), file, line );
    close_files();
    exit( EXIT_FAILURE );
    return(0);
}
#endif

