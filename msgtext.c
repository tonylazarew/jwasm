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
* Description:  handle/display (error) message texts
*
****************************************************************************/

#include "globals.h"
#include "token.h"
#include "tokenize.h"
#include "msgtext.h"

#define USERESOURCES 0 /* 1=use Win string resources, won't work for Linux! */

char banner_printed = FALSE;

#if USERESOURCES

/*
 If Win32 resource strings are to be used, the
 makefiles must contain a call of the resource compiler!
 Resource file is H/JWasm.rc.
 */

#include "win32.h"
typedef void *HRSRC;
typedef void *HGLOBAL;
WINBASEAPI HRSRC   WINAPI FindResource( void *, char *, uint_32 );
WINBASEAPI HGLOBAL WINAPI LoadResource( void *, HRSRC );
WINBASEAPI void *  WINAPI LockResource( HGLOBAL );
WINBASEAPI void    WINAPI WideCharToMultiByte( uint_32, uint_32, uint_16 *, uint_32, uint_16 *, uint_32, uint_32, uint_32 );

#else

#ifdef __I86__

#include <i86.h>
#define FPTR( x ) MK_FP( FP_SEG( TX_MSG_USAGE ), x )
#undef pick
#define pick( code, string )  const char __based( __segname("_CODE") ) TX_ ## code[] = string;
#include "msgdef.h"
#undef pick
#define pick( code, string ) TX_ ## code,
static const char __based ( __segname("_CODE") ) * const msgtexts[] = {
#include "msgdef.h"
};

#else

#define FPTR( x ) x
#undef pick
#define pick( code, string )  string,
static const char * const msgtexts[] = {
#include "msgdef.h"
};
#endif

#endif

static const char usage[] = {
#include "usage.h"
};

/* the compiler string stored in CodeView symbolic debugging info */
#ifdef DEBUG_OUT
const char szCVCompiler[] = { "Microsoft (R) Macro Assembler Version 6.15.8803" };
//const char szCVCompiler[] = { "Microsoft (R) Macro Assembler Version 8.00.50727" };
#else
const char szCVCompiler[] = { "JWasm v" _JWASM_VERSION_ };
#endif

void MsgInit( void )
/******************/
{
}

void MsgFini( void )
/******************/
{
#if 0 /* set to 1 to display all error texts and numbers */
    int i;
    for ( i = 0; i < MSG_LAST; i++ ) {
        printf("%3u: %s\n", i, msgtexts[i] );
    }
#endif
}

char *MsgGet( int msgid, char *buffer )
/*************************************/
{
#if USERESOURCES
    HRSRC hRsrc;
    HGLOBAL hRes;
    WORD * pId;
    int i;

    hRsrc = FindResource( NULL, MAKEINTRESOURCE(1 + (msgid >> 4)), RT_STRING );
    if (hRsrc) {
        hRes = LoadResource( NULL, hRsrc );
        if (hRes) {
            pId = LockResource( hRes );
            for (i = msgid % 16;i;i--)
                pId += *pId + 1;
            i = *pId++;
            if ( buffer == NULL )
                buffer = StringBufferEnd;
            WideCharToMultiByte( CP_ACP, 0, pId, i, buffer, -1, 0, 0 );
            buffer[i] = NULLC;
            return( buffer );
        }
    }
#else
    if ( msgid < MSG_LAST ) {
        if ( buffer ) {
            strcpy( buffer, FPTR( msgtexts[msgid] ) );
            return( buffer );
        } else
            return( (char *) FPTR( msgtexts[msgid] ) );
    }
#endif
    DebugMsg(("MsgGet(%u): Msg not found!!!\n", msgid));
    if ( buffer == NULL )
        buffer = StringBufferEnd;
    sprintf( buffer, "Msg %u", msgid );
    return( buffer );
}

char *MsgGetEx( int msgid )
/*************************/
{
    return( MsgGet( msgid, NULL ) );
}

void MsgPrintUsage( void )
/************************/
{
    const char *p;
    write_logo();
    for ( p = usage; *p != '\n'; ) {
        const char *p2 = p + strlen( p ) + 1;
        printf("%-20s %s\n", p, p2 );
        p = p2 + strlen( p2 ) + 1;
    }
}

char *MsgGetJWasmName( char *buffer )
/***********************************/
{
    sprintf( buffer, MsgGetEx( MSG_JWASM ), _JWASM_VERSION_, __DATE__ );
    return( buffer );
}

int write_logo( void )
/********************/
{
    char buffer[128];
    if( banner_printed == FALSE ) {
        banner_printed = TRUE;
        printf( MsgGetEx( MSG_BANNER ), MsgGetJWasmName( buffer ) );
        return( 4 ); /* return number of lines printed */
    }
    return( 0 );
}

