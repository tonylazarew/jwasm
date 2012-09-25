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
* Description:  prototypes for macro stuff
*
****************************************************************************/

#ifndef _MACRO_H_
#define _MACRO_H_

#define PLACEHOLDER_CHAR '\n' /* "escape" char for macro placeholders */

/* functions in expans.c */

extern int      GetLiteralValue( char *, const char * );
extern int      RunMacro( struct dsym *, int, struct asm_tok[], char *, int, bool * );
extern ret_code ExpandText( char *, struct asm_tok[], unsigned int );
extern int      ExpandLineItems( char *, int, struct asm_tok[], int, int );
extern ret_code ExpandLine( char *, struct asm_tok[] );

/* functions in macro.c */

extern struct dsym *CreateMacro( const char * );/* create a macro symbol */
extern void     ReleaseMacroData( struct dsym * );
extern void     fill_placeholders( char *, const char *, uint, uint, char * * );
extern ret_code StoreMacro( struct dsym *, int, struct asm_tok[], bool );  /* store macro content */
extern ret_code MacroInit( int );
#ifdef DEBUG_OUT
extern void     MacroFini( void );
#endif

/* functions in string.c */

extern struct asym *SetTextMacro( struct asm_tok[], struct asym *, const char *, const char * ); /* EQU for texts */
extern struct asym *AddPredefinedText( const char *, const char * );

extern void     StringInit( void );
#ifdef DEBUG_OUT
extern void     StringFini( void );
#endif

#endif
