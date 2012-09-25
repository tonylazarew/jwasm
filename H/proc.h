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
* Description:  interface to PROC.C routines
*
****************************************************************************/


#ifndef _PROC_H_
#define _PROC_H_

extern struct dsym     *CurrProc;      /* current procedure */
#if AMD64_SUPPORT
extern struct asym     ReservedStack;  /* max stack space required by INVOKE */
#endif

/*---------------------------------------------------------------------------*/

//extern void             pushitem( void *, void * );
//extern void             *popitem( void * );

extern ret_code         ExamineProc( int, struct asm_tok[], struct dsym *, bool );  /* common for PROC/PROTO */
extern struct asym      *CreateProc( struct asym *, const char *, unsigned char );
extern void             DeleteProc( struct dsym * );

extern ret_code         CopyPrototype( struct dsym *, struct dsym * );
extern ret_code         RetInstr( int, struct asm_tok[], int );   /* handle RET/IRET within procedures */
extern void             write_prologue( struct asm_tok[] );
extern void             ProcInit( void );

#endif
