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
* Description:  interface to instruction hash table.
*
****************************************************************************/


#ifndef _RESWORDS_H_INCLUDED
#define _RESWORDS_H_INCLUDED

/* structure of items in the "reserved names" table ResWordTable[] */

struct ReservedWord {
    short next;              /* index next entry (used for hash table) */
    unsigned char len;       /* length of reserved word, i.e. 'AX' = 2 */
    unsigned char flags;
#if 0 /* __I86__ ( may be activated for JWASMR, see reswords.c) */
    const char __based( void ) *name;
#else
    const char *name;        /* reserved word (char[]) */
#endif
};

enum reservedword_flags {
    RWF_SPECIAL  = 1, /* keyword is NO instruction */
    RWF_DISABLED = 2, /* keyword disabled */
    RWF_IA32     = 4, /* keyword specific to IA32 mode */
#if AMD64_SUPPORT
    RWF_X64      = 8, /* keyword specific to IA32+ mode */
#endif
#if AVXSUPP
    RWF_VEX      = 0x10, /* keyword triggers VEX encoding */
#endif
};

extern int      FindResWord( const char *, unsigned char );
extern char     *GetResWName( uint, char * );
extern bool     IsKeywordDisabled( const char *, int );
extern void     DisableKeyword( uint );
#if RENAMEKEY
extern void     RenameKeyword( uint, const char *, uint_8 );
#endif
#if AMD64_SUPPORT
extern void     Set64Bit( bool );
#endif
extern void     ResWordsInit( void );
extern void     ResWordsFini( void );

#endif
