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
* Description:  prototypes for input queueing/processing procedures
*
****************************************************************************/

#ifndef _INPUT_H_INCLUDED
#define _INPUT_H_INCLUDED

struct macro_instance {
    struct srcline *currline;
    struct srcline *startline;
    uint_32 localstart;
    char * *parm_array;
    uint parmcnt;
};

/* for line numbers, the source files have to be stored
 * in a list in the very same order as they appear in
 * the input stream.
 */
struct file_seq {
    struct file_seq *next;
    uint_16         file;  /* index of file in FNames */
};

struct input_status {
    char *token_stringbuf;
    char *currsource;
    char *CurrComment;
    int token_count;
    char line_flags;/* v2.08: added */
#ifdef __I86__
    char *stringbufferend;
    struct asm_tok *tokenarray;
#endif
};

extern void     UpdateLineNumber( struct asym * );
extern uint_32  GetLineNumber( void );
#define LineNumber GetLineNumber()

extern void     NewLineQueue( void );
extern void     DeleteLineQueue( void );
extern void     AddLineQueue( const char *line );
extern void     AddLineQueueX( const char *fmt, ... );
extern ret_code InputQueueFile( const char *path, FILE * *pfile );
extern char     *GetTextLine( char *buffer );
extern void     PushMacro( struct asym *, struct macro_instance *, unsigned );
#if FASTMEM==0
extern bool     MacroInUse( struct dsym * );
#endif
extern void     AddStringToIncludePath( const char *string );
extern void     InputInit( void );
extern void     InputPassInit( void );
extern void     InputFini( void );
extern struct asm_tok *PushInputStatus( struct input_status * );
extern void     PopInputStatus( struct input_status * );
extern int      GetPreprocessedLine( char *, struct asm_tok[] );
extern int      GetCurrSrcPos( char * );
extern void     ClearFileStack( void );
extern uint     get_curr_srcfile( void );
extern void     set_curr_srcfile( uint, uint_32 );
extern const struct fname_list *GetFName( uint );
#ifdef DEBUG_OUT
extern char     *GetTopLine( char * );
#endif
extern bool     is_linequeue_populated( void );
extern void     SkipCurrentQueue( struct asm_tok * );
extern ret_code WriteCodeLabel( char *, struct asm_tok[] );

#define GetAlignedPointer( x, size ) ( x + ( ( size + 1 + 3 ) & ~3 ) )

#endif
