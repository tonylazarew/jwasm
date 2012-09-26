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
* Description:  processing input line data and line queueing for macros
*
****************************************************************************/

#include <ctype.h>
#include <stdarg.h>
#include <sys/stat.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "reswords.h"
#include "condasm.h"
#include "equate.h"
#include "macro.h"
#include "labels.h"
#include "input.h"
#include "tokenize.h"
#include "proc.h"
#include "fastpass.h"
#include "listing.h"
#include "myassert.h"

extern struct ReservedWord  ResWordTable[];
extern ret_code (* const directive[])( int, struct asm_tok[] );
extern char inside_comment;

#define REMOVECOMENT 0 /* 1=remove comments from source       */
#define DETECTCTRLZ 1

/* FILESEQ: if 1, stores a linked list of source files, ordered
 * by usage. Masm stores such a list in the COFF symbol table
 * when -Zd/-Zi is set. It isn't necessary, however, and JWasm's
 * COFF code currently will ignore the list.
 */

#define FILESEQ 0

char   *commentbuffer;

struct asym *FileCur = NULL; /* @FileCur symbol */
struct asym LineCur = { NULL,"@Line", 0 };

static int queue_level; /* number of line queues onto the file stack */

/* SrcAlloc() and SrcFree() are used to store/release
 * "file" items onto the file stack and "line queue" items.
 *
 * MemAlloc() uses the normal C heap functions.
 * LclAlloc() uses the "fast" replacement if FASTMEM=1.
 * it's probably best to use the first, since "file"
 * and "line queue" items are "short-lived".
 */
#define SrcAlloc(x) MemAlloc(x)
#define SrcFree(x)  MemFree(x)

/* item of a line queue */
struct line_list {
    struct line_list *next;
#ifdef DEBUG_OUT
    char lineno;
#endif
    char line[1];
};

struct input_queue {
    struct line_list    *head;
    struct line_list    *tail;
};

struct file_list {
    struct file_list    *next;
    union {
        FILE            *file;      /* if item is a file */
        struct macro_instance *mi;  /* if item is a macro */
        struct input_queue  *lines; /* if item is a line queue */
    };
    uint_32             line_num;   /* current line */
    struct asym         *macro;     /* the symbol if it is a macro */
    uint_16             srcfile;    /* index of file in FNamesTab */
    unsigned char       islinesrc:1;
};

/* NOTE: the line queue is a simple list of lines.
 if it must be nested, it is converted to a file_list item
 and pushed onto the file stack.
 */
//static struct input_queue *line_queue;  /* line queue */
//static struct file_list *file_stack;    /* source item (file/macro) stack */
//static char             *IncludePath;
#define line_queue  ModuleInfo.g.line_queue
#define file_stack  ModuleInfo.g.file_stack

#if FILESEQ
struct qdesc            FileSeq;
#endif

#ifdef DEBUG_OUT
struct asm_tok *end_tokenarray;
char           *end_stringbuf;
static char currlqline;
static long cntppl0;
static long cntppl1;
static long cntppl2;
static long cntflines;
static long cntlines;
int lq_line;
long cnttok0;
long cnttok1;
#endif

/* buffer for source lines
 * since the lines are sometimes concatenated
 * the buffer must be a multiple of MAX_LINE_LEN
 */
static char *srclinebuffer;
char *token_stringbuf;  /* start token string buffer */

/* fixme: add '|| defined(__CYGWIN__)' ? */
#if defined(__UNIX__)

#define INC_PATH_DELIM      ':'
#define INC_PATH_DELIM_STR  ":"
#define DIR_SEPARATOR       '/'
#define filecmp strcmp
#define _stat stat

#else

#define INC_PATH_DELIM      ';'
#define INC_PATH_DELIM_STR  ";"
#define DIR_SEPARATOR       '\\'
#define filecmp _stricmp

#if defined(__CYGWIN__)
#define _stat stat
#endif

#endif

static char *GetFullPath( const char *name, char *buff, size_t max )
/******************************************************************/
{
    char        *p;

    p = _fullpath( buff, name, max );
    if( p == NULL )
        p = (char *)name;

#if defined(__UNIX__)
    if( (p[0] == '/' && p[1] == '/') && (name[0] != '/' || name[1] != '/') ) {
        /*
         * if the _fullpath result has a node number and
         * the user didn't specify one, strip the node number
         * off before returning
         */
        p += 2;
        while( *(p++) != '/' ) ;
    }
#endif
    return( p );
}

static time_t GetFileTimeStamp( const char *filename )
/****************************************************/
{
    struct _stat statbuf;

    if( _stat( filename, &statbuf ) != 0 ) {
        return( 0 );
    }
    return( statbuf.st_mtime );
}

/* check if a file is in the array of known files.
 * if no, store the file at the array's end.
 * returns array index.
 * the array is stored in the standard C heap!
 * the filenames are stored in the "local" heap.
 */
static uint AddFile( char const *fname )
/**************************************/
{
    struct fname_list *newfn;
    uint    index;
    char    name[_MAX_FNAME];
    char    ext[_MAX_EXT];

    DebugMsg(("AddFile(%s) enter\n", fname ));
    for( index = 0; index < ModuleInfo.g.cnt_fnames; index++ ) {
        if( filecmp( fname, ( FNamesTab + index )->fullname ) == 0 )
            return( index );
    }

    if ( ( index % 64 ) == 0 ) {
        newfn = (struct fname_list *)MemAlloc( ( index + 64) * sizeof( struct fname_list ) );
        if ( FNamesTab ) {
            memcpy( newfn, FNamesTab, index * sizeof( struct fname_list ) );
            MemFree( FNamesTab );
        }
        FNamesTab = newfn;
    }
    ModuleInfo.g.cnt_fnames = index + 1;

    _splitpath( fname, NULL, NULL, name, ext );

#if 0
    (FNamesTab+index)->mtime = GetFileTimeStamp( fname );
    (FNamesTab+index)->name = (char *)LclAlloc( strlen( name ) + strlen( ext ) + 1 );
    strcpy( (FNamesTab+index)->name, name );
    strcat( (FNamesTab+index)->name, ext );
    (FNamesTab+index)->fullname = (char *)LclAlloc( strlen( fname ) + 1 );
    strcpy( (FNamesTab+index)->fullname, fname );
#else
    newfn = FNamesTab + index;
    /* timestamp needed for autodependancy records only */
    if( Options.line_numbers )
        newfn->mtime = GetFileTimeStamp( fname );
    newfn->name = (char *)LclAlloc( strlen( name ) + strlen( ext ) + 1 );
    strcpy( newfn->name, name );
    strcat( newfn->name, ext );
    newfn->fullname = (char *)LclAlloc( strlen( fname ) + 1 );
    strcpy( newfn->fullname, fname );
#endif
    return( index );
}

const struct fname_list *GetFName( uint index )
/*********************************************/
{
    return( FNamesTab+index );
}

/* free the file array */

static void FreeFiles( void )
/***************************/
{
#if FASTMEM==0
    int i;
    for ( i = 0; i < ModuleInfo.g.cnt_fnames; i++ ) {
        LclFree( (FNamesTab + i)->name );
        LclFree( (FNamesTab+i)->fullname );
    }
#endif
    MemFree( FNamesTab );
    FNamesTab = NULL;
    return;
}

/* free a line queue */

static void FreeLineQueue( struct input_queue *queue )
/****************************************************/
{
    struct line_list   *curr;
    struct line_list   *next;

    for( curr = queue->head; curr; curr = next ) {
        next = curr->next;
        SrcFree( curr );
    }
    SrcFree( queue );
}

/* clear input source stack (include files and open macros).
 Usually the stack is empty when the END directive occurs,
 but it isn't required that the END directive is located in
 the main source file. Also, an END directive might be
 simulated if a "too many errors" condition occurs.
*/

void ClearFileStack( void )
/*************************/
{
    struct file_list   *nextfile;

    DeleteLineQueue();

    /* dont close the last item (which is the main src file) */
    for( ; file_stack->next ; file_stack = nextfile ) {
        nextfile = file_stack->next;
        if ( file_stack->islinesrc && ( file_stack->macro == NULL ) ) {
            FreeLineQueue( file_stack->lines );
        } else {
            fclose( file_stack->file );
        }
        SrcFree( file_stack );
    }
    return;
}

/* returns value of predefined symbol @Line */

void UpdateLineNumber( struct asym *sym )
/***************************************/
{
    struct file_list *fl;
    for ( fl = file_stack; fl ; fl = fl->next )
        if ( fl->islinesrc == FALSE ) {
            sym->value = fl->line_num;
            break;
        }
    return;
}

uint_32 GetLineNumber( void )
/***************************/
{
    UpdateLineNumber( &LineCur );
    return( LineCur.uvalue );
}

#ifdef DEBUG_OUT
char *GetTopLine( char *buffer )
/******************************/
{
    *buffer = NULLC;
    if ( lq_line )
        sprintf( buffer, "(%u)", lq_line );
    else if( file_stack->islinesrc == TRUE )
        sprintf( buffer, "[%s.%lu]", file_stack->macro ? file_stack->macro->name : "", file_stack->line_num );
    return( buffer );
}
#endif

/* read one line from current source file.
 * returns NULL if EOF has been detected and no char stored in buffer
 * v2.08: 00 in the stream no longer causes an exit. Hence if the
 * char occurs in the comment part, everything is ok.
 */
static char *my_fgets( char *buffer, int max, FILE *fp )
/******************************************************/
{
    char        *ptr = buffer;
    char        *last = buffer + max;
    int         c;

    c = getc( fp );
    while( ptr < last ) {
        switch ( c ) {
        case '\r':
            break; /* don't store CR */
        case '\n':
            /* fall through */
        //case '\0': /* v2.08: */
#ifdef DEBUG_OUT
            if ( Parse_Pass == PASS_1 )
                cntflines++;
#endif
            *ptr = NULLC;
            return( buffer );
#if DETECTCTRLZ
        case 0x1a:
            /* since source files are opened in binary mode, ctrl-z
             * handling must be done here.
             */
            /* no break */
#endif
        case EOF:
            *ptr = NULLC;
            return( ptr > buffer ? buffer : NULL );
        default:
            *ptr++ = c;
        }
        c = getc( fp );
    }
    EmitErr( LINE_TOO_LONG );
    *(ptr-1) = NULLC;
    return( buffer );
}

#if FILESEQ
void AddFileSeq( uint file )
/**************************/
{
    struct file_seq *node;
    node = LclAlloc( sizeof( struct file_seq ) );
    node->next = NULL;
    node->file = file;
    if ( FileSeq.head == NULL )
        FileSeq.head = FileSeq.tail = node;
    else {
        ((struct file_seq *)FileSeq.tail)->next = node;
        FileSeq.tail = node;
    }
}
#endif

/* add a new item to the top of the file stack.
 * is_linesrc: TRUE=item is a macro or a line queue.
 * sym = macro symbol or NULL (for a real file or the line queue)
 */
static struct file_list *PushLineSource( bool is_linesrc, struct asym *sym )
/**************************************************************************/
{
    struct file_list   *fl;

    fl = SrcAlloc( sizeof( struct file_list ) );
    fl->next = file_stack;
    fl->islinesrc = is_linesrc;
    fl->line_num = 0;
    fl->macro = sym;
    file_stack = fl;
#ifdef DEBUG_OUT
    currlqline = 0;
#endif
    return( fl );
}

/*
 * If there's a current line queue, push it onto the file stack.
 */

void NewLineQueue( void )
/***********************/
{
    DebugMsg1(( "NewLineQueue() enter [line_queue=%X]\n", line_queue ));
    if ( line_queue ) {
        struct file_list *fl;
        fl = PushLineSource( TRUE, NULL );
        queue_level++;
        fl->srcfile = get_curr_srcfile();
        fl->lines = line_queue;
        line_queue = NULL;
    }
#ifdef DEBUG_OUT
    currlqline = 0;
#endif
}

void DeleteLineQueue( void )
/**************************/
{
    if ( line_queue ) {
        FreeLineQueue( line_queue );
        line_queue = NULL;
    }
}

bool is_linequeue_populated( void )
/*********************************/
{
    return( line_queue != NULL );
}

/* Add a line to the current line queue. */

void AddLineQueue( const char *line )
/***********************************/
{
    unsigned i = strlen( line );
    struct line_list   *new;

    DebugMsg1(( "AddLineQueue(%u): >%s<\n", ++currlqline, line ));

    if ( line_queue == NULL ) {
        line_queue = SrcAlloc( sizeof( struct input_queue ) );
        line_queue->tail = NULL;
    }
    new = SrcAlloc( sizeof( struct line_list ) + i );
    new->next = NULL;
#ifdef DEBUG_OUT
    new->lineno = currlqline;
#endif
    memcpy( new->line, line, i + 1 );

    if( line_queue->tail == NULL ) {
        line_queue->head = new;
    } else {
        /* insert at the tail */
        line_queue->tail->next = new;
    }
    line_queue->tail = new;
    return;
}

/* Add a line to the current line queue, "printf" format. */

void AddLineQueueX( const char *fmt, ... )
/****************************************/
{
    va_list args;
    char *d;
    int i;
    long l;
    const char *s;
    const char *p;
    char buffer[MAX_LINE_LEN];

    //DebugMsg(("AddlineQueueX(%s) enter\n", fmt ));
    va_start( args, fmt );
    for ( s = fmt, d = buffer; *s; s++ ) {
        if ( *s == '%' ) {
            s++;
            switch ( *s ) {
            case 'r':
                i = va_arg( args, int );
                GetResWName( i , d );
                /* v2.06: the name is already copied */
                //memcpy( d, ResWordTable[i].name, ResWordTable[i].len );
                d += ResWordTable[i].len;
                break;
            case 's':
                p = va_arg( args, char * );
                i = strlen( p );
                memcpy( d, p, i );
                d += i;
                *d = NULLC;
                break;
            case 'd':
            case 'u':
            case 'x':
#ifdef __I86__ /* v2.08: use long only if size(int) is 16-bit */
                l = va_arg( args, long );
#else
                l = va_arg( args, int );
#endif
                if ( *s == 'x' ) {
                    myltoa( l, d, 16, FALSE, FALSE );
                    d += strlen( d );
                } else {
                    myltoa( l, d, 10, l < 0, FALSE );
                    d += strlen( d );
                    /* v2.07: add a 't' suffix if radix is != 10 */
                    if ( ModuleInfo.radix != 10 )
                        *d++ = 't';
                }
                break;
            default:
                *d++ = *s;
            }
        } else
            *d++ = *s;
    }
    *d = NULLC;
    va_end( args );
    //DebugMsg(("AddlineQueueX() done\n" ));
    AddLineQueue( buffer );
    return;
}

/* push the current line queue onto the file stack and
 * associate a macro name to it so it can be displayed
 * in case of errors. Param <line> is != 0 when GOTO
 * is handled.
 */
void PushMacro( struct asym *macro, struct macro_instance *mi, unsigned line )
/****************************************************************************/
{
    struct file_list *fl;

    DebugMsg1(( "PushMacro(%s), queue level=%u\n", macro->name, queue_level ));
    fl = PushLineSource( TRUE, macro );
    fl->mi = mi;
    //line_queue = NULL;
    queue_level++;
    fl->line_num = line;
    return;
}
#if FASTMEM==0
bool MacroInUse( struct dsym *macro )
/***********************************/
{
    struct file_list *fl;

    for ( fl = file_stack; fl ; fl = fl->next )
        if ( fl->macro == &macro->sym )
            return( TRUE );

    return( FALSE );
}
#endif

uint get_curr_srcfile( void )
/***************************/
{
#if 1
    struct file_list *fl;
    for ( fl = file_stack; fl ; fl = fl->next )
        if ( fl->islinesrc == FALSE )
            return( fl->srcfile );
    return( ModuleInfo.srcfile );
#else
    return( file_stack ? file_stack->srcfile : ModuleInfo.srcfile );
#endif
}

void set_curr_srcfile( uint file, uint_32 line_num )
/**************************************************/
{
    if ( file != 0xFFF ) /* 0xFFF is the special value for macro lines */
        file_stack->srcfile = file;
    file_stack->line_num = line_num;
    return;
}

/* for error listing, render the current source file and line */
/* this function is also called if pass is > 1,
 * which is a problem for FASTPASS because the file stack is empty.
 */
int GetCurrSrcPos( char *buffer )
/*******************************/
{
    struct file_list *fl;
    uint_32 line;

    line = LineNumber;
    for( fl = file_stack; fl && fl->islinesrc; fl = fl->next );
    if ( fl ) {
        if ( line )
            return( sprintf( buffer, "%s(%lu) : ", GetFName( fl->srcfile )->name , line ) );
        else
            return( sprintf( buffer, "%s : ", GetFName( fl->srcfile )->name ) );
    }
    *buffer = NULLC;
    return( 0 );
}

/* for error listing, render the source nesting structure.
 * the structure consists of include files and macros.
 */

void print_source_nesting_structure( void )
/*****************************************/
{
    struct file_list       *fl;
    unsigned        tab = 1;

    /* in main source file? */
    if ( file_stack == NULL || file_stack->next == NULL )
        return;

    for( fl = file_stack; fl->next ; fl = fl->next ) {
        if( fl->islinesrc == FALSE ) {
            PrintNote( NOTE_INCLUDED_BY, tab, "", GetFName( fl->srcfile)->name, fl->line_num );
            tab++;
        } else if ( fl->macro != NULL ) {
            //char fname[_MAX_FNAME+_MAX_EXT];
#ifndef __I86__ /* this function may be called on low stack condition */
            char fname[_MAX_FNAME];
            char fext[_MAX_EXT];
#endif
            if (*(fl->macro->name) == NULLC ) {
                PrintNote( NOTE_ITERATION_MACRO_CALLED_FROM, tab, "", "MacroLoop", fl->line_num, fl->macro->value + 1 );
            } else {
#ifdef __I86__
                PrintNote( NOTE_MACRO_CALLED_FROM, tab, "", fl->macro->name, fl->line_num, GetFName(((struct dsym *)fl->macro)->e.macroinfo->srcfile)->name, "" ) ;
#else
                _splitpath( GetFName(((struct dsym *)fl->macro)->e.macroinfo->srcfile)->name, NULL, NULL, fname, fext );
                PrintNote( NOTE_MACRO_CALLED_FROM, tab, "", fl->macro->name, fl->line_num, fname, fext );
#endif
            }
            tab++;
        }
    }
    PrintNote( NOTE_MAIN_LINE_CODE, tab, "", GetFName(fl->srcfile)->name, fl->line_num );
}

/* Scan the include path for a file!
 * variable IncludePath also contains directories set with -I cmdline option
 */
static FILE *open_file_in_include_path( const char *name, char fullpath[] )
/*************************************************************************/
{
    char            *curr;
    char            *next;
    int             i;
    int             namelen;
    FILE            *file = NULL;

    while( isspace( *name ) )
        name++;

    curr = ModuleInfo.g.IncludePath;
    namelen = strlen( name );

    DebugMsg(("open_file_in_include_path(%s) enter\n", name ));
    for ( ; curr; curr = next ) {
        next = strchr( curr, INC_PATH_DELIM );
        if ( next ) {
            i = next - curr;
            next++; /* skip path delimiter char (; or :) */
        } else {
            i = strlen( curr );
        }

        /* v2.06: ignore
         * - "empty" entries in PATH
         * - entries which would cause a buffer overflow
         */
        if ( i == 0 || ( ( i + namelen ) >= _MAX_PATH ) )
            continue;

        memcpy( fullpath, curr, i );
        if( fullpath[i-1] != '/'
#if !defined(__UNIX__)
           && fullpath[i-1] != '\\' && fullpath[i-1] != ':'
#endif
        ) {
            fullpath[i] = DIR_SEPARATOR;
            i++;
        }
        strcpy( fullpath+i, name );

        DebugMsg(("open_file_in_include_path: >%s<\n", fullpath ));
        file = fopen( fullpath, "rb" );
        if( file ) {
            break;
        }
    }
    DebugMsg(("open_file_in_include_path()=%p\n", file ));
    return( file );
}

/* the worker behind the INCLUDE directive. Also used
 * by INCBIN and to include the main source file.
 */

ret_code InputQueueFile( const char *path, FILE * *pfile )
/********************************************************/
{
    FILE        *file = NULL;
    struct file_list   *fl;
    char        fullpath[ _MAX_PATH ];
    char        buffer[ _MAX_PATH ];
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];
    char        fname[_MAX_FNAME];
    char        ext[_MAX_EXT];
    char        drive2[_MAX_DRIVE];
    char        dir2[_MAX_DIR];

    DebugMsg(("InputQueueFile(%s) enter\n", path ));

    _splitpath( path, drive, dir, fname, ext );
    DebugMsg(("InputQueueFile(): drive=%s, dir=%s, fname=%s, ext=%s\n", drive, dir, fname, ext ));

    /* if no absolute path is given, then search in the directory
     of the current source file first! */

    if ( dir[0] != '\\' && dir[0] != '/' ) {
        for ( fl = file_stack; fl ; fl = fl->next ) {
            if ( fl->islinesrc == FALSE ) {
                _splitpath( GetFName( fl->srcfile )->fullname, drive2, dir2, NULL, NULL );
                DebugMsg(("InputQueueFile(): curr src=%s, split into drive=%s, dir=%s\n", GetFName( fl->srcfile)->fullname, drive2, dir2 ));
                if ( dir2[0] == '\\' || dir2[0] == '/' ) {
                    _makepath( fullpath, drive2, dir2, fname, ext );
                    file = fopen( fullpath, "rb" );
                    DebugMsg(("InputQueueFile(): makepath()=%s, file=%X\n", fullpath, file ));
                }
                break;
            }
        }
    }
    if ( file == NULL ) {
        fullpath[0] = NULLC;
        file = fopen( path, "rb" );
        DebugMsg(("InputQueueFile(): file=%X\n", file ));
        /* if the file wasn't found, and include paths have been set,
         * and NO absolute path is given, then search include dirs */
        if( file == NULL && ModuleInfo.g.IncludePath != NULL && dir[0] != '\\' && dir[0] != '/' ) {
            file = open_file_in_include_path( path, fullpath );
            DebugMsg(("InputQueueFile(): open_file_in_include_path(%s, %s) returned file=%X\n", path, fullpath, file ));
        }
        if( file == NULL ) {
            EmitErr( CANNOT_OPEN_FILE, path, ErrnoStr() );
            return( ERROR );
        }
    }
    if ( pfile )
        *pfile = file;
    else {
        fl = PushLineSource( FALSE, NULL );
        fl->srcfile = AddFile( GetFullPath( fullpath[0] ? fullpath : path, buffer, sizeof( buffer ) ) );
        FileCur->string_ptr = GetFName( fl->srcfile )->name;
#if FILESEQ
        if ( Options.line_numbers && Parse_Pass == PASS_1 )
            AddFileSeq( fl->srcfile );
#endif
        fl->file = file;
    }
    return( NOT_ERROR );
}

/* get the next source line. */

char *GetTextLine( char *buffer )
/*******************************/
{
    struct line_list   *inputline;
    struct file_list   *fl;

    *buffer = NULLC;

    /* Check the line_queue first!
     * The line_queue is global and there is ONE only.
     * If it must be nested, it's pushed onto the file stack.
     */

    if ( line_queue != NULL ) {
        if ( inputline = line_queue->head ) {
            strcpy( buffer, inputline->line );
            line_queue->head = inputline->next;
            SrcFree( inputline );
#ifdef DEBUG_OUT
            lq_line++;
            if ( Parse_Pass == PASS_1 ) cntlines++;
#endif
            return( buffer );
        }
#ifdef DEBUG_OUT
        lq_line = 0;
#endif
        SrcFree( line_queue );
        line_queue = NULL;
        DebugMsg1(("GetTextLine: end of line queue\n" ));
        return( NULL );
    }
    /* Now check the file stack!
     * items on the file stack may be
     * - pushed line queues ( islinesrc == TRUE && macro == NULL )
     * - macro line queues ( islinesrc == TRUE && macro != NULL )
     * - assembly files. ( islinesrc == FALSE )
     */
    while( 1 ) {
        fl = file_stack;
        if( fl->islinesrc == FALSE ) {
            if( my_fgets( buffer, MAX_LINE_LEN, fl->file ) ) {
                fl->line_num++;
#ifdef DEBUG_OUT
                if ( Parse_Pass == PASS_1 ) cntlines++;
#endif
                return( buffer );
            }
            /* EOF of main module reached? */
            if ( fl->next == NULL )
                break;

            file_stack = fl->next;
            fclose( fl->file );
            DebugMsg1(("GetTextLine: ***** EOF file %s *****\n", GetFName( fl->srcfile )->name ));
            SrcFree( fl );

            for( fl = file_stack; fl->islinesrc; fl = fl->next );
            FileCur->string_ptr = GetFName( fl->srcfile)->name;
#if FILESEQ
            if ( Options.line_numbers && Parse_Pass == PASS_1 )
                AddFileSeq( fl->srcfile );
#endif

        } else if ( fl->macro ) {
            /* item is a macro */
            fl->mi->currline = ( fl->mi->currline ? fl->mi->currline->next : fl->mi->startline );
            if ( fl->mi->currline ) {
                /* if line contains placeholders, replace them by current values */
                if ( fl->mi->currline->ph_count ) {
                    fill_placeholders( buffer,
                                    fl->mi->currline->line,
                                    fl->mi->parmcnt,
                                    fl->mi->localstart, fl->mi->parm_array );
                } else {
                    strcpy( buffer, fl->mi->currline->line );
                }
                fl->line_num++;
#ifdef DEBUG_OUT
                if ( Parse_Pass == PASS_1 ) cntlines++;
#endif
                return( buffer );
            }
            queue_level--;
            file_stack = fl->next;
            SrcFree( fl );
            DebugMsg1(("GetTextLine: qlvl=%u, end of macro, file stack=%p\n", queue_level, file_stack ));
            break;
        } else {
            /* item is a line queue */
            inputline = fl->lines->head;
            if( inputline != NULL ) {
                fl->line_num++;
                strcpy( buffer, inputline->line );
                fl->lines->head = inputline->next;
                SrcFree( inputline );
#ifdef DEBUG_OUT
                if ( Parse_Pass == PASS_1 ) cntlines++;
#endif
                return( buffer );
            }
            queue_level--;
            file_stack = fl->next;
            SrcFree( fl->lines );
            SrcFree( fl );
            DebugMsg1(("GetTextLine: qlvl=%u, end of line queue, file stack=%p\n", queue_level, file_stack ));
            break;
        }
    }
    return( NULL ); /* end of main source file or macro reached */
}

/* add a string to the include path.
 * called for -I cmdline options.
 * the include path is rebuilt for each assembled module.
 * it is stored in the standard C heap.
 */
void AddStringToIncludePath( const char *string )
/***********************************************/
{
    char *tmp;
    int len;

    DebugMsg(("AddStringToIncludePath(%s) enter\n", string ));
    while( isspace( *string ) )
        string++;
    len = strlen( string );
    if ( len == 0 )
        return;
    if( ModuleInfo.g.IncludePath == NULL ) {
        ModuleInfo.g.IncludePath = MemAlloc( len + 1 );
        strcpy( ModuleInfo.g.IncludePath, string );
    } else {
        tmp = ModuleInfo.g.IncludePath;
        ModuleInfo.g.IncludePath = MemAlloc( strlen( tmp ) + sizeof( INC_PATH_DELIM_STR ) +
                                len + 1 );
        strcpy( ModuleInfo.g.IncludePath, tmp );
        strcat( ModuleInfo.g.IncludePath, INC_PATH_DELIM_STR );
        strcat( ModuleInfo.g.IncludePath, string );
        MemFree( tmp );
    }
}

#if 0
/* function to get value of @FileCur.
 * won't work, because text macros don't use asym.sfunc_ptr
 */
static void GetFileCur( struct asym *sym )
/****************************************/
{
    struct file_list *fl;

    for( fl = file_stack; fl && fl->islinesrc; fl = fl->next );
    sym->string_ptr = GetFName( fl->srcfile )->name;
    DebugMsg1(("GetFileCur: curr value=%s\n", sym->string_ptr ));
}
#endif

#ifdef __I86__
#define SIZE_SRCLINES     ( MAX_LINE_LEN * 2 )
#define SIZE_TOKENARRAY   ( sizeof( struct asm_tok ) * MAX_TOKEN )
#define SIZE_STRINGBUFFER ( MAX_LINE_LEN * 2 )
#else
#define SIZE_SRCLINES     ( MAX_LINE_LEN * ( MAX_MACRO_NESTING + 1 ) )
#define SIZE_TOKENARRAY   ( sizeof( struct asm_tok ) * MAX_TOKEN * MAX_MACRO_NESTING )
#define SIZE_STRINGBUFFER ( MAX_LINE_LEN * MAX_MACRO_NESTING )
#endif

/* Initializer, called once for each module. */

void InputInit( void )
/********************/
{
    struct file_list   *fl;
    char        path[_MAX_PATH];
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];

    DebugMsg(( "InputInit() enter\n" ));
    //cnt_fnames = 0;
    //FNamesTab = NULL;
    //IncludePath = NULL;
    //file_stack = NULL;
#if FILESEQ
    FileSeq.head = NULL;
#endif
    fl = PushLineSource( FALSE, NULL );
    fl->file = CurrFile[ASM];
    fl->srcfile = ModuleInfo.srcfile = AddFile( CurrFName[ASM] );
    /* setting a function pointer won't work for text macros! */
    //FileCur->sfunc_ptr = &GetFileCur;
    FileCur->string_ptr = GetFName( fl->srcfile )->name;

#ifdef DEBUG_OUT
    cntppl0 = 0;
    cntppl1 = 0;
    cntppl2 = 0;
    cnttok0 = 0;
    cnttok1 = 0;
    cntflines = 0;
    cntlines = 0;
#endif

    /* add path of main module to the include path */
    _splitpath( CurrFName[ASM], drive, dir, NULL, NULL );
    if ( drive[0] || dir[0] ) {
        _makepath( path, drive, dir, NULL, NULL );
        AddStringToIncludePath( path );
    }

    srclinebuffer = LclAlloc( SIZE_SRCLINES + SIZE_TOKENARRAY + SIZE_STRINGBUFFER );
    /* the comment buffer is at the end of the source line buffer */
    commentbuffer = srclinebuffer + SIZE_SRCLINES - MAX_LINE_LEN;
    /* behind the comment buffer is the token buffer */
    ModuleInfo.tokenarray = (struct asm_tok *)( srclinebuffer + SIZE_SRCLINES );
    token_stringbuf = srclinebuffer + SIZE_SRCLINES + SIZE_TOKENARRAY;
#ifdef DEBUG_OUT
    end_tokenarray = (struct asm_tok *)token_stringbuf;
    end_stringbuf = token_stringbuf + SIZE_STRINGBUFFER;
#endif
    DebugMsg(( "InputInit() exit\n" ));
}

/* init for each pass */

void InputPassInit( void )
/************************/
{
    line_queue = NULL;
    queue_level = 0;
    file_stack->line_num = 0;
    inside_comment = NULLC;
    CurrSource = srclinebuffer;
    *CurrSource = NULLC;
    StringBufferEnd = token_stringbuf;
    return;
}

void InputFini( void )
/********************/
{
#ifdef DEBUG_OUT
    int   i;
    for( i = 0; i < ModuleInfo.g.cnt_fnames; i++) {
        DebugMsg(( "InputFini: idx=%u name=%s full=%s\n", i, FNamesTab[i].name, FNamesTab[i].fullname ));
    }
#endif
    if ( ModuleInfo.g.IncludePath )
        MemFree( ModuleInfo.g.IncludePath );
    FreeFiles();
#ifdef DEBUG_OUT
    if ( Options.quiet == FALSE ) {
        printf("lines read(files)/processed in pass one: %lu / %lu\n", cntflines, cntlines );
        printf("invokations: GetPreprocessedLine=%lu/%lu/%lu, Tokenize=%lu/%lu\n", cntppl0, cntppl1, cntppl2, cnttok0, cnttok1 );
    }
#endif
    ModuleInfo.tokenarray = NULL;
#ifdef DEBUG_OUT
    token_stringbuf = NULL;
    StringBufferEnd = NULL;
    commentbuffer = NULL;
    CurrSource = NULL;
#endif
    LclFree( srclinebuffer );
    /* v2.03: clear file stack to ensure that GetCurrSrcPos()
     * won't find something when called from main().
     */
    file_stack = NULL;
}

struct asm_tok *PushInputStatus( struct input_status *oldstat )
/*************************************************************/
{
    oldstat->token_stringbuf = token_stringbuf;
    oldstat->token_count = Token_Count;
    oldstat->currsource = CurrSource;
    /* if there's a comment, attach it to current source */
    if ( ModuleInfo.CurrComment ) {
        int i = strlen( CurrSource );
        oldstat->CurrComment = CurrSource + i;
        strcpy( oldstat->CurrComment, ModuleInfo.CurrComment );
    } else
        oldstat->CurrComment = NULL;
    oldstat->line_flags = ModuleInfo.line_flags; /* v2.08 */
#ifdef __I86__
    oldstat->tokenarray = ModuleInfo.tokenarray;
    oldstat->stringbufferend = StringBufferEnd;
    CurrSource = MemAlloc( MAX_LINE_LEN + SIZE_TOKENARRAY + SIZE_STRINGBUFFER );
    ModuleInfo.tokenarray = (struct asm_tok *)( CurrSource + MAX_LINE_LEN );
    token_stringbuf = CurrSource + MAX_LINE_LEN + SIZE_TOKENARRAY;
#else
    token_stringbuf = StringBufferEnd;
    ModuleInfo.tokenarray += Token_Count + 1;
    CurrSource = GetAlignedPointer( CurrSource, strlen( CurrSource ) );
    /**/myassert( ( CurrSource + MAX_LINE_LEN ) <= (char *)ModuleInfo.tokenarray );
    /**/myassert( ( ModuleInfo.tokenarray + sizeof( struct asm_tok ) * MAX_TOKEN ) <= end_tokenarray );
    /**/myassert( ( token_stringbuf + 2 * MAX_LINE_LEN ) <= end_stringbuf );
#endif
    DebugMsg1(("PushInputStatus() stringbuf-tokencnt-currsrc old=%X-%u-%X new=%X-%X-%X\n",
               oldstat->token_stringbuf, oldstat->token_count, oldstat->currsource,
               token_stringbuf, ModuleInfo.tokenarray, CurrSource ));
    return( ModuleInfo.tokenarray );
}

void PopInputStatus( struct input_status *newstat )
/*************************************************/
{
    DebugMsg1(("PopInputStatus() old=%X-%u-%X new=%X-%u-%X\n",
               token_stringbuf, Token_Count, CurrSource,
               newstat->token_stringbuf, newstat->token_count, newstat->currsource ));
#ifdef __I86__
    MemFree( CurrSource );
#else
    StringBufferEnd = token_stringbuf;
#endif
    token_stringbuf = newstat->token_stringbuf;
    Token_Count = newstat->token_count;
    CurrSource = newstat->currsource;
    if ( newstat->CurrComment ) {
        ModuleInfo.CurrComment = commentbuffer;
        strcpy( ModuleInfo.CurrComment, newstat->CurrComment );
        *newstat->CurrComment = NULLC;
    } else
        ModuleInfo.CurrComment = NULL;
#ifdef __I86__
    StringBufferEnd = newstat->stringbufferend;
    ModuleInfo.tokenarray = newstat->tokenarray;
#else
    ModuleInfo.tokenarray -= Token_Count + 1;
#endif
    ModuleInfo.line_flags = newstat->line_flags; /* v2.08 */
    return;
}

/* INCLUDE directive.
 * If a full path is specified, the directory where the included file
 * is located becomes the "source" directory, that is, it is searched
 * FIRST if further INCLUDE directives are found inside the included file.
 */
ret_code IncludeDirective( int i, struct asm_tok tokenarray[] )
/*************************************************************/
{

    DebugMsg(("IncludeDirective enter\n"));

    if ( CurrFile[LST] ) {
        LstWriteSrcLine();
    }

    i++; /* skip directive */
    /* v2.03: allow plain numbers as file name argument */
    //if ( tokenarray[i].token == T_FINAL || tokenarray[i].token == T_NUM ) {
    if ( tokenarray[i].token == T_FINAL ) {
        EmitError( EXPECTED_FILE_NAME );
        return( ERROR );
    }

    /* if the filename is enclosed in <>, just use this literal */

    if ( tokenarray[i].token == T_STRING && tokenarray[i].string_delim == '<' ) {
        if ( tokenarray[i+1].token != T_FINAL ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i+1].tokpos );
        } else {
            /* v2.08: use GetLiteralValue() to translate the name */
            GetLiteralValue( StringBufferEnd, tokenarray[i].string_ptr );
            InputQueueFile( StringBufferEnd, NULL );
        }
    } else {
        char *name;
        char *p;
        /* if the filename isn't enclosed in <>, use anything that comes
         * after INCLUDE
         */
        name = tokenarray[i].tokpos;
        for ( p = tokenarray[Token_Count].tokpos - 1; p > name && isspace(*p); *p = NULLC, p-- );
        InputQueueFile( name, NULL );
    }
    return( NOT_ERROR );
}

/* Read the current queue until it's done.
 */

void SkipCurrentQueue( struct asm_tok tokenarray[] )
/**************************************************/
{
    char buffer[MAX_LINE_LEN];

    /* The queue isn't just thrown away, because any
     * coditional assembly directives found in the source
     * must be executed.
     */
    while ( GetTextLine( buffer ) ) {
        Tokenize( buffer, 0, tokenarray, TOK_DEFAULT );
    }

}

/* preprocessor directive or macro procedure is preceded
 * by a code label.
 */
ret_code WriteCodeLabel( char *line, struct asm_tok tokenarray[] )
/****************************************************************/
{
    int oldcnt;
    int oldtoken;
    char oldchar;

    if ( tokenarray[0].token != T_ID ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
        return( ERROR );
    }
    /* ensure the listing is written with the FULL source line */
    if ( CurrFile[LST] ) LstWrite( LSTTYPE_LABEL, 0, NULL );
    /* v2.04: call ParseLine() to parse the "label" part of the line */
    oldcnt = Token_Count;
    oldtoken = tokenarray[2].token;
    oldchar = *tokenarray[2].tokpos;
    Token_Count = 2;
    tokenarray[2].token = T_FINAL;
    *tokenarray[2].tokpos = NULLC;
    ParseLine( tokenarray );
    if ( Options.preprocessor_stdout == TRUE )
        WritePreprocessedLine( line );
    Token_Count = oldcnt;
    tokenarray[2].token = oldtoken;
    *tokenarray[2].tokpos = oldchar;
    return( NOT_ERROR );
}

/* GetPreprocessedLine() is the "preprocessor".
 * 1. a line is read with GetTextLine()
 * 2. the line is tokenized with Tokenize(), Token_Count set
 * 3. (text) macros are expanded by ExpandLine()
 * 4. "preprocessor" directives are executed
 */
int GetPreprocessedLine( char *line, struct asm_tok tokenarray[] )
/****************************************************************/
{
    int i;

    if( GetTextLine( line ) == NULL ) {
        DebugMsg1(("GetPreprocessedLine: GetTextLine() returned NULL (end of file/macro)\n" ));
        return( -1 ); /* EOF */
    }
    /* v2.08: moved here from GetTextLine() */
    ModuleInfo.CurrComment = NULL;
    /* v2.06: moved here from Tokenize() */
    ModuleInfo.line_flags = 0;
    /* Token_Count is the number of tokens scanned */
    Token_Count = Tokenize( line, 0, tokenarray, TOK_DEFAULT );

#ifdef DEBUG_OUT
    cntppl0++;
    if ( file_stack && file_stack->islinesrc ) {
        if ( file_stack->macro )
            DebugMsg1(("GetPreprocessedLine(mac=%s): >%s<\n", file_stack->macro->name, line ));
        else
            DebugMsg1(("GetPreprocessedLine: >%s<\n", line ));
    } else if ( file_stack && file_stack->srcfile ) {
        DebugMsg1(("GetPreprocessedLine(%s): >%s<\n", GetFName( file_stack->srcfile )->name, line ));
    } else
        DebugMsg1(("GetPreprocessedLine(cnt=%u): >%s<\n", Token_Count, line));
#endif

#if REMOVECOMENT == 0
    if ( Token_Count == 0 && ( CurrIfState == BLOCK_ACTIVE || ModuleInfo.listif ) )
        LstWriteSrcLine();
#endif

#ifdef DEBUG_OUT
    /* option -np, skip preprocessor? */
    if ( Options.skip_preprocessor )
        return( Token_Count );
#endif

    if ( Token_Count == 0 )
        return( 0 );

    /* CurrIfState != BLOCK_ACTIVE && Token_Count == 1 | 3 may happen
     * if a conditional assembly directive has been detected by Tokenize().
     * However, it's important NOT to expand then */
    if ( CurrIfState == BLOCK_ACTIVE ) {
        if ( ( tokenarray[Token_Count].bytval & TF3_EXPANSION ? ExpandText( line, tokenarray, TRUE ) : ExpandLine( line, tokenarray ) ) != NOT_ERROR )
            return( 0 );
    }

#ifdef DEBUG_OUT
    cntppl1++;
#endif

    i = 0;
    if ( Token_Count > 2 && ( tokenarray[1].token == T_COLON || tokenarray[1].token == T_DBL_COLON ) )
        i = 2;

    /* handle "preprocessor" directives:
     * IF, ELSE, ENDIF, ...
     * FOR, REPEAT, WHILE, ...
     * PURGE
     * INCLUDE
     * since v2.05, error directives are no longer handled here!
     */
    if ( tokenarray[i].token == T_DIRECTIVE &&
        tokenarray[i].dirtype <= DRT_INCLUDE ) {

        /* if i != 0, then a code label is located before the directive */
        if ( i > 1 ) {
            if ( ERROR == WriteCodeLabel( line, tokenarray ) )
                return( 0 );
        }
        directive[tokenarray[i].dirtype]( i, tokenarray );
        return( 0 );
    }

    /* handle preprocessor directives which need a label */

    if ( tokenarray[0].token == T_ID && tokenarray[1].token == T_DIRECTIVE ) {
        struct asym *sym;
        switch ( tokenarray[1].dirtype ) {
        case DRT_EQU:
            /*
             * EQU is a special case:
             * If an EQU directive defines a text equate
             * it MUST be handled HERE and 0 must be returned to the caller.
             * This will prevent further processing, nothing will be stored
             * if FASTPASS is on.
             * Since one cannot decide whether EQU defines a text equate or
             * a number before it has scanned its argument, we'll have to
             * handle it in ANY case and if it defines a number, the line
             * must be stored and, if -EP is set, written to stdout.
             */
            if ( sym = CreateConstant( tokenarray ) ) {
                if ( sym->state != SYM_TMACRO ) {
#if FASTPASS
                    if ( StoreState ) FStoreLine(0);
#endif
                    if ( Options.preprocessor_stdout == TRUE )
                        WritePreprocessedLine( line );
                }
                /* v2.03: LstWrite() must be called AFTER StoreLine()! */
                if ( ModuleInfo.list == TRUE ) {
                    LstWrite( LSTTYPE_EQUATE, 0, sym );
                }
            }
            return( 0 );
        case DRT_MACRO:
        case DRT_CATSTR: /* CATSTR + TEXTEQU directives */
        case DRT_SUBSTR:
            directive[tokenarray[1].dirtype]( 1, tokenarray );
            return( 0 );
        }
    }

#ifdef DEBUG_OUT
    cntppl2++;
#endif
    return( Token_Count );
}
