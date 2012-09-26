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
* Description:  Memory allocation routines.
*
****************************************************************************/

/*
 * if TRMEM is defined, trmem functions are used which will help tracking
 * memory usage.
 */

#if defined(__UNIX__) && defined(__GNUC__)
    #include <sys/mman.h>
#endif

#include "globals.h"

/* FASTMEM is a simple memory alloc approach which allocates chunks of 512 kB
 * and will release it only at MemFini().
 *
 * May be considered to create an additional "line heap" to store lines of
 * loop macros and generated code - since this is hierarchical, a simple
 * Mark/Release mechanism will do the memory management.
 * currently generated code lines are stored in the C heap, while
 * loop macro lines go to the "fastmem" heap.
 */
#if FASTMEM
 #define BLKSIZE 0x80000
 #ifndef __UNIX__
  #if defined(__OS2__)
   #include "os2.h"
  #elif defined(__DJGPP__)
   #include "dpmi.h"
  #else
   #include "win32.h"
  #endif
 #endif
#endif

/* what items are stored in the heap?
 * - symbols + symbol names ( asym, dsym; symbol.c )
 * - macro lines ( StoreMacro(); macro.c )
 * - file names ( CurrFName[]; assemble.c )
 * - temp items + buffers ( omf.c, bin.c, coff.c, elf.c )
 * - contexts ( context.c )
 * - codeview debug info ( dbgcv.c )
 * - library names ( includelib; directiv.c )
 * - src lines for FASTPASS ( fastpass.c )
 * - fixups ( fixup.c )
 * - hll items (reused!) ( .IF, .WHILE, .REPEAT; hll.c )
 * - one big input buffer ( src line buffer, tokenarray, string buffer; input.c )
 * - src filenames array ( AddFile(); input.c )
 * - line number info ( -Zd, -Zi; linnum.c )
 * - macro parameter array + default values ( macro.c )
 * - prologue, epilogue macro names ??? ( option.c )
 * - dll names ( OPTION DLLIMPORT; option.c )
 * - std queue items ( see queues in struct module_vars; globals.h, queue.c )
 * - renamed keyword queue ( reswords.c )
 * - safeseh queue ( safeseh.c )
 * - segment alias names ( segment.c )
 * - segment stack ( segment.c )
 * - segment buffers ( 1024 for omf, else may be HUGE ) ( segment.c )
 * - segment names for simplified segment directives (simsegm.c )
 * - strings of text macros ( string.c )
 * - struct/union/record member items + default values ( types.c )
 * - procedure prologue argument, debug info ( proc.c )
 */

#include "memalloc.h"

#ifdef TRMEM
#include <fcntl.h>
#ifdef __UNIX__
 #include <unistd.h>
#else
 #include <io.h>
#endif
#include <sys/stat.h>
#include "trmem.h"

static _trmem_hdl   memHandle;
static int          memFile;     /* file handle we'll write() to */
static int          memcalls;
#endif

#ifdef TRMEM
static void memLine( int *fh, const char *buf, unsigned size )
{
    write( 2, "***", 3 );
    write( 2, buf, size );
    if( *fh != -1 ) {
        write( *fh, buf, size );
    }
}
#endif

#if defined(__UNIX__) && defined(__WATCOMC__)

#define SYS_mmap                 90
#define SYS_munmap               91

uint_32 sys_call1( uint_32 func, uint_32 r_ebx );
#pragma aux sys_call1 =                         \
    "int    0x80"                               \
    parm [eax] [ebx]                            \
    value [eax];

uint_32 sys_call2( uint_32 func, uint_32 r_ebx, uint_32 r_ecx );
#pragma aux sys_call2 =                         \
    "int    0x80"                               \
    parm [eax] [ebx] [ecx]                      \
    value [eax];

struct mmap {
    uint_32 base;   /* linear base (or 0) */
    uint_32 size;   /* size in bytes */
    uint_32 access; /* prot_read + prot_write = 3 */
    uint_32 flags;  /* 0x22 = map_anonymous + map_private */
    uint_32 fd;     /* should be -1 */
    uint_32 offset; /* ignored */
};
/* 0x22 = MAP_PRIVATE | MAP_ANON */
static struct mmap mymmap = {0, 0, 3, 0x22, -1, 0};
#endif
#if defined(__GNUC__)
uint_32 mymmap_size = 0;   /* size in bytes */
#endif

#if FASTMEM
static uint_8 *pBase; /* start list of 512 kB blocks */
static uint_8 *pCurr; /* points into current block */
static int currfree;  /* free memory left in current block */
#ifdef DEBUG_OUT
static int blocks;    /* number of blocks allocated so far */
#endif
#endif

void MemInit( void )
/******************/
{
#ifdef TRMEM
    memFile = _open( "~jwasm.trk", O_WRONLY | O_CREAT | O_TRUNC, S_IREAD | S_IWRITE );
    memHandle = _trmem_open( malloc, free, realloc, _expand, &memFile, memLine,
        _TRMEM_ALLOC_SIZE_0 |
        _TRMEM_FREE_NULL |
        _TRMEM_OUT_OF_MEMORY |
        _TRMEM_CLOSE_CHECK_FREE
    );
    if( memHandle == NULL ) {
        exit( EXIT_FAILURE );
    }
    memcalls = 0;
#endif
#if FASTMEM
    pBase = NULL;
    currfree = 0;
#ifdef DEBUG_OUT
    blocks = 0;
#endif
#endif
}

void MemFini( void )
/******************/
{
#ifdef TRMEM
    if( memHandle != NULL ) {
        _trmem_prt_list( memHandle );
        _trmem_close( memHandle );
        if( memFile != -1 ) {
            _close( memFile );
        }
        memHandle = NULL;
    }
#endif

#if FASTMEM
#ifdef DEBUG_OUT
    if ( Options.quiet == FALSE )
        printf("memory used: %u kB\n", (blocks * BLKSIZE - currfree) / 1024);
#endif
    while (pBase) {
        uint_8 * pNext = *((uint_8 * *)pBase);
#ifndef __UNIX__
 #if defined(__OS2__)
        DosFreeMem( pBase );
 #elif defined(__NT__) || defined(_WIN64)
        VirtualFree( pBase, 0, MEM_RELEASE );
 #else
        free( pBase );
 #endif
#else
  #if defined(__WATCOMC__)
        sys_call2( SYS_munmap, (uint_32)pBase, 0 );
  #else
        munmap( (void *)pBase, 0 );
  #endif
#endif
        pBase = pNext;
    }
#endif
}

void *LclAlloc( size_t size )
/***************************/
{
    void        *ptr;

#if FASTMEM
    size = (size + 3) & ~3;
    if ( currfree < size ) {
        DebugMsg(("LclAlloc: new block, req. size=%Xh\n", size ));
        if ( size > BLKSIZE-4 ) {
#ifndef __UNIX__
 #if defined(__OS2__)
            DosAllocMem( (void**)&pCurr, size+4, PAG_COMMIT|PAG_READ|PAG_WRITE);
 #elif defined(__NT__) || defined(_WIN64)
            pCurr = (uint_8 *)VirtualAlloc( NULL, size+4, MEM_COMMIT, PAGE_READWRITE );
 #else
            pCurr = malloc( size+4 );
 #endif
#else
 #if defined(__GNUC__)
            mymmap_size = size+4;
            pCurr = (char *)mmap( 0, mymmap_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0 );
            if ( pCurr == MAP_FAILED )
                pCurr = NULL;
 #else
            mymmap.size = size+4;
            pCurr = (char *)sys_call1( SYS_mmap, (uint_32)&mymmap);
 #endif
#endif
            currfree = size;
        } else {
#ifndef __UNIX__
 #if defined(__OS2__)
            DosAllocMem( (void **)&pCurr, BLKSIZE, PAG_COMMIT|PAG_READ|PAG_WRITE );
 #elif defined(__NT__) || defined(_WIN64)
            pCurr = (uint_8 *)VirtualAlloc(NULL, BLKSIZE, MEM_COMMIT, PAGE_READWRITE);
 #else
            pCurr = malloc( BLKSIZE );
 #endif
#else
 #if defined(__GNUC__)
            mymmap_size = BLKSIZE;
            pCurr = (char *)mmap( 0, mymmap_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0 );
            if ( pCurr == MAP_FAILED )
                pCurr = NULL;
 #else
            mymmap.size = BLKSIZE;
            pCurr = (char *)sys_call1( SYS_mmap, (uint_32)&mymmap);
 #endif
#endif
            currfree = BLKSIZE - sizeof(uint_8 *);
        }
        if ( !pCurr ) {
            currfree = 0;
            Fatal( OUT_OF_MEMORY );
        }
        *(uint_8 * *)pCurr = pBase;
        pBase = pCurr;
        pCurr += sizeof(uint_8 *);
#ifdef DEBUG_OUT
        blocks++;
#endif
    }
    ptr = pCurr;
    pCurr += size;
    currfree -= size;

#else /* ! FASTMEM */

#ifdef TRMEM
    ptr = _trmem_alloc( size, _trmem_guess_who(), memHandle );
    memcalls++;
    DebugMsg(("LclAlloc(0x%X)=%X cnt=%d\n", size, ptr, memcalls ));
#else
    ptr = malloc( size );
#endif
    if( ptr == NULL ) {
        Fatal( OUT_OF_MEMORY );
    }
#endif
    return( ptr );
}

#if FASTMEM==0
void LclFree( void *ptr )
/***********************/
{
    if( ptr != NULL ) {
#ifdef TRMEM
        _trmem_free( ptr, _trmem_guess_who(), memHandle );
        DebugMsg(("LclFree(0x%X) cnt=%d\n", ptr, memcalls ));
        memcalls--;
#else
        free( ptr );
#endif
    }
}
#endif

void *MemAlloc( size_t size )
/***************************/
{
    void        *ptr;
    ptr = malloc( size );
#ifdef TRMEM
    memcalls++;
    DebugMsg(("MemAlloc(0x%X)=%X cnt=%d\n", size, ptr, memcalls ));
#endif
    if( ptr == NULL ) {
        Fatal( OUT_OF_MEMORY );
    }
    return( ptr );
}

void MemFree( void *ptr )
/***********************/
{
#ifdef TRMEM
    DebugMsg(("MemFree(0x%X) cnt=%d\n", ptr, memcalls ));
    memcalls--;
#endif
    free( ptr );
    return;
}

#if 0
void *MemRealloc( void *ptr, size_t size )
/****************************************/
{
    void *new;

    new = realloc( ptr, size );
    if( new == NULL && size != 0 ) {
        Fatal( OUT_OF_MEMORY );
    }
    return( new );
}
#endif

#if 0 //def DEBUG_OUT
#ifdef __WATCOMC__
/* the C heap is used for line queues only - due to speed issues. */
/* so this heap check function has become less useful. */
void heap( char *func )
/*********************/
{
    switch(_heapchk()) {
    case _HEAPBADNODE:
    case _HEAPBADBEGIN:
    DebugMsg(("Function : %s - ", func ));
        DebugMsg(("ERROR - heap is damaged\n"));
        exit(1);
        break;
    default:
        break;
    }
}
#endif
#endif


