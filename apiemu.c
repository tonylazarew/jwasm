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
*  Description: API emulations:
*               gcc:     _makepath(), _splitpath(), _fullpath(), strupr()
*               OW:      CharUpperA()
*               PellesC: _makepath()
*
****************************************************************************/

#if defined(__UNIX__) || defined(__CYGWIN__) || defined(__DJGPP__)

#include <unistd.h>
#include "globals.h"

#define __set_errno( err ) errno = (err)

/****************************************************************************
*
* Description:  Platform independent _splitpath() implementation.
*
****************************************************************************/

#if defined(__UNIX__)
#define PC '/'
#define ISPC(x) (x == PC)
#else
#define PC '\\'
#define ALT_PC '/'
#define ISPC(x) ((x == PC) || (x == ALT_PC))
#endif

static void copypart( char *buf, const char *p, int len, int maxlen )
/*******************************************************************/
{
    if( buf != NULL ) {
        if( len > maxlen ) len = maxlen;
        memcpy( buf, p, len );
        buf[len] = '\0';
    }
}

#if !defined(_MAX_NODE)
#define _MAX_NODE   _MAX_DRIVE  /*  maximum length of node name w/ '\0' */
#endif

/* split full path name into its components */

/* Under QNX we will map drive to node, dir to dir, and
 * filename to (filename and extension)
 *          or (filename) if no extension requested.
 */

void _splitpath( const char *path, char *drive, char *dir, char *fname, char *ext )
/*********************************************************************************/
{
    const char *dotp;
    const char *fnamep;
    const char *startp;
    unsigned    ch;

    /* take apart specification like -> //0/hd/user/fred/filename.ext for QNX */
    /* take apart specification like -> c:\fred\filename.ext for DOS, OS/2 */

    /* process node/drive specification */
    startp = path;
    if( path[0] == PC  &&  path[1] == PC ) {
        path += 2;
        for( ;; ) {
            if( *path == '\0' ) break;
            if( *path == PC ) break;
            if( *path == '.' ) break;
            path++;
        }
    }
    copypart( drive, startp, path - startp, _MAX_NODE );

    /* process /user/fred/filename.ext for QNX */
    /* process /fred/filename.ext for DOS, OS/2 */
    dotp = NULL;
    fnamep = path;
    startp = path;

    for(;;) {           /* 07-jul-91 DJG -- save *path in ch for speed */
        if( *path == '\0' )  break;
        ch = *path;
        if( ch == '.' ) {
            dotp = path;
            ++path;
            continue;
        }
        path++;
        if( ISPC(ch) ) {
            fnamep = path;
            dotp = NULL;
        }
    }
    copypart( dir, startp, fnamep - startp, _MAX_DIR - 1 );
    if( dotp == NULL ) dotp = path;
    copypart( fname, fnamep, dotp - fnamep, _MAX_FNAME - 1 );
    copypart( ext,   dotp,   path - dotp,   _MAX_EXT - 1);
}

/* create full Unix style path name from the components */

void _makepath( char *path, const char *node, const char *dir, const char *fname, const char *ext )
/*************************************************************************************************/
{
    *path = '\0';

    if( node != NULL ) {
        if( *node != '\0' ) {
            strcpy( path, node );
            path = strchr( path, '\0' );

            /* if node did not end in '/' then put in a provisional one */
            if( path[-1] == PC )
                path--;
            else
                *path = PC;
        }
    }
    if( dir != NULL ) {
        if( *dir != '\0' ) {
            /*  if dir does not start with a '/' and we had a node then
                    stick in a separator
            */
            if( ( ! ISPC(*dir) ) && ( ISPC(*path) ) ) path++;

            strcpy( path, dir );
            path = strchr( path, '\0' );

            /* if dir did not end in '/' then put in a provisional one */
            if( path[-1] == PC )
                path--;
            else
                *path = PC;
        }
    }

    if( fname != NULL ) {
        if( ( !ISPC(*fname) ) && ( ISPC(*path) ) ) path++;

        strcpy( path, fname );
        path = strchr( path, '\0' );

    } else {
        if( ISPC(*path) ) path++;
    }
    if( ext != NULL ) {
        if( *ext != '\0' ) {
            if( *ext != '.' )  *path++ = '.';
            strcpy( path, ext );
            path = strchr( path, '\0' );
        }
    }
    *path = '\0';
}

#define _WILL_FIT( c )  if(( (c) + 1 ) > size ) {       \
                            __set_errno( ERANGE );      \
                            return( NULL );             \
                        }                               \
                        size -= (c);

static char *_sys_fullpath( char *buff, const char *path, size_t size )
/*********************************************************************/
{

    const char  *p;
    char        *q;
    size_t      len;
    char        curr_dir[_MAX_PATH];

    p = path;
    q = buff;
    if( ! ISPC( p[0] ) ) {
        if( getcwd( curr_dir, sizeof(curr_dir) ) == NULL ) {
            __set_errno( ENOENT );
            return( NULL );
        }
        len = strlen( curr_dir );
        _WILL_FIT( len );
        strcpy( q, curr_dir );
        q += len;
        if( q[-1] != '/' ) {
            _WILL_FIT( 1 );
            *(q++) = '/';
        }
        for(;;) {
            if( p[0] == '\0' ) break;
            if( p[0] != '.' ) {
                _WILL_FIT( 1 );
                *(q++) = *(p++);
                continue;
            }
            ++p;
            if( ISPC( p[0] ) ) {
                /* ignore "./" in directory specs */
                if( ! ISPC( q[-1] ) ) {
                    *q++ = '/';
                }
                ++p;
                continue;
            }
            if( p[0] == '\0' ) break;
            if( p[0] == '.' && ISPC( p[1] ) ) {
                /* go up a directory for a "../" */
                p += 2;
                if( ! ISPC( q[-1] ) ) {
                    return( NULL );
                }
                q -= 2;
                for(;;) {
                    if( q < buff ) {
                        return( NULL );
                    }
                    if( ISPC( *q ) ) break;
                    --q;
                }
                ++q;
                *q = '\0';
                continue;
            }
            _WILL_FIT( 1 );
            *(q++) = '.';
        }
        *q = '\0';
    } else {
        len = strlen( p );
        _WILL_FIT( len );
        strcpy( q, p );
    }
    return( buff );
}

char *_fullpath( char *buff, const char *path, size_t size )
/**********************************************************/
{
    char *ptr = NULL;

    if( buff == NULL ) {
        size = _MAX_PATH;
        ptr = malloc( size );
        if( ptr == NULL ) __set_errno( ENOMEM );
        buff = ptr;
    }
    if( buff != NULL ) {
        buff[0] = '\0';
        if( path == NULL || path[0] == '\0' ) {
            buff = getcwd( buff, size );
        } else {
            buff = _sys_fullpath( buff, path, size );
        }
        if( buff == NULL ) {
            if( ptr != NULL ) free( ptr );
        }
    }
    return( buff );
}

#if 0
int _memicmp( const void *in_s1, const void *in_s2, size_t len )
/**************************************************************/
{
    const unsigned char *s1 = (const unsigned char *)in_s1;
    const unsigned char *s2 = (const unsigned char *)in_s2;
    unsigned char        c1;
    unsigned char        c2;

    for( ; len; --len )  {
        c1 = *s1;
        c2 = *s2;
        if( c1 >= 'A'  &&  c1 <= 'Z' )  c1 += 'a' - 'A';
        if( c2 >= 'A'  &&  c2 <= 'Z' )  c2 += 'a' - 'A';
        if( c1 != c2 ) return( c1 - c2 );
        ++s1;
        ++s2;
    }
    return( 0 );    /* both operands are equal */
}
#endif

#if defined(__UNIX__)

char *strupr( char *str )
/***********************/
{
    char    *p;
    unsigned char   c;

    p = str;
    while( (c = *p) ) {
        c -= 'a';
        if( c <= 'z' - 'a' ) {
            c += 'A';
            *p = c;
        }
        ++p;
    }
    return( str );
}
#endif

#endif

/* emulations for Open Watcom */

#if defined(__WATCOMC__) && !defined(__UNIX__)

#ifdef __FLAT__

#ifndef DEBUG_OUT /* OW v1.8 WDW has a problem with locally defined imports */

union cu {
    int c;
    char *p;
};

/* this is an emulation of the Win32 function which is called
 * by the OW runtime. It's the only USER32 function used.
 * By defining it here the binary will just need KERNEL32 to load.
 */
char * _stdcall CharUpperA( char *lpsz )
/**************************************/
{
    union cu p;
    p.p = lpsz;

    if ( p.c < 0x10000 )
        if ( p.c >= 'a' )
            return( (char *)p.c - 0x20 );
        else
            return( (char *)p.c );
    else
        for ( ; *p.p; p.p++ )
            if ( *p.p >= 'a' )
                *p.p = *p.p - 0x20;
    return( lpsz );
}
#endif
#endif
#endif

/* emulations for Pelles C */

#ifdef __POCC__

#include "globals.h"

/* There's an error in PellesC's v5 _makepath() implementation which makes
 * it unusable. The error was reported and might get fixed for v6.
 */
#define PC '\\'
#define ALT_PC '/'
#define ISPC(x) ((x == PC) || (x == ALT_PC))

void _makepath( char *path, const char *node, const char *dir, const char *fname, const char *ext )
/*************************************************************************************************/
{
    *path = '\0';

    if( node != NULL ) {
        if( *node != '\0' ) {
            strcpy( path, node );
            path = strchr( path, '\0' );

            /* if node did not end in '/' then put in a provisional one */
            if( path[-1] == PC )
                path--;
            else
                *path = PC;
        }
    }
    if( dir != NULL ) {
        if( *dir != '\0' ) {
            /*  if dir does not start with a '/' and we had a node then
                    stick in a separator
            */
            if( ( ! ISPC(*dir) ) && ( ISPC(*path) ) ) path++;

            strcpy( path, dir );
            path = strchr( path, '\0' );

            /* if dir did not end in '/' then put in a provisional one */
            if( path[-1] == PC )
                path--;
            else
                *path = PC;
        }
    }

    if( fname != NULL ) {
        if( ( !ISPC(*fname) ) && ( ISPC(*path) ) ) path++;

        strcpy( path, fname );
        path = strchr( path, '\0' );

    } else {
        if( ISPC(*path) ) path++;
    }
    if( ext != NULL ) {
        if( *ext != '\0' ) {
            if( *ext != '.' )  *path++ = '.';
            strcpy( path, ext );
            path = strchr( path, '\0' );
        }
    }
    *path = '\0';
}

#endif

