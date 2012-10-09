/****************************************************************************
*
*  Description: file for GNU (Posix) CRT. Included by globals.h
*               if __UNIX__ or __CYGWIN__ is defined.
*
****************************************************************************/

#define _stricmp strcasecmp
#define _strcmpi strcasecmp
#define _strnicmp strncasecmp
#ifndef __WATCOMC__
#define _memicmp strncasecmp
#endif

#define _ltoa   ltoa
#define _strupr strupr

char *_fullpath( char *, const char *, size_t );

#define _MAX_DRIVE      48      /*  maximum length of node name w/ '\0' */
#define _MAX_DIR        512     /*  maximum length of subdirectory      */
#define _MAX_FNAME      128     /*  maximum length of a file name       */
#define _MAX_EXT        48      /*  maximum length of a file extension  */

#ifndef _MAX_PATH
 #define _MAX_PATH      512     /*  maximum length of path name         */
#endif


