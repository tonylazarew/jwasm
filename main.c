/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  JWasm top level module
*
****************************************************************************/

#include <signal.h>

#include "globals.h"
#include "msgtext.h"
#include "cmdline.h"

#if defined(__UNIX__) || defined(__CYGWIN__) || defined(__DJGPP__)

#define WILDCARDS 0
#define CATCHBREAK 0

#else

#define WILDCARDS 1
#ifdef __POCC__
#define CATCHBREAK 0
#else
#define CATCHBREAK 1
#endif

#endif

#if WILDCARDS
#ifdef __UNIX__
 #include <unistd.h>
#else
 #include <io.h>
#endif
#endif

static void genfailure( int signo )
/*********************************/
{
#if CATCHBREAK
    if (signo != SIGBREAK)
#else
    if (signo != SIGTERM)
#endif
        EmitError( GENERAL_FAILURE );
    close_files();
    exit( EXIT_FAILURE );
}

int main( int argc, char **argv )
/*******************************/
{
    char    *pEnv;
    int     numArgs = 0;
    int     numFiles = 0;
    int     rc = 0;
#if WILDCARDS
    long    fh; /* _findfirst/next/close() handle, must be long! */
    struct  _finddata_t finfo;
    char    drv[_MAX_DRIVE];
    char    dir[_MAX_DIR];
    char    fname[_MAX_PATH];
#endif

#if 0 //def DEBUG_OUT    /* DebugMsg() cannot be used that early */
    int i;
    for ( i = 1; i < argc; i++ ) {
        printf("argv[%u]=>%s<\n", i, argv[i] );
    }
#endif

    pEnv = getenv( "JWASM" );
    if ( pEnv == NULL )
        pEnv = "";
    argv[0] = pEnv;

#ifndef DEBUG_OUT
    signal(SIGSEGV, genfailure);
#endif

#if CATCHBREAK
    signal(SIGBREAK, genfailure);
#else
    signal(SIGTERM, genfailure);
#endif

    MsgInit();

    while ( 1 ) {
        if ( ParseCmdline( (const char **)argv, &numArgs ) == NULL )
            break;  /* exit if no source file name supplied */
        numFiles++;
        write_logo();
#if WILDCARDS
        if ((fh = _findfirst( Options.names[ASM], &finfo )) == -1 ) {
            DebugMsg(("main: _findfirst(%s) failed\n", Options.names[ASM] ));
            EmitErr( CANNOT_OPEN_FILE, Options.names[ASM], ErrnoStr() );
            break;
        }
        _splitpath( Options.names[ASM], drv, dir, NULL, NULL );
        DebugMsg(("main: _splitpath(%s): drv=\"%s\" dir=\"%s\"\n", Options.names[ASM], drv, dir ));
        do {
            _makepath( fname, drv, dir, finfo.name, NULL );
            DebugMsg(("main: _makepath(\"%s\", \"%s\", \"%s\")=\"%s\"\n", drv, dir, finfo.name, fname ));
            rc = AssembleModule( fname );  /* assemble 1 module */
        } while ( ( _findnext( fh, &finfo ) != -1 ) );
        _findclose( fh );
#else
        rc = AssembleModule( Options.names[ASM] );
#endif
    };
    CmdlineFini();
    if ( numArgs == 0 ) {
        write_logo();
        printf( MsgGetEx( MSG_USAGE ) );
    } else if ( numFiles == 0 )
        EmitError( NO_FILENAME_SPECIFIED );

    MsgFini();
    return( 1 - rc ); /* zero if no errors */
}
