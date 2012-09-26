/*
 * this file contains prototypes of the Win32 functions which are
 * called directly by JWasm (in memalloc.c if FASTMEM=1).
 */

#if defined(__WATCOMC__) || defined(__PCC__) || defined(__OCC__)
/* __declspec(dllimport) is a problem if Win32 is linked statically (HX)
 * and WLINK is used. MS link is smarter, it knows what to do if a symbol
 * has been declared with dllimport and then turns out to be static.
 */
#define WINBASEAPI
#else
#define WINBASEAPI __declspec(dllimport)
#endif

#ifdef _WIN64
#define WINAPI __fastcall
#else
#define WINAPI __stdcall
#endif

#define MEM_COMMIT     0x1000
#define MEM_RELEASE    0x8000
#define PAGE_READWRITE 0x0004

WINBASEAPI void *WINAPI VirtualAlloc( void *, uint_32, uint_32, uint_32 );
WINBASEAPI int   WINAPI VirtualFree( void *, uint_32, uint_32 );
