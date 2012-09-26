/****************************************************************************
*
* Description:  listing interface.
*
****************************************************************************/


#ifndef _LISTING_H_INCLUDED
#define _LISTING_H_INCLUDED

enum lsttype {
 LSTTYPE_DATA      = 0,
 LSTTYPE_CODE      = 1,
 LSTTYPE_EQUATE    = 2,
 LSTTYPE_DIRECTIVE = 3,
 LSTTYPE_MACRO     = 4,
 LSTTYPE_STRUCT    = 5,
 LSTTYPE_LABEL     = 6,
 LSTTYPE_MACROLINE = 7
};

extern void LstOpenFile( void );
extern void LstCloseFile( void );
extern void LstWrite( enum lsttype, uint_32 ofs, void * sym );
extern void LstWriteSrcLine( void );
extern void LstWriteCRef( void );
extern void LstPrintf( const char *format, ... );
extern void LstNL( void );
#if FASTPASS
extern void LstSetPosition( void );
#endif
extern uint_32 list_pos;        /* current LST file position */

#endif
