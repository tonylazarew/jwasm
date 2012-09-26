
/* interface to OMF format output functions */

#ifndef _OMF_H_INCLUDED_
#define _OMF_H_INCLUDED_

/* max size of LEDATA data is 1024 - (1+2+1/2+2/4+1) = 1014 */

#define MAX_LEDATA_THRESHOLD    (1024 - 10)
#define MAX_PUB_LENGTH          1024 /* OMF: max length of pubdef record */
#define MAX_EXT_LENGTH          1020 /* OMF: max length ( in chars ) of extdef */

void      omf_init( struct module_info *, FILE * );
void      omf_fini( void );
ret_code  omf_write_public( bool );
void      omf_write_alias( void );
ret_code  omf_write_autodep( void );
void      omf_write_header( void );
ret_code  omf_write_comdef( void );
void      omf_write_extdef( void );
void      omf_write_lnames( void );
void      omf_write_seg( bool );
void      omf_write_grp( void );
void      omf_write_export( void );
void      omf_write_lib( void );
void      omf_write_dosseg( void );
void      omf_end_of_pass1( void );
void      omf_set_filepos( void );
void      omf_write_ledata( struct dsym * );
void      omf_write_linnum( void );
void      omf_write_modend( struct fixup *, uint_32 );
void      omf_OutSelect( bool );
void      omf_FlushCurrSeg( void );

void      omf_write_header_dbgcv( void );
void      omf_write_debug_tables( void );

#endif // _OMF_H_INCLUDED_

