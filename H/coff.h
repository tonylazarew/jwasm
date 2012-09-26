
/* prototypes of functions defined in coff.c */

#ifndef _COFF_H_INCLUDED
#define _COFF_H_INCLUDED

struct qditem {
    uint_8 *next;
    uint_16 size;
};

ret_code coff_write_header( struct module_info * );
ret_code coff_write_section_table( struct module_info * );
ret_code coff_write_data( struct module_info * );
ret_code coff_write_symbols( struct module_info * );

#endif
