
/* prototypes of functions defined in elf.c */

#ifndef _ELF_H_INCLUDED_
#define _ELF_H_INCLUDED_

ret_code elf_write_header( struct module_info * );
ret_code elf_write_data( struct module_info * );
void     elf_init( struct module_info * );

#endif // _ELF_H_INCLUDED_
