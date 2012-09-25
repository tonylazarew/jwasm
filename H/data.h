
/* include must be placed after directiv.h */

#ifndef DATA_H
#define DATA_H

extern void       atofloat( void *, const char *, unsigned, bool, uint_8 );
extern ret_code   data_dir( int, struct asm_tok[], struct dsym * );

#endif
