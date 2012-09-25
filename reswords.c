/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  reserved word handling, including hash table access
*
****************************************************************************/

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "reswords.h"
#include "expreval.h"
#include "condasm.h"
#include "codegen.h"
#ifdef __I86__
#include "i86.h"
#endif

//#define HASH_TABITEMS 211
#if AVXSUPP
#define HASH_TABITEMS 811
#else
#define HASH_TABITEMS 599
#endif

#if 0 // def __I86__
/* optionally, for JWASMR, use a void based pointer for the name field.
 * However, this requires to deactivate the RENAMEKEYWORD option!
 */
#define GetPtr( x, y ) seg:>x->y
#define BASEPTR
#else
#define GetPtr( x, y ) x->y
#endif

static short resw_table[ HASH_TABITEMS ];

/* define unary operand (LOW, HIGH, OFFSET, ...) type flags */
enum unary_operand_types {
#define res( value, func ) UOT_ ## value,
#include "unaryop.h"
#undef res
};

/* v2.06: the following operand combinations are used
 * inside InstrTable[] only, they don't need to be known
 * by the parser.
 */
enum operand_sets {
    OP_R_MS      = ( OP_R | OP_MS ),
    OP_R8_M08    = ( OP_R8 | OP_M08 ),
    OP_RGT8_MS   = ( OP_RGT8 | OP_MS ),
    OP_RGT8_MGT8 = ( OP_RGT8 | OP_MGT8 ),
    OP_RMGT16    = ( OP_RGT16 | OP_MGT16 ),
    OP_RGT16_M08 = ( OP_RGT16 | OP_M08 ),
    OP_R16_R32   = ( OP_R16 | OP_R32 ),
    OP_R16_M16   = ( OP_R16 | OP_M16 ),
    OP_R32_M08   = ( OP_R32 | OP_M08 ),
    OP_R32_M16   = ( OP_R32 | OP_M16 ),
    OP_R32_M32   = ( OP_R32 | OP_M32 ),
#if AMD64_SUPPORT
    OP_R16_R64   = ( OP_R16 | OP_R64 ),
    OP_R64_M64   = ( OP_R64 | OP_M64 ),
    OP_M16_M64   = ( OP_M16 | OP_M64 ),
#endif
    OP_M16_M32   = ( OP_M16 | OP_M32 ),
    OP_MMX_M64   = ( OP_MMX | OP_M64 ),
    OP_XMM_M16   = ( OP_XMM | OP_M16 ),
    OP_XMM_M32   = ( OP_XMM | OP_M32 ),
    OP_XMM_M64   = ( OP_XMM | OP_M64 ),
    OP_XMM_M128  = ( OP_XMM | OP_M128 ),
#if MASM_SSE_MEMX
/* extended Masm syntax: sometimes Masm accepts 2 mem types
 * for the memory operand, although the mem access will always
 * be QWORD/OWORD.
 */
    OP_MMX_M64_08  = ( OP_MMX | OP_M64  | OP_M08 ),
    OP_MMX_M64_16  = ( OP_MMX | OP_M64  | OP_M16 ),
    OP_MMX_M64_32  = ( OP_MMX | OP_M64  | OP_M32 ),

    OP_XMM_M128_08 = ( OP_XMM | OP_M128 | OP_M08 ),
    OP_XMM_M128_16 = ( OP_XMM | OP_M128 | OP_M16 ),
    OP_XMM_M128_32 = ( OP_XMM | OP_M128 | OP_M32 ),
    OP_XMM_M128_64 = ( OP_XMM | OP_M128 | OP_M64 ),
#else
/* see macro OpCls() below */
#define OPC_MMXMMX_M64_08NONE  OPC_MMXMMX_M64NONE
#define OPC_MMXMMX_M64_16NONE  OPC_MMXMMX_M64NONE
#define OPC_MMXMMX_M64_32NONE  OPC_MMXMMX_M64NONE

#define OPC_XMMXMM_M128_08NONE OPC_XMMXMM_M128NONE
#define OPC_XMMXMM_M128_16NONE OPC_XMMXMM_M128NONE
#define OPC_XMMXMM_M128_32NONE OPC_XMMXMM_M128NONE
#define OPC_XMMXMM_M128_64NONE OPC_XMMXMM_M128NONE
#endif
#if AVXSUPP
    OP_YMM_M256  = ( OP_YMM | OP_M256 ),
#endif
};

/* v2.06: operand types have been removed from InstrTable[], they
 * are stored now in their own table, opnd_clstab[], below.
 * This will allow to add a 4th operand ( AVX ) more effectively.
 */
enum opnd_variants {
#define OpCls( op1, op2, op3 ) OPC_ ## op1 ## op2 ## op3,
#include "opndcls.h"
#undef OpCls
};

/* the tables to handle "reserved words" are now generated:
 * 1. InstrTable: contains info for instructions.
 *    instructions may need multiple rows!
 * 2. SpecialTable: contains info for reserved words which are
 *    NOT instructions. One row each.
 * 3. optable_idx: array of indices for InstrTable.
 * 4. resw_strings: strings of reserved words. No terminating x'00'!
 * 5. ResWordTable: array of reserved words (name, name length, flags).
 *
 * Each reserved word has a "token" value assigned, which is a short integer.
 * This integer can be used as index for:
 * - SpecialTable
 * - optable_idx ( needs adjustment, better use macro IndexFromToken() )
 * - ResWordTable
 */

/* create InstrTable. */

#define OpCls( op1, op2, op3 ) OPC_ ## op1 ## op2 ## op3

const struct instr_item InstrTable[] = {
#define ins(tok,string,len,  opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { opcls, byte1_info, prefix, 1, rm_info, op_dir, 0, cpu, opcode, rm_byte },
#define insx(tok,string,len, opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) \
    { opcls, byte1_info, prefix, 1, rm_info, op_dir, 0, cpu, opcode, rm_byte },
#define insn(tok,suffix,     opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { opcls, byte1_info, prefix, 0, rm_info, op_dir, 0, cpu, opcode, rm_byte },
#define insm(tok,suffix,     opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { opcls, byte1_info, prefix, 1, rm_info, op_dir, 0, cpu, opcode, rm_byte },
#include "instruct.h"
#include "instr64.h"
ins (NULL,0,0,OpCls(NONE,NONE,NONE),0,0,0,0,0,0,0) /* T_NULL entry. Must be last! */
#undef insm
#undef insn
#undef insx
#undef ins
};
#undef OpCls

/* create SpecialTable. */

const struct special_item SpecialTable[] = {
#define res(tok, string, len, type, value, bytval, flags, cpu, sflags ) \
    { value, sflags, cpu, bytval, type },
#include "special.h"
#undef res
#define res(tok, string, len, value, bytval, flags, cpu, sflags ) \
    { value, sflags, cpu, bytval, RWT_DIRECTIVE },
#include "directve.h"
#undef res
};

/* define symbolic indices for InstrTable[] */

enum res_idx {
#define  ins(tok,string,len, opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I,
#define insx(tok,string,len, opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) T_ ## tok ## _I,
#define insn(tok,suffix,     opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix,
#define insm(tok,suffix,     opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix,
#include "instruct.h"
#undef insm
#undef insn
#undef ins

#define  ins(tok,string,len, opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I64,
#define insn(tok,suffix,     opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
#define insm(tok,suffix,     opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
#include "instr64.h"
#undef insm
#undef insn
#undef insx
#undef ins
//T_NULL_I /* v2.06: removed */
};

/* create optable_idx, the index array for InstrTable.
 * This is needed because instructions often need more than
 * one entry in InstrTable.
 */

short optable_idx[] = {

#define  ins(tok,string,len, opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I,
#define insx(tok,string,len, opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) T_ ## tok ## _I,
#define insn(tok,suffix,     opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insm(tok,suffix,     opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#include "instruct.h"
#undef insm
#undef insn
#undef insx
#undef ins

    /* v2.06: this was superfluous, because the additional entries for
     * 64-bit are only needed in InstrTable[]. For optable_idx[], a
     * patch is done (see patchtabr[])
     */
//#define  ins(tok,string,len, op1,op2,op3, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I64,
//#define insn(tok,suffix, op1,op2,op3, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
//#define insm(tok,suffix, op1,op2,op3, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
//#include "instr64.h"
//#undef insm
//#undef insn
//#undef ins

#if AVXSUPP
#define avxins( tok, string, len, cpu, flgs ) T_ ## tok ## _I,
#include "instravx.h"
#undef avxins
#endif
    //T_NULL_I /* v2.06: also not needed */
};

/* table of instruction operand classes */
const struct opnd_class opnd_clstab[] = {
#define OpCls( op1, op2, op3 ) { { OP_ ## op1, OP_ ## op2 }, OP3_ ## op3 },
#include "opndcls.h"
#undef OpCls
};

/* create the strings for all reserved words */

static const char resw_strings[] = {
#define res(tok, string, len, type, value, bytval, flags, cpu, sflags) \
 # string
#include "special.h"
#undef res
#define res(tok, string, len, value, bytval, flags, cpu, sflags) \
 # string
#include "directve.h"
#undef res

#define ins(tok,string,len, opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
 # string
#define insn(tok,suffix,    opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insm(tok,suffix,    opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insx(tok,string,len,opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) \
 # string
#include "instruct.h"
#if AVXSUPP
#define avxins( tok, string, len, cpu, flgs ) # string
#include "instravx.h"
#undef avxins
#endif
    "syscall_" /* replacement for "syscall" language type in 64-bit */
};
#define strSyscall_ &resw_strings[sizeof(resw_strings)-9]
#undef insx
#undef insm
#undef insn
#undef ins

/* create the 'reserved words' table (ResWordTable).
 * this table's entries will be used to create the instruction hash table.
 */
struct ReservedWord ResWordTable[] = {
#define res(tok, string, len, type, value, bytval, flags, cpu, sflags) \
    { 0, len, RWF_SPECIAL | flags, NULL },
#include "special.h"
#undef res
#define res(tok, string, len, value, bytval, flags, cpu, sflags) \
    { 0, len, RWF_SPECIAL | flags, NULL },
#include "directve.h"
#undef res

#define ins(tok,string,len, opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { 0, len, 0, NULL },
#define insn(tok,suffix,    opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insm(tok,suffix,    opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insx(tok,string,len,opcls, byte1_info,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flags) \
    { 0, len, flags, NULL },
#include "instruct.h"
#undef insx
#undef insm
#undef insn
#undef ins
#if AVXSUPP
#define avxins( tok, string, len, cpu, flgs ) \
    { 0, len, RWF_VEX, NULL },
#include "instravx.h"
#undef avxins
#endif
    { 0, 0, 0, NULL } /* dummy entry for T_NULL */
};

#if AVXSUPP
/* these is a special 1-byte array for vex-encoded instructions.
 * it could probably be moved to InstrTable[] (there is an unused byte),
 * but in fact it's the wrong place, since the content of vex_flags[]
 * are associated with opcodes, not with instruction variants.
 */
const uint_8 vex_flags[] = {
    /* flags for the AVX instructions in instruct.h. The order must
     * be equal to the one in instruct.h! ( this is to be improved.)
     * For a description of the VX_ flags see codegen.h
     */
    VX_NND,      /* VBROADCASTSS   */
    VX_NND,      /* VBROADCASTSD   */
    VX_NND,      /* VBROADCASTF128 */
    VX_L,        /* VBLENDVPD      */
    VX_L,        /* VBLENDVPS      */
    0,           /* VINSERTF128    */
    VX_NND,      /* VEXTRACTF128   */
    VX_L,        /* VMASKMOVPS     */
    VX_L,        /* VMASKMOVPD     */
    0,           /* VPBLENDVB      */
    VX_L|VX_IMM, /* VPERMILPD      */
    VX_L|VX_IMM, /* VPERMILPS      */
    /* VPERMIL2xx has been dropped */
    //VX_L,      /* VPERMIL2PD     */
    //VX_L,      /* VPERMIL2PS     */
    0,           /* VPERM2F128     */
    VX_L|VX_NND, /* VTESTPS        */
    VX_L|VX_NND, /* VTESTPD        */
    VX_L,        /* VZEROALL       */
    0,           /* VZEROUPPER     */
    VX_NND,      /* VCVTPD2DQ      */
    VX_NND,      /* VCVTTPD2DQ     */
    VX_NND,      /* VCVTPD2PS      */
    VX_NND,      /* VMOVDDUP       */
#define avxins( tok, string, len, cpu, flgs ) flgs,
#include "instravx.h"
#undef avxins
};
#endif

#if AMD64_SUPPORT

/* keywords to be added for 64-bit */
static const enum instr_token patchtab64[] = {
    T_SPL,             /* add x64 register part of special.h */
    T_FRAME,           /* add x64 reserved word part of special.h */
    T_DOT_ALLOCSTACK,  /* add x64 directive part of directve.h (win64) */
    T_JRCXZ,           /* branch instructions must be grouped together */
    T_CDQE,            /* add x64 part of instruct.h */
#if AVXSUPP
    T_VPEXTRQ,         /* add x64 part of instravx.h */
#endif
};

/* keywords to be removed for 64-bit */
static const enum instr_token patchtab32[] = {
    T_DOT_SAFESEH,  /* directives invalid for IA32+             */
    T_AAA,          /* instructions invalid for IA32+           */
    T_JCXZ,         /* 1. branch instructions invalid for IA32+ */
    T_LOOPW         /* 2. branch instructions invalid for IA32+ */
};

struct replace_ins {
    short         tok; /* is an optable_idx[] index */
    enum res_idx  idx32;
    enum res_idx  idx64;
};

/* keyword entries to be changed for 64-bit (see instr64.h) */
static const struct replace_ins patchtabr[] = {
    { T_LGDT - SPECIAL_LAST, T_LGDT_I, T_LGDT_I64 },
    { T_LIDT - SPECIAL_LAST, T_LIDT_I, T_LIDT_I64 },
    { T_CALL - SPECIAL_LAST, T_CALL_I, T_CALL_I64 },
    { T_JMP  - SPECIAL_LAST, T_JMP_I,  T_JMP_I64  },
    { T_POP  - SPECIAL_LAST, T_POP_I,  T_POP_I64  }, /* v2.06: added */
    { T_PUSH - SPECIAL_LAST, T_PUSH_I, T_PUSH_I64 }, /* v2.06: added */
#if 1
    /* with Masm, in 16/32-bit SLDT|SMSW|STR accept a WORD argument only -
     * in 64-bit (ML64), 32- and 64-bit registers are also accepted!
     */
    { T_SLDT - SPECIAL_LAST, T_SLDT_I, T_SLDT_I64 },
    { T_SMSW - SPECIAL_LAST, T_SMSW_I, T_SMSW_I64 },
    { T_STR  - SPECIAL_LAST, T_STR_I,  T_STR_I64  },
#endif
};

#endif

#if RENAMEKEY
static struct qdesc renamed_keys = { NULL, NULL };
#endif

/* global queue of "disabled" reserved words.
 * just indices of ResWordTable[] are used.
 */
static short RemovedFirst = EMPTY;
static short RemovedTail  = EMPTY;

#if AMD64_SUPPORT
static bool  b64bit = FALSE; /* resw tables in 64bit mode? */
#endif
static bool  fInit = FALSE;  /* init flag */

static unsigned int get_hash( const char *s, unsigned char size )
/***************************************************************/
{
    uint_32 h;
    uint_32 g;

    for( h = 0; size; size-- ) {
        /* ( h & ~0x0fff ) == 0 is always true here */
        h = (h << 3) + (*s++ | ' ');
        g = h & ~0x1fff;
        h ^= g;
        h ^= g >> 13;
    }
    return( h % HASH_TABITEMS );
}

int FindResWord( const char *name, unsigned char size )
/*****************************************************/
/* search reserved word in hash table */
{
    struct ReservedWord *inst;
    int i;
#ifdef BASEPTR
    __segment seg = FP_SEG( resw_strings );
#endif

    for( i = resw_table[ get_hash( name, size ) ]; i != EMPTY; i = inst->next ) {
        inst = &ResWordTable[i];
        /* check if the name matches the entry for this inst in AsmChars */
        //if( name[ inst->len ] == NULLC && _strnicmp( name, inst->name, inst->len ) == 0) {
        if( inst->len == size && _memicmp( name, GetPtr( inst, name ), inst->len ) == 0 ) {
            return( i );
        }
    }
    return( -1 );
}

/* add reserved word to hash table */

static void AddResWord( int token )
/*********************************/
{
    int i;
    int old;
    int curr;
#ifdef BASEPTR
    __segment seg = FP_SEG( resw_strings );
#endif

    i = get_hash( ResWordTable[token].name, ResWordTable[token].len );

    /* sort the items of a line by length! */

    for( curr = resw_table[i], old = EMPTY; curr != EMPTY && ResWordTable[curr].len <= ResWordTable[token].len; old = curr, curr = ResWordTable[curr].next );

    if ( old == EMPTY ) {
        ResWordTable[token].next = resw_table[i];
        resw_table[i] = token;
    } else {
        ResWordTable[token].next = ResWordTable[old].next;
        ResWordTable[old].next = token;
    }

    return;
}

/* remove a reserved word from the hash table. */

static int RemoveResWord( int token )
/***********************************/
{
    int i;
    int old;
    int curr;
#ifdef BASEPTR
    __segment seg = FP_SEG( resw_strings );
#endif

    i = get_hash( ResWordTable[token].name, ResWordTable[token].len );

    for( curr = resw_table[i], old = EMPTY ; curr != EMPTY ; old = curr, curr = ResWordTable[curr].next )  {
        if( curr == token ) {
            if ( old != EMPTY )
                ResWordTable[old].next = ResWordTable[curr].next;
            else
                resw_table[i] = ResWordTable[curr].next;
            return( TRUE );
        }
    }
    return( FALSE );
}

#if RENAMEKEY

struct rename_node {
    struct rename_node *next;
    const char *name; /* the original name in resw_strings[] */
    uint_16 token; /* is either enum instr_token or enum special_token */
    uint_8 length;
};

/* Rename a keyword.
 * - token: keyword to rename
 * - newname: new name of keyword
 * - length: length of new name
 */

void RenameKeyword( uint token, const char *newname, uint_8 length )
/******************************************************************/
{
    struct rename_node *rn;

    RemoveResWord( token );
    /* if it is the first rename action for this keyword,
     * the original name must be saved.
     */
    if ( ResWordTable[token].name >= resw_strings &&
        ResWordTable[token].name < ( resw_strings + sizeof( resw_strings ) ) ) {
        rn = LclAlloc( sizeof( struct rename_node ) );
        rn->next = NULL;
        rn->name = ResWordTable[token].name;
        rn->token = token;
        rn->length = ResWordTable[token].len;
        if ( renamed_keys.head == NULL ) {
            renamed_keys.head = renamed_keys.tail = rn;
        } else {
            ((struct rename_node *)renamed_keys.tail)->next = rn;
            renamed_keys.tail = rn;
        }
    } else {
        LclFree( (void *)ResWordTable[token].name );
    }
    ResWordTable[token].name = LclAlloc( length );
    /* convert to lowercase? */
    memcpy( (void *)ResWordTable[token].name, newname, length );
    ResWordTable[token].len = length;
    AddResWord( token );
}

#endif

#if AMD64_SUPPORT

/* depending on 64bit on or off, some instructions must be added,
 * some removed. Currently this is a bit hackish.
 */
void Set64Bit( bool newmode )
/***************************/
{
    static const char *syscallname;   /* "true" syscall name stored here */
    int token;
    int i;

    if ( newmode != b64bit ) {
        DebugMsg(("Set64Bit(%u): mode is to change\n", newmode ));
        if ( newmode != FALSE ) {
            optable_idx[ T_INC - SPECIAL_LAST ]++;   /* skip the one-byte register INC */
            optable_idx[ T_DEC - SPECIAL_LAST ]++;   /* skip the one-byte register DEC */
            /*
             * change SYSCALL to SYSCALL_ language in long mode.
             * one cannot just change the name, since the hash value
             * will differ!
             */
            RemoveResWord( T_SYSCALL );
            syscallname = ResWordTable[T_SYSCALL].name; /* save the "true" name */
            ResWordTable[T_SYSCALL].name = strSyscall_;
            ResWordTable[T_SYSCALL].len++;
            AddResWord( T_SYSCALL );

            for ( i = 0; i < sizeof( patchtab64 ) / sizeof( patchtab64[0] ); i++ )
                for( token = patchtab64[i]; ResWordTable[token].flags & RWF_X64; token++ )
                    if ( !( ResWordTable[token].flags & RWF_DISABLED ) )
                        AddResWord( token );
            for ( i = 0; i < sizeof( patchtab32 ) / sizeof( patchtab32[0] ); i++ )
                for( token = patchtab32[i]; ResWordTable[token].flags & RWF_IA32; token++ )
                    if ( !( ResWordTable[token].flags & RWF_DISABLED ) )
                        RemoveResWord( token );
            for ( i = 0; i < sizeof( patchtabr) / sizeof( patchtabr[0] ); i++ ) {
                optable_idx[ patchtabr[i].tok] = patchtabr[i].idx64;
            }
        } else  {
            optable_idx[T_INC - SPECIAL_LAST]--;   /* restore the one-byte register INC */
            optable_idx[T_DEC - SPECIAL_LAST]--;   /* restore the one-byte register DEC */

            for ( i = 0; i < sizeof( patchtab64 ) / sizeof( patchtab64[0] ); i++ )
                for( token = patchtab64[i]; ResWordTable[token].flags & RWF_X64; token++ )
                    if ( !( ResWordTable[token].flags & RWF_DISABLED ) )
                        RemoveResWord( token );
            for ( i = 0; i < sizeof( patchtab32 ) / sizeof( patchtab32[0] ); i++ )
                for( token = patchtab32[i]; ResWordTable[token].flags & RWF_IA32; token++ )
                    if ( !( ResWordTable[token].flags & RWF_DISABLED ) )
                        AddResWord( token );
            for ( i = 0; i < sizeof( patchtabr) / sizeof( patchtabr[0] ); i++ ) {
                optable_idx[patchtabr[i].tok] = patchtabr[i].idx32;
            }

            /* change calling convention syscall_ back to syscall */
            RemoveResWord( T_SYSCALL );
            ResWordTable[T_SYSCALL].name = syscallname; /* restore "true" name */
            ResWordTable[T_SYSCALL].len--;
            AddResWord( T_SYSCALL );
        }
        b64bit = newmode;
    }
}
#endif

void DisableKeyword( uint token )
/*******************************/
{
    if ( !( ResWordTable[token].flags & RWF_DISABLED ) ) {
        RemoveResWord( token );
        ResWordTable[token].next = EMPTY;
        ResWordTable[token].flags |= RWF_DISABLED;
        if ( RemovedFirst == EMPTY )
            RemovedFirst = RemovedTail = token;
        else {
            ResWordTable[RemovedTail].next = token;
            RemovedTail = token;
        }
    }
}

/* check if a keyword is in the list of disabled words.
 */

bool IsKeywordDisabled( const char *name, int len )
/*************************************************/
{
    uint  token;
    for ( token = RemovedFirst; token != EMPTY; token = ResWordTable[token].next )
        if( ResWordTable[token].name[ len ] == NULLC && _memicmp( name, ResWordTable[token].name, len ) == 0 )
            return( TRUE );
    return( FALSE );
}

/* get current name of a reserved word.
 * max size is 255.
 */

char *GetResWName( uint resword, char *buff )
/*******************************************/
{
#ifdef __I86__
    static char intbuff[32];
#else
    static char intbuff[256];
#endif
    if ( !buff )
        buff = intbuff;
    memcpy( buff, ResWordTable[resword].name, ResWordTable[resword].len );
    buff[ResWordTable[resword].len] = NULLC;
    return( buff );
}

/* ResWordsInit() is called once per module */

void ResWordsInit( void )
/***********************/
{
    int next;
    int i;

    DebugMsg(("ResWordsInit() enter\n"));

    if( fInit == FALSE ) {  /* if not initialized */
        const char *p = resw_strings;
        fInit = TRUE;
        /* if first call, initialize hash table (in IA32 mode) */
        for ( i = 0; i < HASH_TABITEMS; i++ )
            resw_table[i] = EMPTY;
#if AVXSUPP && AMD64_SUPPORT
        /* currently these flags must be set manually, since the
         * RWF_ flags aren't contained in instravx.h */
        ResWordTable[T_VPEXTRQ].flags |= RWF_X64;
        ResWordTable[T_VPINSRQ].flags |= RWF_X64;
#endif
        for( i = 0; i < T_NULL; i++ ) {
            ResWordTable[i].name = p;
            p += ResWordTable[i].len;
#if AMD64_SUPPORT /* don't add the words specific to x64 */
            if ( !(ResWordTable[i].flags & RWF_X64 ) )
#endif
                AddResWord( i );
        }
    } else {
        /* reenter disabled keywords */
        for( i = RemovedFirst; i != EMPTY; i = next ) {
            next = ResWordTable[i].next;
            ResWordTable[i].flags &= ~RWF_DISABLED;
#if AMD64_SUPPORT /* don't add the words specific to x64 */
            if ( !(ResWordTable[i].flags & RWF_X64 ) )
#endif
                AddResWord( i );
            DebugMsg(("ResWordsInit(): %s reenabled\n", GetResWName( i, NULL ) ));
        }
        RemovedFirst = RemovedTail = EMPTY;
    }
#if 0 //def DEBUG_OUT
    DebugMsg(("SpecialTable\n"));
    DebugMsg(("keyword             value   sflags  cpu val8 type\n"));
    DebugMsg(("-------------------------------------------------\n"));
    for ( i = 0; i < sizeof( SpecialTable ) / sizeof( SpecialTable[0] ); i++ ) {
        DebugMsg(("%-16s %8X %8X %4X %4X  %2X\n", GetResWName( i, NULL ),
                  SpecialTable[i].value, SpecialTable[i].sflags,
                  SpecialTable[i].cpu, SpecialTable[i].bytval,
                  SpecialTable[i].type ));
    }
    DebugMsg(("-------------------------------------------------\n"));

    DebugMsg(("\nInstructionTable\n"));
    DebugMsg(("keyword          cls cpu opc rmb b1 rmi pfx fst\n"));
    DebugMsg(("-------------------------------------------------------\n"));
    for ( i = INS_FIRST_1 + 1; i < T_NULL; i++ ) {
        const struct instr_item *ins = &InstrTable[IndexFromToken( i )];
        DebugMsg(("%-16s %02X %4X  %02X  %02X %2u %X   %X   %u\n", GetResWName( i, NULL ),
                  ins->opclsidx,
                  ins->cpu, ins->opcode, ins->rm_byte, ins->byte1_info,
                  ins->rm_info, ins->allowed_prefix, ins->first ));
    }
    DebugMsg(("---------------------------------------------------------------\n"));
#endif
    DebugMsg(("ResWordsInit() exit\n"));
    return;
}

/* ResWordsFini() is called once per module */

void ResWordsFini( void )
/***********************/
{
#if RENAMEKEY
    struct rename_node  *rencurr;
#endif
    DebugMsg(("ResWordsFini() enter\n"));
#if RENAMEKEY
    /* restore renamed keywords */
    for ( rencurr = renamed_keys.head; rencurr; ) {
        struct rename_node *tmp = rencurr->next;
        RemoveResWord( rencurr->token );
        /* v2.06: this is the correct name to free */
        LclFree( (void *)ResWordTable[rencurr->token].name );
        ResWordTable[rencurr->token].name = rencurr->name;
        ResWordTable[rencurr->token].len = rencurr->length;
        AddResWord( rencurr->token );
        DebugMsg(("ResWordsFini(): %s restored\n", GetResWName( rencurr->token, NULL ) ));
        //LclFree( (void *)rencurr->name ); /* v2.06: this was the wrong one */
        LclFree( rencurr );
        rencurr = tmp;
    }
    renamed_keys.head = NULL;
#endif
    return;
}

#ifdef DEBUG_OUT

#define RWLOG 1

void DumpInstrStats( void )
/*************************/
{
    unsigned            i;
    int                 inst;
    unsigned            count = 0;
    unsigned            max = 0;
    unsigned            curr = 0;
    unsigned            num[8] = {0,0,0,0,0,0,0,0};

    if ( !fInit )
        return;
#if RWLOG
    DebugMsg(("\nReserved Word Hash Table\n"));
    DebugMsg(("Idx keywords\n"));
    DebugMsg(("---------------------------\n"));
#endif
    for( i = 0; i < HASH_TABITEMS; i++ ) {
#if RWLOG
        DebugMsg(("%3u ", i ));
#endif
        for( inst = resw_table[i], curr = 0; inst != EMPTY; inst = ResWordTable[inst].next ) {
#if RWLOG
            DebugMsg((" %-8s", GetResWName( inst, NULL ) ));
#endif
            curr++;
        }
#if RWLOG
        DebugMsg(("\n" ));
#endif
        count += curr;
        if ( curr <= 7 )
            num[curr]++;
        if (max < curr)
            max = curr;
    }
#if RWLOG
    DebugMsg(("---------------------------\n"));
#endif
    if ( Options.quiet == FALSE ) {
        printf( "%u items in resw table, max items/line=%u ", count, max );
        printf( "[0=%u 1=%u %u %u %u %u %u %u]\n", num[0], num[1], num[2], num[3], num[4], num[5], num[6], num[7] );
    }
}
#endif
