/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  Processing of INVOKE directive.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "reswords.h"
#include "expreval.h"
#include "input.h"
#include "equate.h"
#include "assume.h"
#include "segment.h"
#include "listing.h"
#include "myassert.h"
#if DLLIMPORT
#include "mangle.h"
#include "extern.h"
#endif
#include "proc.h"

extern int_64           maxintvalues[];
extern int_64           minintvalues[];
#ifdef __I86__
#define NUMQUAL (long)
#else
#define NUMQUAL
#endif

enum reg_used_flags {
    R0_USED       = 0x01, /* register contents of AX/EAX/RAX is destroyed */
    R0_H_CLEARED  = 0x02, /* 16bit: high byte of R0 (=AH) has been set to 0 */
    R0_X_CLEARED  = 0x04, /* 16bit: register R0 (=AX) has been set to 0 */
#if AMD64_SUPPORT
    RCX_USED      = 0x08, /* win64: register contents of CL/CX/ECX/RCX is destroyed */
    RDX_USED      = 0x10, /* win64: register contents of DL/DX/EDX/RDX is destroyed */
    R8_USED       = 0x20, /* win64: register contents of R8B/R8W/R8D/R8 is destroyed */
    R9_USED       = 0x40, /* win64: register contents of R9B/R9W/R9D/R9 is destroyed */
#define RPAR_START 3 /* Win64: RCX first param start at bit 3 */
#endif
#if OWFC_SUPPORT
    ROW_AX_USED   = 0x08, /* watc: register contents of AL/AX/EAX is destroyed */
    ROW_DX_USED   = 0x10, /* watc: register contents of DL/DX/EDX is destroyed */
    ROW_BX_USED   = 0x20, /* watc: register contents of BL/BX/EBX is destroyed */
    ROW_CX_USED   = 0x40, /* watc: register contents of CL/CX/ECX is destroyed */
#define ROW_START 3 /* watc: irst param start at bit 3 */
#endif
};

static int size_vararg;
static int fcscratch;  /* exclusively to be used by FASTCALL helper functions */

struct fastcall_conv {
    int  (* invokestart)( struct dsym const *, int, int, struct asm_tok[], int * );
    void (* invokeend)  ( struct dsym const *, int, int );
    int  (* handleparam)( struct dsym const *, int, struct dsym *, bool, struct expr *, char *, uint_8 * );
};

static  int ms32_fcstart( struct dsym const *, int, int, struct asm_tok[], int * );
static void ms32_fcend  ( struct dsym const *, int, int );
static  int ms32_param  ( struct dsym const *, int, struct dsym *, bool, struct expr *, char *, uint_8 * );
#if OWFC_SUPPORT
static  int watc_fcstart( struct dsym const *, int, int, struct asm_tok[], int * );
static void watc_fcend  ( struct dsym const *, int, int );
static  int watc_param  ( struct dsym const *, int, struct dsym *, bool, struct expr *, char *, uint_8 * );
#endif
#if AMD64_SUPPORT
static  int ms64_fcstart( struct dsym const *, int, int, struct asm_tok[], int * );
static void ms64_fcend  ( struct dsym const *, int, int );
static  int ms64_param  ( struct dsym const *, int, struct dsym *, bool, struct expr *, char *, uint_8 * );
#define REGPAR_WIN64 0x0306 /* regs 1, 2, 8 and 9 */
#endif

static const struct fastcall_conv fastcall_tab[] = {
 { ms32_fcstart, ms32_fcend , ms32_param }, /* FCT_MSC */
#if OWFC_SUPPORT
 { watc_fcstart, watc_fcend , watc_param }, /* FCT_WATCOMC */
#endif
#if AMD64_SUPPORT
 { ms64_fcstart, ms64_fcend , ms64_param } /* FCT_WIN64 */
#endif
};

static const enum special_token stackreg[] = { T_SP, T_ESP,
#if AMD64_SUPPORT
T_RSP
#endif
};
static const enum special_token regax[] = { T_AX, T_EAX,
#if AMD64_SUPPORT
T_RAX
#endif
};

/* 16-bit MS fastcall uses up to 3 registers (AX, DX, BX )
 * and additional params are pushed in PASCAL order!
 */
static const enum special_token ms16_regs[] = {
    T_AX, T_DX, T_BX
};
static const enum special_token ms32_regs[] = {
    T_ECX, T_EDX
};

#if AMD64_SUPPORT
static const enum special_token ms64_regs[] = {
 T_CL,  T_DL,  T_R8B, T_R9B,
 T_CX,  T_DX,  T_R8W, T_R9W,
 T_ECX, T_EDX, T_R8D, T_R9D,
 T_RCX, T_RDX, T_R8,  T_R9
};
#endif

/* segment register names, order must match ASSUME_ enum */
//static const enum special_token segreg_tab[] = {
//    T_ES, T_CS, T_SS, T_DS, T_FS, T_GS };

static int ms32_fcstart( struct dsym const *proc, int numparams, int start, struct asm_tok tokenarray[], int *value )
/*******************************************************************************************************************/
{
    struct dsym *param;
    DebugMsg1(("ms32_fcstart(proc=%s, ofs=%u)\n", proc->sym.name, GetSymOfssize( &proc->sym ) ));
    if ( GetSymOfssize( &proc->sym ) == USE16 )
        return( 0 );
    /* v2.07: count number of register params */
    for ( param = proc->e.procinfo->paralist ; param ; param = param->nextparam )
        if ( param->sym.state == SYM_TMACRO )
            fcscratch++;
    return( 1 );
}

static void ms32_fcend( struct dsym const *proc, int numparams, int value )
/*******************************************************************/
{
    /* nothing to do */
    return;
}

static int ms32_param( struct dsym const *proc, int index, struct dsym *param, bool addr, struct expr *opndx, char *paramvalue, uint_8 *r0used )
/**********************************************************************************************************************************************/
{
    enum special_token const *pst;

    DebugMsg1(("ms32_param(proc=%s, ofs=%u, index=%u, param=%s) fcscratch=%u\n", proc->sym.name, proc->sym.Ofssize, index, param->sym.name, fcscratch ));
    if ( param->sym.state != SYM_TMACRO )
        return( 0 );
    if ( GetSymOfssize( &proc->sym ) == USE16 ) {
        pst = ms16_regs + fcscratch;
        fcscratch++;
    } else {
        fcscratch--;
        pst = ms32_regs + fcscratch;
    }
    if ( addr )
        AddLineQueueX( " lea %r, %s", *pst, paramvalue );
    else {
        enum special_token reg = *pst;
        int size;
        /* v2.08: adjust register if size of operand won't require the full register */
        if ( ( opndx->kind != EXPR_CONST ) &&
            ( size = SizeFromMemtype( param->sym.mem_type, USE_EMPTY, param->sym.type ) ) < SizeFromRegister( *pst ) ) {
            if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                AddLineQueueX( " %s %r, %s", ( param->sym.mem_type & MT_SIGNED ) ? "movsx" : "movzx", reg, paramvalue );
            } else {
                /* this is currently always UNSIGNED */
                AddLineQueueX( " mov %r, %s", T_AL + GetRegNo( reg ), paramvalue );
                AddLineQueueX( " mov %r, 0", T_AH + GetRegNo( reg ) );
            }
        } else {
            /* v2.08: optimization */
            if ( opndx->kind == EXPR_REG && opndx->indirect == 0 && opndx->base_reg ) {
                if ( opndx->base_reg->tokval == reg )
                    return( 1 );
            }
            AddLineQueueX( " mov %r, %s", reg, paramvalue );
        }
    }
    if ( *pst == T_AX )
        *r0used |= R0_USED;
    return( 1 );
}

#if AMD64_SUPPORT
static int ms64_fcstart( struct dsym const *proc, int numparams, int start, struct asm_tok tokenarray[], int *value )
/*******************************************************************************************************************/
{
    /* v2.04: VARARG didn't work */
    if ( proc->e.procinfo->is_vararg ) {
        //numparams = ( tokenarray[start].token != T_FINAL ? 1 : 0 );
        for ( numparams = 0; tokenarray[start].token != T_FINAL; start++ )
            if ( tokenarray[start].token == T_COMMA )
                numparams++;
    }
    DebugMsg1(("ms64_fcstart(%s, numparams=%u) vararg=%u\n", proc->sym.name, numparams, proc->e.procinfo->is_vararg ));
    if ( numparams < 4 )
        numparams = 4;
    else if ( numparams & 1 )
        numparams++;
    *value = numparams;
    if ( ModuleInfo.win64_flags & W64F_AUTOSTACKSP ) {
        if ( ( numparams * sizeof( uint_64 ) ) > ReservedStack.value )
            ReservedStack.value = numparams * sizeof( uint_64 );
    } else
        AddLineQueueX( " sub %r, %d", T_RSP, numparams * sizeof( uint_64 ) );
    /* since Win64 fastcall doesn't push, it's a better/faster strategy to
     * handle the arguments from left to right.
     */
    return( 0 );
}

static void ms64_fcend( struct dsym const *proc, int numparams, int value )
/*************************************************************************/
{
    /* use <value>, which has been set by ms64_fcstart() */
    if ( !( ModuleInfo.win64_flags & W64F_AUTOSTACKSP ) )
        AddLineQueueX( " add %r, %d", T_RSP, value * 8 );
    return;
}

/* macro to convert register number to param number:
 * 1 -> 0 (rCX)
 * 2 -> 1 (rDX)
 * 8 -> 2 (r8)
 * 9 -> 3 (r9)
 */
#define GetParmIndex( x)  ( ( (x) >= 8 ) ? (x) - 6 : (x) - 1 )

/*
 * parameter for Win64 FASTCALL.
 * the first 4 parameters are hold in registers: rcx, rdx, r8, r9 for non-float arguments,
 * xmm0, xmm1, xmm2, xmm3 for float arguments. If parameter size is > 8, the address of
 * the argument is used instead of the value.
 */

static int ms64_param( struct dsym const *proc, int index, struct dsym *param, bool addr, struct expr *opndx, char *paramvalue, uint_8 *regs_used )
/*************************************************************************************************************************************************/
{
    uint_32 size;
    uint_32 psize;
    int reg;
    int reg2;
    int i;
    int base;
    bool destroyed = FALSE;

    DebugMsg1(("ms64_param(%s, index=%u, param.memtype=%Xh, addr=%u) enter\n", proc->sym.name, index, param->sym.mem_type, addr ));
    psize = SizeFromMemtype( param->sym.mem_type, USE64, param->sym.type );
    if ( index >= 4 ) {
        /* check for register overwrites */
        if ( opndx->base_reg != NULL ) {
            reg = opndx->base_reg->tokval;
            if ( GetValueSp( reg ) & OP_R ) {
                i = GetRegNo( reg );
                if ( REGPAR_WIN64 & ( 1 << i ) ) {
                    base = GetParmIndex( i );
                    if ( *regs_used & ( 1 << ( base + RPAR_START ) ) )
                        destroyed = TRUE;
                } else if ( (*regs_used & R0_USED ) && ( ( GetValueSp( reg ) & OP_A ) || reg == T_AH ) ) {
                    destroyed = TRUE;
                }
            }
        }
        if ( opndx->idx_reg != NULL ) {
            reg2 = opndx->idx_reg->tokval;
            if ( GetValueSp( reg2 ) & OP_R ) {
                i = GetRegNo( reg2 );
                if ( REGPAR_WIN64 & ( 1 << i ) ) {
                    base = GetParmIndex( i );
                    if ( *regs_used & ( 1 << ( base + RPAR_START ) ) )
                        destroyed = TRUE;
                } else if ( (*regs_used & R0_USED ) && ( ( GetValueSp( reg2 ) & OP_A ) || reg2 == T_AH ) ) {
                    destroyed = TRUE;
                }
            }
        }
        if ( destroyed ) {
            EmitErr( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
            *regs_used = 0;
        }
        if ( opndx->indirect == FALSE && opndx->base_reg != NULL ) {
            i = reg;
            size = SizeFromRegister( reg );
            if ( size != psize ) {
                if ( param->sym.is_vararg == FALSE ) {
                    DebugMsg(("ms64_param(%s, param=%u): type error size.p/a=%u/%u flags=%X\n", proc->sym.name, index, psize, size, *regs_used ));
                    EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, index+1 );
                }
                psize = size;
            }
        }
        if ( opndx->mem_type == MT_EMPTY && addr == FALSE ) {
            /* v2.06: support 64-bit constants for params > 4 */
            if ( psize == 8 && opndx->kind == EXPR_CONST &&
                ( opndx->value64 > LONG_MAX || opndx->value64 < LONG_MIN ) ) {
                AddLineQueueX( " mov %r ptr [%r+%u], %r ( %s )", T_DWORD, T_RSP, NUMQUAL index*8, T_LOW32, paramvalue );
                AddLineQueueX( " mov %r ptr [%r+%u], %r ( %s )", T_DWORD, T_RSP, NUMQUAL index*8+4, T_HIGH32, paramvalue );
            } else if ( param->sym.mem_type == MT_REAL8 && opndx->kind == EXPR_FLOAT ) {
                *regs_used |= R0_USED;
                AddLineQueueX( " mov %r, %r ptr %s", T_RAX, T_REAL8, paramvalue );
                AddLineQueueX( " mov [%r+%u], %r", T_RSP, NUMQUAL index*8, T_RAX );
            } else {
                switch ( psize ) {
                case 1:   i = T_BYTE; break;
                case 2:   i = T_WORD; break;
                case 4:   i = T_DWORD; break;
                default:  i = T_QWORD; break;
                }
                AddLineQueueX( " mov %r ptr [%r+%u], %s", i, T_RSP, NUMQUAL index*8, paramvalue );
            }
            DebugMsg(("ms64_param(%s, param=%u): MT_EMPTY size.p/a=%u/%u flags=%X\n", proc->sym.name, index, psize, size, *regs_used ));
        } else {
            if ( addr == FALSE ) {
                if ( opndx->indirect == FALSE && opndx->base_reg != NULL ) {
                    DebugMsg(("ms64_param(%s, param=%u): REG size.p/a=%u/%u flags=%X\n", proc->sym.name, index, psize, size, *regs_used ));
                } else {
                    DebugMsg(("ms64_param(%s, param=%u): MEM size.p/a=%u/%u flags=%X\n", proc->sym.name, index, psize, size, *regs_used ));
                    size = SizeFromMemtype( opndx->mem_type, USE64, opndx->type );
                    if ( size != psize && param->sym.is_vararg == FALSE )
                        EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, index+1 );
                    switch ( size ) {
                    case 1:  i = T_AL;  break;
                    case 2:  i = T_AX;  break;
                    case 4:  i = T_EAX; break;
                    default: i = T_RAX; break;
                    }
                    *regs_used |= R0_USED;
                    if ( size <= 8 )
                        AddLineQueueX( " mov %r, %s", i, paramvalue );
                    else {
                        AddLineQueueX( " lea %r, %s", i, paramvalue );
                    }
                }
            } else {
                if ( param->sym.is_vararg == TRUE )
                    psize = 8;
                switch ( psize ) {
                case 4: i = T_EAX;   break;
                case 8: i = T_RAX;   break;
                default:
                    EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, index+1 );
                    i = T_RAX;
                }
                *regs_used |= R0_USED;
                AddLineQueueX( " lea %r, %s", i, paramvalue );
                DebugMsg(("ms64_param(%s, param=%u): ADDR flags=%X\n", proc->sym.name, index, *regs_used ));
            }
            AddLineQueueX( " mov [%r+%u], %r", T_RSP, NUMQUAL index*8, i );
        }
    } else if ( param->sym.mem_type == MT_REAL4 ||
               param->sym.mem_type == MT_REAL8 ) {
        /* v2.04: check if argument is the correct XMM register already */
        if ( opndx->indirect == FALSE && opndx->base_reg != NULL ) {
            reg = opndx->base_reg->tokval;
            if ( GetValueSp( reg ) & OP_XMM ) {
                if ( reg == T_XMM0 + index )
                    DebugMsg(("ms64_param(%s, param=%u): argument optimized\n", proc->sym.name, index ));
                else
                    AddLineQueueX( " movq %r, %s", T_XMM0 + index, paramvalue );
                return( 1 );
            }
        }
        if ( opndx->kind == EXPR_FLOAT ) {
            *regs_used |= R0_USED;
            if ( param->sym.mem_type == MT_REAL4 ) {
                AddLineQueueX( " mov %r, %s", T_EAX, paramvalue );
                AddLineQueueX( " movd %r, %r", T_XMM0 + index, T_EAX );
            } else {
                AddLineQueueX( " mov %r, %r ptr %s", T_RAX, T_REAL8, paramvalue );
                AddLineQueueX( " movd %r, %r", T_XMM0 + index, T_RAX );
            }
        } else {
            if ( param->sym.mem_type == MT_REAL4 )
                AddLineQueueX( " movd %r, %s", T_XMM0 + index, paramvalue );
            else
                AddLineQueueX( " movq %r, %s", T_XMM0 + index, paramvalue );
        }
    } else {
        if ( addr || psize > 8 ) { /* psize > 8 shouldn't happen! */
            if ( psize == 4 )
                AddLineQueueX( " lea %r, %s", ms64_regs[index+2*4], paramvalue );
            //else if ( psize > 4 ) /* v2.07: psize==0 happens for VARARG */
            else if ( psize > 4 || param->sym.is_vararg )
                AddLineQueueX( " lea %r, %s", ms64_regs[index+3*4], paramvalue );
            else
                EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, index+1 );
            *regs_used |= ( 1 << ( index + RPAR_START ) );
            return( 1 );
        }
        /* register argument? */
        if ( opndx->indirect == FALSE && opndx->base_reg != NULL ) {
            reg = opndx->base_reg->tokval;
            size = SizeFromRegister( reg );
        } else {
            if ( opndx->mem_type != MT_EMPTY )
                size = SizeFromMemtype( opndx->mem_type, USE64, opndx->type );
            else {
                size = psize;
            }
        }
        if ( size != psize && param->sym.is_vararg == FALSE ) {
            DebugMsg(("ms64_param(%s, param=%u): type error size.p/a=%u/%u flags=%X\n", proc->sym.name, index, psize, size, *regs_used ));
            EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, index+1 );
        }
        switch ( size ) {
        case 1: base =  0*4; break;
        case 2: base =  1*4; break;
        case 4: base =  2*4; break;
        default:base =  3*4; break;
        }
        /* optimization if the register holds the value already */
        if ( opndx->indirect == FALSE && opndx->base_reg != NULL ) {
            if ( GetValueSp( reg ) & OP_R ) {
                if ( ms64_regs[index+base] == reg ) {
                    DebugMsg(("ms64_param(%s, param=%u): argument optimized\n", proc->sym.name, index ));
                    return( 1 );
                }
                i = GetRegNo( reg );
                if ( REGPAR_WIN64 & ( 1 << i ) ) {
                    i = GetParmIndex( i );
                    if ( *regs_used & ( 1 << ( i + RPAR_START ) ) )
                        EmitErr( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
                }
            }
        }
        AddLineQueueX( " mov %r, %s", ms64_regs[index+base], paramvalue );
        *regs_used |= ( 1 << ( index + RPAR_START ) );
        DebugMsg(("ms64_param(%s, param=%u): size=%u flags=%X\n", proc->sym.name, index, size, *regs_used ));
    }
    return( 1 );
}

#endif

/* get segment part of an argument
 * v2.05: extracted from PushInvokeParam(),
 * so it could be used by watc_param() as well.
 */

static void GetSegmentPart( struct expr *opndx, char *buffer, const char *fullparam )
/***********************************************************************************/
{
    if ( opndx->override != NULL ) {
        strcpy( buffer, opndx->override->string_ptr );
    } else if ( opndx->sym != NULL && opndx->sym->segment != NULL ) {
        struct dsym *dir = GetSegm( opndx->sym );
        enum assume_segreg as;
        if ( dir->e.seginfo->segtype == SEGTYPE_DATA ||
            dir->e.seginfo->segtype == SEGTYPE_BSS )
            as = search_assume( (struct asym *)dir, ASSUME_DS, TRUE );
        else
            as = search_assume( (struct asym *)dir, ASSUME_CS, TRUE );
        if ( as != ASSUME_NOTHING ) {
            //GetResWName( segreg_tab[as], buffer );
            GetResWName( T_ES + as, buffer ); /* v2.08: T_ES is first seg reg in special.h */
        } else {
            struct asym *seg;
            seg = GetGroup( opndx->sym );
            if (seg == NULL)
                seg = &dir->sym;
            if ( seg )
                strcpy( buffer, seg->name );
            else {
                strcpy( buffer, "seg " );
                strcat( buffer, fullparam );
            }
        }
    } else if ( opndx->sym && opndx->sym->state == SYM_STACK ) {
        GetResWName( T_SS, buffer );
    } else {
        strcpy( buffer,"seg " );
        strcat( buffer, fullparam );
    }
    return;
}

#if OWFC_SUPPORT

/* the watcomm fastcall variant is somewhat peculiar:
 * 16-bit:
 * - for BYTE/WORD arguments, there are 4 registers: AX,DX,BX,CX
 * - for DWORD arguments, there are 2 register pairs: DX::AX and CX::BX
 * - there is a "usage" flag for each register. Thus the prototype:
 *   sample proto :WORD, :DWORD, :WORD
 *   will assign AX to the first param, CX::BX to the second, and DX to
 *   the third!
 */

static int watc_fcstart( struct dsym const *proc, int numparams, int start, struct asm_tok tokenarray[], int *value )
/*******************************************************************************************************************/
{
    DebugMsg1(("watc_fcstart(%s, %u, %u)\n", proc->sym.name, numparams, start ));
    return( 1 );
}

static void watc_fcend( struct dsym const *proc, int numparams, int value )
/*************************************************************************/
{
    DebugMsg1(("watc_fcend(%s, %u, %u)\n", proc->sym.name, numparams, value ));
    if ( proc->e.procinfo->is_vararg ) {
        AddLineQueueX( " add %r, %u", stackreg[ModuleInfo.Ofssize], NUMQUAL proc->e.procinfo->parasize + size_vararg );
    } else if ( fcscratch < proc->e.procinfo->parasize ) {
        AddLineQueueX( " add %r, %u", stackreg[ModuleInfo.Ofssize], NUMQUAL ( proc->e.procinfo->parasize - fcscratch ) );
    }
    return;
}

/* get the register for parms 0 to 3,
 * using the watcom register parm passing conventions ( A D B C )
 */
static int watc_param( struct dsym const *proc, int index, struct dsym *param, bool addr, struct expr *opndx, char *paramvalue, uint_8 *r0used )
/**********************************************************************************************************************************************/
{
    int opc;
    int qual;
    int i;
    char regs[64];
    char *reg[4];
    char *p;
    int psize = SizeFromMemtype( param->sym.mem_type, USE_EMPTY, param->sym.type );

    DebugMsg1(("watc_param(%s, param=%u [name=%s, state=%u]),addr=%u: psize=%u\n", proc->sym.name, index, param->sym.name, param->sym.state, addr, psize ));
    if ( param->sym.state != SYM_TMACRO )
        return( 0 );
    DebugMsg1(("watc_param(%s): register param=%s\n", proc->sym.name, param->sym.string_ptr ));

    fcscratch += CurrWordSize;

    /* the "name" might be a register pair */

    reg[0] = param->sym.string_ptr;
    reg[1] = NULL;
    reg[2] = NULL;
    reg[3] = NULL;
    if ( strchr( reg[0], ':' ) ) {
        strcpy( regs, reg[0] );
        fcscratch += CurrWordSize;
        for ( p = regs, i = 0; i < 4; i++ ) {
            reg[i] = p;
            p = strchr( p, ':' );
            if ( p == NULL )
                break;
            *p++ = NULLC;
            p++;
        }
    }

    if ( addr ) {
        if ( opndx->kind == T_REG || opndx->sym->state == SYM_STACK ) {
            opc = T_LEA;
            qual = T_NULL;
        } else {
            opc = T_MOV;
            qual = T_OFFSET;
        }
        /* v2.05: filling of segment part added */
        i = 0;
        if ( reg[1] != NULL ) {
            char buffer[128];
            GetSegmentPart( opndx, buffer, paramvalue );
            AddLineQueueX( "%r %s, %s", T_MOV, reg[0],  buffer );
            i++;
        }
        AddLineQueueX( "%r %s, %r %s", opc, reg[i], qual, paramvalue );
        return( 1 );
    }
    for ( i = 3; i >= 0; i-- ) {
        if ( reg[i] ) {
            if ( opndx->kind == EXPR_CONST ) {
                if ( i > 0 )
                    qual = T_LOWWORD;
                else if ( i == 0 && reg[1] != NULL )
                    qual = T_HIGHWORD;
                else
                    qual = T_NULL;
                if ( qual != T_NULL )
                    AddLineQueueX( "mov %s, %r (%s)", reg[i], qual, paramvalue );
                else
                    AddLineQueueX( "mov %s, %s", reg[i], paramvalue );
            } else if ( opndx->kind == EXPR_REG ) {
                AddLineQueueX( "mov %s, %s", reg[i], paramvalue );
            } else {
                if ( i == 0 && reg[1] == NULL )
                    AddLineQueueX( "mov %s, %s", reg[i], paramvalue );
                else {
                    if ( ModuleInfo.Ofssize )
                        qual = T_DWORD;
                    else
                        qual = T_WORD;
                    AddLineQueueX( "mov %s, %r %r %s[%u]", reg[i], qual, T_PTR, paramvalue, psize - ( (i+1) * ( 2 << ModuleInfo.Ofssize ) ) );
                }
            }
        }
    }
    return( 1 );
}

#endif

static void SkipTypecast( char *fullparam, int i, struct asm_tok tokenarray[] )
/*****************************************************************************/
{
    int j;
    fullparam[0] = NULLC;
    for ( j = i; ; j++ ) {
        if (( tokenarray[j].token == T_COMMA ) || ( tokenarray[j].token == T_FINAL ) )
            break;
        if (( tokenarray[j+1].token == T_BINARY_OPERATOR ) && ( tokenarray[j+1].tokval == T_PTR ) )
            j = j + 1;
        else {
            if ( fullparam[0] != NULLC )
                strcat( fullparam," " );
            strcat( fullparam, tokenarray[j].string_ptr );
        }
    }
}

/*
 * push one parameter of a procedure called with INVOKE onto the stack
 * - i       : index of the start of the parameter list
 * - tokenarray : token array
 * - proc    : the PROC to call
 * - curr    : the current parameter
 * - reqParam: the index of the parameter which is to be pushed
 * - r0flags : flags for register usage across params
 *
 * psize,asize: size of parameter/argument in bytes.
 */

static int PushInvokeParam( int i, struct asm_tok tokenarray[], struct dsym *proc, struct dsym *curr, int reqParam, uint_8 *r0flags)
/**********************************************************************************************************************************/
{
    int currParm;
    int psize;
    int asize;
    int pushsize;
    int j;
    int fptrsize;
    bool addr = FALSE; /* ADDR operator found */
    struct expr opndx;
    char fullparam[MAX_LINE_LEN];
    char buffer[MAX_LINE_LEN];

    DebugMsg1(("PushInvokeParam(%s, param=%s:%u, i=%u ) enter\n", proc->sym.name, curr ? curr->sym.name : "NULL", reqParam, i ));
    for ( currParm = 0; currParm <= reqParam; ) {
        if ( tokenarray[i].token == T_FINAL ) { /* this is no real error! */
            DebugMsg1(("PushInvokeParam(%s): T_FINAL token, i=%u\n", proc->sym.name, i));
            return( ERROR );
        }
        if ( tokenarray[i].token == T_COMMA ) {
            currParm++;
        }
        i++;
    }
    /* if curr is NULL this call is just a parameter check */
    if ( !curr ) return( NOT_ERROR );

#if 1 /* v2.05 */
    psize = curr->sym.total_size;
    DebugMsg1(("PushInvokeParam(%s,%u): memtype=%X, psize=%u\n", proc->sym.name, reqParam, curr->sym.mem_type, psize ));
#else
    /* set psize (size of parameter) */
    if ( curr->is_ptr ) {
        psize = 2 << curr->sym.Ofssize;
        if ( curr->sym.isfar )
            psize += 2;
    } else
        psize = SizeFromMemtype( curr->sym.mem_type, curr->sym.Ofssize, curr->sym.type );
    DebugMsg1(("PushInvokeParam(%s,%u): is_ptr=%u, memtype=%X, psize=%u\n", proc->sym.name, reqParam, curr->is_ptr, curr->sym.mem_type, psize ));
#endif

    /* ADDR: the argument's address is to be pushed? */
    if ( tokenarray[i].token == T_RES_ID && tokenarray[i].tokval == T_ADDR ) {
        addr = TRUE;
        i++;
    }

    /* copy the parameter tokens to fullparam */
    for ( j = i; tokenarray[j].token != T_COMMA && tokenarray[j].token != T_FINAL; j++ );
    memcpy( fullparam, tokenarray[i].tokpos, tokenarray[j].tokpos - tokenarray[i].tokpos );
    fullparam[tokenarray[j].tokpos - tokenarray[i].tokpos] = NULLC;

    j = i;
    fptrsize = 2 + ( 2 << GetSymOfssize( &proc->sym ) );

    if ( addr ) {
        /* v2.06: don't handle forward refs if -Zne is set */
        //if ( EvalOperand( &j, Token_Count, &opndx, 0 ) == ERROR )
        if ( EvalOperand( &j, tokenarray, Token_Count, &opndx, ModuleInfo.invoke_exprparm ) == ERROR )
            return( ERROR );

        /* DWORD (16bit) and FWORD(32bit) are treated like FAR ptrs */
        if ( psize > fptrsize ) {
            /* QWORD is NOT accepted as a FAR ptr */
            DebugMsg1(("PushInvokeParm(%u): error, psize=%u, fptrsize=%u\n",
                      reqParam, psize, fptrsize));
            EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
            return( NOT_ERROR );
        }

        if ( proc->sym.langtype == LANG_FASTCALL )
            if ( fastcall_tab[ModuleInfo.fctype].handleparam( proc, reqParam, curr, addr, &opndx, fullparam, r0flags ) )
                return( NOT_ERROR );

        if ( opndx.kind == EXPR_REG || opndx.indirect ) {
            if ( curr->sym.isfar || psize == fptrsize ) {
                DebugMsg1(("PushInvokeParam: far ptr, %s isfar=%u, psize=%u, fptrsize=%u\n", curr->sym.name, curr->sym.isfar, psize, fptrsize ));
                if ( opndx.sym && opndx.sym->state == SYM_STACK )
                    GetResWName( T_SS, buffer );
                else if ( opndx.override != NULL )
                    strcpy( buffer, opndx.override->string_ptr );
                else
                    GetResWName( T_DS, buffer );
                AddLineQueueX( " push %s", buffer );
            }
            AddLineQueueX( " lea %r, %s", regax[ModuleInfo.Ofssize], fullparam );
            *r0flags |= R0_USED;
            AddLineQueueX( " push %r", regax[ModuleInfo.Ofssize] );
        } else {
        push_address:

            /* push segment part of address? */
            if ( curr->sym.isfar || psize == fptrsize ) {

                GetSegmentPart( &opndx, buffer, fullparam );
                AddLineQueueX( " push %s", buffer );
            }
            /* push offset part of address */
            if ( (ModuleInfo.curr_cpu & P_CPU_MASK ) < P_186 ) {
                AddLineQueueX( " mov %r, offset %s", T_AX, fullparam );
                AddLineQueueX( " push %r", T_AX );
                *r0flags |= R0_USED;
            } else {
                if ( curr->sym.is_vararg &&
                    opndx.Ofssize == USE_EMPTY &&
                    opndx.sym )
                    opndx.Ofssize = GetSymOfssize( opndx.sym );
                /* v2.04: expand 16-bit offset to 32 */
                if ( opndx.Ofssize == USE16 && CurrWordSize > 2 ) {
                    AddLineQueueX( " pushd offset %s", fullparam );
                } else {
                    AddLineQueueX( " push offset %s", fullparam );
                    /* v2.04: a 32bit offset pushed in 16-bit code */
                    if ( curr->sym.is_vararg &&
                        CurrWordSize == 2 &&
                        opndx.Ofssize > USE16 )
                        size_vararg += CurrWordSize;
                }
            }
        }
        if ( curr->sym.is_vararg ) {
            size_vararg += CurrWordSize;
            DebugMsg1(("PushInvokeParm(%u): %u added to size_vararg, now=%u\n", reqParam, CurrWordSize, size_vararg ));
            if ( curr->sym.isfar ) {
                size_vararg += CurrWordSize;
                DebugMsg1(("PushInvokeParm(%u): %u added to size_vararg, now=%u\n", reqParam, CurrWordSize, size_vararg ));
            }
        }
    } else { /* ! ADDR branch */

        /* handle the <reg>::<reg> case here, the evaluator wont handle it */
        if ( tokenarray[j].token == T_REG &&
            tokenarray[j+1].token == T_DBL_COLON &&
            tokenarray[j+2].token == T_REG ) {
            int asize2;
            /* for pointers, segreg size is assumed to be always 2 */
            if ( GetValueSp( tokenarray[j].tokval ) & OP_SR )
                asize2 = 2;
            else
                asize2 = SizeFromRegister( tokenarray[j].tokval );
            asize = SizeFromRegister( tokenarray[j+2].tokval );
            AddLineQueueX( " push %r", tokenarray[j].tokval );
            /* v2.04: changed */
            if (( curr->sym.is_vararg ) && (asize + asize2) != CurrWordSize )
                size_vararg += asize2;
            else
                asize += asize2;
            strcpy( fullparam, tokenarray[j+2].string_ptr );

            opndx.kind = EXPR_REG;
            opndx.indirect = FALSE;
            opndx.sym = NULL;
            opndx.base_reg = &tokenarray[j+2]; /* for error msg 'eax overwritten...' */
        } else {
            /* v2.06: don't handle forward refs if -Zne is set */
            //if ( EvalOperand( &j, Token_Count, &opndx, 0 ) == ERROR ) {
            if ( EvalOperand( &j, tokenarray, Token_Count, &opndx, ModuleInfo.invoke_exprparm ) == ERROR ) {
                return( ERROR );
            }
            /* for a simple register, get its size */
            if ( opndx.kind == EXPR_REG && opndx.indirect == FALSE ) {
                asize = SizeFromRegister( opndx.base_reg->tokval );
            } else if ( opndx.mem_type == MT_EMPTY ) {
                asize = psize;
                /* v2.04: added, to catch 0-size params ( STRUCT without members ) */
                if ( psize == 0 ) {
                    if ( curr->sym.is_vararg == FALSE ) {
                        DebugMsg1(("PushInvokeParm(%u): error, psize=0\n" ));
                        EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                    }
                    /* v2.07: for VARARG, get the member's size if it is a structured var */
                    if ( opndx.mbr && opndx.mbr->mem_type == MT_TYPE )
                        asize = SizeFromMemtype( opndx.mbr->mem_type, opndx.Ofssize, opndx.mbr->type );
                }
                DebugMsg1(("PushInvokeParm(%u): memtype EMPTY, asize=%u psize=%u\n", reqParam, asize, psize ));
            } else if ( opndx.mem_type != MT_TYPE ) {
                if ( opndx.kind == EXPR_ADDR &&
                     opndx.indirect == FALSE &&
                     opndx.sym &&
                     opndx.instr == EMPTY &&
                     (opndx.mem_type == MT_PROC ||
                      opndx.mem_type == MT_FAR ||
                      opndx.mem_type == MT_NEAR))
                    goto push_address;
                if ( opndx.Ofssize == USE_EMPTY )
                    opndx.Ofssize = ModuleInfo.Ofssize;
                asize = SizeFromMemtype( opndx.mem_type, opndx.Ofssize, opndx.type );
            } else {
                if ( opndx.sym != NULL )
                    asize = opndx.sym->type->total_size;
                else
                    asize = opndx.mbr->type->total_size;
            }
        }

        if ( curr->sym.is_vararg == TRUE )
            psize = asize;

#ifdef DEBUG_OUT
        if ( opndx.sym )
            DebugMsg1(("PushInvokeParam(%s, %u): arg name=%s, asize=%u, psize=%u\n", proc->sym.name, reqParam, opndx.sym->name, asize, psize));
        else
            DebugMsg1(("PushInvokeParam(%s, %u): arg no name, asize=%u, psize=%u\n", proc->sym.name, reqParam, asize, psize));
#endif
        pushsize = CurrWordSize;

        if ( proc->sym.langtype == LANG_FASTCALL )
            if ( fastcall_tab[ModuleInfo.fctype].handleparam( proc, reqParam, curr, addr, &opndx, fullparam, r0flags ) )
                return( NOT_ERROR );

        /* v2.04: this check has been moved behind the fastcall_tab() call */
        if ( asize > psize ) { /* argument's size too big? */
            DebugMsg(("PushInvokeParm(%u): argsize error, arg size=%d, parm size=%d\n", reqParam, asize, psize));
            EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
            return( NOT_ERROR );
        }

        if ( ( opndx.kind == EXPR_ADDR && opndx.instr != T_OFFSET ) ||
            ( opndx.kind == EXPR_REG && opndx.indirect == TRUE ) ) {

            /* catch the case when EAX has been used for ADDR,
             * and is later used as addressing register!
             *
             */
            if ( *r0flags &&
                (( opndx.base_reg != NULL &&
                  ( opndx.base_reg->tokval == T_EAX
#if AMD64_SUPPORT
                   || opndx.base_reg->tokval == T_RAX
#endif
                  )) ||
                 ( opndx.idx_reg != NULL &&
                  ( opndx.idx_reg->tokval == T_EAX
#if AMD64_SUPPORT
                   || opndx.idx_reg->tokval == T_RAX
#endif
                 )))) {
                EmitErr( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
                *r0flags = 0;
            }

            if ( asize > pushsize ) {
                short dw = T_WORD;
                if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                    pushsize = 4;
                    dw = T_DWORD;
                }
                if ( curr->sym.is_vararg ) {
                    size_vararg += asize;
                    DebugMsg1(("PushInvokeParm(%u): asize=%u added to size_vararg, now=%u\n", reqParam, asize, size_vararg ));
                }

                /* in params like "qword ptr [eax]" the typecast
                 * has to be removed */
                if ( opndx.explicit ) {
                    SkipTypecast( fullparam, i, tokenarray );
                    opndx.explicit = FALSE;
                }

                while ( asize > 0 ) {
#if 0
                    if ( opndx.explicit ) {
                        sprintf( buffer, " push %s", fullparam );
                        asize = 0;
                    } else if ( asize & 2 ) {
#else
                    if ( asize & 2 ) {
#endif
                        /* ensure the stack remains dword-aligned in 32bit */
                        if ( ModuleInfo.Ofssize > USE16 ) {
                            /* v2.05: better push a 0 word? */
                            //AddLineQueueX( " pushw 0" );
#if AMD64_SUPPORT
                            AddLineQueueX( " sub %r, 2", stackreg[ModuleInfo.Ofssize] );
#else
                            AddLineQueueX( " sub %r, 2", T_ESP );
#endif
                        }
                        AddLineQueueX( " push word ptr %s+%u", fullparam, NUMQUAL asize-2 );
                        asize -= 2;
                    } else {
                        AddLineQueueX( " push %r ptr %s+%u", dw, fullparam, NUMQUAL asize-pushsize );
                        asize -= pushsize;
                    }
                }
                return( NOT_ERROR );
            } else if ( asize < pushsize ) {
                if ( curr->sym.is_vararg ) {
                    size_vararg += pushsize;
                    DebugMsg1(("PushInvokeParm(%u): %u added to size_vararg, now=%u\n", reqParam, pushsize, size_vararg ));
                }
                if ( psize > 4 ) {
                    DebugMsg1(("PushInvokeParm(%u): error, ADDR, psize=%u, is > 4\n",
                              reqParam, psize ));
                    EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                }
                //switch (sym->mem_type) {
                switch ( opndx.mem_type ) {
                case MT_BYTE:
                case MT_SBYTE:
                    if ( psize == 1 && curr->sym.is_vararg == FALSE ) {
                        AddLineQueueX( " mov %r, %s", T_AL, fullparam );
                        AddLineQueueX( " push %r", pushsize == 4 ? T_EAX : T_AX );
                    } else if ( pushsize == 2 ) {
                        if ( psize == 4 )
                            AddLineQueue( " push 0" );
                        AddLineQueueX( " mov %r, %s", T_AL, fullparam );
                        if ( opndx.mem_type == MT_BYTE ) {
                            if ( !( *r0flags & R0_H_CLEARED ))
                                AddLineQueueX( " mov %r, 0", T_AH );
                            *r0flags |= R0_H_CLEARED;
                        } else {
                            *r0flags = 0; /* reset AH_CLEARED */
                            AddLineQueue( " cbw" );
                        }
                        AddLineQueueX( " push %r", T_AX );
                    } else {
                        AddLineQueueX( " %r %r, %s", opndx.mem_type == MT_BYTE ? T_MOVZX : T_MOVSX, T_EAX, fullparam );
                        AddLineQueueX( " push %r", T_EAX );
                    }
                    *r0flags |= R0_USED;
                    break;
                case MT_WORD:
                case MT_SWORD:
                    /* v2.04: use the Masm-compatible, non-destructive
                     * PUSH if psize is 2.
                     */
                    //if ( Options.masm_compat_gencode ) {
                    if ( Options.masm_compat_gencode || psize == 2 ) {
                        /* v2.05: push a 0 word if argument is VARARG */
                        if ( curr->sym.is_vararg )
                            AddLineQueueX( " pushw 0" );
                        else {
#if AMD64_SUPPORT
                            AddLineQueueX( " sub %r, 2", stackreg[ModuleInfo.Ofssize] );
#else
                            AddLineQueueX( " sub %r, 2", T_ESP );
#endif
                        }
                        AddLineQueueX( " push %s", fullparam );
                    } else {
                        AddLineQueueX( " %r %r, %s", opndx.mem_type == MT_WORD ? T_MOVZX : T_MOVSX, T_EAX, fullparam );
                        AddLineQueueX( " push %r", T_EAX );
                        *r0flags = R0_USED; /* reset R0_H_CLEARED  */
                    }
                    break;
                default:
                    AddLineQueueX( " push %s", fullparam );
                }
            } else { /* asize == pushsize */
                if (( pushsize == 2 ) || (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ))
                    AddLineQueueX( " push %s", fullparam );
                else {
                    AddLineQueueX( " push word ptr %s+2", tokenarray[i].string_ptr );
                    AddLineQueueX( " push word ptr %s", tokenarray[i].string_ptr );
                }
                if ( curr->sym.is_vararg ) {
                    size_vararg += pushsize;
                    DebugMsg1(("PushInvokeParm(%u): asize=%u added to size_vararg, now=%u\n", reqParam, pushsize, size_vararg ));
                }
            }
        } else { /* the parameter is a register or constant value! */
            char is_r0 = FALSE;
            if ( opndx.kind == EXPR_REG ) {
                int reg = opndx.base_reg->tokval;
                uint optype = GetValueSp( reg );

                /* v2.06: check if register is valid to be pushed.
                 * ST(n), MMn, XMMn and special registers are NOT valid!
                 */
                if ( optype & ( OP_STI | OP_MMX | OP_XMM | OP_RSPEC ) ) {
                    EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                    return( ERROR );
                }

                if ( reg == T_AH || ( optype & OP_A ) ) {
                    is_r0 = TRUE;
                    if ( *r0flags & R0_USED ) {
                        EmitErr( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
                        *r0flags = 0;
                    }
                }
                if ( asize != psize || asize < pushsize ) {
                    /* register size doesn't match the needed parameter size.
                     */
                    if ( psize > 4 ) {
                        DebugMsg1(("PushInvokeParm(%u): error, REG, asize=%u, psize=%u, pushsize=%u\n",
                                  reqParam, asize, psize, pushsize ));
                        EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                    }
                    if ( asize <= 2 && ( psize == 4 || pushsize == 4 ) ) {
                        if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 &&
                            asize == psize ) {
                            if ( asize == 1 )
                                reg = reg - T_AL + T_EAX;
                            else
                                reg = reg - T_AX + T_EAX;
                            asize = 2; /* done */
                        } else if ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_186 ) {
                            if ( pushsize == 4 ) {
                                if ( asize == 1 ) {
                                    ;
                                } else if ( psize <= 2 ) {
#if AMD64_SUPPORT
                                    AddLineQueueX( " sub %r, 2", stackreg[ModuleInfo.Ofssize] );
#else
                                    AddLineQueueX( " sub %r, 2", T_ESP );
#endif
                                } else {
                                    AddLineQueue( " pushw 0" );
                                }
                            } else
                                AddLineQueue( " pushw 0" );
                        } else {
                            if ( !(*r0flags & R0_X_CLEARED) )
                                AddLineQueueX( " xor %r, %r", T_AX, T_AX );
                            AddLineQueueX( " push %r", T_AX );
                            *r0flags = R0_USED | R0_H_CLEARED | R0_X_CLEARED;
                        }
                    }

                    if ( asize == 1 ) {
                        if ( ( reg >= T_AH && reg <= T_BH ) || psize > 1 ) {
                            if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                                if ( pushsize == 4 )
                                    reg = T_EAX;
                                else
                                    reg = T_AX;
                                AddLineQueueX( " movzx %r, %s", reg, fullparam );
                                *r0flags |= (R0_USED | R0_H_CLEARED );
                            } else {
                                if ( reg != T_AL ) {
                                    AddLineQueueX( " mov %r, %s", T_AL, fullparam );
                                    *r0flags |= R0_USED;
                                    *r0flags &= ~R0_X_CLEARED;
                                }
                                if (!( *r0flags & R0_H_CLEARED )) {
                                    AddLineQueueX( " mov %r, 0", T_AH );
                                    *r0flags |= R0_H_CLEARED;
                                }
                                reg = T_AX;
                            }
                        } else {
                            /* convert 8-bit to 16/32-bit register name */
                            if ( (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386) &&
                                ( psize == 4 || pushsize == 4 ) ) {
                                reg = reg - T_AL + T_EAX;
                            } else
                                reg = reg - T_AL + T_AX;
                        }
                    }
                    if ( is_r0 && ( *r0flags & R0_USED ) ) {
                        EmitErr( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
                        *r0flags = 0;
                    }
                }
                AddLineQueueX( " push %r", reg );
                /* v2.05: don't change psize if > pushsize */
                if ( psize < pushsize )
                    /* v2.04: adjust psize ( for siz_vararg update ) */
                    psize = pushsize;
            } else { /* constant value */

                /* v2.06: size check */
                if ( psize ) {
                    if ( opndx.kind == EXPR_FLOAT )
                        asize = 4;
                    else if ( opndx.value64 <= 255 && opndx.value64 >= -255 )
                        asize = 1;
                    else if ( opndx.value64 <= 65535 && opndx.value64 >= -65535 )
                        asize = 2;
                    else if ( opndx.value64 <= maxintvalues[0] && opndx.value64 >= minintvalues[0] )
                        asize = 4;
                    else
                        asize = 8;
                    if ( psize < asize )
                        EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                }

                if ( psize < pushsize )  /* ensure that the default pushsize is met */
                    if ( psize == 0 && curr->sym.is_vararg ) {
                        /* v2.04: push a dword constant in 16-bit */
                        if ( pushsize == 2 &&
                            ( opndx.value > 0xFFFFL || opndx.value < -65535L ) )
                            psize = 4;
                        else
                            psize = pushsize;
                    } else
                        psize = pushsize;

                asize = CurrWordSize;

                if ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_186 ) {
                    *r0flags |= R0_USED;
                    switch ( psize ) {
                    case 2:
                        if ( opndx.value != 0 || opndx.kind == EXPR_ADDR ) {
                            AddLineQueueX( " mov %r, %s", T_AX, fullparam );
                        } else {
                            if ( !(*r0flags & R0_X_CLEARED ) ) {
                                AddLineQueueX( " xor %r, %r", T_AX, T_AX );
                            }
                            *r0flags |= R0_H_CLEARED | R0_X_CLEARED;
                        }
                        break;
                    case 4:
                        if ( opndx.uvalue <= 0xFFFF )
                            AddLineQueueX( " xor %r, %r", T_AX, T_AX );
                        else
                            AddLineQueueX( " mov %r, %r (%s)", T_AX, T_HIGHWORD, fullparam );
                        AddLineQueueX( " push %r", T_AX );
                        if ( opndx.uvalue != 0 || opndx.kind == EXPR_ADDR ) {
                            AddLineQueueX( " mov %r, %r (%s)", T_AX, T_LOWWORD, fullparam );
                        } else {
                            *r0flags |= R0_H_CLEARED | R0_X_CLEARED;
                        }
                        break;
                    default:
                        DebugMsg1(("PushInvokeParm(%u): error, CONST, asize=%u, psize=%u, pushsize=%u\n",
                                  reqParam, asize, psize, pushsize ));
                        EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                    }
                    AddLineQueueX( " push %r", T_AX );
                } else {
                    uint_16 instr = T_PUSH;
                    int qual = EMPTY;
                    if ( asize != psize ) {
                        switch ( psize ) {
                        case 2:
                            instr = T_PUSHW;
                            break;
                        case 6: /* v2.04: added */
                            AddLineQueueX( " pushw (%s) shr 32", fullparam );
                            /* no break */
                        case 4:
                            if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 )
                                instr = T_PUSHD;
                            else {
                                AddLineQueueX( " pushw %r (%s)", T_HIGHWORD, fullparam );
                                instr = T_PUSHW;
                                qual = T_LOWWORD;
                            }
                            break;
                        case 8:
#if AMD64_SUPPORT
                            if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_64 )
                                break;
#endif
                            /* v2.06: added support for double constants */
                            if ( opndx.kind == EXPR_CONST || opndx.kind == EXPR_FLOAT ) {
                                AddLineQueueX( " pushd %r (%s)", T_HIGH32, fullparam );
                                qual = T_LOW32;
                                instr = T_PUSHD;
                                break;
                            }
                        default:
                            DebugMsg1(("PushInvokeParm(%u): error, CONST, asize=%u, psize=%u, pushsize=%u\n",
                                      reqParam, asize, psize, pushsize ));
                            EmitErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                        }
                    }
                    if ( qual != EMPTY )
                        AddLineQueueX( " %r %r (%s)", instr, qual, fullparam );
                    else
                        AddLineQueueX( " %r %s", instr, fullparam );
                }
            }
            if ( curr->sym.is_vararg ) {
                size_vararg += psize;
                DebugMsg1(("PushInvokeParm(%u): psize=%u added to size_vararg, now=%u\n", reqParam, psize, size_vararg ));
            }
        }
    }
    return( NOT_ERROR );
}

/* generate a call for a prototyped procedure */

ret_code InvokeDirective( int i, struct asm_tok tokenarray[] )
/************************************************************/
{
    struct asym    *sym;
    struct dsym    *proc;
    char           *p;
    //char         *param;
    int            numParam;
    int            value;
    int            size;
    int            parmpos;
    int            namepos;
    int            porder;
    uint_8         r0flags = 0;
    bool           uselabel = FALSE;
    struct proc_info *info;
    struct dsym    *curr;
    struct expr    opndx;
    //char           buffer[MAX_LINE_LEN];

    DebugMsg1(("InvokeDir(%s) enter\n", tokenarray[i].tokpos ));

    i++; /* skip INVOKE directive */
    namepos = i;

    /* if there is more than just an ID item describing the invoke target,
     use the expression evaluator to get it
     */
    if ( tokenarray[i].token != T_ID || ( tokenarray[i+1].token != T_COMMA && tokenarray[i+1].token != T_FINAL ) ) {
    //if ( tokenarray[i+1].token != T_COMMA && tokenarray[i+1].token != T_FINAL ) {
        if ( ERROR == EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) )
            return( ERROR );
        DebugMsg1(("InvokeDir: target is expression, kind=%u sym=%s mbr=%s type=%s memtype=%X ofssize=%u\n",
                   opndx.kind,
                   opndx.sym ? opndx.sym->name : "NULL",
                   opndx.mbr ? opndx.mbr->name : "NULL",
                   opndx.type ? opndx.type->name : "NULL",
                   opndx.mem_type, opndx.Ofssize ));
        sym = NULL;
#if 1
        /* a typecast with PTR? Since v1.95, this has highest priority */
        //if (opndx.explicit == TRUE && opndx.type != NULL && opndx.type->state == SYM_TYPE ) {
        /* v1.96: removed opndx.explicit!!! */
        if ( opndx.type != NULL && opndx.type->state == SYM_TYPE ) {
            sym = opndx.type;
            proc = (struct dsym *)sym;
            if ( opndx.label_tok != NULL )
                uselabel = TRUE;
            if ( proc->sym.mem_type == MT_PROC )  /* added for v1.95 */
                goto isfnproto;
            goto isfnptr;
#endif
        } else if ( opndx.mbr != NULL ) {
            sym = opndx.mbr;
            /* it may be a typecast. then the mbr member contains the explicit type
             * and sym->state is SYM_TYPE
             * v1.96: the sentence above describes an obsolete feature.
             * Not sure if <mbr> can contain a type anymore.
             * this code is to be removed/modified! */
            if ( sym->state == SYM_TYPE ) {
                proc = (struct dsym *)sym;
                if ( opndx.label_tok != NULL )
                    uselabel = TRUE;
                goto isfnptr;
            }
        } else if ( opndx.kind == EXPR_ADDR &&
                   opndx.sym != NULL &&
                   ( opndx.sym->isproc ||
                   ( opndx.sym->mem_type == MT_TYPE &&
                   opndx.sym->type->mem_type == MT_PTR ) ) ) {
            sym = opndx.sym;
        } else if ( opndx.kind == EXPR_REG ) {
            if ( GetValueSp( opndx.base_reg->tokval ) & OP_RGT8 )
                sym = GetStdAssume( GetRegNo( opndx.base_reg->tokval ) );
        }
    } else {
        opndx.base_reg = NULL;
        sym = SymSearch( tokenarray[i].string_ptr );
        i++;
    }

    if( sym == NULL ) {
        /* v2.04: msg changed */
        EmitErr( INVOKE_REQUIRES_PROTOTYPE );
        //EmitErr( SYMBOL_NOT_DEFINED, name );
        return( ERROR );
    }
    if( sym->isproc )  /* the most simple case: symbol is a PROC */
        ;
    else if ( sym->mem_type == MT_PTR && sym->target_type && sym->target_type->isproc )
        sym = sym->target_type;
    else if ( sym->mem_type == MT_PTR && sym->target_type && sym->target_type->mem_type == MT_PROC ) {
        proc = (struct dsym *)sym->target_type;
        goto isfnproto;
    } else if ( ( sym->mem_type == MT_TYPE ) && ( sym->type->mem_type == MT_PTR || sym->type->mem_type == MT_PROC ) ) {
        /* second case: symbol is a (function?) pointer */
        proc = (struct dsym *)sym->type;
        if ( proc->sym.mem_type != MT_PROC )
            goto isfnptr;
    isfnproto:
        /* pointer target must be a PROTO typedef */
        if ( proc == NULL || proc->sym.mem_type != MT_PROC ) {
            DebugMsg(("InvokeDir: error sym=%X sym->name=%s, sym->type=%X\n", sym, sym ? sym->name : "", sym ? sym->type : 0 ));
            DebugMsg(("InvokeDir: error proc=%X, proc->name=%s\n", proc, proc ? proc->sym.name : "" ));
            EmitErr( INVOKE_REQUIRES_PROTOTYPE );
            return( ERROR );
        }
    isfnptr:
        /* get the pointer target */
        sym = proc->sym.target_type;
        DebugMsg1(("InvokeDir: proc=%s target_type=>%s<\n",
                  proc ? proc->sym.name : "NULL",
                  sym ? sym->name : "NULL" ));
        if ( sym == NULL ) {
            EmitErr( INVOKE_REQUIRES_PROTOTYPE );
            return( ERROR );
        }
    } else {
        DebugMsg(("InvokeDir: error, sym=%s state=%u memtype=%Xh [type=%s memtype=%Xh]\n",
                  sym->name, sym->state, sym->mem_type,
                  sym->type ? sym->type->name : "NULL",
                  sym->type ? sym->type->mem_type : 0));
#ifdef DEBUG_OUT
        if ( sym->mem_type == MT_PTR || sym->mem_type == MT_PROC )
            DebugMsg(("InvokeDir: error, target_type=%s [memtype=%X pmemtype=%X isproc=%u])\n",
                      sym->target_type->name,
                      sym->target_type->mem_type,
                      sym->target_type->ptr_memtype,
                      sym->target_type->isproc ));
#endif
        EmitErr( INVOKE_REQUIRES_PROTOTYPE );
        return( ERROR );
    }
    proc = (struct dsym *)sym;
    info = proc->e.procinfo;

#if 0 /* v2.05: can't happen anymore */
    /* does FASTCALL variant support INVOKE? */
    if ( proc->sym.langtype == LANG_FASTCALL && fastcall_tab[ModuleInfo.fctype].invokestart == NULL ) {
        EmitError( FASTCALL_VARIANT_NOT_SUPPORTED );
        return( ERROR );
    }
#endif

    /* get the number of parameters */

    for ( curr = info->paralist, numParam = 0 ; curr ; curr = curr->nextparam, numParam++ );
    DebugMsg1(("InvokeDir: numparams=%u\n", numParam ));

    NewLineQueue();

    if ( proc->sym.langtype == LANG_FASTCALL ) {
        fcscratch = 0;
        porder = fastcall_tab[ModuleInfo.fctype].invokestart( proc, numParam, i, tokenarray, &value );
    }

    curr = info->paralist;
    parmpos = i;

    if ( !( info->is_vararg ) ) {
        /* check if there is a superfluous parameter in the INVOKE call */
        if ( PushInvokeParam( i, tokenarray, proc, NULL, numParam, &r0flags ) != ERROR ) {
            DebugMsg(("InvokeDir: superfluous argument, i=%u\n", i));
            EmitErr( TOO_MANY_ARGUMENTS_TO_INVOKE );
            return( ERROR );
        }
    } else {
        int j = (Token_Count - i) / 2;
        /* for VARARG procs, just push the additional params with
         the VARARG descriptor
        */
        numParam--;
        size_vararg = 0; /* reset the VARARG parameter size count */
        while ( curr && curr->sym.is_vararg == FALSE ) curr = curr->nextparam;
        DebugMsg1(("InvokeDir: VARARG proc, numparams=%u, actual (max) params=%u, parasize=%u\n", numParam, j, info->parasize));
        for ( ; j >= numParam; j-- )
            PushInvokeParam( i, tokenarray, proc, curr, j, &r0flags );
        /* move to first non-vararg parameter, if any */
        for ( curr = info->paralist; curr && curr->sym.is_vararg == TRUE; curr = curr->nextparam );
    }

    /* the parameters are usually stored in "push" order.
     * This if() must match the one in proc.c, ParseParams().
     */

    if ( sym->langtype == LANG_STDCALL ||
        sym->langtype == LANG_C ||
        ( sym->langtype == LANG_FASTCALL && porder ) ||
        sym->langtype == LANG_SYSCALL ) {
        for ( ; curr ; curr = curr->nextparam ) {
            numParam--;
            if ( PushInvokeParam( i, tokenarray, proc, curr, numParam, &r0flags ) == ERROR ) {
                DebugMsg(("InvokeDir: PushInvokeParam(curr=%u, i=%u, numParam=%u) failed\n", curr, i, numParam));
                EmitErr( TOO_FEW_ARGUMENTS_TO_INVOKE, sym->name );
            }
        }
    } else {
        for ( numParam = 0 ; curr && curr->sym.is_vararg == FALSE; curr = curr->nextparam, numParam++ ) {
            if ( PushInvokeParam( i, tokenarray, proc, curr, numParam, &r0flags ) == ERROR ) {
                DebugMsg(("InvokeDir: PushInvokeParam(curr=%u, i=%u, numParam=%u) failed\n", curr, i, numParam));
                EmitErr( TOO_FEW_ARGUMENTS_TO_INVOKE, sym->name );
            }
        }
    }
#if 1
    /* v2.05 added. A warning only, because Masm accepts this. */
    if ( opndx.base_reg != NULL &&
        Parse_Pass == PASS_1 &&
        (r0flags & R0_USED ) &&
        opndx.base_reg->bytval == 0 )
        EmitWarn( 2, REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
#endif
    p = StringBufferEnd;
    strcpy( p, " call " );
    p += 6;
    if ( uselabel ) {
        DebugMsg1(("InvokeDir: opnd.label_tok is used: %s\n", opndx.label_tok->string_ptr ));
        strcpy( p, opndx.label_tok->string_ptr );
    } else {
#if DLLIMPORT
        if ( sym->state == SYM_EXTERNAL && sym->dllname ) {
            char *iatname = p;
            strcpy( p, "_imp_" );
            p += 5;
            p += Mangle( sym, p );
            sym->iat_used = TRUE;
            //AddLineQueueX( " externdef %s: %r", iatname, ModuleInfo.Ofssize == USE64 ? T_QWORD : T_DWORD );
            if ( sym->langtype != LANG_NONE && sym->langtype != ModuleInfo.langtype )
                AddLineQueueX( " externdef %r %s: %r %r", sym->langtype + T_C - 1, iatname, T_PTR, T_PROC );
            else
                AddLineQueueX( " externdef %s: %r %r", iatname, T_PTR, T_PROC );
            namepos++;
        }
#endif
        size = tokenarray[parmpos].tokpos - tokenarray[namepos].tokpos;
        memcpy( p, tokenarray[namepos].tokpos, size );
        *(p+size) = NULLC;
    }
    AddLineQueue( StringBufferEnd );

    if (( sym->langtype == LANG_C || sym->langtype == LANG_SYSCALL ) &&
        ( info->parasize || ( info->is_vararg && size_vararg ) )) {
        if ( info->is_vararg ) {
            DebugMsg1(("InvokeDir: size of fix args=%u, var args=%u\n", info->parasize, size_vararg));
            AddLineQueueX( " add %r, %u", stackreg[ModuleInfo.Ofssize], NUMQUAL info->parasize + size_vararg );
        } else
            AddLineQueueX( " add %r, %u", stackreg[ModuleInfo.Ofssize], NUMQUAL info->parasize );
    } else if ( sym->langtype == LANG_FASTCALL ) {
        fastcall_tab[ModuleInfo.fctype].invokeend( proc, numParam, value );
    }

    LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );

    RunLineQueue();

    return( NOT_ERROR );
}

