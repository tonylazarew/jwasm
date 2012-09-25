/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  Processing of PROC/ENDP/LOCAL directives.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "segment.h"
#include "extern.h"
#include "equate.h"
#include "fixup.h"
#include "labels.h"
#include "input.h"
#include "tokenize.h"
#include "expreval.h"
#include "types.h"
#include "condasm.h"
#include "macro.h"
#include "proc.h"
#include "fastpass.h"
#include "listing.h"
#include "posndir.h"
#include "myassert.h"
#include "reswords.h"
#if AMD64_SUPPORT
#include "win64seh.h"
#endif

#ifdef __I86__
#define NUMQUAL (long)
#else
#define NUMQUAL
#endif

extern const char szDgroup[];

/*
 * Masm allows nested procedures
 * but they must NOT have params or locals
 */

/*
 * calling convention FASTCALL supports:
 * - Watcom C: registers e/ax,e/dx,e/bx,e/cx
 * - MS fastcall 16-bit: registers ax,dx,bx (default for 16bit)
 * - MS fastcall 32-bit: registers ecx,edx (default for 32bit)
 * - Win64: registers rcx, rdx, r8, r9 (default for 64bit)
 */

struct dsym             *CurrProc;      /* current procedure */
int                     procidx;        /* procedure index */

static struct proc_info *ProcStack;

bool                    DefineProc;     /* TRUE if definition of procedure
                                         * hasn't ended yet */
#if AMD64_SUPPORT
static bool             endprolog_found;
static uint_8           unw_segs_defined;
static UNWIND_INFO      unw_info = {UNW_VERSION, 0, 0, 0, 0, 0 };
static UNWIND_CODE      unw_code[128];
#endif

#if AMD64_SUPPORT
/* fields: next, name, segment, offset/value */
struct asym ReservedStack = { NULL,"@ReservedStack", 0 };   /* max stack space required by INVOKE */
#endif

/* tables for FASTCALL support */

/* v2.07: 16-bit MS FASTCALL registers are AX, DX, BX.
 * And params on stack are in PASCAL order.
 */
//static const enum special_token ms32_regs16[] = { T_CX, T_DX };
static const enum special_token ms32_regs16[] = { T_AX, T_DX, T_BX };
static const enum special_token ms32_regs32[] = { T_ECX,T_EDX };
/* v2.07: added */
static const int ms32_maxreg[] = {
    sizeof( ms32_regs16) / sizeof(ms32_regs16[0] ),
    sizeof( ms32_regs32) / sizeof(ms32_regs32[0] ),
};
#if OWFC_SUPPORT
static const enum special_token watc_regs8[] = {T_AL, T_DL, T_BL, T_CL };
static const enum special_token watc_regs16[] = {T_AX, T_DX, T_BX, T_CX };
static const enum special_token watc_regs32[] = {T_EAX, T_EDX, T_EBX, T_ECX };
static const enum special_token watc_regs_qw[] = {T_AX, T_BX, T_CX, T_DX };
#endif
#if AMD64_SUPPORT
static const enum special_token ms64_regs[] = {T_RCX, T_RDX, T_R8, T_R9 };
/* win64 non-volatile GPRs:
 * T_RBX, T_RBP, T_RSI, T_RDI, T_R12, T_R13, T_R14, T_R15
 */
static const uint_16 win64_nvgpr = 0xF0E8;
/* win64 non-volatile XMM regs: XMM6-XMM15 */
static const uint_16 win64_nvxmm = 0xFFC0;
#endif

struct fastcall_conv {
    int (* paramcheck)( struct dsym *, struct dsym *, int * );
    void (* handlereturn)( struct dsym *, char *buffer );
};

static  int ms32_pcheck( struct dsym *, struct dsym *, int * );
static void ms32_return( struct dsym *, char * );
#if OWFC_SUPPORT
static  int watc_pcheck( struct dsym *, struct dsym *, int * );
static void watc_return( struct dsym *, char * );
#endif
#if AMD64_SUPPORT
static  int ms64_pcheck( struct dsym *, struct dsym *, int * );
static void ms64_return( struct dsym *, char * );
#endif

/* table of fastcall types.
 * must match order of enum fastcall_type!
 * also see table in mangle.c!
 */

static const struct fastcall_conv fastcall_tab[] = {
    { ms32_pcheck, ms32_return },  /* FCT_MSC */
#if OWFC_SUPPORT
    { watc_pcheck, watc_return },  /* FCT_WATCOMC */
#endif
#if AMD64_SUPPORT
    { ms64_pcheck, ms64_return }   /* FCT_WIN64 */
#endif
};

static const enum special_token basereg[] = { T_BP, T_EBP,
#if AMD64_SUPPORT
T_RBP
#endif
};

static const enum special_token stackreg[] = { T_SP, T_ESP,
#if AMD64_SUPPORT
T_RSP
#endif
};

#define ROUND_UP( i, r ) (((i)+((r)-1)) & ~((r)-1))

#if OWFC_SUPPORT
/* register usage for OW fastcall (register calling convention).
 * registers are used for parameter size 1,2,4,8.
 * if a parameter doesn't fit in a register, a register pair is used.
 * however, valid register pairs are e/dx:e/ax and e/cx:e/bx only!
 * if a parameter doesn't fit in a register pair, registers
 * are used ax:bx:cx:dx!!!
 * stack cleanup for OW fastcall: if the proc is VARARG, the caller
 * will do the cleanup, else the called proc does it.
 * in VARARG procs, all parameters are pushed onto the stack!
 */

static int watc_pcheck( struct dsym *proc, struct dsym *paranode, int *used )
/***************************************************************************/
{
    static char regname[64];
    static char regist[32];
    int newflg;
    int shift;
    int firstreg;
    uint_8 Ofssize = GetSymOfssize( &proc->sym );
    int size = SizeFromMemtype( paranode->sym.mem_type, paranode->sym.Ofssize, paranode->sym.type );

    /* v2.05: VARARG procs don't have register params */
    if ( proc->e.procinfo->is_vararg )
        return( 0 );

    if ( size != 1 && size != 2 && size != 4 && size != 8 )
        return( 0 );

    /* v2.05: rewritten. The old code didn't allow to "fill holes" */
    if ( size == 8 ) {
        newflg = Ofssize ? 3 : 15;
        shift = Ofssize ? 2 : 4;
    } else if ( size == 4 && Ofssize == USE16 ) {
        newflg = 3;
        shift = 2;
    } else {
        newflg = 1;
        shift = 1;
    }

    /* scan if there's a free register (pair/quadrupel) */
    for ( firstreg = 0; firstreg < 4 && (newflg & *used ); newflg <<= shift, firstreg += shift );
    if ( firstreg >= 4 ) /* exit if nothing is free */
        return( 0 );

    paranode->sym.state = SYM_TMACRO;
    switch ( size ) {
    case 1:
        GetResWName( watc_regs8[firstreg], regname );
        break;
    case 2:
        GetResWName( watc_regs16[firstreg], regname );
        break;
    case 4:
        if ( Ofssize ) {
            GetResWName( watc_regs32[firstreg], regname );
        } else {
            sprintf( regname, "%s::%s",
                    GetResWName( watc_regs16[firstreg+1], regist ),
                    GetResWName( watc_regs16[firstreg], NULL ) );
        }
        break;
    case 8:
        if ( Ofssize ) {
            sprintf( regname, "%s::%s",
                    GetResWName( watc_regs32[firstreg+1], regist ),
                    GetResWName( watc_regs32[firstreg], NULL ) );
        } else {
            /* the AX:BX:CX:DX sequence is for 16-bit only */
            for( firstreg = 0, regname[0] = NULLC; firstreg < 4; firstreg++ ) {
                GetResWName( watc_regs_qw[firstreg], regname + strlen( regname ) );
                if ( firstreg != 3 )
                    strcat( regname, "::");
            }
        }
    }
    *used |= newflg;
    paranode->sym.string_ptr = LclAlloc( strlen( regname ) + 1 );
    strcpy( paranode->sym.string_ptr, regname );
    DebugMsg(("watc_pcheck(%s.%s): size=%u ptr=%u far=%u reg=%s\n", proc->sym.name, paranode->sym.name, size, paranode->sym.is_ptr, paranode->sym.isfar, regname ));
    return( 1 );
}

static void watc_return( struct dsym *proc, char *buffer )
/********************************************************/
{
    int value;
    value = 4 * CurrWordSize;
    if( proc->e.procinfo->is_vararg == FALSE && proc->e.procinfo->parasize > value )
        sprintf( buffer + strlen( buffer ), "%d%c", proc->e.procinfo->parasize - value, ModuleInfo.radix != 10 ? 't' : NULLC );
    return;
}
#endif

/* the MS Win32 fastcall ABI is simple: register ecx and edx are used,
 * if the parameter's value fits into the register.
 * there is no space reserved on the stack for a register backup.
 * The 16-bit ABI uses registers AX, DX and BX - additional registers
 * are pushed in PASCAL order (i.o.w.: left to right).
 */

static int ms32_pcheck( struct dsym *proc, struct dsym *paranode, int *used )
/***************************************************************************/
{
    char regname[32];
    int size = SizeFromMemtype( paranode->sym.mem_type, paranode->sym.Ofssize, paranode->sym.type );

    /* v2.07: 16-bit has 3 register params (AX,DX,BX) */
    //if ( size > CurrWordSize || *used >= 2 )
    if ( size > CurrWordSize || *used >= ms32_maxreg[ModuleInfo.Ofssize] )
        return( 0 );
    paranode->sym.state = SYM_TMACRO;
    GetResWName( ModuleInfo.Ofssize ? ms32_regs32[*used] : ms32_regs16[*used], regname );
    paranode->sym.string_ptr = LclAlloc( strlen( regname ) + 1 );
    strcpy( paranode->sym.string_ptr, regname );
    (*used)++;
    return( 1 );
}

static void ms32_return( struct dsym *proc, char *buffer )
/********************************************************/
{
    /* v2.07: changed */
    //if( proc->e.procinfo->parasize > ( 2 * CurrWordSize ) )
    //    sprintf( buffer + strlen( buffer ), "%d%c", proc->e.procinfo->parasize - (2 * CurrWordSize), ModuleInfo.radix != 10 ? 't' : NULLC );
    if( proc->e.procinfo->parasize > ( ms32_maxreg[ModuleInfo.Ofssize] * CurrWordSize ) )
        sprintf( buffer + strlen( buffer ), "%d%c", proc->e.procinfo->parasize - ( ms32_maxreg[ModuleInfo.Ofssize] * CurrWordSize), ModuleInfo.radix != 10 ? 't' : NULLC );
    return;
}

#if AMD64_SUPPORT

/* the MS Win64 fastcall ABI is strict: the first four parameters are
 * passed in registers. If a parameter's value doesn't fit in a register,
 * it's address is used instead. parameter 1 is stored in rcx/xmm0,
 * then comes rdx/xmm1, r8/xmm2, r9/xmm3. The xmm regs are used if the
 * param is a float/double (but not long double!).
 * Additionally, there's space for the registers reserved by the caller on,
 * the stack. On a function's entry it's located at [esp+8] for param 1, 
 * [esp+16] for param 2,... The parameter names refer to those stack
 * locations, not to the register names.
 */

static int ms64_pcheck( struct dsym *proc, struct dsym *paranode, int *used )
/***************************************************************************/
{
    /* since the parameter names refer the stack-backup locations,
     * there's nothing to do here!
     * That is, if a parameter's size is > 8, it has to be changed
     * to a pointer. This is to be done yet.
     */
    return( 0 );
}

static void ms64_return( struct dsym *proc, char *buffer )
/********************************************************/
{
    /* nothing to do, the caller cleans the stack */
    return;
}
#endif

static void pushitem( void *stk, void *elmt )
/*******************************************/
{
    void      **stack = stk;
    struct qnode *node;

    node = LclAlloc( sizeof( struct qnode ));
    node->next = *stack;
    node->elmt = elmt;
    *stack = node;
}

static void *popitem( void *stk )
/*******************************/
{
    void        **stack = stk;
    struct qnode *node;
    void        *elmt;

    node = (struct qnode *)(*stack);
    *stack = node->next;
    elmt = (void *)node->elmt;
    LclFree( node );
    return( elmt );
}

#if 0
void *peekitem( void *stk, int level )
/************************************/
{
    struct qnode  *node = (struct qnode *)stk;

    for ( ; node && level; level-- ) {
        node = node->next;
    }

    if ( node )
        return( node->elt );
    else
        return( NULL );
}
#endif

static void push_proc( struct dsym *proc )
/****************************************/
{
    if ( Parse_Pass == PASS_1 ) /* get the locals stored so far */
        SymGetLocal( (struct asym *)proc );
    pushitem( &ProcStack, proc );
    return;
}

static struct dsym *pop_proc( void )
/**********************************/
{
    if( ProcStack == NULL )
        return( NULL );
    return( (struct dsym *)popitem( &ProcStack ) );
}

/* LOCAL directive. Called on Pass 1 only */

ret_code LocalDir( int i, struct asm_tok tokenarray[] )
/*****************************************************/
{
    char        *name;
    struct dsym *local;
    struct dsym *curr;
    struct proc_info *info;
    //int         size;
    //int         idx;
#if AMD64_SUPPORT
    int         displ;
    int         cnt;
    int         sizestd;
    int         sizexmm;
#endif
    struct qualified_type ti;
    int         align = CurrWordSize;

/*

    LOCAL symbol[,symbol]...
    symbol:name [[count]] [:[type]]
    count: number of array elements, default is 1
    type:  Simple Type, structured type, ptr to simple/structured type

 */
    if ( Parse_Pass != PASS_1 )
        return( NOT_ERROR );

    DebugMsg1(("LocalDir(%u) entry\n", i));

    if( DefineProc == FALSE || CurrProc == NULL ) {
        EmitError( PROC_MACRO_MUST_PRECEDE_LOCAL );
        return( ERROR );
    }

    info = CurrProc->e.procinfo;

    i++; /* go past LOCAL */
#if AMD64_SUPPORT
    if ( info->isframe ) {
        uint_16 *regs = info->regslist;
        sizexmm = 0;
        sizestd = 0;
        /* adjust start displacement for Win64 FRAME procs.
         * v2.06: the list may contain xmm registers, which have size 16!
         */
        if ( regs )
            for( cnt = *regs++; cnt; cnt--, regs++ )
                if ( GetValueSp( *regs ) & OP_XMM )
                    sizexmm += 16;
                else
                    sizestd += 8;
        displ = sizexmm + sizestd;
        /* v2.07: ( fix by habran )
         * see below why this is to be done only when sizexmm is != 0
         */
        if ( sizexmm && (sizestd & 0xf) )
            displ += 8;
    }
#endif

    do  {
        if( tokenarray[i].token != T_ID ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
        name = tokenarray[i].string_ptr;

        DebugMsg1(("LocalDir(%s)\n", name ));

        ti.symtype = NULL;
        ti.is_ptr = 0;
        ti.ptr_memtype = MT_EMPTY;
        if ( SIZE_DATAPTR & ( 1 << ModuleInfo.model ) )
            ti.is_far = TRUE;
        else
            ti.is_far = FALSE;
        ti.Ofssize = ModuleInfo.Ofssize;

#if 0
        /* since v1.95 a local hash table is used. No need to search the
         * symbol before SymLCreate() is called. SymLCreate() will display
         * an error if the symbol is already defined.
         */
        if ((local = (struct dsym *)SymSearch( name )) && local->sym.state != SYM_UNDEFINED ) {
            EmitErr( SYMBOL_ALREADY_DEFINED, name );
            return( ERROR );
        }
#endif
        local = (struct dsym *)SymLCreate( name );
        if( !local ) { /* if it failed, an error msg has been written already */
            DebugMsg(("LocalDir: SymLCreate( %s ) failed\n", name ));
            return( ERROR );
        }

        local->sym.state = SYM_STACK;
        local->sym.isdefined = TRUE;
        local->sym.total_length = 1; /* v2.04: added */
        switch ( ti.Ofssize ) {
        case USE16:
            local->sym.mem_type = MT_WORD;
            break;
#if AMD64_SUPPORT
            /* v2.08: default type for locals in 64-bit is still DWORD (at least in Win64) */
            //case USE64: local->sym.mem_type = MT_QWORD; break;
#endif
        default: 
            local->sym.mem_type = MT_DWORD; break;
        }
        /* v2.08: default size for 64-bit is 4! */
        //ti.size = align;
        ti.size = ( ( ti.Ofssize == USE16 ) ? sizeof(uint_16) : sizeof(uint_32) );

        i++; /* go past name */

        /* get an optional index factor: local name[xx]:... */
        if( tokenarray[i].token == T_OP_SQ_BRACKET ) {
            int j;
            struct expr opndx;
            i++; /* go past '[' */
            /* scan for comma or colon. this isn't really necessary,
             * but will prevent the expression evaluator from emitting
             * confusing error messages.
             */
            for ( j = i; j < Token_Count; j++ )
                if ( tokenarray[j].token == T_COMMA ||
                    tokenarray[j].token == T_COLON)
                    break;
            if ( ERROR == EvalOperand( &i, tokenarray, j, &opndx, 0 ) )
                return( ERROR );
            if ( opndx.kind != EXPR_CONST ) {
                EmitError( CONSTANT_EXPECTED );
                opndx.value = 1;
            }
            // local->factor = tokenarray[i++].value;
            /* zero is allowed as value! */
            local->sym.total_length = opndx.value;
            local->sym.isarray = TRUE;
            if( tokenarray[i].token == T_CL_SQ_BRACKET ) {
                i++; /* go past ']' */
            } else {
                EmitError( EXPECTED_CL_SQ_BRACKET );
            }
        }

        /* get the optional type: local name[xx]:type  */
        if( tokenarray[i].token == T_COLON ) {
            DebugMsg1(("LocalDir(%s): i=%u, token=%X\n", name, i, tokenarray[i].token ));
            i++;

            if ( GetQualifiedType( &i, tokenarray, &ti ) == ERROR )
                return( ERROR );

            local->sym.mem_type = ti.mem_type;
            if ( ti.mem_type == MT_TYPE ) {
                local->sym.type = ti.symtype;
            } else {
                local->sym.target_type = ti.symtype;
            }
            DebugMsg1(("LocalDir: memtype=%X, type=%s, size=%u (curr localsize=%X)\n",
                       local->sym.mem_type,
                       ti.symtype ? ti.symtype->name : "NULL",
                       ti.size, info->localsize ));
        }
        local->sym.is_ptr  = ti.is_ptr;
        local->sym.isfar   = ti.is_far;
        local->sym.Ofssize = ti.Ofssize;
        local->sym.ptr_memtype = ti.ptr_memtype;
        local->sym.total_size = ti.size * local->sym.total_length;

#if AMD64_SUPPORT
        /* v2.07: add the alignment here! */
        if ( info->isframe && ( info->localsize == 0 ) ) {
            if ( sizexmm == 0 && ( displ & 0xf ) )
                info->localsize = 8 - ( local->sym.total_size & 0x7 );
        }
#endif
        info->localsize += local->sym.total_size;

        if ( ti.size > align )
            info->localsize = ROUND_UP( info->localsize, align );
        else if ( ti.size ) /* v2.04: skip if size == 0 */
            info->localsize = ROUND_UP( info->localsize, ti.size );
        DebugMsg1(("LocalDir(%s): aligned local total=%X\n", name, info->localsize));

#if AMD64_SUPPORT
        if ( info->isframe )
            local->sym.offset = - ( info->localsize + displ );
        else
#endif
        local->sym.offset = - info->localsize;
        DebugMsg1(("LocalDir(%s): symbol offset=%d\n", name, local->sym.offset));

        if( info->locallist == NULL ) {
            info->locallist = local;
        } else {
            for( curr = info->locallist; curr->nextlocal ; curr = curr->nextlocal );
            curr->nextlocal = local;
        }

        if ( tokenarray[i].token != T_FINAL )
            if ( tokenarray[i].token == T_COMMA ) {
                if ( (i + 1) < Token_Count )
                    i++;
            } else {
                EmitError( EXPECTING_COMMA );
                return( ERROR );
            }

    } while ( i < Token_Count );

    return( NOT_ERROR );
}

/* parse parameters of a PROC/PROTO
 * i=token buffer index
 */

static ret_code ParseParams( int i, struct asm_tok tokenarray[], struct dsym *proc, bool IsPROC )
/***********************************************************************************************/
{
    char            *name;
    struct asym     *sym;
    int             cntParam;
    int             offset;
    //int             size;
    int             fcint = 0;
    struct qualified_type ti;
    bool            is_vararg;
    struct dsym     *paranode;
    struct dsym     *paracurr;

    /* parse PROC parms */
    /* it's important to remember that params are stored in "push" order! */

    if (proc->sym.langtype == LANG_C ||
        proc->sym.langtype == LANG_SYSCALL ||
#if AMD64_SUPPORT
        ( proc->sym.langtype == LANG_FASTCALL && ModuleInfo.Ofssize != USE64 ) ||
#else
        proc->sym.langtype == LANG_FASTCALL ||
#endif
        proc->sym.langtype == LANG_STDCALL)
        for (paracurr = proc->e.procinfo->paralist; paracurr && paracurr->nextparam; paracurr = paracurr->nextparam );
    else
        paracurr = proc->e.procinfo->paralist;

    for( cntParam = 0 ; tokenarray[i].token != T_FINAL ; cntParam++ ) {

        if ( tokenarray[i].token == T_ID ) {
            name = tokenarray[i++].string_ptr;
        } else if ( IsPROC == FALSE && tokenarray[i].token == T_COLON ) {
            if ( paracurr )
                name = paracurr->sym.name;
            else
                name = "";
        } else {
            /* PROC needs a parameter name, PROTO accepts <void> also */
            DebugMsg(("ParseParams: name missing/invalid for parameter %u, i=%u\n", cntParam+1, i));
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }

        ti.symtype = NULL;
        ti.is_ptr = 0;
        ti.ptr_memtype = MT_EMPTY;
        /* v2.02: init is_far depending on memory model */
        //ti.is_far = FALSE;
        if ( SIZE_DATAPTR & ( 1 << ModuleInfo.model ) )
            ti.is_far = TRUE;
        else
            ti.is_far = FALSE;
        ti.Ofssize = ModuleInfo.Ofssize;
        ti.size = CurrWordSize;

        is_vararg = FALSE;

        /* read colon. It's optional for PROC.
         * Masm also allows a missing colon for PROTO - if there's
         * just one parameter. Probably a Masm bug.
         * JWasm always require a colon for PROTO.
         */
        if( tokenarray[i].token != T_COLON ) {
            if ( IsPROC == FALSE ) {
                EmitError( COLON_EXPECTED );
                return( ERROR );
            }
            switch ( ti.Ofssize ) {
            case USE16:
                ti.mem_type = MT_WORD; break;
#if AMD64_SUPPORT
                /* v2.08: default size for arguments is DWORD in 64-bit ( Win64 ) */
                //case USE64: ti.mem_type = MT_QWORD; break;
#endif
            default:
                ti.mem_type = MT_DWORD; break;
            }
        } else {
            i++;
            if (( tokenarray[i].token == T_RES_ID ) && ( tokenarray[i].tokval == T_VARARG )) {
                switch( proc->sym.langtype ) {
                case LANG_NONE:
                case LANG_BASIC:
                case LANG_FORTRAN:
                case LANG_PASCAL:
                case LANG_STDCALL:
                    EmitError( VARARG_REQUIRES_C_CALLING_CONVENTION );
                    return( ERROR );
                }
                /* v2.05: added check */
                if ( tokenarray[i+1].token != T_FINAL )
                    EmitError( VARARG_PARAMETER_MUST_BE_LAST );
                else
                    is_vararg = TRUE;
                ti.mem_type = MT_EMPTY;
                ti.size = 0;
                i++;
            } else {
                if ( GetQualifiedType( &i, tokenarray, &ti ) == ERROR )
                    return( ERROR );
            }
        }

        /* check if parameter name is defined already */
        if (( IsPROC ) && ( sym = SymSearch( name ) ) && sym->state != SYM_UNDEFINED ) {
            DebugMsg(("ParseParams: %s defined already, state=%u, local=%u\n", sym->name, sym->state, sym->scoped ));
            EmitErr( SYMBOL_REDEFINITION, name );
            return( ERROR );
        }

        /* redefinition? */
        if ( paracurr ) {
#if 0 /* was active till v2.04 */
            int newsize = ti.size;
            int oldsize;

            /* check size only (so UINT <-> DWORD wont cause an error) */
            if ( paracurr->sym.type )
                oldsize = paracurr->sym.total_size;
            else if ( paracurr->sym.mem_type == MT_EMPTY )
                oldsize = 0;
            else if ( paracurr->sym.mem_type == MT_PTR )
                oldsize = SizeFromMemtype( paracurr->sym.isfar ? MT_FAR : MT_NEAR, paracurr->sym.Ofssize, NULL );
            else
                oldsize = SizeFromMemtype( paracurr->sym.mem_type, paracurr->sym.Ofssize, paracurr->sym.type );
            if ( oldsize != newsize ) {
                DebugMsg(("ParseParams: old memtype=%u, new memtype=%u\n", paracurr->sym.mem_type, ti.mem_type));
                EmitErr( CONFLICTING_PARAMETER_DEFINITION, name );
                //return( ERROR );
            }
            /* the parameter type used in PROC has highest priority! */
            if ( IsPROC ) {
                if ( ti.symtype ) {
                    paracurr->sym.type = ti.symtype;
                    paracurr->sym.mem_type = MT_TYPE;
                } else
                    paracurr->sym.mem_type = ti.mem_type;
            }
#else
            struct asym *to;
            struct asym *tn;
            char oo;
            char on;
            for( tn = ti.symtype; tn && tn->type; tn = tn->type );
            to = ( paracurr->sym.mem_type == MT_TYPE ) ? paracurr->sym.type : paracurr->sym.target_type;
            for( ; to && to->type; to = to->type );
            oo = ( paracurr->sym.Ofssize != USE_EMPTY ) ? paracurr->sym.Ofssize : ModuleInfo.Ofssize;
            on = ( ti.Ofssize != USE_EMPTY ) ? ti.Ofssize : ModuleInfo.Ofssize;
            if ( ti.mem_type != paracurr->sym.mem_type ||
                ( ti.mem_type == MT_TYPE && tn != to ) ||
                ( ti.mem_type == MT_PTR &&
                 ( ti.is_far != paracurr->sym.isfar ||
                  on != oo ||
                  ti.ptr_memtype != paracurr->sym.ptr_memtype ||
                  tn != to ))) {
                DebugMsg(("ParseParams: old-new memtype=%X-%X type=%X(%s)-%X(%s) far=%u-%u ind=%u-%u ofss=%d-%d pmt=%X-%X\n",
                          paracurr->sym.mem_type, ti.mem_type, 
                          (paracurr->sym.mem_type == MT_TYPE) ? paracurr->sym.type : paracurr->sym.target_type,
                          (paracurr->sym.mem_type == MT_TYPE) ? paracurr->sym.type->name : paracurr->sym.target_type ? paracurr->sym.target_type->name : "",
                          ti.symtype, ti.symtype ? ti.symtype->name : "",
                          paracurr->sym.isfar, ti.is_far,
                          paracurr->sym.is_ptr, ti.is_ptr,
                          paracurr->sym.Ofssize, ti.Ofssize,
                          paracurr->sym.ptr_memtype, ti.ptr_memtype ));
                EmitErr( CONFLICTING_PARAMETER_DEFINITION, name );
                //return( ERROR );
            }
#endif
            if ( IsPROC ) {
                DebugMsg(("ParseParams: calling SymAddLocal(%s, %s)\n", paracurr->sym.name, name ));
                /* it has been checked already that the name isn't found - SymAddLocal() shouldn't fail */
                SymAddLocal( &paracurr->sym, name );
            }
            /* set paracurr to next parameter */
            if ( proc->sym.langtype == LANG_C ||
                proc->sym.langtype == LANG_SYSCALL ||
#if AMD64_SUPPORT
                ( proc->sym.langtype == LANG_FASTCALL && ti.Ofssize != USE64 ) ||
#else
                proc->sym.langtype == LANG_FASTCALL ||
#endif
                proc->sym.langtype == LANG_STDCALL) {
                struct dsym *l;
                for (l = proc->e.procinfo->paralist;
                     l && ( l->nextparam != paracurr );
                     l = l->nextparam );
                paracurr = l;
            } else
                paracurr = paracurr->nextparam;

        } else if ( proc->e.procinfo->init == TRUE ) {
            /* second definition has more parameters than first */
            DebugMsg(("ParseParams: different param count\n"));
            EmitErr( CONFLICTING_PARAMETER_DEFINITION, "" );
            return( ERROR );
        } else {
            if ( IsPROC ) {
                paranode = (struct dsym *)SymLCreate( name );
            } else
                paranode = (struct dsym *)SymAlloc( "" );/* for PROTO, no param name needed */

            if( paranode == NULL ) { /* error msg has been displayed already */
                DebugMsg(("ParseParams: SymLCreate(%s) failed\n", name ));
                return( ERROR );
            }
            paranode->sym.isdefined = TRUE;
            paranode->sym.mem_type = ti.mem_type;
            if ( ti.mem_type == MT_TYPE ) {
                paranode->sym.type = ti.symtype;
            } else {
                paranode->sym.target_type = ti.symtype;
            }

            /* v2.05: moved BEFORE fastcall_tab() */
            paranode->sym.isfar   = ti.is_far;
            paranode->sym.Ofssize = ti.Ofssize;
            paranode->sym.is_ptr  = ti.is_ptr;
            paranode->sym.ptr_memtype = ti.ptr_memtype;
            paranode->sym.is_vararg = is_vararg;
            if ( proc->sym.langtype == LANG_FASTCALL &&
                fastcall_tab[ModuleInfo.fctype].paramcheck( proc, paranode, &fcint ) ) {
            } else {
                paranode->sym.state = SYM_STACK;
            }

            paranode->sym.total_length = 1; /* v2.04: added */
            paranode->sym.total_size = ti.size;

            if( paranode->sym.is_vararg == FALSE )
                proc->e.procinfo->parasize += ROUND_UP( ti.size, CurrWordSize );

            /* v2.05: the PROC's vararg flag has been set already */
            //proc->e.procinfo->is_vararg |= paranode->sym.is_vararg;

            /* Parameters usually are stored in "push" order.
             * However, for Win64, it's better to store them
             * the "natural" way from left to right, since the
             * arguments aren't "pushed".
             */

            switch( proc->sym.langtype ) {
            case LANG_BASIC:
            case LANG_FORTRAN:
            case LANG_PASCAL:
            left_to_right:
                paranode->nextparam = NULL;
                if( proc->e.procinfo->paralist == NULL ) {
                    proc->e.procinfo->paralist = paranode;
                } else {
                    for( paracurr = proc->e.procinfo->paralist;; paracurr = paracurr->nextparam ) {
                        if( paracurr->nextparam == NULL ) {
                            break;
                        }
                    }
                    paracurr->nextparam = paranode;
                    paracurr = NULL;
                }
                break;
#if AMD64_SUPPORT
            case LANG_FASTCALL:
                if ( ti.Ofssize == USE64 )
                    goto left_to_right;
#endif
                /* v2.07: MS fastcall 16-bit is PASCAL! */
                if ( ti.Ofssize == USE16 && ModuleInfo.fctype == FCT_MSC )
                    goto left_to_right;
            default:
                paranode->nextparam = proc->e.procinfo->paralist;
                proc->e.procinfo->paralist = paranode;
                break;
            }
        }
        if ( tokenarray[i].token != T_FINAL ) {
            if( tokenarray[i].token != T_COMMA ) {
                DebugMsg(("ParseParams: error, cntParam=%u, found %s\n", cntParam, tokenarray[i].tokpos ));
                EmitError( EXPECTING_COMMA );
                return( ERROR );
            }
            i++;    /* go past comma */
        }
    } /* end for */

    if ( proc->e.procinfo->init == TRUE ) {
        if ( paracurr ) {
            /* first definition has more parameters than second */
            DebugMsg(("ParseParams: a param is left over, cntParam=%u\n", cntParam));
            EmitErr( CONFLICTING_PARAMETER_DEFINITION, "" );
            return( ERROR );
        }
    } else {
        int curr;

        /* calc starting offset for parameters,
         * offset from [E]BP : return addr + old [E]BP
         * NEAR: 2 * wordsize, FAR: 3 * wordsize
         *  +4 ; USE16 + NEAR
         *  +8 : USE32 + NEAR
         * +16 : USE64 + NEAR
         *  +6 : USE16 + FAR
         * +12 : USE32 + FAR
         * +24 : USE64 + FAR
         */
        //if( proc->e.procinfo->mem_type == MT_NEAR ) {
        if( proc->sym.mem_type == MT_NEAR ) {
            offset = 4 << ModuleInfo.Ofssize;
        } else {
            offset = 6 << ModuleInfo.Ofssize;
        }

        /* now calculate the (E)BP offsets */

#if AMD64_SUPPORT
        if ( ModuleInfo.Ofssize == USE64 && proc->sym.langtype == LANG_FASTCALL ) {
            for ( paranode = proc->e.procinfo->paralist; paranode ;paranode = paranode->nextparam )
                if ( paranode->sym.state == SYM_TMACRO ) /* register param */
                    ;
                else {
                    paranode->sym.offset = offset;
                    proc->e.procinfo->stackparam = TRUE;
                    offset += ROUND_UP( paranode->sym.total_size, CurrWordSize );
                }
        } else
#endif
        for ( ; cntParam; cntParam-- ) {
            for ( curr = 1, paranode = proc->e.procinfo->paralist; curr < cntParam;paranode = paranode->nextparam, curr++ );
            DebugMsg1(("ParseParams: parm=%s, ofs=%u, size=%d\n", paranode->sym.name, offset, paranode->sym.total_size));
            if ( paranode->sym.state == SYM_TMACRO ) /* register param? */
                ;
            else {
                paranode->sym.offset = offset;
                proc->e.procinfo->stackparam = TRUE;
                offset += ROUND_UP( paranode->sym.total_size, CurrWordSize );
            }
        }
    }
    return ( NOT_ERROR );
}

/*
 * create a PROC type
 * i = start position of attributes
 * strategy to set default value for "offset size" (16/32):
 * 1. if current model is FLAT, use 32, else
 * 2. use the current segment's attribute
 * 3. if no segment is set, use cpu setting
 */

ret_code ExamineProc( int i, struct asm_tok tokenarray[], struct dsym *proc, bool IsPROC )
/****************************************************************************************/
{
    char            *token;
    uint_16         *regist;
    //int             type;
    enum lang_type  langtype;
    enum memtype    newmemtype;
    uint_8          newofssize;
#if FASTPASS
    bool            oldpublic = proc->sym.public;
#endif

    /* set some default values */

    proc->sym.isdefined = TRUE;

    if ( IsPROC ) {
        proc->e.procinfo->export = ModuleInfo.procs_export;
        /* don't overwrite a PUBLIC directive for this symbol! */
        if ( ModuleInfo.procs_private == FALSE )
            proc->sym.public = TRUE;
        /* write epilog code */
        if ( Options.masm_compat_gencode ) {
            /* v2.07: Masm uses LEAVE if
             * - current code is 32-bit/64-bit or
             * - cpu is .286 or .586+ */
            //proc->e.procinfo->pe_type = ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_286 );
            proc->e.procinfo->pe_type = ( ModuleInfo.Ofssize > USE16 ||
                                         ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_286 ||
                                         ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_586 ) ? 1 : 0;
        } else {
            /* use LEAVE for 286, 386 (and x64) */
            proc->e.procinfo->pe_type = ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_286 ||
#if AMD64_SUPPORT
                                         ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_64 ||
#endif
                                         ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_386 ) ? 1 : 0;
        }
    }

#if MANGLERSUPP
    /* OW name mangling */
    if( tokenarray[i].token == T_STRING && IsPROC ) {
        /* SetMangler() will ignore LANG_NONE */
        SetMangler( &proc->sym, LANG_NONE, tokenarray[i].string_ptr );
        i++;
    }
#endif

    /* 1. attribute is <distance> */
    if ( tokenarray[i].token == T_STYPE &&
        tokenarray[i].tokval >= T_NEAR && tokenarray[i].tokval <= T_FAR32 ) {
        uint_8 Ofssize = GetSflagsSp( tokenarray[i].tokval );
        /* v2.06: SimpleType is obsolete */
        /* v2.05: FindStdType() is obsolete */
        //type = tokenarray[i].bytval;
        //type = FindStdType(tokenarray[i].value);
        if ( IsPROC ) {
            if ( ( ModuleInfo.Ofssize >= USE32 && Ofssize == USE16 ) ||
                ( ModuleInfo.Ofssize == USE16 && Ofssize == USE32 ) ) {
                EmitError( DISTANCE_INVALID );
            }
        }
        newmemtype = GetMemtypeSp( tokenarray[i].tokval );
        newofssize = (( Ofssize != USE_EMPTY ) ? Ofssize : ModuleInfo.Ofssize );
        i++;
    } else {
        newmemtype = ( ( SIZE_CODEPTR & ( 1 << ModuleInfo.model ) ) ? MT_FAR : MT_NEAR );
        newofssize = ModuleInfo.Ofssize;
    }

    /* did the distance attribute change? */
    if ( proc->sym.mem_type != MT_EMPTY &&
        ( proc->sym.mem_type != newmemtype ||
         GetSymOfssize( &proc->sym ) != newofssize ) ) {
        DebugMsg(("ExamineProc: error, memtype changed, old-new memtype=%X-%X, ofssize=%X-%X\n", proc->sym.mem_type, newmemtype, proc->sym.Ofssize, newofssize));
        if ( proc->sym.mem_type == MT_NEAR || proc->sym.mem_type == MT_FAR )
            EmitError( PROC_AND_PROTO_CALLING_CONV_CONFLICT );
        else {
            EmitErr( SYMBOL_REDEFINITION, proc->sym.name );
            return( ERROR );
        }
    } else {
        proc->sym.mem_type = newmemtype;
        if ( IsPROC == FALSE )
            proc->sym.seg_ofssize = newofssize;
    }

    /* 2. attribute is <langtype> */
    langtype = ModuleInfo.langtype; /* set the default value */
    GetLangType( &i, tokenarray, &langtype ); /* optionally overwrite the value */
    /* has language changed? */
    if ( proc->sym.langtype != LANG_NONE && proc->sym.langtype != langtype ) {
        DebugMsg(("ExamineProc: error, language changed, %u - %u\n", proc->sym.langtype, langtype ));
        EmitError( PROC_AND_PROTO_CALLING_CONV_CONFLICT );
    } else
        proc->sym.langtype = langtype;

    /* 3. attribute is <visibility> */
    /* note that reserved word PUBLIC is a directive! */
    /* PROTO does NOT accept PUBLIC!
     * PROTO accepts PRIVATE, but this attribute is ignored then!
     */

    if ( tokenarray[i].token == T_ID || tokenarray[i].token == T_DIRECTIVE ) {
        token = tokenarray[i].string_ptr;
        if ( _stricmp( token, "PRIVATE") == 0 ) {
            proc->sym.public = FALSE;
#if FASTPASS
            /* error if there was a PUBLIC directive! */
            proc->sym.scoped = TRUE;
            if ( oldpublic ) {
                SkipSavedState(); /* do a full pass-2 scan */
            }
#endif
            proc->e.procinfo->export = FALSE;
            i++;
        } else if ( IsPROC && (_stricmp(token, "PUBLIC") == 0 ) ) {
            proc->sym.public = TRUE;
            proc->e.procinfo->export = FALSE;
            i++;
        } else if ( _stricmp(token, "EXPORT") == 0 ) {
            proc->sym.public = TRUE;
            proc->e.procinfo->export = TRUE;
            i++;
        }
    }

    /* 4. attribute is <prologuearg>, for PROC only.
     it must be enclosed in <> */
    if ( IsPROC && tokenarray[i].token == T_STRING && tokenarray[i].string_delim == '<' ) {
        int idx = Token_Count + 1;
        int max;
        if ( ModuleInfo.prologuemode == PEM_NONE )
            ; /* no prologue at all */
        else if ( ModuleInfo.prologuemode == PEM_MACRO ) {
            proc->e.procinfo->prologuearg = LclAlloc( tokenarray[i].stringlen + 1 );
            strcpy( proc->e.procinfo->prologuearg, tokenarray[i].string_ptr );
        } else {
            /* check the argument. The default prologue
             understands FORCEFRAME and LOADDS only
             */
            max = Tokenize( tokenarray[i].string_ptr, idx, tokenarray, TOK_RESCAN );
            for ( ; idx < max; idx++ ) {
                if ( tokenarray[idx].token == T_ID ) {
                    if ( _stricmp( tokenarray[idx].string_ptr, "FORCEFRAME") == 0 ) {
                        proc->e.procinfo->forceframe = TRUE;
#if AMD64_SUPPORT
                    } else if ( ModuleInfo.Ofssize != USE64 && (_stricmp( tokenarray[idx].string_ptr, "LOADDS") == 0 ) ) {
#else
                    } else if ( _stricmp( tokenarray[idx].string_ptr, "LOADDS") == 0 ) {
#endif
                        if ( ModuleInfo.model == MODEL_FLAT && Parse_Pass == PASS_1 ) {
                            EmitWarn( 2, LOADDS_IGNORED_IN_FLAT_MODEL );
                        } else
                            proc->e.procinfo->loadds = TRUE;
                    } else {
                        EmitErr( UNKNOWN_DEFAULT_PROLOGUE_ARGUMENT, tokenarray[idx].string_ptr );
                        return( ERROR );
                    }
                    if ( tokenarray[idx+1].token == T_COMMA && tokenarray[idx+2].token != T_FINAL)
                        idx++;
                } else {
                    EmitErr( SYNTAX_ERROR_EX, tokenarray[idx].string_ptr );
                    return( ERROR );
                }
            }
        }
        i++;
    }

#if AMD64_SUPPORT
    /* check for optional FRAME[:exc_proc] */
    if ( ModuleInfo.Ofssize == USE64 &&
        IsPROC &&
        tokenarray[i].token == T_RES_ID &&
        tokenarray[i].tokval == T_FRAME ) {
        /* v2.05: don't accept FRAME for ELF */
        if ( Options.output_format != OFORMAT_COFF ) {
            EmitErr( NOT_SUPPORTED_WITH_CURR_FORMAT, GetResWName( T_FRAME, NULL ) );
            return( ERROR );
        }
        i++;
        if( tokenarray[i].token == T_COLON ) {
            struct asym *sym;
            i++;
            if ( tokenarray[i].token != T_ID ) {
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
                return( ERROR );
            }
            sym = SymSearch( tokenarray[i].string_ptr );
            if ( sym == NULL ) {
                sym = SymCreate( tokenarray[i].string_ptr );
                sym->state = SYM_UNDEFINED;
                sym->used = TRUE;
                sym_add_table( &SymTables[TAB_UNDEF], (struct dsym *)sym ); /* add UNDEFINED */
            } else if ( sym->state != SYM_UNDEFINED &&
                       sym->state != SYM_INTERNAL &&
                       sym->state != SYM_EXTERNAL ) {
                EmitErr( SYMBOL_REDEFINITION, sym->name );
                return( ERROR );
            }
            proc->e.procinfo->exc_handler = sym;
            i++;
        } else
            proc->e.procinfo->exc_handler = NULL;
        proc->e.procinfo->isframe = TRUE;
    }
#endif
    /* check for USES */
    if ( tokenarray[i].token == T_ID && _stricmp( tokenarray[i].string_ptr, "USES" ) == 0 ) {
        int cnt;
        int j;
        if ( !IsPROC ) {/* not for PROTO! */
            DebugMsg(("ExamineProc: USES found in PROTO\n"));
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        }
        i++;
        /* count register names which follow */
        for ( cnt = 0, j = i; tokenarray[j].token == T_REG; j++, cnt++ );

        if ( cnt == 0 ) {
            DebugMsg(("ExamineProc: no registers for regslist\n"));
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i-1].tokpos );
        } else {
            regist = LclAlloc( (cnt + 1) * sizeof( uint_16 ) );
            proc->e.procinfo->regslist = regist;
            *regist++ = cnt;
            /* read in registers */
            for( ; tokenarray[i].token == T_REG; i++ ) {
                if ( SizeFromRegister( tokenarray[i].tokval ) == 1 ) {
                    EmitError( INVALID_USE_OF_REGISTER );
                }
                *regist++ = tokenarray[i].tokval;
            }
        }
    }

    /* the parameters must follow */
    if ( tokenarray[i].token == T_STYPE || tokenarray[i].token == T_RES_ID || tokenarray[i].token == T_DIRECTIVE ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    /* skip optional comma */
    if ( tokenarray[i].token == T_COMMA )
        i++;

    DebugMsg1(("ExamineProc(%s): i=%u, Token_Count=%u, CurrWordSize=%u\n", proc->sym.name, i, Token_Count, CurrWordSize ));

    /* are there parameters at all? */
    if( i >= Token_Count ) {
        if ( proc->e.procinfo->init == TRUE && proc->e.procinfo->paralist != NULL )
            EmitErr( CONFLICTING_PARAMETER_DEFINITION, "" );
    } else if( proc->sym.langtype == LANG_NONE ) {
        EmitError( LANG_MUST_BE_SPECIFIED );
        return ( ERROR );
    } else  {
        /* v2.05: set PROC's vararg flag BEFORE params are scanned! */
        if ( tokenarray[Token_Count - 1].token == T_RES_ID &&
            tokenarray[Token_Count - 1].tokval == T_VARARG )
            proc->e.procinfo->is_vararg = TRUE;
        /* v2.04: removed, comma is checked above already */
        //if( tokenarray[i].token == T_COMMA )
        //    i++;
        if ( ERROR == ParseParams( i, tokenarray, proc, IsPROC ) )
            /* do proceed if the parameter scan returns an error */
            ;//return( ERROR );
    }

    proc->e.procinfo->init = TRUE;
    DebugMsg1(("ExamineProc(%s): memtype=%Xh parasize=%u\n", proc->sym.name, proc->sym.mem_type, proc->e.procinfo->parasize));

    return( NOT_ERROR );
}

/* create a proc item.
 * sym is either NULL, or has type SYM_UNDEFINED or SYM_EXTERNAL */

struct asym *CreateProc( struct asym *sym, const char *name, unsigned char IsPROC )
/*********************************************************************************/
{
    if ( sym == NULL )
        sym = ( *name ? SymCreate( name ) : SymAlloc( name ) );
    else
        sym_remove_table( ( sym->state == SYM_UNDEFINED ) ? &SymTables[TAB_UNDEF] : &SymTables[TAB_EXT], (struct dsym *)sym );

    if ( sym ) {
        struct proc_info *info;
        if ( IsPROC )
            sym->state = SYM_INTERNAL;
        else {
            sym->seg_ofssize = ModuleInfo.Ofssize;
            sym->state = SYM_EXTERNAL;
            sym->weak = TRUE;
        }
        info = LclAlloc( sizeof( struct proc_info ) );
        ((struct dsym *)sym)->e.procinfo = info;
        info->regslist = NULL;
        info->paralist = NULL;
        info->locallist = NULL;
        info->labellist = NULL;
        info->parasize = 0;
        info->localsize = 0;
        info->prologuearg = NULL;
        info->flags = 0;
        if ( *(sym->name) )
            if ( sym->state == SYM_INTERNAL )
                /* v2.04: don't use sym_add_table() and thus
                 * free the <next> member field!
                 */
                if ( SymTables[TAB_PROC].head == NULL )
                    SymTables[TAB_PROC].head = SymTables[TAB_PROC].tail = (struct dsym *)sym;
                else {
                    SymTables[TAB_PROC].tail->nextproc = (struct dsym *)sym;
                    SymTables[TAB_PROC].tail = (struct dsym *)sym;
                }
            else
                sym_add_table( &SymTables[TAB_EXT], (struct dsym *)sym ); /* EXTERNAL */

        if ( IsPROC ) {
            procidx++;
            if ( Options.line_numbers ) {
                sym->debuginfo = LclAlloc( sizeof( struct debug_info ) );
                sym->debuginfo->file = get_curr_srcfile();
            }
        }
    }
    return( sym );
}

/* delete a PROC item */

void DeleteProc( struct dsym *proc )
/**********************************/
{
    struct dsym *symcurr;
    struct dsym *symnext;

    DebugMsg1(("DeleteProc(%s) enter\n", proc->sym.name ));
    /* delete all local symbols ( params, locals, labels ) */
    if ( proc->sym.state == SYM_INTERNAL ) {

        for( symcurr = proc->e.procinfo->labellist; symcurr; ) {
            symnext = symcurr->e.nextll;
            DebugMsg(("DeleteProc(%s): free %s\n", proc->sym.name, symcurr->sym.name ));
            SymFree( &symcurr->sym );
            symcurr = symnext;
        }

        if ( proc->e.procinfo->regslist )
            LclFree( proc->e.procinfo->regslist );

        if ( proc->e.procinfo->prologuearg )
            LclFree( proc->e.procinfo->prologuearg );

        if ( Options.line_numbers && proc->sym.state == SYM_INTERNAL )
            LclFree( proc->sym.debuginfo );
    } else {
        /* PROTOs have just a parameter list, usually without names */
        for( symcurr = proc->e.procinfo->paralist; symcurr; ) {
            symnext = symcurr->nextparam;
            DebugMsg(("DeleteProc(%s): free %p (%s)\n", proc->sym.name, symcurr, symcurr->sym.name ));
            SymFree( &symcurr->sym );
            symcurr = symnext;
        }
    }
    LclFree( proc->e.procinfo );
    return;
}

/* PROC directive. */

ret_code ProcDir( int i, struct asm_tok tokenarray[] )
/****************************************************/
{
    struct asym         *sym;
    unsigned int        ofs;
    char                *name;
    bool                oldpubstate;
    bool                is_global;

    DebugMsg1(("ProcDir enter, curr ofs=%X\n", GetCurrOffset() ));
    if( i != 1 ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    /* v2.04b: check was missing */
    if( CurrSeg == NULL ) {
        EmitError( MUST_BE_IN_SEGMENT_BLOCK );
        return( ERROR );
    }

    name = tokenarray[0].string_ptr;

    if( CurrProc != NULL ) {

        /* this is not needed for JWasm, but Masm will reject nested
         * procs if there are params, locals or used registers.
         */
        if ( CurrProc->e.procinfo->paralist ||
#if AMD64_SUPPORT
            CurrProc->e.procinfo->isframe ||
#endif
            CurrProc->e.procinfo->locallist ||
            CurrProc->e.procinfo->regslist ) {
            EmitErr( CANNOT_NEST_PROCEDURES, name );
            return( ERROR );
        }
        /* nested procs ... push currproc on a stack */
        push_proc( CurrProc );
    }


    if ( ModuleInfo.procalign ) {
        AlignCurrOffset( ModuleInfo.procalign );
    }

    i++; /* go past PROC */

    sym = SymSearch( name );

    if( Parse_Pass == PASS_1 ) {

        oldpubstate = sym ? sym->public : FALSE;
        if( sym == NULL || sym->state == SYM_UNDEFINED ) {
            sym = CreateProc( sym, name, TRUE );
            is_global = FALSE;
        } else if ( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) {
            /* PROTO or EXTERNDEF item */
            is_global = TRUE;
            if ( sym->isproc == TRUE  ) {
                /* don't create the procinfo extension; it exists already */
                procidx++; /* v2.04: added */
                if ( Options.line_numbers ) {
                    sym->debuginfo = LclAlloc( sizeof( struct debug_info ) );
                    sym->debuginfo->file = get_curr_srcfile();
                }
            } else {
                /* it's a simple EXTERNDEF. Create a PROC item!
                 * this will be SYM_INTERNAL */
                /* v2.03: don't call dir_free(), it'll clear field Ofssize */
                //dir_free( (struct dsym *)sym );
                sym = CreateProc( sym, name, TRUE );
            }
        } else {
            /* Masm won't reject a redefinition if "certain" parameters
             * won't change. However, in a lot of cases one gets "internal assembler error".
             * Hence this "feature" isn't active in jwasm.
             */
            //} else if ( sym->state != SYM_INTERNAL || sym->isproc != TRUE ||
            //           sym->offset != GetCurrOffset() || sym->segment != &CurrSeg->sym ) {
            EmitErr( SYMBOL_REDEFINITION, sym->name );
            return( ERROR );
        }
        SetSymSegOfs( sym );

        SymClearLocal();

        /* ExamineProc() will use CurrProc variable! */
        CurrProc = (struct dsym *)sym;
        if( ExamineProc( i, tokenarray, (struct dsym *)sym, TRUE ) == ERROR ) {
            CurrProc = NULL;
            return( ERROR );
        }

        /* v2.04: added */
        if ( is_global && Options.masm8_proc_visibility )
            sym->public = TRUE;

        /* if there was a PROTO (or EXTERNDEF name:PROTO ...),
         * change symbol to SYM_INTERNAL! */
        if ( sym->state == SYM_EXTERNAL && sym->isproc == TRUE ) {
            sym_ext2int( sym );
        }

        sym->isproc = TRUE;

        if( sym->public == TRUE && oldpubstate == FALSE )
            AddPublicData( sym );

        /* v2.04: add the proc to the list of labels attached to curr segment.
         * this allows to reduce the number of passes (see fixup.c)
         */
        ((struct dsym *)sym)->next = (struct dsym *)CurrSeg->e.seginfo->labels;
        CurrSeg->e.seginfo->labels = sym;

    } else {
        /**/myassert( sym != NULL );

        procidx++;
        sym->isdefined = TRUE;

        SymSetLocal( sym );

        /* it's necessary to check for a phase error here
         as it is done in LabelCreate() and data_dir()!
         */
        ofs = GetCurrOffset();

        if ( ofs != sym->offset) {
            DebugMsg(("ProcDir(%s): %spass %u, old ofs=%" FX32 ", new ofs=%" FX32 "\n",
                    sym->name,
                    ModuleInfo.PhaseError ? "" : "phase error ",
                    Parse_Pass+1, sym->offset, ofs ));
            sym->offset = ofs;
            ModuleInfo.PhaseError = TRUE;
        }
        CurrProc = (struct dsym *)sym;
#if AMD64_SUPPORT
        /* check if the exception handler set by FRAME is defined */
        if ( CurrProc->e.procinfo->isframe &&
            CurrProc->e.procinfo->exc_handler &&
            CurrProc->e.procinfo->exc_handler->state == SYM_UNDEFINED ) {
            EmitErr( SYMBOL_NOT_DEFINED, CurrProc->e.procinfo->exc_handler->name );
        }
#endif
    }

    DefineProc = TRUE;
#if AMD64_SUPPORT
    if ( CurrProc->e.procinfo->isframe ) {
        endprolog_found = FALSE;
        if ( CurrProc->e.procinfo->exc_handler )
            unw_info.Flags = UNW_FLAG_FHANDLER;
        else
            unw_info.Flags = 0;
        unw_info.SizeOfProlog = 0;
        unw_info.CountOfCodes = 0;
    }
#endif

    sym->asmpass = Parse_Pass;
    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_LABEL, 0, NULL );

    if( Options.line_numbers ) {
#if COFF_SUPPORT
        if ( Options.output_format == OFORMAT_COFF )
            AddLinnumDataRef( 0 );
        else
#endif
            AddLinnumDataRef( LineNumber );
    }

    BackPatch( sym );
    return( NOT_ERROR );
}

ret_code CopyPrototype( struct dsym *proc, struct dsym *src )
/***********************************************************/
{
    struct dsym *curr;
    struct dsym *newl;
    struct dsym *oldl;

    if ( src->sym.isproc == FALSE )
        return( ERROR );
    memcpy(proc->e.procinfo, src->e.procinfo, sizeof( struct proc_info ) );
    proc->sym.mem_type = src->sym.mem_type;
    proc->sym.langtype = src->sym.langtype;
#if MANGLERSUPP
    proc->sym.mangler  = src->sym.mangler;
#endif
    proc->sym.public   = src->sym.public;
    /* we use the PROTO part, not the TYPE part */
    //dir->sym.seg_ofssize = src->sym.Ofssize;
    proc->sym.seg_ofssize = src->sym.seg_ofssize;
    proc->sym.isproc = TRUE;
    proc->e.procinfo->paralist = NULL;
    for ( curr = src->e.procinfo->paralist; curr; curr = curr->nextparam ) {
        newl = LclAlloc( sizeof( struct dsym ) );
        memcpy( newl, curr, sizeof( struct dsym ) );
        newl->nextparam = NULL;
        if ( proc->e.procinfo->paralist == NULL)
            proc->e.procinfo->paralist = newl;
        else {
            for ( oldl = proc->e.procinfo->paralist; oldl->nextparam; oldl = oldl->nextparam );
            oldl->nextparam = newl;
        }
    }
    DebugMsg1(("CopyPrototype(%s,src=%s): ofssize=%u\n",
               proc->sym.name, src->sym.name, src->sym.seg_ofssize ));
    return( NOT_ERROR );
}

#if AMD64_SUPPORT

/* for FRAME procs, write .pdata and .xdata SEH unwind information */

static void WriteSEHData( struct dsym *proc )
/*******************************************/
{
    struct dsym *xdata;
    char *segname = ".xdata";
    int i;
    int simplespec;
    uint_8 olddotname;
    uint_32 xdataofs = 0;
    char segnamebuff[12];
    char buffer[128];

    if ( endprolog_found == FALSE ) {
        EmitErr( MISSING_ENDPROLOG, proc->sym.name );
    }
    NewLineQueue();
    if ( unw_segs_defined )
        AddLineQueueX("%s %r", segname, T_SEGMENT );
    else {
        AddLineQueueX("%s %r align(%u) flat readonly 'DATA'", segname, T_SEGMENT, 8 );
        AddLineQueue("$xdatasym label near");
    }
    xdataofs = 0;
    xdata = (struct dsym *)SymSearch( segname );
    if ( xdata )
        xdataofs = xdata->sym.offset;
    /* write the .xdata stuff (a UNWIND_INFO entry ) */
    AddLineQueueX( "db 0%xh + (0%xh shl 3), %u, %u, 0%xh + (0%xh shl 4)",
            unw_info.Version, unw_info.Flags, unw_info.SizeOfProlog,
            unw_info.CountOfCodes, unw_info.FrameRegister, unw_info.FrameOffset );
    if ( unw_info.CountOfCodes ) {
        char *pfx = "dw";
        buffer[0] = NULLC;
        /* write the codes from right to left */
        for ( i = unw_info.CountOfCodes; i ; i-- ) {
            sprintf( buffer + strlen( buffer ), "%s 0%xh", pfx, unw_code[i-1] );
            pfx = ",";
            if ( i == 1 || strlen( buffer ) > 72 ) {
                AddLineQueue( buffer );
                buffer[0] = NULLC;
                pfx = "dw";
            }
        }
    }
    AddLineQueueX( "%r 4", T_ALIGN );
    if ( proc->e.procinfo->exc_handler ) {
        AddLineQueueX( "dd %r %s", T_IMAGEREL, proc->e.procinfo->exc_handler->name );
        AddLineQueueX( "%r 8", T_ALIGN );
    }
    AddLineQueueX( "%s %r", segname, T_ENDS );

    /* v2.07: ensure that .pdata items are sorted */
    if ( 0 == strcmp( SimGetSegName( SIM_CODE ), proc->sym.segment->name ) ) {
        segname = ".pdata";
        simplespec = ( unw_segs_defined & 1 );
        unw_segs_defined = 3;
    } else {
        segname = segnamebuff;
        sprintf( segname, ".pdata$%04u", GetSegIdx( proc->sym.segment ) );
        simplespec = 0;
        unw_segs_defined |= 2;
    }

    if ( simplespec )
        AddLineQueueX( "%s %r", segname, T_SEGMENT );
    else
        AddLineQueueX( "%s %r align(%u) flat readonly 'DATA'", segname, T_SEGMENT, 4 );
    /* write the .pdata stuff ( type IMAGE_RUNTIME_FUNCTION_ENTRY )*/
    AddLineQueueX( "dd %r %s, %r %s+0%xh, %r $xdatasym+0%xh",
                  T_IMAGEREL, proc->sym.name,
                  T_IMAGEREL, proc->sym.name, proc->sym.total_size,
                  T_IMAGEREL, xdataofs );
    AddLineQueueX("%s %r", segname, T_ENDS );
    olddotname = ModuleInfo.dotname;
    ModuleInfo.dotname = TRUE; /* set OPTION DOTNAME because .pdata and .xdata */
    RunLineQueue();
    ModuleInfo.dotname = olddotname;
    return;
}
#endif

/* close a PROC
 */

static void ProcFini( struct dsym *proc )
/***************************************/
{
    /* v2.06: emit an error if current segment isn't equal to
     * the one of the matching PROC directive. Close the proc anyway!
     */
    if ( proc->sym.segment == &CurrSeg->sym ) {
        proc->sym.total_size = GetCurrOffset() - proc->sym.offset;
    } else {
        EmitErr( UNMATCHED_BLOCK_NESTING, proc->sym.name );
        proc->sym.total_size = CurrProc->sym.segment->offset - proc->sym.offset;
    }

    /* v2.03: for W3+, check for unused params and locals */
    if ( Options.warning_level > 2 ) {
        struct dsym *curr;
        for ( curr = proc->e.procinfo->paralist; curr; curr = curr->nextparam ) {
            if ( curr->sym.used == FALSE && Parse_Pass == PASS_1 )
                EmitWarn( 3, PROCEDURE_ARGUMENT_OR_LOCAL_NOT_REFERENCED, curr->sym.name );
        }
        for ( curr = proc->e.procinfo->locallist; curr; curr = curr->nextlocal ) {
            if ( curr->sym.used == FALSE && Parse_Pass == PASS_1 )
                EmitWarn( 3, PROCEDURE_ARGUMENT_OR_LOCAL_NOT_REFERENCED, curr->sym.name );
        }
    }
#if AMD64_SUPPORT
    /* save stack space reserved for INVOKE if OPTION WIN64:2 is set */
    if ( Parse_Pass == PASS_1 &&
        ModuleInfo.fctype == FCT_WIN64 &&
        ( ModuleInfo.win64_flags & W64F_AUTOSTACKSP ) ) {
        proc->e.procinfo->ReservedStack = ReservedStack.value;
        DebugMsg1(("ProcFini(%s): localsize=%u ReservedStack=%u\n", proc->sym.name, proc->e.procinfo->localsize, proc->e.procinfo->ReservedStack ));
    }

    /* create the .pdata and .xdata stuff */
    if ( proc->e.procinfo->isframe ) {
#if FASTPASS
        LstSetPosition(); /* needed if generated code is done BEFORE the line is listed */
#endif
        WriteSEHData( proc );
    }
#endif
    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_LABEL, 0, NULL );

    /* create the list of locals */
    if ( Parse_Pass == PASS_1 )
        SymGetLocal( (struct asym *)CurrProc );

    CurrProc = pop_proc();
    if ( CurrProc )
        SymSetLocal( (struct asym *)CurrProc );  /* restore local symbol table */

    DefineProc = FALSE; /* in case there was an empty PROC/ENDP pair */
}

/* ENDP directive */

ret_code EndpDir( int i, struct asm_tok tokenarray[] )
/****************************************************/
{
    DebugMsg1(("EndpDir(%s) enter, curr ofs=%" FX32 "\n", tokenarray[0].string_ptr, GetCurrOffset() ));
    if( i != 1 || tokenarray[2].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
        return( ERROR );
    }
    if( CurrProc &&
       ( SymCmpFunc(CurrProc->sym.name, tokenarray[0].string_ptr, CurrProc->sym.name_size ) == 0 ) ) {
        ProcFini( CurrProc );
    } else {
        EmitErr( UNMATCHED_BLOCK_NESTING, tokenarray[0].string_ptr );
        return( ERROR );
    }
    return( NOT_ERROR );
}

#if AMD64_SUPPORT

/* handles win64 directives
 * .allocstack
 * .endprolog
 * .pushframe
 * .pushreg
 * .savereg
 * .savexmm128
 * .setframe
 */

ret_code ExcFrameDirective( int i, struct asm_tok tokenarray[] )
/**************************************************************/
{
    struct expr opndx;
    int token;
    unsigned int size;
    uint_8 reg;
    uint_8 ofs;
    UNWIND_CODE *puc;

    DebugMsg(("ExcFrameDirective(%s) enter\n", tokenarray[i].string_ptr ));
    /* v2.05: accept directives for windows only */
    if ( Options.output_format != OFORMAT_COFF ) {
        EmitErr( NOT_SUPPORTED_WITH_CURR_FORMAT, GetResWName( tokenarray[i].tokval, NULL ) );
        return( ERROR );
    }
    if ( CurrProc == NULL || endprolog_found == TRUE ) {
        EmitError( ENDPROLOG_FOUND_BEFORE_EH_DIRECTIVES );
        return( ERROR );
    }
    if ( CurrProc->e.procinfo->isframe == FALSE ) {
        EmitError( MISSING_FRAME_IN_PROC );
        return( ERROR );
    }
    puc = &unw_code[unw_info.CountOfCodes];
    ofs = GetCurrOffset() - CurrProc->sym.offset;
    token = tokenarray[i].tokval;
    switch ( token ) {
    case T_DOT_ALLOCSTACK: /* syntax: .ALLOCSTACK size */
        i++;
        if ( ERROR == EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) {
            EmitError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if ( opndx.value & 7 ) {
            EmitError( BAD_ALIGNMENT_FOR_OFFSET_IN_UNWIND_CODE );
            return( ERROR );
        }
        if ( opndx.value == 0 ) {
            EmitError( NONZERO_VALUE_EXPECTED );
            return( ERROR );
        }
        opndx.value -= 8;
        if ( opndx.value > 16*8 ) {
            if ( opndx.value > 65536 * 8 ) {
                puc->FrameOffset = ( opndx.value >> 19 );
                puc++;
                puc->FrameOffset = ( opndx.value >> 3 );
                puc++;
                unw_info.CountOfCodes += 2;
                puc->OpInfo = 1;
            } else {
                puc->FrameOffset = ( opndx.value >> 3 );
                puc++;
                unw_info.CountOfCodes++;
                puc->OpInfo = 0;
            }
            puc->UnwindOp = UWOP_ALLOC_LARGE;
        } else {
            puc->UnwindOp = UWOP_ALLOC_SMALL;
            puc->OpInfo = ( opndx.value >> 3 );
        }
        puc->CodeOffset = ofs;
        unw_info.CountOfCodes++;
        break;
    case T_DOT_ENDPROLOG: /* syntax: .ENDPROLOG */
        opndx.value = GetCurrOffset() - CurrProc->sym.offset;
        if ( opndx.uvalue > 255 ) {
            EmitError( SIZE_OF_PROLOG_TOO_BIG );
            return( ERROR );
        }
        unw_info.SizeOfProlog = (uint_8)opndx.uvalue;
        endprolog_found = TRUE;
        i++;
        break;
    case T_DOT_PUSHFRAME: /* syntax: .PUSHFRAME [code] */
        i++;
        puc->CodeOffset = ofs;
        puc->UnwindOp = UWOP_PUSH_MACHFRAME;
        puc->OpInfo = 0;
        if ( tokenarray[i].token == T_ID && (_stricmp( tokenarray[i].string_ptr, "CODE") == 0 ) ) {
            puc->OpInfo = 1;
            i++;
        }
        unw_info.CountOfCodes++;
        break;
    case T_DOT_PUSHREG: /* syntax: .PUSHREG r64 */
        i++;
        if ( tokenarray[i].token != T_REG || !( GetValueSp( tokenarray[i].tokval ) & OP_R64) ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
        puc->CodeOffset = ofs;
        puc->UnwindOp = UWOP_PUSH_NONVOL;
        puc->OpInfo = GetRegNo( tokenarray[i].tokval );
        unw_info.CountOfCodes++;
        i++;
        break;
    case T_DOT_SAVEREG:    /* syntax: .SAVEREG r64, offset       */
    case T_DOT_SAVEXMM128: /* syntax: .SAVEXMM128 xmmreg, offset */
    case T_DOT_SETFRAME:   /* syntax: .SETFRAME r64, offset      */
        i++;
        if ( tokenarray[i].token != T_REG ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
        if ( token == T_DOT_SAVEXMM128 ) {
            if ( !( GetValueSp( tokenarray[i].tokval ) & OP_XMM ) ) {
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
                return( ERROR );
            }
        } else {
            if ( !( GetValueSp( tokenarray[i].tokval ) & OP_R64 ) ) {
                EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
                return( ERROR );
            }
        }
        reg = GetRegNo( tokenarray[i].tokval );

        if ( token == T_DOT_SAVEREG )
            size = 8;
        else
            size = 16;

        i++;
        if ( tokenarray[i].token != T_COMMA ) {
            EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
        i++;
        if ( ERROR == EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) {
            EmitError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if ( opndx.value & (size - 1) ) {
            EmitError( BAD_ALIGNMENT_FOR_OFFSET_IN_UNWIND_CODE );
            return( ERROR );
        }
        switch ( token ) {
        case T_DOT_SAVEREG:
            puc->OpInfo = reg;
            if ( opndx.value > 65536 * size ) {
                puc->FrameOffset = ( opndx.value >> 19 );
                puc++;
                puc->FrameOffset = ( opndx.value >> 3 );
                puc++;
                puc->UnwindOp = UWOP_SAVE_NONVOL_FAR;
                unw_info.CountOfCodes += 3;
            } else {
                puc->FrameOffset = ( opndx.value >> 3 );
                puc++;
                puc->UnwindOp = UWOP_SAVE_NONVOL;
                unw_info.CountOfCodes += 2;
            }
            puc->CodeOffset = ofs;
            puc->OpInfo = reg;
            break;
        case T_DOT_SAVEXMM128:
            if ( opndx.value > 65536 * size ) {
                puc->FrameOffset = ( opndx.value >> 20 );
                puc++;
                puc->FrameOffset = ( opndx.value >> 4 );
                puc++;
                puc->UnwindOp = UWOP_SAVE_XMM128_FAR;
                unw_info.CountOfCodes += 3;
            } else {
                puc->FrameOffset = ( opndx.value >> 4 );
                puc++;
                puc->UnwindOp = UWOP_SAVE_XMM128;
                unw_info.CountOfCodes += 2;
            }
            puc->CodeOffset = ofs;
            puc->OpInfo = reg;
            break;
        case T_DOT_SETFRAME:
            if ( opndx.uvalue > 240 ) {
                EmitConstError( &opndx );
                return( ERROR );
            }
            unw_info.FrameRegister = reg;
            unw_info.FrameOffset = ( opndx.uvalue >> 4 );
            puc->CodeOffset = ofs;
            puc->UnwindOp = UWOP_SET_FPREG;
            //puc->OpInfo = ( opndx.uvalue >> 4 );
            puc->OpInfo = reg;
            unw_info.CountOfCodes++;
            break;
        }
        break;
    }
    if ( tokenarray[i].token != T_FINAL ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    DebugMsg(("ExcFrameDirective() exit, ok\n" ));
    return( NOT_ERROR );
}
#endif

/* see if there are open procedures.
 * called when the END directive has been found.
 */
void ProcCheckOpen( void )
/************************/
{
    while( CurrProc != NULL ) {
        EmitErr( UNMATCHED_BLOCK_NESTING, CurrProc->sym.name );
        ProcFini( CurrProc );
    }
}

static ret_code write_userdef_prologue( struct asm_tok tokenarray[] )
/*******************************************************************/
{
    int                 len;
    int                 i;
    struct proc_info    *info;
    char                *p;
    bool                is_exitm;
    struct dsym         *dir;
    //int                 align = CurrWordSize;
    int                 flags = CurrProc->sym.langtype; /* set bits 0-2 */
    uint_16             *regs;
    char                reglst[128];
    char                buffer[128+128];
    char                retvalue[MAX_LINE_LEN]; /* stores literal returned by RunMacro() */

#if FASTPASS
    if ( Parse_Pass > PASS_1 && UseSavedState )
        return( NOT_ERROR );
#endif

    info = CurrProc->e.procinfo;
    info->localsize = ROUND_UP( info->localsize, CurrWordSize );
#if AMD64_SUPPORT
    /* to be compatible with ML64, translate FASTCALL to 0 (not 7) */
    if ( CurrProc->sym.langtype == LANG_FASTCALL && ModuleInfo.fctype == FCT_WIN64 )
        flags = 0;
#endif
    /* set bit 4 if the caller restores (E)SP */
    if ( CurrProc->sym.langtype == LANG_C ||
        CurrProc->sym.langtype == LANG_SYSCALL ||
        CurrProc->sym.langtype == LANG_FASTCALL )
        flags |= 0x10;

    if ( CurrProc->sym.mem_type == MT_FAR )
        flags |= 0x20;

    if ( CurrProc->sym.public == FALSE )
        flags |= 0x40;

    //flags |= CurrProc->sym.export ? 0 : 0x80; /* bit 7: 1 if export */

    p = reglst;
    if ( info->regslist ) {
        regs = info->regslist;
        for ( len = *regs++; len; len--, regs++ ) {
            GetResWName( *regs, p );
            p += strlen( p );
            if ( len > 1 )
                *p++ = ',';
        }
    }
    *p = NULLC;

    dir = (struct dsym *)SymSearch( ModuleInfo.proc_prologue );
    if ( dir == NULL || dir->sym.state != SYM_MACRO || dir->sym.isfunc != TRUE ) {
        EmitError( PROLOGUE_MUST_BE_MACRO_FUNC );
        return( ERROR );
    }

    /* if -EP is on, emit "prologue: none" */
    if ( Options.preprocessor_stdout )
        printf( "option prologue:none\n" );

#if 0
    /* v2.07: make this work with radix != 10 */
    //sprintf( buffer,"%s(%s, %u, %u, %u, <<%s>>, <%s>)", ModuleInfo.proc_prologue,
    sprintf( buffer,"%s(%s, 0%XH, 0%XH, 0%XH, <<%s>>, <%s>)", ModuleInfo.proc_prologue,
             CurrProc->sym.name, flags, info->parasize, info->localsize,
            reglst, info->prologuearg ? info->prologuearg : "" );
#else
    sprintf( buffer,"(%s, 0%XH, 0%XH, 0%XH, <<%s>>, <%s>)",
             CurrProc->sym.name, flags, info->parasize, info->localsize,
            reglst, info->prologuearg ? info->prologuearg : "" );
    i = Token_Count + 1;
    Token_Count = Tokenize( buffer, i, tokenarray, TOK_RESCAN );
#endif
    //RunMacro( dir, buffer, retvalue, FALSE, &is_exitm );
    RunMacro( dir, i, tokenarray, retvalue, -1, &is_exitm );
    Token_Count = i - 1;
    DebugMsg(("write_userdef_prologue: macro %s returned >%s<\n", ModuleInfo.proc_prologue, retvalue));

    if ( Parse_Pass == PASS_1 ) {
        struct dsym *curr;
        len = atoi(retvalue) - info->localsize;
        for ( curr = info->locallist; curr; curr = curr->nextlocal ) {
            curr->sym.offset -= len;
        }
    }

    return ( NOT_ERROR );
}

#if AMD64_SUPPORT

/* save up to 4 register parameters for WIN64 fastcall */

static void win64_SaveRegParams( struct proc_info *info )
/*******************************************************/
{
    int i;
    struct dsym *param;

    for ( i = 0, param = info->paralist; param && ( i < 4 ); i++ ) {
        /* v2.05: save XMMx if type is float/double */
        if ( param->sym.is_vararg == FALSE ) {
            if ( param->sym.mem_type & MT_FLOAT )
                AddLineQueueX( "movq [%r+%u], %r", T_RSP, 8 + i * 8, T_XMM0 + i );
            else
                AddLineQueueX( "mov [%r+%u], %r", T_RSP, 8 + i * 8, ms64_regs[i] );
            param = param->nextparam;
        }
    }
    return;
}

/* win64 default prologue when PROC FRAME and
 * OPTION FRAME:AUTO is set */

static ret_code write_win64_default_prologue( struct proc_info *info )
/********************************************************************/
{
    uint_16             *regist;
    int                 sizestd = 0;
    int                 sizexmm = 0;

    DebugMsg1(("write_win64_default_prologue enter\n"));
    NewLineQueue();

    if ( ModuleInfo.win64_flags & W64F_SAVEREGPARAMS )
        win64_SaveRegParams( info );
    /*
     * PUSH RBP
     * .PUSHREG RBP
     * MOV RBP, RSP
     * .SETFRAME RBP, 0
     */
    AddLineQueueX( "push %r", T_RBP );
    AddLineQueueX( "%r %r", T_DOT_PUSHREG, T_RBP );
    AddLineQueueX( "mov %r, %r", T_RBP, T_RSP );
    AddLineQueueX( "%r %r, 0", T_DOT_SETFRAME, T_RBP );

    /* after the "push rbp", the stack is xmmword aligned */

    /* Push the registers */
    if( info->regslist ) {
        int cnt;
        regist = info->regslist;
        for( cnt = *regist++; cnt; cnt--, regist++ ) {
            //int i;
            if ( GetValueSp( *regist ) & OP_XMM ) {
                sizexmm += 16;
            } else {
                sizestd += 8;
                AddLineQueueX( "push %r", *regist );
                if ( ( 1 << GetRegNo( *regist ) ) & win64_nvgpr ) {
                    AddLineQueueX( "%r %r", T_DOT_PUSHREG, *regist );
                }
            }
        } /* end for */

        DebugMsg1(("write_win64_default_prologue: sizestd=%u, sizexmm=%u\n", sizestd, sizexmm ));
        sizestd &= 0xF; /* result will be 8 or 0. Just this amount is needed below */
#if 1
        /* save xmm registers */
        if ( sizexmm ) {
            int i;
            AddLineQueueX( "sub %r, %d", T_RSP, NUMQUAL sizexmm + sizestd );
            AddLineQueueX( "%r %d", T_DOT_ALLOCSTACK, NUMQUAL sizexmm + sizestd );
            sizestd = 0; /* stack is aligned now. Don't use sizestd anymore */
            regist = info->regslist;
            for( cnt = *regist++, i = 0; cnt; cnt--, regist++ ) {
                if ( GetValueSp( *regist ) & OP_XMM ) {
                    AddLineQueueX( "movdqa [%r+%u], %r", T_RSP, NUMQUAL i, *regist );
                    if ( ( 1 << GetRegNo( *regist ) ) & win64_nvxmm )  {
                        AddLineQueueX( "%r %r, %u", T_DOT_SAVEXMM128, *regist, NUMQUAL i );
                    }
                    i += 16;
                }
            }
        }
#endif
    }
    info->localsize = ROUND_UP( info->localsize, CurrWordSize );

    /* alloc space for local variables and align the stack. */
    //if( info->localsize + sizestd ) {
    if( info->localsize + sizestd + ReservedStack.value ) {

        /* align the stack if necessary. */
        if ( ( sizestd && (!(info->localsize & 0xF ) ) ) ||
            ( sizestd == 0 && (info->localsize & 0xF ) ) )
            info->localsize += 8;
        DebugMsg1(("write_win64_default_prologue: localsize=%u sizestd=%u\n", info->localsize, sizestd ));

        /*
         * SUB  RSP, localsize
         * .ALLOCSTACK localsize
         */
        if ( ReservedStack.value ) {
            AddLineQueueX( "sub %r, %d + @ReservedStack", T_RSP, NUMQUAL info->localsize );
            AddLineQueueX( "%r %d + @ReservedStack", T_DOT_ALLOCSTACK, NUMQUAL info->localsize );
        } else {
            AddLineQueueX( "sub %r, %d", T_RSP, NUMQUAL info->localsize );
            AddLineQueueX( "%r %d", T_DOT_ALLOCSTACK, NUMQUAL info->localsize );
        }
    }
    AddLineQueueX( "%r", T_DOT_ENDPROLOG );

#if FASTPASS
    /* special case: generated code runs BEFORE the line */
    if ( ModuleInfo.list && UseSavedState )
        if ( Parse_Pass == PASS_1 )
            info->list_pos = list_pos;
        else
            list_pos = info->list_pos;
#endif
    RunLineQueue();

#if FASTPASS
    if ( ModuleInfo.list && UseSavedState && (Parse_Pass > PASS_1))
         LineStoreCurr->list_pos = list_pos;
#endif

    return( NOT_ERROR );
}
#endif

/* write PROC prologue
 * this is to be done after the LOCAL directives
 * and *before* any real instruction
 */
/* prolog code timings

                                                  best result
               size  86  286  386  486  P     86  286  386  486  P
 push bp       2     11  3    2    1    1
 mov bp,sp     2     2   2    2    1    1
 sub sp,immed  4     4   3    2    1    1
              -----------------------------
               8     17  8    6    3    3     x   x    x    x    x

 push ebp      2     -   -    2    1    1
 mov ebp,esp   2     -   -    2    1    1
 sub esp,immed 6     -   -    2    1    1
              -----------------------------
               10    -   -    6    3    3              x    x    x

 enter imm,0   4     -   11   10   14   11

 write prolog code
*/

static ret_code write_default_prologue( void )
/********************************************/
{
    struct proc_info    *info;
    uint_8              oldlinenumbers;
    //int                 align = CurrWordSize;

    info = CurrProc->e.procinfo;

#if AMD64_SUPPORT
    if ( info->isframe ) {
        //DebugMsg(("write_default_prologue: isframe\n"));
        if ( ModuleInfo.frame_auto )
            return( write_win64_default_prologue( info ) );
        return( NOT_ERROR );
    }
#endif
    /* default processing. if no params/locals are defined, continue */
    if( info->forceframe == FALSE &&
       info->localsize == 0 &&
       info->stackparam == FALSE &&
       info->is_vararg == FALSE &&
#if AMD64_SUPPORT
       ReservedStack.value == 0 &&
#endif
       info->regslist == NULL )
        return( NOT_ERROR );

    info->localsize = ROUND_UP( info->localsize, CurrWordSize );
    NewLineQueue();

#if AMD64_SUPPORT
    /* initialize shadow space for register params */
    if ( ModuleInfo.Ofssize == USE64 &&
        CurrProc->sym.langtype == LANG_FASTCALL &&
        ModuleInfo.fctype == FCT_WIN64 &&
        ( ModuleInfo.win64_flags & W64F_SAVEREGPARAMS ) )
        win64_SaveRegParams( info );
#endif
    if( info->localsize || info->stackparam || info->is_vararg || info->forceframe ) {

        /* write 80386 prolog code
         * PUSH [E|R]BP
         * MOV  [E|R]BP, [E|R]SP
         * SUB  [E|R]SP, localsize
         */
        AddLineQueueX( "push %r", basereg[ModuleInfo.Ofssize] );
        AddLineQueueX( "mov %r, %r", basereg[ModuleInfo.Ofssize], stackreg[ModuleInfo.Ofssize] );
    }
#if AMD64_SUPPORT
    if( ReservedStack.value ) {
        /* if no framepointer was pushed, add 8 to align stack on OWORD */
        if( !(info->localsize || info->stackparam || info->is_vararg || info->forceframe ))
            AddLineQueueX( "sub %r, 8 + @ReservedStack", stackreg[ModuleInfo.Ofssize] );
        else
            AddLineQueueX( "sub %r, %d + @ReservedStack", stackreg[ModuleInfo.Ofssize], NUMQUAL info->localsize );
    } else
#endif
    if( info->localsize  ) {
        /* using ADD and the 2-complement has one advantage:
         * it will generate short instructions up to a size of 128.
         * with SUB, short instructions work up to 127 only.
         */
        if ( Options.masm_compat_gencode || info->localsize == 128 )
            AddLineQueueX( "add %r, %d", stackreg[ModuleInfo.Ofssize], NUMQUAL - info->localsize );
        else
            AddLineQueueX( "sub %r, %d", stackreg[ModuleInfo.Ofssize], NUMQUAL info->localsize );
    }

    if ( info->loadds ) {
        AddLineQueueX( "push %r", T_DS );
        AddLineQueueX( "mov %r, %s", T_AX, szDgroup );
        AddLineQueueX( "mov %r, %r", T_DS, ModuleInfo.Ofssize ? T_EAX : T_AX );
    }

    /* Push the USES registers */
    if ( info->regslist ) {
        uint_16 *regist = info->regslist;
        int cnt;
        for( cnt = *regist++; cnt; cnt--, regist++ ) {
            AddLineQueueX( "push %r", *regist );
        }
    }
#if FASTPASS
    /* special case: generated code runs BEFORE the line.*/
    if ( ModuleInfo.list && UseSavedState )
        if ( Parse_Pass == PASS_1 )
            info->list_pos = list_pos;
        else
            list_pos = info->list_pos;
#endif
    /* line number debug info also needs special treatment
     * because current line number is the first true src line
     * IN the proc.
     */
    oldlinenumbers = Options.line_numbers;
    Options.line_numbers = FALSE; /* temporarily disable line numbers */
    RunLineQueue();
    Options.line_numbers = oldlinenumbers;

#if FASTPASS
    if ( ModuleInfo.list && UseSavedState && (Parse_Pass > PASS_1))
         LineStoreCurr->list_pos = list_pos;
#endif

    return( NOT_ERROR );
}

void write_prologue( struct asm_tok tokenarray[] )
/************************************************/
{
    DefineProc = FALSE;

#if AMD64_SUPPORT
    if ( ModuleInfo.fctype == FCT_WIN64 && ( ModuleInfo.win64_flags & W64F_AUTOSTACKSP ) ) {
        /* in pass one init reserved stack with 4*8 to force stack frame creation */
        ReservedStack.value = ( Parse_Pass == PASS_1 ? 4 * sizeof( uint_64 ) : CurrProc->e.procinfo->ReservedStack );
    }
#endif
    /* there are 3 cases:
     * option prologue:NONE           proc_prologue == NULL
     * option prologue:default        *proc_prologue == NULLC
     * option prologue:usermacro      *proc_prologue != NULLC
     */
    if ( ModuleInfo.prologuemode == PEM_DEFAULT ) {
        DebugMsg1(("write_prologue(%s): default prologue\n", CurrProc->sym.name ));
        write_default_prologue();
    } else if ( ModuleInfo.prologuemode == PEM_NONE ) {
        DebugMsg1(("write_prologue(%s): prologue is NULL\n", CurrProc->sym.name  ));
    } else {
        DebugMsg1(("write_prologue(%s): userdefined prologue %s\n", CurrProc->sym.name , ModuleInfo.proc_prologue ));
        write_userdef_prologue( tokenarray );
    }
    return;
}

static void pop_register( uint_16 *regist )
/*****************************************/
/* Pop the register when a procedure ends */
{
    int cnt;
    if( regist == NULL )
        return;
    cnt = *regist;
    regist += cnt;
    for ( ; cnt; cnt--, regist-- ) {
        /* don't "pop" xmm registers */
        if ( GetValueSp( *regist ) & OP_XMM )
            continue;
        AddLineQueueX( "pop %r", *regist );
    }
}

#if AMD64_SUPPORT

/* Win64 default epilogue if PROC FRAME and OPTION FRAME:AUTO is set
 */

static void write_win64_default_epilogue( struct proc_info *info )
/****************************************************************/
{
    uint sizexmm = 0;
    uint sizestd = 0;

    /* restore non-volatile xmm registers */
    if ( info->regslist ) {
        uint_16 *regist = info->regslist;
        int cnt;
        for( cnt = *regist++; cnt; cnt--, regist++ ) {
            if ( GetValueSp( *regist ) & OP_XMM ) {
                AddLineQueueX( "movdqa %r, [%r+%u]", *regist, stackreg[ModuleInfo.Ofssize], NUMQUAL info->localsize + sizexmm );
                sizexmm += 16;
            } else
                sizestd += 8;
        }
    }
    sizestd &= 0xf;
    /* v2.06: must match alignment of prologue! */
    if ( sizexmm && sizestd )
        sizexmm += sizestd;
    //sprintf( buffer, "add %s, %d", GetResWName( stackreg[ModuleInfo.Ofssize], NULL ), info->localsize + sizexmm + sizestd );

    if ( ModuleInfo.fctype == FCT_WIN64 && ( ModuleInfo.win64_flags & W64F_AUTOSTACKSP ) )
        AddLineQueueX( "add %r, %d + @ReservedStack", stackreg[ModuleInfo.Ofssize], NUMQUAL info->localsize + sizexmm );
    else
        AddLineQueueX( "add %r, %d", stackreg[ModuleInfo.Ofssize], NUMQUAL info->localsize + sizexmm );
    pop_register( CurrProc->e.procinfo->regslist );
    AddLineQueueX( "pop %r", basereg[ModuleInfo.Ofssize] );
    return;
}
#endif

/* write default epilogue code
 * if a RET/IRET instruction has been found inside a PROC.

 * epilog code timings
 *
 *                                                  best result
 *              size  86  286  386  486  P      86  286  386  486  P
 * mov sp,bp    2     2   2    2    1    1
 * pop bp       2     8   5    4    4    1
 *             -----------------------------
 *              4     10  7    6    5    2      x             x    x
 *
 * mov esp,ebp  2     -   -    2    1    1
 * pop ebp      2     -   -    4    4    1
 *             -----------------------------
 *              4     -   -    6    5    2                    x    x
 *
 * leave        1     -   5    4    5    3          x    x    x
 *
 * !!!! DECISION !!!!
 *
 * leave will be used for .286 and .386
 * .286 code will be best working on 286,386 and 486 processors
 * .386 code will be best working on 386 and 486 processors
 * .486 code will be best working on 486 and above processors
 *
 *   without LEAVE
 *
 *         86  286  386  486  P
 *  .8086  0   -2   -2   0    +1
 *  .286   -   -2   -2   0    +1
 *  .386   -   -    -2   0    +1
 *  .486   -   -    -    0    +1
 *
 *   LEAVE 286 only
 *
 *         86  286  386  486  P
 *  .8086  0   -2   -2   0    +1
 *  .286   -   0    +2   0    -1
 *  .386   -   -    -2   0    +1
 *  .486   -   -    -    0    +1
 *
 *   LEAVE 286 and 386
 *
 *         86  286  386  486  P
 *  .8086  0   -2   -2   0    +1
 *  .286   -   0    +2   0    -1
 *  .386   -   -    0    0    -1
 *  .486   -   -    -    0    +1
 *
 *   LEAVE 286, 386 and 486
 *
 *         86  286  386  486  P
 *  .8086  0   -2   -2   0    +1
 *  .286   -   0    +2   0    -1
 *  .386   -   -    0    0    -1
 *  .486   -   -    -    0    -1
 */

static void write_default_epilogue( void )
/****************************************/
{
    struct proc_info   *info;

    info = CurrProc->e.procinfo;

#if AMD64_SUPPORT
    if ( info->isframe ) {
        if ( ModuleInfo.frame_auto )
            write_win64_default_epilogue( info );
        return;
    }
#endif

    /* Pop the registers */
    pop_register( CurrProc->e.procinfo->regslist );

    if ( info->loadds ) {
        AddLineQueueX( "pop %r", T_DS );
    }

    if( ( info->localsize == 0 ) &&
       info->stackparam == FALSE &&
       info->is_vararg == FALSE &&
#if AMD64_SUPPORT
       ReservedStack.value == 0 &&
#endif
       info->forceframe == FALSE )
        return;

    /* restore registers e/sp and e/bp.
     * emit either "leave" or "mov e/sp,e/bp, pop e/bp".
     */
    if( info->pe_type ) {
        AddLineQueue( "leave" );
    } else  {
        /*
         MOV [E|R]SP, [E|R]BP
         POP [E|R]BP
         */
        if( info->localsize != 0 ) {
            AddLineQueueX( "mov %r, %r", stackreg[ModuleInfo.Ofssize], basereg[ModuleInfo.Ofssize] );
        }
        AddLineQueueX( "pop %r", basereg[ModuleInfo.Ofssize] );
    }
}

/* write userdefined epilogue code
 * if a RET/IRET instruction has been found inside a PROC.
 */
static ret_code write_userdef_epilogue( bool flag_iret, struct asm_tok tokenarray[] )
/***********************************************************************************/
{
    uint_16 *regs;
    int i;
    char *p;
    bool is_exitm;
    struct proc_info   *info;
    int flags = CurrProc->sym.langtype; /* set bits 0-2 */
    struct dsym *dir;
    char reglst[128];
    char buffer[MAX_LINE_LEN]; /* stores string for RunMacro() */

    dir = (struct dsym *)SymSearch( ModuleInfo.proc_epilogue );
    if (dir == NULL ||
        dir->sym.state != SYM_MACRO ||
        dir->sym.isfunc == TRUE ) {
        EmitErr( EPILOGUE_MUST_BE_MACRO_PROC, ModuleInfo.proc_epilogue );
        return( ERROR );
    }

    info = CurrProc->e.procinfo;

#if AMD64_SUPPORT
    /* to be compatible with ML64, translate FASTCALL to 0 (not 7) */
    if ( CurrProc->sym.langtype == LANG_FASTCALL && ModuleInfo.fctype == FCT_WIN64 )
        flags = 0;
#endif
    if ( CurrProc->sym.langtype == LANG_C ||
         CurrProc->sym.langtype == LANG_SYSCALL ||
         CurrProc->sym.langtype == LANG_FASTCALL)
        flags |= 0x10;

    if ( CurrProc->sym.mem_type == MT_FAR)
        flags |= 0x20;

    if ( CurrProc->sym.public == FALSE )
        flags |= 0x40;

    //flags |= CurrProc->sym.export ? 0 : 0x80; /* bit 7: 1 if export */
    flags |= flag_iret ? 0x100 : 0;           /* bit 8: 1 if IRET    */

    p = reglst;
    if ( info->regslist ) {
        int cnt = *info->regslist;
        regs = info->regslist + cnt;
        for ( ; cnt; regs--, cnt-- ) {
            GetResWName( *regs, p );
            p += strlen( p );
            if ( cnt != 1 )
                *p++ = ',';
        }
    }
    *p = NULLC;
    //strcat( reglst, ">" );

    /* v2.07: make the numeric arguments more Masm-compatible */
    //sprintf( buffer,"%s %s, %02XH, %02XH, %02XH, <<%s>>, <%s>", ModuleInfo.proc_epilogue,
    sprintf( buffer,"%s, 0%XH, 0%XH, 0%XH, <<%s>>, <%s>",
            CurrProc->sym.name, flags, info->parasize, info->localsize,
            reglst, info->prologuearg ? info->prologuearg : "" );
    i = Token_Count + 1;
    Tokenize( buffer, i, tokenarray, TOK_RESCAN );

    /* if -EP is on, emit "epilogue: none" */
    if ( Options.preprocessor_stdout )
        printf( "option epilogue:none\n" );

    //RunMacro( dir, buffer, NULL, FALSE, &is_exitm );
    RunMacro( dir, i, tokenarray, NULL, -1, &is_exitm );
    Token_Count = i - 1;
    return( NOT_ERROR );
}

/* a RET <nnnn> or IRET/IRETD has occured inside a PROC.
 * count = number of tokens in buffer (=Token_Count)
 * it's ensured already that ModuleInfo.proc_epilogue isn't NULL.
 */
ret_code RetInstr( int i, struct asm_tok tokenarray[], int count )
/****************************************************************/
{
    struct proc_info   *info;
    bool        is_iret = FALSE;
    char        *p;
#ifdef DEBUG_OUT
    ret_code    rc;
#endif
    char        buffer[MAX_LINE_LEN]; /* stores modified RETN/RETF/IRET instruction */

    DebugMsg1(( "RetInstr() enter\n" ));

#if AMD64_SUPPORT
    if( tokenarray[i].tokval == T_IRET || tokenarray[i].tokval == T_IRETD || tokenarray[i].tokval == T_IRETQ )
#else
    if( tokenarray[i].tokval == T_IRET || tokenarray[i].tokval == T_IRETD )
#endif
        is_iret = TRUE;

    if ( ModuleInfo.epiloguemode == PEM_MACRO ) {
#if FASTPASS
        /* don't run userdefined epilogue macro if pass > 1 */
        if ( UseSavedState ) {
            if ( Parse_Pass > PASS_1 ) {
                DebugMsg(( "RetInstr() exit\n" ));
                //return( NOT_ERROR );
                return( ParseLine( tokenarray ) );
            }
            /* handle the current line as if it is REPLACED by the macro content */
            *(LineStoreCurr->line) = ';';
        }
#endif
#ifdef DEBUG_OUT
        rc = write_userdef_epilogue( is_iret, tokenarray );
        DebugMsg(( "RetInstr() exit\n" ));
        return( rc );
#else
        return( write_userdef_epilogue( is_iret, tokenarray ) );
#endif
    }

    if ( ModuleInfo.list ) {
        LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );
    }

    strcpy( buffer, tokenarray[i].string_ptr );
    p = buffer + strlen( buffer );

    NewLineQueue();

    write_default_epilogue();

    info = CurrProc->e.procinfo;

    /* skip this part for IRET */
    if( is_iret == FALSE ) {
        if ( CurrProc->sym.mem_type == MT_FAR )
            *p++ = 'f';   /* ret -> retf */
        else
            *p++ = 'n';     /* ret -> retn */
    }
    i++; /* skip directive */
    if ( info->parasize || ( count != i ) )
        *p++ = ' ';
    *p = NULLC;
    /* RET without argument? Then calculate the value */
    if( is_iret == FALSE && count == i ) {
        if ( ModuleInfo.epiloguemode != PEM_NONE ) {
            switch( CurrProc->sym.langtype ) {
            case LANG_BASIC:
            case LANG_FORTRAN:
            case LANG_PASCAL:
                if( info->parasize != 0 ) {
                    sprintf( p, "%d%c", info->parasize, ModuleInfo.radix != 10 ? 't' : NULLC );
                }
                break;
            case LANG_FASTCALL:
                fastcall_tab[ModuleInfo.fctype].handlereturn( CurrProc, buffer );
                break;
            case LANG_STDCALL:
                if( !info->is_vararg && info->parasize != 0 ) {
                    sprintf( p, "%d%c", info->parasize, ModuleInfo.radix != 10 ? 't' : NULLC  );
                }
                break;
            }
        }
    } else {
        /* v2.04: changed. Now works for both RET nn and IRETx */
        /* v2.06: changed. Now works even if RET has ben "renamed" */
        strcpy( p, tokenarray[i].tokpos );
    }
    AddLineQueue( buffer );
    RunLineQueue();

    DebugMsg1(( "RetInstr() exit\n" ));

    return( NOT_ERROR );
}

/* init this module. called for every pass. */

void ProcInit( void )
/*******************/
{
    ProcStack = NULL;
    CurrProc  = NULL;
    procidx = 1;
    DefineProc = FALSE;
    /* v2.06: no forward references in INVOKE if -Zne is set */
    ModuleInfo.invoke_exprparm = ( Options.strict_masm_compat ? EXPF_NOLCREATE : 0 );
#if AMD64_SUPPORT
    unw_segs_defined = 0;
#endif
}
