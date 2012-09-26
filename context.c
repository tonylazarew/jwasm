/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  Processing of PUSHCONTEXT and POPCONTEXT directives.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "assume.h"
#include "expreval.h"
#include "fastpass.h"
#include "listing.h"

enum {
    CONT_ASSUMES   = 0x01,
    CONT_RADIX     = 0x02,
    CONT_LISTING   = 0x04,
    CONT_CPU       = 0x08,
    CONT_ALIGNMENT = 0x10, /* new for v2.0, specific for JWasm */
};

static char *contextnames[] = {
    "ASSUMES", "RADIX", "LISTING", "CPU", "ALIGNMENT", NULL };

#if AMD64_SUPPORT
#define NUM_STDREGS 16
#else
#define NUM_STDREGS 8
#endif

struct assumes_context {
    struct assume_info SegAssumeTable[NUM_SEGREGS];
    struct assume_info StdAssumeTable[NUM_STDREGS];
    struct stdassume_typeinfo type_content[NUM_STDREGS];
};

struct listing_context {
    enum listmacro list_macro;
    unsigned char list:1;
    unsigned char cref:1;
    unsigned char listif:1;
    unsigned char list_generated_code:1;
};

struct cpu_context {
    short cpu;              /* saved ModuleInfo.cpu      */
    enum cpu_info curr_cpu; /* saved ModuleInfo.curr_cpu */
};

struct radix_context {
    uint_8 radix; /* saved ModuleInfo.radix */
};

struct alignment_context {
    uint_8 fieldalign; /* saved ModuleInfo.fieldalign */
    uint_8 procalign;  /* saved ModuleInfo.procalign */
};

struct context {
    struct context *next;
    uint_8 flags;
    struct radix_context   rc;
    struct alignment_context alc;
    struct listing_context lc;
    struct cpu_context     cc;
    struct assumes_context ac; /* must be last member */
};

extern struct asym *sym_Cpu;

static struct context *ContextStack;

#if FASTPASS
static int saved_numcontexts;
static struct context *saved_contexts;
#endif

static int GetContextSize( uint_8 flags )
/***************************************/
{
    if ( flags & CONT_ASSUMES )
        return( sizeof( struct context ) );
    /* spare the large assumes context space if not needed */
    return( sizeof( struct context ) - sizeof( struct assumes_context ) );
}

ret_code ContextDirective( int i, struct asm_tok tokenarray[] )
/*************************************************************/
{
    int type;
    int start = i;
    int directive = tokenarray[i].tokval;

    uint_8 flags = 0;
    uint_8 all;
    struct context *pcontext;

    all = CONT_ASSUMES | CONT_RADIX | CONT_LISTING | CONT_CPU;
    if ( Options.strict_masm_compat == FALSE )
        all |= CONT_ALIGNMENT;

    DebugMsg(( "%s directive enter\n", tokenarray[i].string_ptr ));
    i++;
    while ( tokenarray[i].token == T_ID ) {
        char **p;
        for ( p = contextnames, type = 0; *p ; p++, type++ ) {
            if ( _stricmp(*p, tokenarray[i].string_ptr ) == 0 ) {
                flags |= ( 1 << type );
                break;
            }
        }
        if ( *p == NULL )
            if ( _stricmp( "ALL", tokenarray[i].string_ptr ) == 0 ) {
                flags |= all;
            } else {
                break;
            }
        /* reject ALIGNMENT if strict masm compat is on */
        if ( Options.strict_masm_compat && ( flags & CONT_ALIGNMENT ) )
            break;
        i++;
        if ( tokenarray[i].token == T_COMMA && tokenarray[i+1].token != T_FINAL )
            i++;
    }

    if ( tokenarray[i].token != T_FINAL || flags == 0 ) {
        EmitErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    switch ( directive ) {
    case  T_POPCONTEXT:
        DebugMsg(( "POPCONTEXT flags=%X\n", flags ));
        /* for POPCONTEXT, check if the items are pushed */
        pcontext = ContextStack;
        if ( pcontext == NULL || ( pcontext->flags & flags ) != flags ) {
            EmitErr( UNMATCHED_BLOCK_NESTING, tokenarray[start].string_ptr );
            return( ERROR );
        }
        ContextStack = pcontext->next;

        /* restore the values */
        if ( flags & CONT_ASSUMES ) {
            SetSegAssumeTable( pcontext->ac.SegAssumeTable );
            SetStdAssumeTable( pcontext->ac.StdAssumeTable, pcontext->ac.type_content );
        }
        if ( flags & CONT_RADIX ) {
            ModuleInfo.radix = pcontext->rc.radix;
        }
        if ( flags & CONT_ALIGNMENT ) {
            ModuleInfo.fieldalign = pcontext->alc.fieldalign;
            ModuleInfo.procalign  = pcontext->alc.procalign;
        }
        if ( flags & CONT_LISTING ) {
            ModuleInfo.list_macro = pcontext->lc.list_macro;
            ModuleInfo.list       = pcontext->lc.list;
            ModuleInfo.cref       = pcontext->lc.cref;
            ModuleInfo.listif     = pcontext->lc.listif;
            ModuleInfo.list_generated_code = pcontext->lc.list_generated_code;
        }
        if ( flags & CONT_CPU ) {
            ModuleInfo.cpu      = pcontext->cc.cpu;
            if ( sym_Cpu )
                sym_Cpu->value  = pcontext->cc.cpu;
            ModuleInfo.curr_cpu = pcontext->cc.curr_cpu;
        }

        LclFree( pcontext );
        break;
    case  T_PUSHCONTEXT:
        DebugMsg(( "PUSHCONTEXT flags=%X\n", flags ));
        /* setup a context item */
        pcontext = LclAlloc( GetContextSize( flags) );
        pcontext->flags = flags;

        if ( flags & CONT_ASSUMES ) {
            GetSegAssumeTable( pcontext->ac.SegAssumeTable );
            GetStdAssumeTable( pcontext->ac.StdAssumeTable, pcontext->ac.type_content );
        }
        if ( flags & CONT_RADIX ) {
            pcontext->rc.radix = ModuleInfo.radix;
        }
        if ( flags & CONT_ALIGNMENT ) {
            pcontext->alc.fieldalign = ModuleInfo.fieldalign;
            pcontext->alc.procalign  = ModuleInfo.procalign;
        }
        if ( flags & CONT_LISTING ) {
            pcontext->lc.list_macro = ModuleInfo.list_macro;
            pcontext->lc.list       = ModuleInfo.list;
            pcontext->lc.cref       = ModuleInfo.cref;
            pcontext->lc.listif     = ModuleInfo.listif;
            pcontext->lc.list_generated_code = ModuleInfo.list_generated_code;
        }
        if ( flags & CONT_CPU ) {
            pcontext->cc.cpu      = ModuleInfo.cpu;
            pcontext->cc.curr_cpu = ModuleInfo.curr_cpu;
        }
        pcontext->next = ContextStack;
        ContextStack = pcontext;
        break;
    }
    return( NOT_ERROR );
}

#if FASTPASS

/* save current context status */

void ContextSaveState( void )
/***************************/
{
    int i;
    uint_32 size = 0;
    struct context *p;
    struct context *p2;

    for ( i = 0, p=ContextStack ; p ; i++, p = p->next )
        size += GetContextSize( p->flags );

    saved_numcontexts = i;
    if ( i ) {
        saved_contexts = LclAlloc( size );
        for ( p = ContextStack, p2 = saved_contexts ; p ; p = p->next ) {
            memcpy( p2, p, GetContextSize( p->flags ) );
            p2 = (struct context *)((char *)p2 + GetContextSize ( p->flags ) );
        }
    }
}

/* restore context status */

static void ContextRestoreState( void )
/*************************************/
{
    int i;
    int size;
    struct context *p;
    struct context *p2;
    struct context *p3 = NULL;

    for ( i = saved_numcontexts, p2 = saved_contexts; i ; i-- ) {
        size = GetContextSize( p2->flags );
        p = LclAlloc( size );
        if ( p3 == NULL )
            ContextStack = p;
        else
            p3->next = p;
        p3 = p;
        memcpy( p, p2, size );
        p->next = NULL;
        p2 = (struct context *)((char *)p2 + size );
    }
}

#endif

/* init context, called once per pass */

void ContextInit( int pass )
/**************************/
{
    ContextStack = NULL;
#if FASTPASS
    if ( pass == PASS_1 )
        saved_numcontexts = 0;
    else {
        ContextRestoreState();
    }
#endif
}


