/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  handles line numbers if -Zd or -Zi is set.
*
****************************************************************************/

#include <ctype.h>
#include <time.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "input.h"
#include "tokenize.h"
#include "segment.h"
#include "assume.h"
#include "proc.h"
#include "expreval.h"
#include "extern.h"
#include "fastpass.h"
#include "msgtext.h"
#include "myassert.h"
#include "linnum.h"

extern struct qdesc   LinnumQueue;    /* queue of line_num_info items */
extern int            procidx;

static struct asym    *dmyproc;
static uint_32        lastLineNumber;

static void AddLinnumData( struct line_num_info *data )
/*****************************************************/
{
    struct qdesc *q;
#if COFF_SUPPORT
    if ( Options.output_format == OFORMAT_COFF ) {
        q = (struct qdesc *)CurrSeg->e.seginfo->LinnumQueue;
        if ( q == NULL ) {
            q = LclAlloc( sizeof( struct qdesc ) );
            CurrSeg->e.seginfo->LinnumQueue = q;
            q->head = NULL;
        }
    } else
#endif
        q = &LinnumQueue;

    data->next = NULL;
    if ( q->head == NULL)
        q->head = q->tail = data;
    else {
        ((struct line_num_info *)q->tail)->next = data;
        q->tail = data;
    }
}

void AddLinnumDataRef( uint_32 line_num )
/***************************************/
/* store a reference for the current line at the current address */
{
    struct line_num_info    *curr;

    /* COFF line number info is related to functions/procedures. Since
     * assembly allows code lines outside of procs, "dummy" procs must
     * be generated. A dummy proc lasts until a true PROC is detected or
     * the source file changes.
     */
#if COFF_SUPPORT
    if ( Options.output_format == OFORMAT_COFF &&
        CurrProc == NULL &&
        ( dmyproc == NULL ||
        dmyproc->debuginfo->file != get_curr_srcfile() ) ) {
        char procname[10];
        if ( dmyproc ) {
            myassert( dmyproc->segment );
            dmyproc->total_size =
                ((struct dsym *)dmyproc->segment)->e.seginfo->current_loc -
                dmyproc->offset;
        }
        sprintf( procname, "$$$%05u", procidx );
        DebugMsg(("AddLinnumDataRef: generating proc=%s\n", procname ));
        dmyproc = SymSearch( procname );

        /* in pass 1, create the proc */
        if ( dmyproc == NULL ) {
            dmyproc = CreateProc( NULL, procname, TRUE );
            DebugMsg(("AddLinnumDataRef: new proc %s created\n", procname ));
            dmyproc->isproc = TRUE; /* fixme: should be set inside CreateProc */
            dmyproc->included = TRUE;
            AddPublicData( dmyproc );
        } else
            procidx++; /* for passes > 1, adjust procidx */

        /* if the symbols isn't a PROC, the symbol name has been used
         * by the user - bad! A warning should be displayed */
        if ( dmyproc->isproc == TRUE ) {
            SetSymSegOfs( dmyproc );
            dmyproc->Ofssize = ModuleInfo.Ofssize;
            dmyproc->langtype = ModuleInfo.langtype;
            if ( write_to_file == TRUE ) {
                curr = LclAlloc( sizeof( struct line_num_info ) );
                curr->sym = dmyproc;
                curr->line_number = LineNumber;
                curr->file = get_curr_srcfile();
                curr->number = 0;
                DebugMsg(("AddLinnumDataRef: sym=%s (#=%u.%u)\n", curr->sym->name, curr->file, curr->line_number ));
                AddLinnumData( curr );
            }
        }
    }
#endif

    if(  line_num && ( write_to_file == FALSE || lastLineNumber == line_num )) {
#ifdef DEBUG_OUT
        if ( write_to_file == TRUE )
            DebugMsg(("AddLinnumDataRef(#=%u) line skipped, lastline=%u\n", line_num, lastLineNumber ));
#endif
        return;
    }
    DebugMsg(("AddLinnumDataRef(#=%u) enter, currofs=%Xh, CurrProc=%s, GeneratedCode=%u\n", line_num, GetCurrOffset(), CurrProc ? CurrProc->sym.name : "NULL", GeneratedCode ));
    curr = LclAlloc( sizeof( struct line_num_info ) );
    curr->number = line_num;
    if ( line_num == 0 ) { /* happens for COFF only */
        /* changed v2.03 (CurrProc might have been NULL) */
        /* if ( Options.output_format == OFORMAT_COFF && CurrProc->sym.public == FALSE ) { */
#if COFF_SUPPORT
        if ( Options.output_format == OFORMAT_COFF && CurrProc && CurrProc->sym.public == FALSE ) {
            CurrProc->sym.included = TRUE;
            AddPublicData( (struct asym *)CurrProc );
        }
#endif
        /* changed v2.03 */
        /* curr->sym = (struct asym *)CurrProc; */
        if ( CurrProc )
            curr->sym = (struct asym *)CurrProc;
        else
            curr->sym = (struct asym *)dmyproc;
        curr->line_number = LineNumber;
        curr->file        = get_curr_srcfile();
        /* set the function's size! */
        if ( dmyproc ) {
            myassert( dmyproc->segment );
            dmyproc->total_size =
                ((struct dsym *)dmyproc->segment)->e.seginfo->current_loc -
                dmyproc->offset;
        }
        dmyproc = NULL;
    } else {
        curr->offset = GetCurrOffset();
        curr->srcfile = get_curr_srcfile();
    }
    lastLineNumber = line_num;
    AddLinnumData( curr );

    return;
}

void QueueDeleteLinnum( struct qdesc *queue )
/*******************************************/
{
    struct line_num_info    *curr;
    struct line_num_info    *next;

    if ( queue == NULL )
        return;
    curr = queue->head;
    for( ; curr ; curr = next ) {
        next = curr->next;
        LclFree( curr );
    }
    return;
}

/* if -Zd is set and there is trailing code not inside
 * a function, set the dummy function's length now.
 */
void LinnumFini( void )
/*********************/
{
    if ( dmyproc ) {
        dmyproc->total_size =
            ((struct dsym *)dmyproc->segment)->e.seginfo->current_loc -
            dmyproc->offset;
        DebugMsg(("OnePass: last dummy proc size=%Xh\n"));
    }
}

void LinnumInit( void )
/*********************/
{
    lastLineNumber = 0;
    dmyproc = NULL;
}
