/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  assemble a module.
*
****************************************************************************/

#include <ctype.h>
#include <time.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "reswords.h"
#include "input.h"
#include "tokenize.h"
#include "condasm.h"
#include "segment.h"
#include "assume.h"
#include "proc.h"
#include "expreval.h"
#include "hll.h"
#include "context.h"
#include "types.h"
#include "labels.h"
#include "macro.h"
#include "extern.h"
#include "fixup.h"
#include "omf.h"
#include "fastpass.h"
#include "listing.h"
#include "msgtext.h"
#include "myassert.h"
#include "linnum.h"
#include "cpumodel.h"
#if DLLIMPORT
#include "mangle.h"
#endif

#if COFF_SUPPORT
#include "coff.h"
#endif
#if ELF_SUPPORT
#include "elf.h"
#endif
#if BIN_SUPPORT
#include "bin.h"
#endif

#if 1 //def __SW_BD
#include <setjmp.h>
jmp_buf jmpenv;
#endif

#ifdef __SW_BD
#define EXPQUAL __stdcall
#else
#define EXPQUAL
#endif

#define USELSLINE 1 /* must match switch in listing.c! */

extern void             ProcCheckOpen( void );
extern void             SortSegments( void );

extern uint_32          LastCodeBufSize;
extern char             *DefaultDir[NUM_FILE_TYPES];
extern const char       *ModelToken[];
#if FASTMEM==0
extern void             FreeLibQueue();
#endif

#ifdef DEBUG_OUT
extern int lq_line;
#endif

/* fields: next, name, segment, offset/value */
struct asym WordSize = { NULL,"@WordSize", 0 };

/* names for output formats. order must match enum oformat */
static const struct format_options formatoptions[] = {
    { NULL,     BIN_DISALLOWED,  "BIN"  },
    { NULL,     OMF_DISALLOWED,  "OMF"  },
#if COFF_SUPPORT
    { NULL,     COFF32_DISALLOWED, "COFF" },
#endif
#if ELF_SUPPORT
    { elf_init, ELF32_DISALLOWED,  "ELF"  },
#endif
};
#if AMD64_SUPPORT
#if COFF_SUPPORT
const struct format_options coff64_fmtopt = { NULL, COFF64_DISALLOWED, "PE32+" };
#endif
#if ELF_SUPPORT
const struct format_options elf64_fmtopt = { elf_init, ELF64_DISALLOWED,  "ELF64"  };
#endif
#endif

struct module_info      ModuleInfo;
unsigned int            Parse_Pass;     /* assembly pass */
unsigned int            GeneratedCode;
struct qdesc            LinnumQueue;    /* queue of line_num_info items */

bool write_to_file;     /* write object module */

/* Write a byte to the segment buffer.
 * in OMF, the segment buffer is flushed when the max. record size is reached.
 */
void OutputByte( unsigned char byte )
/***********************************/
{
    if( write_to_file == TRUE ) {
        uint_32 idx = CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc;
#ifdef DEBUG_OUT
        if ( CurrSeg->e.seginfo->current_loc < CurrSeg->e.seginfo->start_loc ) {
            //_asm int 3;
        }
#endif
        myassert( CurrSeg->e.seginfo->current_loc >= CurrSeg->e.seginfo->start_loc );
        if( Options.output_format == OFORMAT_OMF && idx >= MAX_LEDATA_THRESHOLD ) {
            omf_FlushCurrSeg();
            idx = CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc;
        }
        //DebugMsg(("OutputByte: buff=%p, idx=%" FX32 ", byte=%X, codebuff[0]=%X\n", CurrSeg->e.seginfo->CodeBuffer, idx, byte, *CurrSeg->e.seginfo->CodeBuffer ));
        CurrSeg->e.seginfo->CodeBuffer[idx] = byte;
    }
#if 1
    /* check this in pass 1 only */
    else if( CurrSeg->e.seginfo->current_loc < CurrSeg->e.seginfo->start_loc ) {
        DebugMsg(("OutputByte: segment start loc changed from %" FX32 "h to %" FX32 "h\n",
                  CurrSeg->e.seginfo->start_loc,
                  CurrSeg->e.seginfo->current_loc));
        CurrSeg->e.seginfo->start_loc = CurrSeg->e.seginfo->current_loc;
    }
#endif
    CurrSeg->e.seginfo->current_loc++;
    CurrSeg->e.seginfo->bytes_written++;
    CurrSeg->e.seginfo->written = TRUE;
    if( CurrSeg->e.seginfo->current_loc > CurrSeg->sym.max_offset )
        CurrSeg->sym.max_offset = CurrSeg->e.seginfo->current_loc;
}

#if 0 /* v2.03: OutputCodeByte is obsolete */
void OutputCodeByte( unsigned char byte )
/***************************************/
{
    // if ( ModuleInfo.CommentDataInCode )
    // omf_OutSelect( FALSE );
    OutputByte( byte );
}
#endif

void FillDataBytes( unsigned char byte, int len )
/***********************************************/
{
    if ( ModuleInfo.CommentDataInCode )
        omf_OutSelect( TRUE );
    for( ; len; len-- )
        OutputByte( byte );
}

/*
 * this function is to output (small, <= 8) amounts of bytes which must
 * not be separated ( for omf, because of fixups )
 */

void OutputBytes( const unsigned char *pbytes, int len, struct fixup *fixup )
/***************************************************************************/
{
    if( write_to_file == TRUE ) {
        uint_32 idx = CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc;
#if 0 /* def DEBUG_OUT */
        if ( CurrSeg->e.seginfo->current_loc < CurrSeg->e.seginfo->start_loc )
            _asm int 3;
#endif
        myassert( CurrSeg->e.seginfo->current_loc >= CurrSeg->e.seginfo->start_loc );
        if( Options.output_format == OFORMAT_OMF && ((idx + len) > MAX_LEDATA_THRESHOLD ) ) {
            omf_FlushCurrSeg();
            idx = CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc;
        }
        if ( fixup )
            store_fixup( fixup, (int_32 *)pbytes );
        //DebugMsg(("OutputBytes: buff=%p, idx=%" FX32 ", byte=%X\n", CurrSeg->e.seginfo->CodeBuffer, idx, *pbytes ));
        memcpy( &CurrSeg->e.seginfo->CodeBuffer[idx], pbytes, len );
    }
#if 1
    /* check this in pass 1 only */
    else if( CurrSeg->e.seginfo->current_loc < CurrSeg->e.seginfo->start_loc ) {
        DebugMsg(("OutputBytes: segment start loc changed from %" FX32 "h to %" FX32 "h\n",
                  CurrSeg->e.seginfo->start_loc,
                  CurrSeg->e.seginfo->current_loc));
        CurrSeg->e.seginfo->start_loc = CurrSeg->e.seginfo->current_loc;
    }
#endif
    CurrSeg->e.seginfo->current_loc += len;
    CurrSeg->e.seginfo->bytes_written += len;
    CurrSeg->e.seginfo->written = TRUE;
    if( CurrSeg->e.seginfo->current_loc > CurrSeg->sym.max_offset )
        CurrSeg->sym.max_offset = CurrSeg->e.seginfo->current_loc;
}

/* set current offset in a segment (usually CurrSeg) without to write anything */

ret_code SetCurrOffset( struct dsym *seg, uint_32 value, bool relative, bool select_data )
/****************************************************************************************/
{
    if( relative )
        value += seg->e.seginfo->current_loc;

    if ( Options.output_format == OFORMAT_OMF ) {
        if ( seg == CurrSeg ) {
            if ( write_to_file == TRUE )
                omf_FlushCurrSeg( );

        /* for debugging, tell if data is located in code sections*/
            if( select_data )
                if ( ModuleInfo.CommentDataInCode )
                    omf_OutSelect( TRUE );
            LastCodeBufSize = value;
        }
        seg->e.seginfo->start_loc = value;
    /* for -bin, if there's an ORG (relative==false) and no initialized data
     * has been set yet, set start_loc!
     * v1.96: this is now also done for COFF and ELF
     */
    /* else if ( Options.output_format == OFORMAT_BIN && relative == FALSE ) { */
    } else {
        if ( write_to_file == FALSE ) {
            if ( relative ) {
#if 0 /* don't include "preceding" uninitialized data */
                if( seg->e.seginfo->current_loc < seg->e.seginfo->start_loc )
                    seg->e.seginfo->start_loc = seg->e.seginfo->current_loc;
#endif
            } else {
                if ( seg->e.seginfo->bytes_written == 0 )
                    seg->e.seginfo->start_loc = value;
            }
        }
    }

    seg->e.seginfo->current_loc = value;
    seg->e.seginfo->written = FALSE;

    if( seg->e.seginfo->current_loc > seg->sym.max_offset )
        seg->sym.max_offset = seg->e.seginfo->current_loc;

    return( NOT_ERROR );
}

/* finish module writes
 * for OMF, just write the MODEND record
 * for COFF,ELF and BIN, write the section data and symbol table
 */
static ret_code WriteContent( void )
/**********************************/
{
    DebugMsg(("WriteContent enter\n"));
    switch ( Options.output_format ) {
    case OFORMAT_OMF:
        //if( ModendRec == NULL ) {
        //    EmitError( UNEXPECTED_END_OF_FILE );
        //    return( ERROR );
        //}
        /* -if Zi is set, write symbols and types */
        if ( Options.debug_symbols )
            omf_write_debug_tables();
        omf_write_modend( ModuleInfo.start_fixup, ModuleInfo.start_displ );
        break;
#if COFF_SUPPORT
    case OFORMAT_COFF:
        coff_write_data( &ModuleInfo );
        coff_write_symbols( &ModuleInfo );
        break;
#endif
#if ELF_SUPPORT
    case OFORMAT_ELF:
        elf_write_data( &ModuleInfo );
        break;
#endif
#if BIN_SUPPORT
    case OFORMAT_BIN:
        bin_write_data( &ModuleInfo );
        break;
#endif
    }
#if DLLIMPORT
    if ( Options.names[OPTN_LNKDEF_FN] ) {
        FILE *ld;
        struct dsym *curr;
        ld = fopen( Options.names[OPTN_LNKDEF_FN], "w" );
        if ( ld == NULL ) {
            EmitErr( CANNOT_OPEN_FILE, Options.names[OPTN_LNKDEF_FN], ErrnoStr() );
            return( ERROR );
        }
        for ( curr = SymTables[TAB_EXT].head; curr != NULL ; curr = curr->next ) {
            DebugMsg(("WriteContent: ext=%s, isproc=%u, weak=%u\n", curr->sym.name, curr->sym.isproc, curr->sym.weak ));
            if ( curr->sym.isproc && ( curr->sym.weak == FALSE || curr->sym.iat_used ) &&
                curr->sym.dllname && *curr->sym.dllname != NULLC ) {
                int size;
                Mangle( &curr->sym, StringBufferEnd );
                size = sprintf( CurrSource, "import '%s'  %s.%s\n", StringBufferEnd, curr->sym.dllname, curr->sym.name );
                if ( fwrite( CurrSource, 1, size, ld ) != size )
                    WriteError();
            }
        }
        fclose( ld );
    }
#endif
    DebugMsg(("WriteContent exit\n"));
    return( NOT_ERROR );
}

#if BIN_SUPPORT || PE_SUPPORT
static ret_code CheckExternal( void )
/***********************************/
{
    struct dsym *curr;
    for ( curr = SymTables[TAB_EXT].head; curr != NULL ; curr = curr->next )
        if( curr->sym.weak == FALSE || curr->sym.used == TRUE ) {
            DebugMsg(("CheckExternal: error, %s weak=%u\n", curr->sym.name, curr->sym.weak ));
            EmitErr( FORMAT_DOESNT_SUPPORT_EXTERNALS, curr->sym.name );
            return( ERROR );
        }
    return( NOT_ERROR );
}
#endif

/*
 * write the OMF/COFF/ELF header
 * for OMF, this is called twice, once after Pass 1 is done
 * and then again after assembly has finished without errors.
 * for COFF/ELF/BIN, it's just called once after assembly passes.
 */
static ret_code WriteHeader( bool initial )
/*****************************************/
{
    ret_code    rc = NOT_ERROR;
    struct dsym *curr;

    DebugMsg(("WriteHeader(%u) enter\n", initial));

    /* check limit of segments */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->Ofssize == USE16 && curr->sym.max_offset > 0x10000 ) {
            if ( Options.output_format == OFORMAT_OMF )
                EmitErr( SEGMENT_EXCEEDS_64K_LIMIT, curr->sym.name );
            else
                EmitWarn( 2, SEGMENT_EXCEEDS_64K_LIMIT, curr->sym.name );
        }
    }

    switch ( Options.output_format ) {
    case OFORMAT_OMF:
        if ( initial == TRUE ) {
            omf_write_header();
            /* if( Options.no_dependencies == FALSE ) */
            if( Options.line_numbers )
                omf_write_autodep();
            if( ModuleInfo.segorder == SEGORDER_DOSSEG )
                omf_write_dosseg();
            else if( ModuleInfo.segorder == SEGORDER_ALPHA )
                SortSegments();
            omf_write_lib();
            omf_write_lnames();
        }
        omf_write_seg( initial );
        if ( initial == TRUE ) {
            omf_write_grp();
            omf_write_extdef();
            omf_write_comdef();
            omf_write_alias();
        }
        omf_write_public( initial );
        if ( initial == TRUE ) {
            omf_write_export();
            omf_end_of_pass1();
        }
        break;
#if COFF_SUPPORT
    case OFORMAT_COFF:
#if PE_SUPPORT
        if ( ModuleInfo.header_format == HFORMAT_PE32
#if AMD64_SUPPORT
            || ModuleInfo.header_format == HFORMAT_PE64
#endif
           ) {
            rc = CheckExternal();
            break;
        }
#endif
        coff_write_header( &ModuleInfo );
        coff_write_section_table( &ModuleInfo );
        break;
#endif
#if ELF_SUPPORT
    case OFORMAT_ELF:
        elf_write_header( &ModuleInfo );
        break;
#endif
#if BIN_SUPPORT
    case OFORMAT_BIN:
        rc = CheckExternal(); /* check if externals are used */
        break;
#endif
#ifdef DEBUG_OUT
    default:
        /* this shouldn't happen */
        printf("unknown output format: %u\n", Options.output_format);
        rc = ERROR;
#endif
    }
    DebugMsg(("WriteHeader exit, rc=%d\n", rc));
    return( rc );
}

#define is_valid_first_char( ch )  ( isalpha(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' || ch=='.' )

static int is_valid_identifier( char *id )
/****************************************/
{
    /* special handling of first char of an id: it can't be a digit,
     but can be a dot (don't care about ModuleInfo.dotname!). */

    if( is_valid_first_char( *id ) == 0 )
        return( ERROR );
    id++;
    for( ; *id != NULLC; id++ ) {
        if ( is_valid_id_char( *id ) == FALSE )
            return( ERROR );
    }
    /* don't allow a single dot! */
    if ( *(id-1) == '.' )
        return( ERROR );

    return( NOT_ERROR );
}

/* add text macros defined with the -D cmdline switch */

static void add_cmdline_tmacros( void )
/****************************************/
{
    struct qitem *p;
    char *name;
    char *value;
    int len;
    struct asym *sym;

    DebugMsg(("add_cmdline_tmacros enter\n"));
    for ( p = Options.queues[OPTQ_MACRO]; p; p = p->next ) {
        DebugMsg(("add_cmdline_tmacros: found >%s<\n", p->value));
        name = p->value;
        value = strchr( name, '=' );
        if( value == NULL ) {
            /* v2.06: ensure that 'value' doesn't point to r/o space */
            //value = "";
            value = name + strlen( name ); /* use the terminating NULL */
        } else {
            len = value - name;
            name = (char *)myalloca( len + 1 );
            memcpy( name, p->value, len );
            *(name + len) = NULLC;
            value++;
        }

        /* there's no check whether the name is a reserved word!
         */
        if( is_valid_identifier( name ) == ERROR ) {
            DebugMsg(("add_cmdline_tmacros: name >%s< invalid\n", name ));
            EmitErr( SYNTAX_ERROR_EX, name );
        } else {
            sym = SymSearch( name );
            if ( sym == NULL ) {
                sym = SymCreate( name );
                sym->state = SYM_TMACRO;
            }
            if ( sym->state == SYM_TMACRO ) {
                sym->isdefined = TRUE;
                sym->predefined = TRUE;
                sym->string_ptr = value;
            } else
                EmitErr( SYMBOL_ALREADY_DEFINED, name );
        }
    }
    return;
}

/* add the include paths set by -I option */

static void add_incpaths( void )
/******************************/
{
    struct qitem *p;
    DebugMsg(("add_incpaths: enter\n"));
    for ( p = Options.queues[OPTQ_INCPATH]; p; p = p->next ) {
        AddStringToIncludePath( p->value );
    }
}

/* this is called for every pass.
 * symbol table and ModuleInfo are initialized.
 */
static void CmdlParamsInit( int pass )
/************************************/
{
    struct qitem *pq;
    DebugMsg(("CmdlParamsInit(%u) enter\n", pass));

#if BUILD_TARGET
    if ( pass == PASS_1 ) {
        struct asym *sym;
        char *tmp;
        char *p;

        _strupr( Options.build_target );
        tmp = myalloca( strlen( Options.build_target ) + 5 ); /* null + 4 uscores */
        strcpy( tmp, uscores );
        strcat( tmp, Options.build_target );
        strcat( tmp, uscores );

        /* define target */
        sym = CreateVariable( tmp, 0 );
        sym->predefined = TRUE;

        p = NULL;
        if( stricmp( Options.build_target, "DOS" ) == 0 ) {
            p = "__MSDOS__";
        } else if( stricmp( Options.build_target, "NETWARE" ) == 0 ) {
            if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                p = "__NETWARE_386__";
            } else {
                /* do nothing ... __NETWARE__ already defined */
            }
        } else if( stricmp( Options.build_target, "WINDOWS" ) == 0 ) {
            if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                p = "__WINDOWS_386__";
            } else {
                /* do nothing ... __WINDOWS__ already defined */
            }
        } else if( stricmp( Options.build_target, "QNX" ) == 0 ) {
            p = "__UNIX__";
        } else if( stricmp( Options.build_target, "LINUX" ) == 0 ) {
            p = "__UNIX__";
        }
        if ( p ) {
            sym = CreateVariable( p, 0 );
            sym->predefined = TRUE;
        }
    }
#endif

    for ( pq = Options.queues[OPTQ_FINCLUDE]; pq; pq = pq->next ) {
        DebugMsg(("add_include file: %s\n", pq->value ));
        InputQueueFile( pq->value, NULL );
    }

    if ( pass == PASS_1 ) {
        char *env;
        /* v2.06: this is done in ModulePassInit now */
        //SetCPU( Options.cpu );
        add_cmdline_tmacros();
        add_incpaths();
        if ( Options.ignore_include == FALSE )
            if ( env = getenv( "INCLUDE" ) )
                AddStringToIncludePath( env );
    }
    DebugMsg(("CmdlParamsInit exit\n"));
    return;
}

void WritePreprocessedLine( const char *string )
/**********************************************/
/* print out preprocessed source lines
 */
{
    static bool PrintEmptyLine = TRUE;
    const char *p;

#if 0 /* v2.08: removed, obsolete */
    /* filter some macro specific directives */
    if ( tokenarray[0].token == T_DIRECTIVE &&
         ( tokenarray[0].tokval == T_ENDM ||
           tokenarray[0].tokval == T_EXITM))
        return;
    /* don't print generated code - with one exception:
     if the code was generated as a result of structure initialization,
     then do!
     */
    if ( GeneratedCode )
        return;
#endif
    if ( Token_Count > 0 ) {
        /* v2.08: don't print a leading % (this char is no longer filtered) */
        for ( p = string; isspace( *p ); p++ );
        printf("%s\n", *p == '%' ? p+1 : string );
        PrintEmptyLine = TRUE;
    } else if ( PrintEmptyLine ) {
        PrintEmptyLine = FALSE;
        printf("\n");
    }
}

/* set Masm v5.1 compatibility options */

void SetMasm510( bool value )
/***************************/
{
    ModuleInfo.m510 = value;
    ModuleInfo.oldstructs = value;
    /* ModuleInfo.oldmacros = value; not implemented yet */
    ModuleInfo.dotname = value;
    ModuleInfo.setif2 = value;

    if ( value ) {
        if ( ModuleInfo.model == MODEL_NONE ) {
            /* if no model is specified, set OFFSET:SEGMENT */
            ModuleInfo.offsettype = OT_SEGMENT;
            if ( ModuleInfo.langtype == LANG_NONE ) {
                ModuleInfo.scoped = FALSE;
                ModuleInfo.procs_private = TRUE;
            }
        }
    }
    return;
}

/* called for each pass */

static void ModulePassInit( void )
/********************************/
{
    enum cpu_info cpu = Options.cpu;
    enum model_type model = Options.model;

    DebugMsg(( "ModulePassInit enter\n" ));
    /* set default values not affected by the masm 5.1 compat switch */
    ModuleInfo.procs_private = FALSE;
    ModuleInfo.procs_export = FALSE;
    ModuleInfo.offsettype = OT_GROUP;
    ModuleInfo.scoped = TRUE;


#if FASTPASS
    /* v2.03: don't generate the code if fastpass is active */
    /* v2.08: query UseSavedState instead of StoreState */
    //if ( StoreState == FALSE ) {
    if ( UseSavedState == FALSE ) {
#endif
        NewLineQueue(); /* ensure line queue is empty */
        ModuleInfo.langtype = Options.langtype;
        ModuleInfo.fctype = Options.fctype;
#if AMD64_SUPPORT
        if ( ModuleInfo.header_format == HFORMAT_WIN64 || ModuleInfo.header_format == HFORMAT_ELF64 ) {
            /* v2.06: force cpu to be at least P_64, without side effect to Options.cpu */
            if ( ( cpu &  P_CPU_MASK ) < P_64 ) /* enforce cpu to be 64-bit */
                cpu = P_64;
            /* ignore -m switch for 64-bit formats.
             * there's no other model than FLAT possible.
             */
            model = MODEL_FLAT;
            if ( ModuleInfo.header_format == HFORMAT_WIN64 ) {
                if ( ModuleInfo.langtype == LANG_NONE ) {
                    ModuleInfo.langtype = LANG_FASTCALL;
                }
                ModuleInfo.fctype = FCT_WIN64;
            }
        } else
#endif
            /* if model FLAT is to be set, ensure that cpu is compat. */
            if ( model == MODEL_FLAT && ( cpu & P_CPU_MASK ) < P_386 ) /* cpu < 386? */
                cpu = P_386;

        SetCPU( cpu );
        /* table ModelToken starts with MODEL_TINY, which is index 1" */
        if ( model != MODEL_NONE )
            AddLineQueueX( "%r %s", T_DOT_MODEL, ModelToken[model - 1] );

#if FASTPASS
    }
#endif

    SetMasm510( Options.masm51_compat );
    ModuleInfo.defOfssize = USE16;
    ModuleInfo.ljmp     = TRUE;

    ModuleInfo.list   = Options.write_listing;
    ModuleInfo.cref   = TRUE;
    ModuleInfo.listif = Options.listif;
    ModuleInfo.list_generated_code = Options.list_generated_code;
    ModuleInfo.list_macro = Options.list_macro;

    ModuleInfo.case_sensitive = Options.case_sensitive;
    ModuleInfo.convert_uppercase = Options.convert_uppercase;
    SymSetCmpFunc();

    ModuleInfo.segorder = SEGORDER_SEQ;
    ModuleInfo.radix = 10;
    ModuleInfo.fieldalign = Options.fieldalign;
#if PROCALIGN
    ModuleInfo.procalign = 0;
#endif
}

void RunLineQueue( void )
/***********************/
{
    struct input_status oldstat;
    struct asm_tok *tokenarray;

    DebugMsg1(( "RunLineQueue() enter\n" ));
    /* v2.03: ensure the current source buffer is still aligned */
    tokenarray = PushInputStatus( &oldstat );
    GeneratedCode++;
    while ( ( Token_Count = GetPreprocessedLine( CurrSource, tokenarray ) ) >= 0 ) {
        if ( Token_Count )
            ParseLine( tokenarray );
    }
#ifdef DEBUG_OUT
    if ( ModuleInfo.EndDirFound == TRUE ) {
        DebugMsg(("!!!!! Error: End directive found in generated-code parser loop!\n"));
    }
    lq_line = 0;
#endif
    GeneratedCode--;
    PopInputStatus( &oldstat );

    DebugMsg1(( "RunLineQueue() exit\n" ));
    return;
}

/* this is called by InitializeStructure(), which is a special case */

void RunLineQueueEx( void )
/*************************/
{
    struct input_status oldstat;
    struct asm_tok *tokenarray;

    DebugMsg1(( "RunLineQueueEx() enter\n" ));
    tokenarray = PushInputStatus( &oldstat );
    while ( ( Token_Count = GetPreprocessedLine( CurrSource, tokenarray ) ) >= 0 ) {
        if ( Token_Count ) {
            ParseLine( tokenarray );
            /* handle special case 'structure initialization' */
            if ( Options.preprocessor_stdout == TRUE )
                WritePreprocessedLine( CurrSource );
        }
    }
#ifdef DEBUG_OUT
    if ( ModuleInfo.EndDirFound == TRUE ) {
        DebugMsg(("!!!!! Error: End directive found in generated-code parser loop!\n"));
    }
    lq_line = 0;
#endif
    PopInputStatus( &oldstat );

    DebugMsg1(( "RunLineQueueEx() exit\n" ));
    return;
}

/*
 * set index field for EXTERN/PROTO/COMM.
 * This is called after PASS 1 has been finished.
 */

static void set_ext_idx( void )
/*****************************/
{
    struct dsym *curr;
    uint        index = 0;

    DebugMsg(("set_ext_idx() enter\n"));
    /* scan ALIASes for COFF/ELF */
#if FASTPASS
#if COFF_SUPPORT || ELF_SUPPORT
    if ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
        || Options.output_format == OFORMAT_ELF
#endif
       ) {
        for( curr = SymTables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
            struct asym *sym;
            sym = curr->sym.substitute;
            /* check if symbol is external or public */
            if ( sym == NULL ||
                ( sym->state != SYM_EXTERNAL &&
                 ( sym->state != SYM_INTERNAL || sym->public == FALSE ))) {
                SkipSavedState();
                break;
            }
            /* make sure it becomes a strong external */
            if ( sym->state == SYM_EXTERNAL )
                sym->used = TRUE;
        }
    }
#endif
#endif

    /* scan the EXTERN/EXTERNDEF items */

    for( curr = SymTables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* v2.01: externdefs which have been "used" become "strong" */
        if ( curr->sym.used )
            curr->sym.weak = FALSE;
        /* skip COMM and unused EXTERNDEF/PROTO items. */
        if (( curr->sym.iscomm == TRUE ) || ( curr->sym.weak == TRUE ))
            continue;
#if FASTPASS==0
        /* v2.05: clear fixup list (used for backpatching in pass one) */
        if ( curr->sym.fixup ) {
            struct fixup *c;
            struct fixup *n;
            for( c = curr->sym.fixup ; c; ) {
                n = c->nextbp;
                LclFree( c );
                c = n;
            }
        }
#endif
        index++;
        curr->sym.ext_idx = index;
        /* optional alternate symbol must be INTERNAL or EXTERNAL.
         * COFF ( and ELF? ) also wants internal symbols public.
         */
#if FASTPASS
        if ( curr->sym.altname ) {
            if ( curr->sym.altname->state == SYM_INTERNAL ) {
#if COFF_SUPPORT || ELF_SUPPORT
                /* for COFF/ELF, the altname must be public or external */
                if ( curr->sym.altname->public == FALSE &&
                    ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
                     || Options.output_format == OFORMAT_ELF
#endif
                    ) ) {
                    SkipSavedState();
                }
#endif
            } else if ( curr->sym.altname->state != SYM_EXTERNAL ) {
                /* do not use saved state, scan full source in second pass */
                SkipSavedState();
            }
        }
#endif
    }

    /* now scan the COMM items */

    for( curr = SymTables[TAB_EXT].head; curr != NULL; curr = curr->next ) {
        if ( curr->sym.iscomm == FALSE )
            continue;
        index++;
        curr->sym.ext_idx = index;
    }

    DebugMsg(("set_ext_idx() exit\n"));
    return;
}

#if 0 /* v2.07: removed */
/* scan - and clear - global queue (EXTERNDEFs).
 * items which have been defined within the module
 * will become public.
 * PROTOs aren't included in the global queue.
 * They will become public when - and if - the PROC directive
 * for the symbol is met.
 */
static void scan_globals( void )
/******************************/
{
    struct qnode    *curr;
    struct qnode    *next;
    struct asym     *sym;

    /* turn EXTERNDEFs into PUBLICs if defined in the module.
     * PROCs are handled differently - so ignore these entries here!
     */
    /* obsolete since v2.07.
     * it's simpler and better to make the symbol public if it turns
     * from SYM_EXTERNAL to SYM_INTERNAL.
     * the other case, that is, the EXTERNDEF comes AFTER the definition,
     * is handled in ExterndefDirective()
     */
    DebugMsg(("scan_globals: GlobalQueue=%X\n", ModuleInfo.g.GlobalQueue));
    for ( curr = ModuleInfo.g.GlobalQueue.head; curr; curr = next ) {
        next = curr->next;
        sym = (struct asym *)curr->elmt;
        DebugMsg(("scan_globals: %s state=%u used=%u public=%u\n", sym->name, sym->state, sym->used, sym->public ));
        if( sym->state == SYM_INTERNAL && sym->public == FALSE && sym->isproc == FALSE ) {
            /* add it to the public queue */
            sym->public = TRUE;
            QEnqueue( &ModuleInfo.g.PubQueue, curr );
            DebugMsg(("scan_globals: %s added to public queue\n", sym->name ));
            continue; /* don't free this item! */
        }
        LclFree( curr );
    }
    /* the queue is empty now */
    ModuleInfo.g.GlobalQueue.head = NULL;
}
#endif

/* checks after pass one has been finished without errors */

static void PassOneChecks( void )
/*******************************/
{
#if FASTPASS || defined(DEBUG_OUT)
    struct dsym *curr;
#endif
#if FASTPASS
    struct qnode *q;
#endif
    /* check for open structures and segments has been done inside the
     * END directive handling already */
    ProcCheckOpen();
    HllCheckOpen();
    CondCheckOpen();

    if( ModuleInfo.EndDirFound == FALSE )
        EmitError( END_DIRECTIVE_REQUIRED );

#ifdef DEBUG_OUT
    for ( curr = SymTables[TAB_UNDEF].head; curr; curr = curr->next ) {
        DebugMsg(("PassOneChecks: undefined symbol %s\n", curr->sym.name ));
    }
#endif
#if FASTPASS
    if ( SymTables[TAB_UNDEF].head ) {
        /* to force a full second pass in case of missing symbols,
         * activate the next line. It was implemented to have proper
         * error displays if a forward reference wasn't found.
         * However, v1.95 final won't need this anymore, because both
         * filename + lineno for every line is known now in pass 2.
         */
        /* SkipSavedState(); */
    }

    /* check if there's an undefined segment reference.
     * This segment was an argument to a group definition then.
     * Just do a full second pass, the GROUP directive will report
     * the error.
     */
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        if( curr->sym.segment == NULL ) {
            DebugMsg(("PassOneChecks: undefined segment %s\n", curr->sym.name ));
            SkipSavedState();
            break;
        }
    }
    /* v2.04: scan the publics queue. This check was previously done
     * in GetPublicData(), but there it works for OMF format only.
     */
    for( q = ModuleInfo.g.PubQueue.head; q; q = q->next ) {
        const struct asym *sym = q->elmt;
        if ( sym->state == SYM_INTERNAL )
            continue;
        if ( sym->state != SYM_EXTERNAL || sym->weak == FALSE )
            SkipSavedState();
            break;
    }
#if COFF_SUPPORT
    /* if there's an item in the safeseh list which is not an
     * internal proc, make a full second pass to emit a proper
     * error msg at the .SAFESEH directive
     */
    if ( ModuleInfo.g.SafeSEHList.head ) {
        struct qnode *node;
        for ( node = ModuleInfo.g.SafeSEHList.head; node; node = node->next )
            if ( ((struct asym *)node->elmt)->state != SYM_INTERNAL || ((struct asym *)node->elmt)->isproc == FALSE ) {
                SkipSavedState();
                break;
            }
    }
#endif
#endif

#ifdef DEBUG_OUT
    DebugMsg(("PassOneChecks: forward references:\n"));
    for( curr = SymTables[TAB_SEG].head; curr; curr = curr->next ) {
        int i;
        int j;
        struct asym * sym;
        struct fixup * fix;
        for ( i = 0, j = 0, sym = curr->e.seginfo->labels;sym;sym = (struct asym *)((struct dsym *)sym)->next ) {
            i++;
            for ( fix = sym->fixup; fix ; fix = fix->nextbp, j++ );
        }
        DebugMsg(("PassOneChecks: segm=%s, labels=%u forward refs=%u\n", curr->sym.name, i, j));
    }
#endif
    return;
}

/* do ONE assembly pass
 * the FASTPASS variant (which is default now) doesn't scan the full source
 * for each pass. For this to work, the following things are implemented:
 * 1. in pass one, save state if the first byte is to be emitted.
 *    <state> is the segment stack, moduleinfo state, ...
 * 2. once the state is saved, all preprocessed lines must be stored.
 *    this can be done here, in OnePass, the line is in <string>.
 *    Preprocessed macro lines are stored in RunMacro().
 * 3. for subsequent passes do
 *    - restore the state
 *    - read preprocessed lines and feed ParseLine() with it
 */
static unsigned long OnePass( void )
/**********************************/
{

    InputPassInit();
    ModulePassInit();
    SymPassInit( Parse_Pass );
    LabelsInit();
    SegmentInit( Parse_Pass );
    ContextInit( Parse_Pass );
    ProcInit();
    TypesInit();
    HllInit( Parse_Pass );
    MacroInit( Parse_Pass ); /* insert predefined macros */
    AssumeInit();
    CmdlParamsInit( Parse_Pass );

    ModuleInfo.EndDirFound = FALSE;
    ModuleInfo.PhaseError = FALSE;
    //Modend = FALSE;
    /* LineNumber = 0; */
    LinnumInit();

#ifdef DEBUG_OUT
    if ( Parse_Pass > PASS_1 ) {
        DebugMsg(("OnePass(%u) segments (current=%s):\n", Parse_Pass + 1, CurrSeg ? CurrSeg->sym.name : "NULL" ));
        {
            struct dsym *dir;
            for( dir = SymTables[TAB_SEG].head; dir; dir = dir->next ) {
                DebugMsg(("OnePass(%u): segm=%-8s typ=%X start=%8X max_ofs=%8X\n", Parse_Pass + 1,
                          dir->sym.name, dir->e.seginfo->segtype, dir->e.seginfo->start_loc, dir->sym.max_offset ));
            }
        }
    }
#endif
    /* the functions above might have written something to the line queue */
    if ( is_linequeue_populated() )
        RunLineQueue();
#if FASTPASS
    StoreState = FALSE;
    if ( Parse_Pass > PASS_1 && UseSavedState == TRUE ) {
        LineStoreCurr = RestoreState();
        while ( LineStoreCurr && ModuleInfo.EndDirFound == FALSE ) {
            /* the source line is modified in Tokenize() if it contains a comment! */
#if USELSLINE==0
            strcpy( CurrSource, LineStoreCurr->line );
#endif
            set_curr_srcfile( LineStoreCurr->srcfile, LineStoreCurr->lineno );
            /* v2.06: list flags now initialized on the top level */
            ModuleInfo.line_flags = 0;
            MacroLevel = ( LineStoreCurr->srcfile == 0xFFF ? 1 : 0 );
            DebugMsg1(("OnePass(%u) cur/nxt=%X/%X src=%X.%u mlvl=%u: >%s<\n", Parse_Pass+1, LineStoreCurr, LineStoreCurr->next, LineStoreCurr->srcfile, LineStoreCurr->lineno, MacroLevel, LineStoreCurr->line ));
            ModuleInfo.CurrComment = NULL; /* v2.08: added (var is never reset because GetTextLine() isn't called) */
#if USELSLINE
            if ( Token_Count = Tokenize( LineStoreCurr->line, 0, ModuleInfo.tokenarray, TOK_DEFAULT ) )
#else
            if ( Token_Count = Tokenize( CurrSource, 0, ModuleInfo.tokenarray, TOK_DEFAULT ) )
#endif
                ParseLine( ModuleInfo.tokenarray );
            LineStoreCurr = LineStoreCurr->next;
        }
    } else
#endif
        while ( ModuleInfo.EndDirFound == FALSE && ( ( Token_Count = GetPreprocessedLine( CurrSource, ModuleInfo.tokenarray ) ) >= 0 ) ) {
            if ( Token_Count ) {
                ParseLine( ModuleInfo.tokenarray );
                if ( Options.preprocessor_stdout == TRUE && Parse_Pass == PASS_1 )
                    WritePreprocessedLine( CurrSource );
            }

        }

    LinnumFini();

    if ( Parse_Pass == PASS_1 )
        PassOneChecks();

    ClearFileStack();

    return( 1 );
}

#if BUILD_TARGET
/*
 * from WASM : get os-specific xxx_INCLUDE environment variable.
 *             if set, add string to include path.
 */

static void get_os_include( void )
/********************************/
{
    char *env;
    char *tmp;

    /* add OS_include to the include path */

    tmp = myalloca( strlen( Options.build_target ) + 10 );
    strcpy( tmp, Options.build_target );
    strcat( tmp, "_INCLUDE" );

    env = getenv( tmp );
    if( env != NULL ) {
        AddStringToIncludePath( env );
    }
}

#endif

static void get_module_name( void )
/*********************************/
{
    char dummy[_MAX_EXT];
    char        *p;

    /* v2.08: prefer name given by -nm option */
    if ( Options.names[OPTN_MODULE_NAME] ) {
        strncpy( ModuleInfo.name, Options.names[OPTN_MODULE_NAME], sizeof( ModuleInfo.name ) );
        ModuleInfo.name[ sizeof( ModuleInfo.name ) - 1] = NULLC;
    } else
        _splitpath( CurrFName[ASM], NULL, NULL, ModuleInfo.name, dummy );

    _strupr( ModuleInfo.name );
    /* the module name must be a valid identifier, because it's used
     * as part of a segment name in certain memory models.
     */
    for( p = ModuleInfo.name; *p; ++p ) {
        if( !( isalnum( *p ) || ( *p == '_' ) || ( *p == '$' )
            || ( *p == '@' ) || ( *p == '?') ) ) {
            /* it's not a legal character for a symbol name */
            *p = '_';
        }
    }
    /* first character can't be a digit either */
    if( isdigit( ModuleInfo.name[0] ) ) {
        ModuleInfo.name[0] = '_';
    }
}

/* called by AssembleInit(), once per source module
 * symbol table has been initialized
 */
static void ModuleInit( void )
/****************************/
{
    ModuleInfo.CommentDataInCode = (Options.output_format == OFORMAT_OMF &&
                         Options.no_comment_data_in_code_records == FALSE);
    ModuleInfo.g.error_count = 0;
    ModuleInfo.g.warning_count = 0;
    ModuleInfo.model = MODEL_NONE;
    /* ModuleInfo.distance = STACK_NONE; */
    ModuleInfo.ostype = OPSYS_DOS;
    ModuleInfo.emulator = (Options.floating_point == FPO_EMULATION);
    //ModuleInfo.flatgrp_idx = 0;

    get_module_name(); /* set ModuleInfo.name */

    /* v2.06: ST_PROC has been removed */
    //SimpleType[ST_PROC].mem_type = MT_NEAR;

    memset( SymTables, 0, sizeof( SymTables[0] ) * TAB_LAST );
    if ( ModuleInfo.fmtopt->init )
        ModuleInfo.fmtopt->init( &ModuleInfo );
    return;
}

static void ReswTableInit( void )
/*******************************/
{
    /* initialize reserved words hash table.
     * this must be called for each source module.
     */
    ResWordsInit();

    /* this must be done AFTER ResWordsInit() */
    if ( Options.output_format == OFORMAT_OMF ) {
        /* DebugMsg(("InitAsm: disable IMAGEREL+SECTIONREL\n")); */
        /* for OMF, IMAGEREL and SECTIONREL are disabled */
#if IMAGERELSUPP
        DisableKeyword( T_IMAGEREL );
#endif
#if SECTIONRELSUPP
        DisableKeyword( T_SECTIONREL );
#endif
    }

    if ( Options.strict_masm_compat == TRUE ) {
        DebugMsg(("ReswTableInit: disable INCBIN + FASTCALL keywords\n"));
        DisableKeyword( T_INCBIN );
        DisableKeyword( T_FASTCALL );
    }

    return;
}

static void open_files( void )
/****************************/
{
    /* open ASM file */
    DebugMsg(("open_files() enter\n" ));

    //memset( CurrFile, 0, sizeof( CurrFile ) );
    /* CurrFile[ASM] = fopen( CurrFName[ASM], "r" ); */
    CurrFile[ASM] = fopen( CurrFName[ASM], "rb" );
    if( CurrFile[ASM] == NULL ) {
        DebugMsg(("open_files(): cannot open source file, fopen(\"%s\") failed\n", CurrFName[ASM] ));
        Fatal( CANNOT_OPEN_FILE, CurrFName[ASM], ErrnoStr() );
    }

    /* open OBJ file */
    if ( Options.syntax_check_only == FALSE
         && Options.preprocessor_stdout == FALSE ) {
        CurrFile[OBJ] = fopen( CurrFName[OBJ], "wb" );
        if( CurrFile[OBJ] == NULL ) {
            DebugMsg(("open_files(): cannot open object file, fopen(\"%s\") failed\n", CurrFName[OBJ] ));
            Fatal( CANNOT_OPEN_FILE, CurrFName[OBJ], ErrnoStr() );
        }
        DebugMsg(("open_files(): output, fopen(\"%s\") ok\n", CurrFName[OBJ] ));
    }

    return;
}

void close_files( void )
/**********************/
{
    /* close ASM file */
    if( CurrFile[ASM] != NULL ) {
        if( fclose( CurrFile[ASM] ) != 0 )
            Fatal( CANNOT_CLOSE_FILE, CurrFName[ASM], errno  );
        CurrFile[ASM] = NULL;
    }

    if ( Options.output_format == OFORMAT_OMF )
        omf_fini();

    /* close OBJ file */
    if ( CurrFile[OBJ] != NULL ) {
        if ( fclose( CurrFile[OBJ] ) != 0 )
            Fatal( CANNOT_CLOSE_FILE, CurrFName[OBJ], errno  );
        CurrFile[OBJ] = NULL;
    }
    /* delete the object module if errors occured */
    if ( Options.syntax_check_only == FALSE &&
        ModuleInfo.g.error_count > 0 ) {
        remove( CurrFName[OBJ] );
    }

    LstCloseFile();

    /* close ERR file */
    if ( CurrFile[ERR] != NULL ) {
        fclose( CurrFile[ERR] );
        CurrFile[ERR] = NULL;
    } else if ( CurrFName[ERR] )
        /* nothing written, delete any existing ERR file */
        remove( CurrFName[ERR] );
    return;
}

/* get default file extension for error, object and listing files */

static char *GetExt( int type )
/*****************************/
{
    switch ( type ) {
    case ERR:
        return( ERR_EXT );
    case OBJ:
#if BIN_SUPPORT
        if ( Options.output_format == OFORMAT_BIN )
#if MZ_SUPPORT
            if ( ModuleInfo.header_format == HFORMAT_MZ )
                return("EXE");
            else
#endif
                return("BIN");
#endif
        return( OBJ_EXT );
    case LST:
        return( LST_EXT );
    }
    return( NULL );
}

/* set filenames for .obj, .lst and .err
 * in:
 *  name: full assembly source name
 *  DefaultDir[]: default directory part for .obj, .lst and .err
 * in:
 *  CurrFName[] for .obj, .lst and .err ( may be NULL )
 */

static void SetFilenames( const char *name )
/******************************************/
{
    int i;
    char fnamesrc[_MAX_FNAME];
    char drive[_MAX_DRIVE];
    char dir[_MAX_DIR];
    char fname[_MAX_FNAME];
    char ext[_MAX_EXT];
    char path[ _MAX_PATH ];

    DebugMsg(("SetFilenames(\"%s\") enter\n", name ));
    //memset( CurrFName, 0, sizeof( CurrFName ) );
    CurrFName[ASM] = LclAlloc( strlen( name ) + 1 );
    strcpy( CurrFName[ASM], name );
    _splitpath( name, NULL, NULL, fnamesrc, NULL );
    for ( i = ASM+1; i < NUM_FILE_TYPES; i++ ) {
        if( Options.names[i] == NULL ) {
            drive[0] = NULLC;
            dir[0] = NULLC;
            if ( DefaultDir[i])
                _splitpath( DefaultDir[i], drive, dir, NULL, NULL );
            _makepath( path, drive, dir, fnamesrc, GetExt( i ) );
        } else {
            /* filename has been set by cmdline option -Fo, -Fl or -Fr */
            _splitpath( Options.names[i], drive, dir, fname, ext );
            if( fname[0] == NULLC )
                strcpy( fname, fnamesrc );
            if( ext[0] == NULLC )
                strcpy( ext, GetExt( i ) );

            _makepath( path, drive, dir, fname, ext );
        }
        CurrFName[i] = LclAlloc( strlen( path ) + 1 );
        strcpy( CurrFName[i], path );
    }
    return;
}

/* init assembler. called once per module */

static void AssembleInit( const char *source )
/********************************************/
{
    DebugMsg(("AssembleInit(\"%s\") enter\n", source ));

    MemInit();
    //start_label   = NULL;
    //start_displ   = 0;
    write_to_file = FALSE;
    GeneratedCode = 0;
    LinnumQueue.head = NULL;

    ModuleInfo.header_format = Options.header_format;
#if AMD64_SUPPORT
    if ( Options.header_format == HFORMAT_WIN64 )
        ModuleInfo.fmtopt = &coff64_fmtopt;
    else if ( Options.header_format == HFORMAT_ELF64 )
        ModuleInfo.fmtopt = &elf64_fmtopt;
    else
#endif
    ModuleInfo.fmtopt = &formatoptions[Options.output_format];
    SetFilenames( source );

#if FASTPASS
    FastpassInit();
#endif
    open_files();
#if BUILD_TARGET
    get_os_include();
#endif
    ReswTableInit();
    SymInit();
    InputInit();

    if ( Options.output_format == OFORMAT_OMF ) {
        omf_init( &ModuleInfo, CurrFile[OBJ] );
    }

    ModuleInit();
    CondInit();
    ExprEvalInit();
    DebugMsg(("AssembleInit() exit\n"));
    return;
}

#ifdef DEBUG_OUT
void DumpInstrStats( void );
#endif

/* called once per module. AssembleModule() cleanup */

static void AssembleFini( void )
/******************************/
{
    int i;
#ifdef DEBUG_OUT
    SegmentFini();
#endif
    SymFini();
    ResWordsFini();
#ifdef DEBUG_OUT
    DumpInstrStats();
    MacroFini();
#endif
    FreePubQueue();
    FreeLnameQueue();
#if FASTMEM==0
    FreeLibQueue();
#endif
    InputFini();
    close_files();

#if FASTPASS
#if FASTMEM==0
    /* this is debugging code only. Usually FASTPASS and FASTMEM
     * are both either TRUE or FALSE.
     * It's active if both DEBUG and TRMEM is set in Makefile.
     */
    {
        struct line_item *next;
        for ( LineStoreCurr = LineStoreHead; LineStoreCurr; ) {
            next = LineStoreCurr->next;
            LclFree( LineStoreCurr );
            LineStoreCurr = next;
        }
    }
#endif
#endif

    for ( i = 0; i < NUM_FILE_TYPES; i++ ) {
        LclFree( CurrFName[i] );
        /* v2.05: make sure the pointer for ERR is cleared */
        CurrFName[i] = NULL;
    }
    MemFini();
    return;
}

/* AssembleModule() assembles one source file */

int EXPQUAL AssembleModule( const char *source )
/**********************************************/
{
    unsigned long       prev_written = -1;
    unsigned long       curr_written;
    int                 starttime;
    int                 endtime;
    struct dsym         *dir;

    DebugMsg(("AssembleModule(\"%s\") enter\n", source ));

    memset( &ModuleInfo, 0, sizeof(ModuleInfo) );
#ifdef DEBUG_OUT
    ModuleInfo.cref = TRUE; /* enable debug displays */
#endif
#if 1 //def __SW_BD
    /* if compiled as a dll/lib, fatal errors won't exit! */
    if ( setjmp( jmpenv ) ) {
        if ( ModuleInfo.g.file_stack )
            ClearFileStack(); /* avoid memory leaks! */
        goto done;
    }
#endif

    AssembleInit( source );

    LstOpenFile();

#if 0 /* ndef __UNIX__ */
    starttime = GetTickCount();
#else
    starttime = clock();
#endif

    for( Parse_Pass = PASS_1; ; Parse_Pass++ ) {

        DebugMsg(( "*************\npass %u\n*************\n", Parse_Pass + 1 ));
        OnePass();

        if ( Parse_Pass == PASS_1 && ModuleInfo.g.error_count == 0 ) {
            DebugMsg(("AssembleModule(%u): pass 1 actions\n", Parse_Pass + 1));

            /* make all symbols of type SYM_INTERNAL, which aren't
             a constant, public.  */
            if ( Options.all_symbols_public )
                SymMakeAllSymbolsPublic();

            /* convert EXTERNDEFs into EXTERNs, PUBLICs or nothing */
            /* v2.07: removed */
            //scan_globals();
            /* set index field in externals */
            set_ext_idx();
            if ( Options.syntax_check_only == FALSE
                 && Options.preprocessor_stdout == FALSE )
                write_to_file = TRUE;

            if ( write_to_file && ( Options.output_format == OFORMAT_OMF ) ) {
                WriteHeader( TRUE );
            }
        }

        DebugMsg(("AssembleModule(%u): errorcnt=%u\n", Parse_Pass + 1, ModuleInfo.g.error_count ));
        if( ModuleInfo.g.error_count > 0 )
            break;

        DebugMsg(("AssembleModule(%u) segments:\n", Parse_Pass + 1));
        for ( curr_written = 0, dir = SymTables[TAB_SEG].head; dir ; dir = dir->next ) {
            /* v2.04: use <max_offset> instead of <bytes_written>
             * (the latter is not always reliable due to backpatching).
             */
            //curr_written += dir->e.seginfo->bytes_written;
            curr_written += dir->sym.max_offset;
            DebugMsg(("AssembleModule(%u): segm=%-8s start=%8" FX32 " max_ofs=%8" FX32 " written=%" FX32 "\n",
                      Parse_Pass + 1, dir->sym.name,
                      dir->e.seginfo->start_loc,
                      dir->sym.max_offset,
                      dir->e.seginfo->bytes_written ));
        }

        DebugMsg(("AssembleModule(%u): PhaseError=%u, prev_written=%" FX32 ", curr_written=%" FX32 "\n", Parse_Pass + 1, ModuleInfo.PhaseError, prev_written, curr_written));
        if( !ModuleInfo.PhaseError && prev_written == curr_written )
            break;
#ifdef DEBUG_OUT
        if ( curr_written < prev_written && prev_written != -1 ) {
            printf( "size shrank from %" FX32 " to %" FX32 " in pass %u\n", prev_written, curr_written, Parse_Pass + 1 );
        }
#endif

        DebugMsg(("AssembleModule(%u): prepare for next pass\n", Parse_Pass + 1));
        prev_written = curr_written;

        if ( Parse_Pass % 200 == 199 )
            EmitWarn( 2, ASSEMBLY_PASSES, Parse_Pass+1 );
#ifdef DEBUG_OUT
        if ( Options.max_passes && Parse_Pass == (Options.max_passes - 1) )
            break;

        /* only do pass one for -EP, like MASM does. */
        if ( Parse_Pass == PASS_1 && Options.preprocessor_stdout == TRUE )
            break;
#endif
        if ( Options.line_numbers ) {
#if COFF_SUPPORT
            if ( Options.output_format == OFORMAT_COFF ) {
                for( dir = SymTables[TAB_SEG].head; dir; dir = dir->next ) {
                    if ( dir->e.seginfo->LinnumQueue )
                        QueueDeleteLinnum( dir->e.seginfo->LinnumQueue );
                    dir->e.seginfo->LinnumQueue = NULL;
                }
            } else {
#endif
                QueueDeleteLinnum( &LinnumQueue );
                LinnumQueue.head = NULL;
#if COFF_SUPPORT
            }
#endif
        }

        /* set file position of ASM and LST files for next pass */

        rewind( CurrFile[ASM] );
        if ( write_to_file && Options.output_format == OFORMAT_OMF )
            omf_set_filepos();

#if FASTPASS
        if ( UseSavedState == FALSE && CurrFile[LST] ) {
#else
        if ( CurrFile[LST] ) {
#endif
            LstCloseFile();
            LstOpenFile();
        }
    } /* end for() */

    if ( ( Parse_Pass > PASS_1 ) && write_to_file ) {
        uint_32 tmp = LineNumber;
        set_curr_srcfile( 0, 0 ); /* no line reference for errors/warnings */
        if ( Options.output_format == OFORMAT_OMF ) {
            WriteContent();
            WriteHeader( FALSE );
        } else {
            if ( WriteHeader( TRUE ) == NOT_ERROR )
                WriteContent();
        }
        set_curr_srcfile( 0, tmp );
    }
    if ( ModuleInfo.pCodeBuff ) {
        LclFree( ModuleInfo.pCodeBuff );
    }
    DebugMsg(("AssembleModule: finished, cleanup\n"));

    /* Write a symbol listing file (if requested) */
    LstWriteCRef();

#if 0 /* ndef __UNIX__ */
    endtime = GetTickCount();
#else
    endtime = clock(); /* is in ms already */
#endif

    sprintf( CurrSource, MsgGetEx( MSG_ASSEMBLY_RESULTS ),
             GetFName( ModuleInfo.srcfile )->name,
             LineNumber,
             Parse_Pass + 1,
             endtime - starttime,
             ModuleInfo.g.warning_count,
             ModuleInfo.g.error_count);
    if ( Options.quiet == FALSE )
        printf( "%s\n", CurrSource );

    if ( CurrFile[LST] ) {
        LstPrintf( CurrSource );
        LstNL();
        LstCloseFile();
    }
#if 1 //def __SW_BD
done:
#endif
    AssembleFini();
    DebugMsg(("AssembleModule exit\n"));
    return( ModuleInfo.g.error_count == 0 );
}
