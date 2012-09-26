/****************************************************************************
*
*                            Open Watcom Project
*
*    Portions Copyright (c) 1983-2002 Sybase, Inc. All Rights Reserved.
*
*  ========================================================================
*
*    This file contains Original Code and/or Modifications of Original
*    Code as defined in and that are subject to the Sybase Open Watcom
*    Public License version 1.0 (the 'License'). You may not use this file
*    except in compliance with the License. BY USING THIS FILE YOU AGREE TO
*    ALL TERMS AND CONDITIONS OF THE LICENSE. A copy of the License is
*    provided with the Original Code and Modifications, and is also
*    available at www.sybase.com/developer/opensource.
*
*    The Original Code and all software distributed under the License are
*    distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
*    EXPRESS OR IMPLIED, AND SYBASE AND ALL CONTRIBUTORS HEREBY DISCLAIM
*    ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
*    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR
*    NON-INFRINGEMENT. Please see the License for the specific language
*    governing rights and limitations under the License.
*
*  ========================================================================
*
* Description:  interface to expression evaluator
*
****************************************************************************/


#ifndef EXPREVAL_H
#define EXPREVAL_H

enum exprtype {
    EXPR_EMPTY = EMPTY,
    EXPR_UNDEF = 0,     /* undefined type when error occures or result is undefined */
    EXPR_ADDR,          /* e.g. "foo", "seg foo" and "offset foo" */
    EXPR_CONST,         /* constant; note that "label1 - label2" -> constant */
    EXPR_REG,           /* register */
    EXPR_FLOAT          /* v2.05: float */
};

/* argument types accepted by unary operators */

enum oparg_types {
    AT_TYPE  = 0x01, /* type */
    AT_LABEL = 0x02, /* label (direct memory) */
    AT_IND   = 0x04, /* indirect memory */
    AT_REG   = 0x08, /* register */
    AT_FIELD = 0x10, /* struct field */
    AT_NUM   = 0x20, /* number */
    AT_BF    = 0x40, /* bitfield and record types */
    AT_UNDEF = 0x80, /* undefined label */
    AT_FLOAT = 0x100,/* float constant */
    AT_CONST = AT_TYPE | AT_NUM,
    AT_TL    = AT_TYPE | AT_LABEL,
    AT_TLN   = AT_TYPE | AT_LABEL | AT_NUM,
    AT_TLF   = AT_TYPE | AT_LABEL | AT_FIELD,
    AT_TLFN  = AT_TYPE | AT_LABEL | AT_FIELD | AT_NUM,
    AT_TBF   = AT_TYPE | AT_BF,
    AT_LF    = AT_LABEL| AT_FIELD,
    AT_LIF   = AT_LABEL| AT_IND | AT_FIELD,
    AT_LFN   = AT_LABEL| AT_FIELD | AT_NUM,
    AT_TLR   = AT_TYPE | AT_LABEL | AT_REG,
    AT_ALL   = AT_TYPE | AT_LABEL | AT_IND | AT_REG | AT_FIELD | AT_NUM | AT_UNDEF | AT_BF | AT_FLOAT
};

/* expression, returned by expression evaluator */

struct expr {
    union {                         /* value of expression */
        struct {
            int_32  value;
            int_32  hvalue;
        };
        struct {
            uint_64 llvalue;
            uint_64 hlvalue;
        };
        uint_32     uvalue;
        int_64      value64;
        float       fvalue;
        int         st_idx;         /* EXPR_REG: index if reg is ST */
        uint_8      chararray[16];
    };
    union {
        struct asm_tok *quoted_string; /* for EXPR_CONST + quoted strings only */
        struct asm_tok *float_tok;     /* for EXPR_FLOAT only */
    };
    struct asm_tok  *base_reg;      /* token holding base register */
                                    /* if type is EXPR_REG, it holds register */
    struct asm_tok  *idx_reg;       /* token holding index register */
    struct asm_tok  *label_tok;     /* token holding the label (for INVOKE only) */
    struct asm_tok  *override;      /* token holding the override label */
                                    /* or register */
    enum special_token instr;       /* operator token */

    enum exprtype   kind;           /* Type of expression */
    enum memtype    mem_type;       /* memory type if expr is a memory ref. */
    uint_8          scale;          /* scaling factor 1, 2, 4, or 8 - 386 code only */
    uint_8          Ofssize;        /* 16,32,64 bit if MT_NEAR, MT_FAR */
    union {
        uint_8      flags1;
        struct {
            unsigned indirect : 1;  /* Whether inside [] or not */
            unsigned explicit : 1;  /* Whether expression type explicitly given (to be removed!) */
            unsigned abs      : 1;  /* external ABS */
            unsigned is_type  : 1;  /* constant is a type */
            unsigned is_opattr: 1;  /* current operator is OPATTR */
            unsigned negative : 1;  /* for EXPR_FLOAT only */
            //unsigned ftype    : 1;  /* for EXPR_FLOAT only (float type) */
            unsigned assumecheck: 1;/* v2.07: for ASSUMEd std registers */
        };
    };
    struct asym     *sym;   /* label used */
    struct asym     *mbr;   /* struct member */
    struct asym     *type;  /* for DOT operator. Must be last (see TokenAssign)! */
};

/* flags for last argument of EvalOperand() */
enum expr_flags {
    EXPF_NOERRMSG  = 1,  /* suppress error messages */
    EXPF_NOLCREATE = 2   /* don't create label if it isn't defined yet */
};

extern void         EmitConstError( const struct expr * );
extern ret_code     EvalOperand( int *, struct asm_tok[], int, struct expr *, uint_8 );
extern void         ExprEvalInit( void );

#endif
