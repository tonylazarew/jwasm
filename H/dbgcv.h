/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  MS codeview debug info header. Values and structures
*               in this header are derived from document:
*               "Microsoft Symbol and Type Information"
*               Format Specifications for Windows Version 1.0
*               Tool Interface Standards (TIS)
*
*               CV 5 format is described in
*               "Visual C++ 5.0 Symbolic Debug Information Specification",
*               CV 5 is 32-bit only, which is bad. OTOH, the advantage of
*               this format - the type indices are 32bit instead of 16bit -
*               is virtually irrelevant for JWasm. So CV 5 is ignored.
*
*               CV 8 is not "officially" described.
*               It's an extended CV5 format. Not yet supported by JWasm.
****************************************************************************/

#ifndef CVDBG_H
#define CVDBG_H 1

enum cv_version_signature {
    CV4_SIGNATURE = 1,
    CV5_SIGNATURE = 2,
    CV8_SIGNATURE = 4
};

/* reserved primitive types (0x0000-0x0FFF) */

struct cv_primitive_type {
    uint_16 size:3,
    reserved:1,
    type:4,
    mode:3,
    reserved2:1;
};

enum cv_predef_type_types {
    CV_PDT_SPECIAL           = 0x00,
    CV_PDT_SIGNED_INTEGRAL   = 0x01,
    CV_PDT_UNSIGNED_INTEGRAL = 0x02,
    CV_PDT_BOOLEAN           = 0x03,
    CV_PDT_REAL              = 0x04,
    CV_PDT_COMPLEX           = 0x05,
    CV_PDT_SPECIAL2          = 0x06,
    CV_PDT_REAL_INT_VALUE    = 0x07
    /* values 08-0F are reserved */
};

enum cv_predef_type_sizes {
    CV_PDS_SPECIAL_NO_TYPE           = 0x00,
    CV_PDS_SPECIAL_ABSOLUTE          = 0x01,
    CV_PDS_SPECIAL_SEGMENT           = 0x02,
    CV_PDS_SPECIAL_VOID              = 0x03,
    CV_PDS_SPECIAL_BASIC_CURRENCY    = 0x04,
    CV_PDS_SPECIAL_BASIC_NEAR_STRING = 0x05,
    CV_PDS_SPECIAL_BASIC_FAR_STRING  = 0x06,
    CV_PDS_SPECIAL_UNTRANSLATED      = 0x07,
    CV_PDS_INTEGRAL_1BYTE            = 0x00,
    CV_PDS_INTEGRAL_2BYTE            = 0x01,
    CV_PDS_INTEGRAL_4BYTE            = 0x02,
    CV_PDS_INTEGRAL_8BYTE            = 0x03,
    CV_PDS_REAL_32BIT                = 0x00,
    CV_PDS_REAL_64BIT                = 0x01,
    CV_PDS_REAL_80BIT                = 0x02,
    CV_PDS_REAL_128BIT               = 0x03,
    CV_PDS_REAL_48BIT                = 0x04
};

enum cv_predef_type_modes {
    CV_PDM_DIRECT            = 0x00,
    CV_PDM_NEARPTR           = 0x01,
    CV_PDM_FARPTR            = 0x02,
    CV_PDM_HUGEPTR           = 0x03,
    CV_PDM_NEAR32PTR         = 0x04,
    CV_PDM_FAR32PTR          = 0x05,
    CV_PDM_NEAR64PTR         = 0x06
};

struct cv_attribute {
    uint_16 access:2,
    mprop:3,
    pseudo:1,
    noinherit:1,
    noconstruct:1,
    reserved:8;
};

enum cv_attr_access {
    CV_ATTR_ACC_NOPROTECTION = 0,
    CV_ATTR_ACC_PRIVATE      = 1,
    CV_ATTR_ACC_PROTECTED    = 2,
    CV_ATTR_ACC_PUBLIC       = 3,
};

enum cv_attr_mprop {
    CV_ATTR_MPR_VANILLA      = 0,
    CV_ATTR_MPR_VIRTUAL      = 1,
    CV_ATTR_MPR_STATIC       = 2,
    CV_ATTR_MPR_FRIEND       = 3,
    CV_ATTR_MPR_INTRO_VIRTUAL = 4,
    CV_ATTR_MPR_PURE_VIRTUAL = 5,
    CV_ATTR_MPR_PURE_INTRO_VIRTUAL = 6,
};

enum cv_leaf_indices {
    LF_MODIFIER  = 0x0001,
    LF_POINTER   = 0x0002,
    LF_ARRAY     = 0x0003,
    LF_CLASS     = 0x0004,
    LF_STRUCTURE = 0x0005,
    LF_UNION     = 0x0006,
    LF_ENUM      = 0x0007,
    LF_PROCEDURE = 0x0008,
    LF_MFUNCTION = 0x0009,
    LF_VTSHAPE   = 0x000A,
    LF_BARRAY    = 0x000D,
    LF_LABEL     = 0x000E,

    LF_FIELDLIST = 0x0204,
    LF_BITFIELD  = 0x0206,

    LF_MEMBER    = 0x0406,

    LF_NUMERIC   = 0x8000,
    LF_CHAR      = 0x8000,
    LF_SHORT     = 0x8001,
    LF_USHORT    = 0x8002,
    LF_LONG      = 0x8003,
    LF_ULONG     = 0x8004,

    LF_PAD0      = 0xF0,
    LF_PAD1      = 0xF1,
    LF_PAD2      = 0xF2,
    LF_PAD3      = 0xF3,
    LF_PAD4      = 0xF4,
    LF_PAD5      = 0xF5,
    LF_PAD6      = 0xF6,
    LF_PAD7      = 0xF7,
    LF_PAD8      = 0xF8,
    LF_PAD9      = 0xF9,
    LF_PAD10     = 0xFA,
    LF_PAD11     = 0xFB,
    LF_PAD12     = 0xFC,
    LF_PAD13     = 0xFD,
    LF_PAD14     = 0xFE,
    LF_PAD15     = 0xFF
};

struct cv_typerec {
    uint_16 size;
    uint_16 leaf;
};

struct cv_typerec_label {
    struct cv_typerec tr;
    uint_16 mode;  /* 0=near, 4=far */
};

/* label flags (values for mode) */
enum cv_typerec_label_values {
    CV_TYPE_LABEL_NEAR      = 0x00,
    CV_TYPE_LABEL_FAR       = 0x04
};

struct cv_typerec_structure {
    struct cv_typerec tr;
    uint_16 count;
    uint_16 field;   /* typeref */
    uint_16 property;
    uint_16 dList;   /* typeref */
    uint_16 vshape;  /* typeref */
    uint_16 length;  /* numeric leaf */
    // length-prefixed name
};

struct cv_typerec_union {
    struct cv_typerec tr;
    uint_16 count;
    uint_16 field;   /* typeref */
    uint_16 property;
    uint_16 length;  /* numeric leaf */
    // length-prefixed name
};

struct cv_typerec_fieldlist {
    struct cv_typerec tr;
};

struct cv_typerec_member {
    uint_16 leaf;
    uint_16 type; /* typeref */
    struct cv_attribute attribute;
    uint_16 offset; /* numeric leaf */
    // length-prefixed name
};

struct cv_typerec_bitfield {
    struct cv_typerec tr;
    uint_8 length;
    uint_8 position;
    uint_16 type; /* typeref */
};

enum cv4_symbol_types {
    S_COMPILE  = 0x0001,
    S_REGISTER = 0x0002,
    S_CONSTANT = 0x0003,
    S_UDT      = 0x0004,
    S_SSEARCH  = 0x0005,
    S_ENDBLK   = 0x0006,
    S_SKIP     = 0x0007,
    S_CVRES    = 0x0008,
    S_OBJNAME  = 0x0009,
    S_ENDARG   = 0x000A,
    S_RETURN   = 0x000D,

    S_BPREL16  = 0x0100,
    S_LDATA16  = 0x0101,
    S_GDATA16  = 0x0102,
    S_PUB16    = 0x0103,
    S_LPROC16  = 0x0104,
    S_GPROC16  = 0x0105,
    S_LABEL16  = 0x0109,

    S_BPREL32  = 0x0200,
    S_LDATA32  = 0x0201,
    S_GDATA32  = 0x0202,
    S_PUB32    = 0x0203,
    S_LPROC32  = 0x0204,
    S_GPROC32  = 0x0205,
    S_LABEL32  = 0x0209,
};

/* CV5 symbol types.
 * the new types > 0x1000 have a 4 byte typeref.
 * CV5 format currently isn't used, since JWasm
 * can live very well with 2-byte type indices.
 */

enum cv5_symbol_types {
    //S_COMPILE  = 0x0001,
    //S_SSEARCH  = 0x0005,
    //S_ENDBLK   = 0x0006,
    //S_SKIP     = 0x0007,
    //S_CVRES    = 0x0008,
    //S_OBJNAME  = 0x0009,
    //S_ENDARG   = 0x000A,
    //S_RETURN   = 0x000D,

    CV5_S_REGISTER = 0x1001,
    CV5_S_CONSTANT = 0x1002,
    CV5_S_UDT      = 0x1003,
    CV5_S_MANYREG  = 0x1005,
    CV5_S_BPREL32  = 0x1006,
    CV5_S_LDATA32  = 0x1007,
    CV5_S_GDATA32  = 0x1008,
    CV5_S_PUB32    = 0x1009,
    CV5_S_LPROC32  = 0x100A,
    CV5_S_GPROC32  = 0x100B,

    //S_LABEL32  = 0x0209,
};

enum cv8_symbol_types {
    CV8_FILE   = 0x1101,  /* object filename */
    CV8_LABEL  = 0x1105,
    CV8_TYPE   = 0x1108,
    CV8_BPREL  = 0x110B,
    CV8_LDATA  = 0x110C,
    CV8_GDATA  = 0x110D,
    CV8_PROC32 = 0x1110,
};

enum cv8_s_sections {
    CV8_SYMBOLS   = 0xF1,
    CV8_LINNUM    = 0xF2,
    CV8_FILENAMES = 0xF3,
    CV8_FILES     = 0xF4
};

struct cv8_linenumber_item {
    uint_32 offset;
    uint_32 line;
};

struct cv8_linenumber_header {
    uint_32 section_start;
    uint_16 section_index;
    uint_32 section_length;
    uint_32 src_file;
    uint_32 num_items; /* number of cv8_linenumber_info items */
    uint_32 length;
};

#pragma pack(push, 1)

struct cv_symrec {
    uint_16 size;
    uint_16 type;
    //uint_8  data[];
};

struct cv_symrec_compile {
    struct cv_symrec sr;
    uint_8 machine;  /* see below */
    uint_8 Language; /* see below */
    uint_8 PCodePresent:1,
    FloatPrecision:2,
    Floatpackage:2,
    AmbientData:3;
    uint_8 AmbientCode:3,
    Mode32:1,
    Reserved:4;
    uint_8 version[];
};

enum cv_machines {
    CV_MACH_8080    = 0,  /* ??? */
    CV_MACH_8086    = 1,  /* also 80186 */
    CV_MACH_80286   = 2,
    CV_MACH_80386   = 3,
    CV_MACH_80486   = 4,
    CV_MACH_PENTIUM = 5,
};

enum cv_languages {
    CV_LANG_C       = 0,
    CV_LANG_CPP     = 1,
    CV_LANG_FORTRAN = 2,
    CV_LANG_MASM    = 3,
    CV_LANG_PASCAL  = 4,
    CV_LANG_BASIC   = 5,
    CV_LANG_COBOL   = 6
};

struct cv_symrec_udt {
    struct cv_symrec sr;
    uint_16 typeref;
    uint_8 name[];
};

struct cv_symrec_endblk {
    struct cv_symrec sr;
};

struct cv_symrec_objname {
    struct cv_symrec sr;
    uint_32 Signature;
    uint_8 name[];
};

struct cv_symrec_bprel16 {
    struct cv_symrec sr;
    int_16 offset;
    uint_16 typeref;
    uint_8 name[];
};

struct cv_symrec_ldata16 {
    struct cv_symrec sr;
    int_16 offset;
    uint_16 segment;
    uint_16 typeref;
    uint_8 name[];
};

struct cv_symrec_lproc16 {
    struct cv_symrec sr;
    uint_32 pParent;
    uint_32 pEnd;
    uint_32 pNext;
    uint_16 proc_length;
    uint_16 debug_start;
    uint_16 debug_end;
    uint_16 offset;
    uint_16 segment;
    uint_16 proctype; /*typeref */
    uint_8 flags;
    uint_8 name[];
};

struct cv_symrec_label16 {
    struct cv_symrec sr;
    int_16 offset;
    uint_16 segment;
    uint_8 flags;
    uint_8 name[];
};

struct cv_symrec_bprel32 {
    struct cv_symrec sr;
    int_32 offset;
    uint_16 typeref;
    uint_8 name[];
};

struct cv_symrec_ldata32 {
    struct cv_symrec sr;
    int_32 offset;
    uint_16 segment;
    uint_16 typeref;
    uint_8 name[];
};

struct cv_symrec_lproc32 {
    struct cv_symrec sr;
    uint_32 pParent;
    uint_32 pEnd;
    uint_32 pNext;
    uint_32 proc_length;
    uint_32 debug_start;
    uint_32 debug_end;
    uint_32 offset;
    uint_16 segment;
    uint_16 proctype; /*typeref */
    uint_8 flags;
    uint_8 name[];
};

struct cv_symrec_label32 {
    struct cv_symrec sr;
    int_32 offset;
    uint_16 segment;
    uint_8 flags;
    uint_8 name[];
};

#pragma pack(pop)

#endif
