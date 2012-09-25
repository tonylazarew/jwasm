
/* some structures for Win64 SEH */

/* .pdata items */

typedef struct _IMAGE_RUNTIME_FUNCTION_ENTRY
{
    uint_32 BeginAddress;
    uint_32 EndAddress;
    uint_32 UnwindData; /* RVA of UNWIND_INFO */
} IMAGE_RUNTIME_FUNCTION_ENTRY;

/* .xdata items */

enum {
    UWOP_PUSH_NONVOL = 0,
    UWOP_ALLOC_LARGE = 1,
    UWOP_ALLOC_SMALL = 2,
    UWOP_SET_FPREG   = 3,
    UWOP_SAVE_NONVOL = 4,
    UWOP_SAVE_NONVOL_FAR = 5,
    UWOP_SAVE_XMM    = 6,
    UWOP_SAVE_XMM_FAR = 7,
    UWOP_SAVE_XMM128 = 8,
    UWOP_SAVE_XMM128_FAR = 9,
    UWOP_PUSH_MACHFRAME = 10
};

typedef union _UNWIND_CODE {
    struct {
        uint_8 CodeOffset;    /* offset within prolog */
        uint_8 UnwindOp : 4;
        uint_8 OpInfo   : 4;
    };
    uint_16 FrameOffset;
} UNWIND_CODE;

typedef struct _UNWIND_INFO {
    uint_8 Version       : 3; /* is 1 */
    uint_8 Flags         : 5; /* see below */
    uint_8 SizeOfProlog;      /* size of prolog in bytes */
    uint_8 CountOfCodes;      /* number of UNWIND_CODE entries */
    uint_8 FrameRegister : 4;
    uint_8 FrameOffset   : 4;
    UNWIND_CODE UnwindCode[1];
    union {
        uint_32 ExceptionHandler; /* RVA of language specific handler */
        uint_32 FunctionEntry;
    };
    uint_32 ExceptionData[];
} UNWIND_INFO;

#define UNW_VERSION 1

enum {
    UNW_FLAG_NHANDLER = 0,
    UNW_FLAG_EHANDLER = 1, /* function to examine exceptions */
    UNW_FLAG_UHANDLER = 2, /* function to unwind an exception */
    UNW_FLAG_FHANDLER = 3, /* inofficial, is E+U */
    UNW_FLAG_CHAININFO = 4
};

