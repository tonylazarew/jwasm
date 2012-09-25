/****************************************************************************
*
* Description:  table of directives
*
****************************************************************************/

/* v1.96: items needn't be sorted anymore!
 * The items are stored in structures of type special_item.
 * If an item is inserted, moved or deleted, the project needs
 * a full rebuild.
 */

/* directives field usage:
 * value  = DF_  flags    SpecialTable.value  uint
 * bytval = DRT_ value    SpecialTable.bytval uint_8
 * flags  = RWF_ flags    ResWordTable.flags  uint_8
 * cpu    = cpu  flags    SpecialTable.cpu    uint_16
 * sflags = dep. on DRT_  SpecialTable.sflags uint
 */

/* cpu directives */

/* token      str        len value     bytval  flags  cpu   sflags */
res(DOT_8086, .8086,       5,  0,      DRT_CPU,  0,   P_86,  P_86   )
res(DOT_186,  .186,        4,  0,      DRT_CPU,  0,   P_86,  P_186  )
res(DOT_286,  .286,        4,  0,      DRT_CPU,  0,   P_86,  P_286  )
res(DOT_286C, .286c,       5,  0,      DRT_CPU,  0,   P_86,  P_286  )
res(DOT_286P, .286p,       5,  0,      DRT_CPU,  0,   P_86,  P_286p )
res(DOT_386,  .386,        4,  0,      DRT_CPU,  0,   P_86,  P_386  )
res(DOT_386C, .386c,       5,  0,      DRT_CPU,  0,   P_86,  P_386  )
res(DOT_386P, .386p,       5,  0,      DRT_CPU,  0,   P_86,  P_386p )
res(DOT_486,  .486,        4,  0,      DRT_CPU,  0,   P_86,  P_486  )
res(DOT_486P, .486p,       5,  0,      DRT_CPU,  0,   P_86,  P_486p )
res(DOT_586,  .586,        4,  0,      DRT_CPU,  0,   P_86,  P_586  )
res(DOT_586P, .586p,       5,  0,      DRT_CPU,  0,   P_86,  P_586p )
res(DOT_686,  .686,        4,  0,      DRT_CPU,  0,   P_86,  P_686  )
res(DOT_686P, .686p,       5,  0,      DRT_CPU,  0,   P_86,  P_686p )
res(DOT_K3D,  .k3d,        4,  0,      DRT_CPU,  0,   P_586, P_K3D|P_MMX )
res(DOT_MMX,  .mmx,        4,  0,      DRT_CPU,  0,   P_586, P_MMX  )
res(DOT_XMM,  .xmm,        4,  0,      DRT_CPU,  0,   P_686, P_MMX|P_SSEALL )
#if AMD64_SUPPORT
res(DOT_X64,  .x64,        4,  0,      DRT_CPU,  0,   P_86,  P_64  )
res(DOT_X64P, .x64p,       5,  0,      DRT_CPU,  0,   P_86,  P_64p )
#endif

res(DOT_8087, .8087,       5,  0,      DRT_CPU,  0,   P_86,  P_87   )
res(DOT_287,  .287,        4,  0,      DRT_CPU,  0,   P_86,  P_287  )
res(DOT_387,  .387,        4,  0,      DRT_CPU,  0,   P_86,  P_387  )
res(DOT_NO87, .no87,       5,  0,      DRT_CPU,  0,   P_86,  P_NO87 )

/* listing directives */
/* .LFCOND is synonym for .LISTIF
 * .SFCOND is synonym for .NOLISTIF
 * .TFCOND toggles .LFCOND, .SFCOND
 */

res(DOT_CREF,         .cref,          5, 0,           DRT_LISTING, 0,  P_86, 0)
res(DOT_LFCOND,       .lfcond,        7, 0,           DRT_LISTING, 0,  P_86, 0)
res(DOT_LIST,         .list,          5, 0,           DRT_LISTING, 0,  P_86, 0)
res(DOT_LISTALL,      .listall,       8, 0,           DRT_LISTING, 0,  P_86, 0)
res(DOT_LISTIF,       .listif,        7, 0,           DRT_LISTING, 0,  P_86, 0)
res(DOT_NOCREF,       .nocref,        7, DF_NOEXPAND, DRT_LISTING, 0,  P_86, 0)
res(DOT_NOLIST,       .nolist,        7, 0,           DRT_LISTING, 0,  P_86, 0)
res(DOT_NOLISTIF,     .nolistif,      9, 0,           DRT_LISTING, 0,  P_86, 0)
res(DOT_SFCOND,       .sfcond,        7, 0,           DRT_LISTING, 0,  P_86, 0)
res(DOT_TFCOND,       .tfcond,        7, 0,           DRT_LISTING, 0,  P_86, 0)
res(DOT_XCREF,        .xcref,         6, DF_NOEXPAND, DRT_LISTING, 0,  P_86, 0)
res(DOT_XLIST,        .xlist,         6, 0,           DRT_LISTING, 0,  P_86, 0)
res(PAGE,             page,           4, 0,           DRT_LISTING, 0,  P_86, 0)
res(SUBTITLE,         subtitle,       8, 0,           DRT_LISTING, 0,  P_86, 0)
res(SUBTTL,           subttl,         6, 0,           DRT_LISTING, 0,  P_86, 0)
res(TITLE,            title,          5, 0,           DRT_LISTING, 0,  P_86, 0)

/* list macro directives
 * .XALL is synonym for .LISTMACRO
 * .LALL is synonym for .LISTMACROALL
 * .SALL is synonym for .NOLISTMACRO
 */
res(DOT_LISTMACRO,    .listmacro,    10, 0,           DRT_LISTMAC, 0,  P_86, LM_LISTMACRO)
res(DOT_LISTMACROALL, .listmacroall, 13, 0,           DRT_LISTMAC, 0,  P_86, LM_LISTMACROALL)
res(DOT_NOLISTMACRO,  .nolistmacro,  12, 0,           DRT_LISTMAC, 0,  P_86, LM_NOLISTMACRO)
res(DOT_XALL,         .xall,          5, 0,           DRT_LISTMAC, 0,  P_86, LM_LISTMACRO)
res(DOT_LALL,         .lall,          5, 0,           DRT_LISTMAC, 0,  P_86, LM_LISTMACROALL)
res(DOT_SALL,         .sall,          5, 0,           DRT_LISTMAC, 0,  P_86, LM_NOLISTMACRO)


res(DOT_ALPHA,     .alpha,      6,  0,          DRT_SEGORDER, 0,  P_86, SEGORDER_ALPHA )
res(DOT_DOSSEG,    .dosseg,     7,  0,          DRT_SEGORDER, 0,  P_86, SEGORDER_DOSSEG)
res(DOT_SEQ,       .seq,        4,  0,          DRT_SEGORDER, 0,  P_86, SEGORDER_SEQ   )
res(DOSSEG,        dosseg,      6,  0,          DRT_SEGORDER, 0,  P_86, SEGORDER_DOSSEG)

/* simplified segment directives, numbers in sflags must match enum sim_seg (simseg.c) */
res(DOT_CODE,       .code,      5,  DF_NOSTRUC|DF_PROC|DF_CGEN, DRT_SIMSEG, 0,  P_86, 0)
res(DOT_STACK,      .stack,     6,  DF_NOSTRUC|DF_PROC|DF_CGEN, DRT_SIMSEG, 0,  P_86, 1)
res(DOT_DATA,       .data,      5,  DF_NOSTRUC|DF_PROC|DF_CGEN, DRT_SIMSEG, 0,  P_86, 2)
res(DOT_DATA_UN,    .data?,     6,  DF_NOSTRUC|DF_PROC|DF_CGEN, DRT_SIMSEG, 0,  P_86, 3)
res(DOT_FARDATA,    .fardata,   8,  DF_NOSTRUC|DF_PROC|DF_CGEN, DRT_SIMSEG, 0,  P_86, 4)
res(DOT_FARDATA_UN, .fardata?,  9,  DF_NOSTRUC|DF_PROC|DF_CGEN, DRT_SIMSEG, 0,  P_86, 5)
res(DOT_CONST,      .const,     6,  DF_NOSTRUC|DF_PROC|DF_CGEN, DRT_SIMSEG, 0,  P_86, 6)

/* hll directives */

res(DOT_IF,         .if,        3,  DF_CGEN|DF_CEXPR|DF_NOSTRUC|DF_STORE|DF_PROC, DRT_HLLSTART, 0, P_86, 0)
res(DOT_REPEAT,     .repeat,    7,  DF_CGEN|DF_NOSTRUC|DF_STORE|DF_PROC,          DRT_HLLSTART, 0, P_86, 0)
res(DOT_WHILE,      .while,     6,  DF_CGEN|DF_CEXPR|DF_NOSTRUC|DF_STORE|DF_PROC, DRT_HLLSTART, 0, P_86, 0)
res(DOT_BREAK,      .break,     6,  DF_CGEN,                                      DRT_HLLEXIT,  0, P_86, 0)
res(DOT_CONTINUE,   .continue,  9,  DF_CGEN,                                      DRT_HLLEXIT,  0, P_86, 0)
res(DOT_ELSE,       .else,      5,  DF_CGEN,                                      DRT_HLLEXIT,  0, P_86, 0)
res(DOT_ELSEIF,     .elseif,    7,  DF_CGEN|DF_CEXPR,                             DRT_HLLEXIT,  0, P_86, 0)
res(DOT_ENDIF,      .endif,     6,  DF_CGEN,                                      DRT_HLLEND,   0, P_86, 0)
res(DOT_ENDW,       .endw,      5,  DF_CGEN,                                      DRT_HLLEND,   0, P_86, 0)
res(DOT_UNTIL,      .until,     6,  DF_CGEN|DF_CEXPR,                             DRT_HLLEND,   0, P_86, 0)
res(DOT_UNTILCXZ,   .untilcxz,  9,  DF_CGEN|DF_CEXPR,                             DRT_HLLEND,   0, P_86, 0)

res(DOT_EXIT,       .exit,      5,  DF_CGEN|DF_STORE,  DRT_STARTEXIT,  0,   P_86, 0)
res(DOT_STARTUP,    .startup,   8,  DF_CGEN|DF_STORE,  DRT_STARTEXIT,  0,   P_86, 0)

res(DOT_MODEL,      .model,     6,  DF_CGEN,   DRT_MODEL,  0,   P_86, 0)
res(DOT_RADIX,      .radix,     6,  0,         DRT_RADIX,  0,   P_86, 0)

/* directives invalid for IA32+ */

#if COFF_SUPPORT
res(DOT_SAFESEH,    .safeseh,   8,  0,     DRT_SAFESEH,  RWF_IA32,   P_386, 0)
#endif

/* error directives */

res(DOT_ERR,     .err,         4, DF_STRPARM|DF_STORE,  DRT_ERRDIR,  0, P_86, 0)
res(DOT_ERR1,    .err1,        5, DF_STRPARM|DF_STORE,  DRT_ERRDIR,  0, P_86, CC_PASS1 )
res(DOT_ERR2,    .err2,        5, DF_STRPARM|DF_STORE,  DRT_ERRDIR,  0, P_86, CC_PASS2 )
res(DOT_ERRE,    .erre,        5, DF_STORE,             DRT_ERRDIR,  0, P_86, CC_NUMARG)
res(DOT_ERRNZ,   .errnz,       6, DF_STORE,             DRT_ERRDIR,  0, P_86, CC_NUMARG)
res(DOT_ERRDIF,  .errdif,      7, DF_STRPARM|DF_STORE,  DRT_ERRDIR,  0, P_86, CC_LITARG)
res(DOT_ERRDIFI, .errdifi,     8, DF_STRPARM|DF_STORE,  DRT_ERRDIR,  0, P_86, CC_LITARG)
res(DOT_ERRIDN,  .erridn,      7, DF_STRPARM|DF_STORE,  DRT_ERRDIR,  0, P_86, CC_LITARG)
res(DOT_ERRIDNI, .erridni,     8, DF_STRPARM|DF_STORE,  DRT_ERRDIR,  0, P_86, CC_LITARG)
res(DOT_ERRB,    .errb,        5, DF_STRPARM|DF_STORE,  DRT_ERRDIR,  0, P_86, CC_BLKARG)
res(DOT_ERRNB,   .errnb,       6, DF_STRPARM|DF_STORE,  DRT_ERRDIR,  0, P_86, CC_BLKARG)
res(DOT_ERRDEF,  .errdef,      7, DF_STRPARM|DF_STORE,  DRT_ERRDIR,  0, P_86, CC_SYMARG)
res(DOT_ERRNDEF, .errndef,     8, DF_STRPARM|DF_STORE,  DRT_ERRDIR,  0, P_86, CC_SYMARG)

/* conditional assembly directives, handled by preprocessor */

/* token          str        len  value        bytval     flgs cpu  sflags */
res(COMMENT,      comment,     7, 0,           DRT_CONDDIR, 0, P_86, 0)
res(IF,           if,          2, 0,           DRT_CONDDIR, 0, P_86, CC_NUMARG)
res(IFE,          ife,         3, 0,           DRT_CONDDIR, 0, P_86, CC_NUMARG)
res(IF1,          if1,         3, 0,           DRT_CONDDIR, 0, P_86, CC_PASS1 )
res(IF2,          if2,         3, 0,           DRT_CONDDIR, 0, P_86, CC_PASS2 )
res(IFDIF,        ifdif,       5, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_LITARG)
res(IFDIFI,       ifdifi,      6, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_LITARG)
res(IFIDN,        ifidn,       5, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_LITARG)
res(IFIDNI,       ifidni,      6, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_LITARG)
res(IFB,          ifb,         3, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_BLKARG)
res(IFNB,         ifnb,        4, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_BLKARG)
res(IFDEF,        ifdef,       5, DF_NOEXPAND, DRT_CONDDIR, 0, P_86, CC_SYMARG)
res(IFNDEF,       ifndef,      6, DF_NOEXPAND, DRT_CONDDIR, 0, P_86, CC_SYMARG)
res(ELSE,         else,        4, 0,           DRT_CONDDIR, 0, P_86, 0)
res(ELSEIF,       elseif,      6, 0,           DRT_CONDDIR, 0, P_86, CC_NUMARG)
res(ELSEIFE,      elseife,     7, 0,           DRT_CONDDIR, 0, P_86, CC_NUMARG)
res(ELSEIF1,      elseif1,     7, 0,           DRT_CONDDIR, 0, P_86, CC_PASS1 )
res(ELSEIF2,      elseif2,     7, 0,           DRT_CONDDIR, 0, P_86, CC_PASS2 )
res(ELSEIFDIF,    elseifdif,   9, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_LITARG)
res(ELSEIFDIFI,   elseifdifi, 10, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_LITARG)
res(ELSEIFIDN,    elseifidn,   9, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_LITARG)
res(ELSEIFIDNI,   elseifidni, 10, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_LITARG)
res(ELSEIFB,      elseifb,     7, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_BLKARG)
res(ELSEIFNB,     elseifnb,    8, DF_STRPARM,  DRT_CONDDIR, 0, P_86, CC_BLKARG)
res(ELSEIFDEF,    elseifdef,   9, DF_NOEXPAND, DRT_CONDDIR, 0, P_86, CC_SYMARG)
res(ELSEIFNDEF,   elseifndef, 10, DF_NOEXPAND, DRT_CONDDIR, 0, P_86, CC_SYMARG)
res(ENDIF,        endif,       5, 0,           DRT_CONDDIR, 0, P_86, 0)

/* assembly time loop directives, handled by preprocessor */

res(FOR,          for,         3, DF_NOEXPAND, DRT_LOOPDIR, 0, P_86, 0)
res(FORC,         forc,        4, DF_NOEXPAND, DRT_LOOPDIR, 0, P_86, 0)
res(IRP,          irp,         3, 0,           DRT_LOOPDIR, 0, P_86, 0)
res(IRPC,         irpc,        4, 0,           DRT_LOOPDIR, 0, P_86, 0)
res(REPEAT,       repeat,      6, 0,           DRT_LOOPDIR, 0, P_86, 0)
res(REPT,         rept,        4, 0,           DRT_LOOPDIR, 0, P_86, 0)
res(WHILE,        while,       5, 0,           DRT_LOOPDIR, 0, P_86, 0)

/* other preprocessor directives */

res(MACRO,     macro,          5, DF_LABEL,              DRT_MACRO,   0, P_86, 0)
res(EXITM,     exitm,          5, DF_STRPARM,            DRT_MACINT,  0, P_86, 0)
res(ENDM,      endm,           4, 0,                     DRT_MACINT,  0, P_86, 0)
res(GOTO,      goto,           4, 0,                     DRT_MACINT,  0, P_86, 0)
res(PURGE,     purge,          5, 0,                     DRT_PURGE,   0, P_86, 0)
res(INCLUDE,   include,        7, DF_NOEXPAND|DF_NOCONCAT,DRT_INCLUDE,0, P_86, 0)
res(TEXTEQU,   textequ,        7, DF_STRPARM | DF_LABEL, DRT_CATSTR,  0, P_86, 0)
res(CATSTR,    catstr,         6, DF_STRPARM | DF_LABEL, DRT_CATSTR,  0, P_86, 0)
res(SUBSTR,    substr,         6, DF_STRPARM | DF_LABEL, DRT_SUBSTR,  0, P_86, 0)

/* INSTR and SIZESTR aren't true preprocessor directives */
res(INSTR,     instr,          5, DF_STRPARM | DF_LABEL, DRT_INSTR,   0, P_86, 0)
res(SIZESTR,   sizestr,        7, DF_STRPARM | DF_LABEL, DRT_SIZESTR, 0, P_86, 0)

/* data definition directives */
res(DB,        db,             2, DF_LABEL,    DRT_DATADIR, 0, P_86, T_BYTE )
res(DW,        dw,             2, DF_LABEL,    DRT_DATADIR, 0, P_86, T_WORD )
res(DD,        dd,             2, DF_LABEL,    DRT_DATADIR, 0, P_86, T_DWORD)
res(DF,        df,             2, DF_LABEL,    DRT_DATADIR, 0, P_86, T_FWORD)
res(DQ,        dq,             2, DF_LABEL,    DRT_DATADIR, 0, P_86, T_QWORD)
res(DT,        dt,             2, DF_LABEL,    DRT_DATADIR, 0, P_86, T_TBYTE)

#if AMD64_SUPPORT
res(DOT_ALLOCSTACK, .allocstack,11, 0,        DRT_EXCFRAME, RWF_X64,  P_64, 0)
res(DOT_ENDPROLOG,  .endprolog, 10, 0,        DRT_EXCFRAME, RWF_X64,  P_64, 0)
res(DOT_PUSHFRAME,  .pushframe, 10, 0,        DRT_EXCFRAME, RWF_X64,  P_64, 0)
res(DOT_PUSHREG,    .pushreg,   8,  0,        DRT_EXCFRAME, RWF_X64,  P_64, 0)
res(DOT_SAVEREG,    .savereg,   8,  0,        DRT_EXCFRAME, RWF_X64,  P_64, 0)
res(DOT_SAVEXMM128, .savexmm128,11, 0,        DRT_EXCFRAME, RWF_X64,  P_64, 0)
res(DOT_SETFRAME,   .setframe,  9,  0,        DRT_EXCFRAME, RWF_X64,  P_64, 0)
#endif

/* "type" directives */
res(STRUC,       struc,       5, DF_LABEL,             DRT_STRUCT, 0,  P_86, 0)
res(STRUCT,      struct,      6, DF_LABEL,             DRT_STRUCT, 0,  P_86, 0)
res(UNION,       union,       5, DF_LABEL,             DRT_STRUCT, 0,  P_86, 0)
res(TYPEDEF,     typedef,     7, DF_LABEL,             DRT_TYPEDEF,0,  P_86, 0)
res(RECORD,      record,      6, DF_LABEL,             DRT_RECORD, 0,  P_86, 0)

/* "global" directives */
res(COMM,        comm,        4, 0,                    DRT_COMM,     0,  P_86, 0)
res(EXTERN,      extern,      6, 0,                    DRT_EXTERN,   0,  P_86, 0)
res(EXTRN,       extrn,       5, 0,                    DRT_EXTERN,   0,  P_86, 0)
res(EXTERNDEF,   externdef,   9, 0,                    DRT_EXTERNDEF,0,  P_86, 0)
res(PUBLIC,      public,      6, 0,                    DRT_PUBLIC,   0,  P_86, 0)

/* "proc" directives */
res(PROTO,       proto,       5, DF_LABEL,                            DRT_PROTO,  0,  P_86, 0)
res(PROC,        proc,        4, DF_CGEN|DF_LABEL|DF_NOSTRUC|DF_STORE,DRT_PROC,   0,  P_86, 0)
res(ENDP,        endp,        4, DF_LABEL|DF_NOSTRUC,                 DRT_ENDP,   0,  P_86, 0)
res(LOCAL,       local,       5, 0,                                   DRT_LOCAL,  0,  P_86, 0)
res(LABEL,       label,       5, DF_LABEL|DF_NOSTRUC|DF_STORE,        DRT_LABEL,  0,  P_86, 0)
res(INVOKE,      invoke,      6, DF_CGEN|DF_NOSTRUC|DF_PROC|DF_STORE, DRT_INVOKE, 0,  P_86, 0)

/* other directives */

/* token         str        len  value                       bytval     flags cpu sflags */
res(ORG,         org,         3, 0,                          DRT_ORG,     0,  P_86, 0)
res(ALIGN,       align,       5, 0,                          DRT_ALIGN,   0,  P_86, 0)
res(EVEN,        even,        4, 0,                          DRT_ALIGN,   0,  P_86, 0)

res(SEGMENT,     segment,     7, DF_LABEL|DF_NOSTRUC|DF_PROC,DRT_SEGMENT, 0,  P_86, 0)
res(ENDS,        ends,        4, DF_LABEL|DF_PROC,           DRT_ENDS,    0,  P_86, 0)
res(GROUP,       group,       5, DF_LABEL,                   DRT_GROUP,   0,  P_86, 0)
res(ASSUME,      assume,      6, 0,                          DRT_ASSUME,  0,  P_86, 0)

res(ALIAS,       alias,       5, 0,                          DRT_ALIAS,   0,  P_86, 0)
res(ECHO,        echo,        4, DF_NOEXPAND|DF_NOCONCAT,    DRT_ECHO,    0,  P_86, 0)
res(END,         end,         3, DF_CGEN|DF_NOSTRUC|DF_STORE,DRT_END,     0,  P_86, 0)
res(EQU,         equ,         3, DF_STRPARM|DF_LABEL,        DRT_EQU,     0,  P_86, 0)
#if INCBINSUPP
res(INCBIN,      incbin,      6, DF_NOSTRUC|DF_PROC|DF_STORE,DRT_INCBIN,  0,  P_86, 0)
#endif
res(INCLUDELIB,  includelib, 10, DF_NOCONCAT,                DRT_INCLIB,  0,  P_86, 0)
res(NAME,        name,        4, 0,                          DRT_NAME,    0,  P_86, 0)
res(OPTION,      option,      6, 0,                          DRT_OPTION,  0,  P_86, 0)
res(POPCONTEXT,  popcontext, 10, 0,                          DRT_CONTEXT, 0,  P_86, 0)
res(PUSHCONTEXT, pushcontext,11, 0,                          DRT_CONTEXT, 0,  P_86, 0)

