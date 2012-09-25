
# this makefile (NMake) creates the JWasm Win32 binary with the Intel C++
# compiler. It has been tested with:
# - Intel C++ on IA-32, v11.1
# the Intel compiler needs external includes and libraries, compatible with
# MS Visual C. The MS VC++ Toolkit 2003 has been used here.
# directory paths to adjust:
# CDIR   - root directory of the Intel C compiler
# VCDIR  - root directory for Visual C includes and libraries
# W32LIB - directory for Win32 import library files (kernel32.lib).
#          Default is WinInc ( may be changed to the MS Platform SDK ).

name = jwasm

CDIR  = g:\intelcpp
VCDIR = \msvc71
W32LIB = \WinInc\Lib

# use the MS linker or jwlink (jwlink is experimental!)
!ifndef MSLINK
MSLINK=1
!endif

!ifndef DEBUG
DEBUG=0
!endif

!ifndef OUTD
!if $(DEBUG)
OUTD=ICD
!else
OUTD=ICR
!endif
!endif

inc_dirs  = -IH -I"$(CDIR)\include" -I"$(VCDIR)\include"

TRMEM=0

linker = $(CDIR)\Bin\xilink.exe
lib = $(CDIR)\Bin\xilib.exe

!if $(DEBUG)
!if $(TRMEM)
extra_c_flags = -Zd -Od -DDEBUG_OUT -DTRMEM
!else
extra_c_flags = -Zd -Od -DDEBUG_OUT -FAa -Fa$* 
!endif
!else
extra_c_flags = -O2 -Gs -DNDEBUG
#extra_c_flags = -Ox -DNDEBUG
!endif

c_flags =-D__NT__ $(extra_c_flags) $(c_flags64)

# if MSVC++ 2005 EE is used:
# 1. define __STDC_WANT_SECURE_LIB__=0 to avoid "deprecated" warnings
# 2. define -GS- to disable security checks
#c_flags =-D__NT__ $(extra_c_flags) -D__STDC_WANT_SECURE_LIB__=0 -GS-

#lflags stuff
#########
LOPT = /NOLOGO
!if $(DEBUG)
LOPTD = /debug
!endif

lflagsw = $(LOPTD) /SUBSYSTEM:CONSOLE $(LOPT) /map:$^*.map /OPT:NOWIN98

CC=$(CDIR)\bin\icl.exe -c -nologo $(inc_dirs) $(c_flags)

.c{$(OUTD)}.obj:
	@$(CC) -Fo$* $<

proj_obj = $(OUTD)/main.obj     $(OUTD)/assemble.obj $(OUTD)/assume.obj  \
           $(OUTD)/directiv.obj $(OUTD)/posndir.obj  $(OUTD)/segment.obj \
           $(OUTD)/expreval.obj $(OUTD)/memalloc.obj $(OUTD)/errmsg.obj  \
           $(OUTD)/macro.obj    $(OUTD)/string.obj   $(OUTD)/condasm.obj \
           $(OUTD)/types.obj    $(OUTD)/fpfixup.obj  $(OUTD)/invoke.obj  \
           $(OUTD)/equate.obj   $(OUTD)/mangle.obj   $(OUTD)/loop.obj    \
           $(OUTD)/parser.obj   $(OUTD)/tokenize.obj $(OUTD)/input.obj   \
           $(OUTD)/expans.obj   $(OUTD)/symbols.obj  $(OUTD)/labels.obj  \
           $(OUTD)/fixup.obj    $(OUTD)/codegen.obj  $(OUTD)/data.obj    \
           $(OUTD)/reswords.obj $(OUTD)/branch.obj   $(OUTD)/queue.obj   \
           $(OUTD)/hll.obj      $(OUTD)/proc.obj     $(OUTD)/option.obj  \
           $(OUTD)/omf.obj      $(OUTD)/omfint.obj   $(OUTD)/omffixup.obj\
           $(OUTD)/coff.obj     $(OUTD)/elf.obj      $(OUTD)/bin.obj     \
           $(OUTD)/listing.obj  $(OUTD)/safeseh.obj \
           $(OUTD)/context.obj  $(OUTD)/extern.obj   $(OUTD)/simsegm.obj \
           $(OUTD)/cmdline.obj  $(OUTD)/linnum.obj   $(OUTD)/fastpass.obj\
!if $(TRMEM)
           $(OUTD)/trmem.obj    \
!endif
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   \
           $(OUTD)/dbgcv.obj    $(OUTD)/end.obj      $(OUTD)/cpumodel.obj
######

TARGET1=$(OUTD)\$(name).exe

ALL: $(OUTD) $(TARGET1)

$(OUTD):
	@mkdir $(OUTD)

$(OUTD)\$(name).exe : $(OUTD)/main.obj $(OUTD)/$(name).lib
!if $(MSLINK)
	@$(linker) @<<
$(lflagsw) $(OUTD)/main.obj $(OUTD)/$(name).lib
/LIBPATH:"$(CDIR)\Lib\Ia32" /LIBPATH:"$(VCDIR)\Lib" /LIBPATH:"$(W32LIB)" /OUT:$@
<<
!else
	@jwlink format windows pe file $(OUTD)/main.obj name $@ lib $(OUTD)/$(name).lib libpath "$(CDIR)\Lib\Ia32" libpath "$(VCDIR)\Lib" libpath "$(W32LIB)" op start=_mainCRTStartup, norelocs, eliminate, map=$(OUTD)/$(name).map
!endif

$(OUTD)\$(name).lib : $(proj_obj)
	@$(lib) /nologo /out:$(OUTD)\$(name).lib $(proj_obj)

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	@$(CC) -Fo$* msgtext.c

$(OUTD)/reswords.obj: reswords.c H/instruct.h H/special.h H/directve.h
	@$(CC) -Fo$* reswords.c

######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
