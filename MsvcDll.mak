
# this makefile (NMake) creates the JWasm Win32 dll with MS Visual C++.
# it has been tested with:
# - VC++ Toolkit 2003 ( = VC++ 7.1 )

name = jwasm

# directory paths to adjust
# VCDIR  - root directory for VC compiler, linker, include and lib files
# W32LIB - directory for Win32 import library files (kernel32.lib)

VCDIR  = \msvc71
W32LIB = \wininc\lib

!ifndef DEBUG
DEBUG=0
!endif

!ifndef OUTD
!if $(DEBUG)
OUTD=MsvcDllD
!else
OUTD=MsvcDllR
!endif
!endif

inc_dirs  = -IH -I"$(VCDIR)\include"

linker = $(VCDIR)\Bin\link.exe
lib = $(VCDIR)\Bin\lib.exe

!if $(DEBUG)
extra_c_flags = -Zd -Od -DDEBUG_OUT
!else
extra_c_flags = -O2 -Gs -DNDEBUG
#extra_c_flags = -Ox -DNDEBUG
!endif

c_flags =-D__NT__ -D__SW_BD $(extra_c_flags) $(c_flags64)

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

lflagsw = $(LOPTD) $(LOPT) /map:$^*.map /OPT:NOWIN98

CC=@$(VCDIR)\bin\cl.exe -c -nologo $(inc_dirs) $(c_flags)

.c{$(OUTD)}.obj:
	$(CC) -Fo$* $<

proj_obj = $(OUTD)/assemble.obj $(OUTD)/assume.obj  \
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
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   \
           $(OUTD)/dbgcv.obj    $(OUTD)/end.obj      $(OUTD)/cpumodel.obj
######

ALL: $(OUTD) $(OUTD)\$(name).dll

$(OUTD):
	@mkdir $(OUTD)

$(OUTD)\$(name).dll : $(OUTD)/$(name)s.lib
	$(linker) @<<
$(lflagsw) $(OUTD)/$(name)s.lib
/LIBPATH:"$(VCDIR)\Lib" /DLL "$(W32LIB)/kernel32.lib" /OUT:$@
/EXPORT:AssembleModule /EXPORT:ParseCmdline /EXPORT:CmdlineFini
<<

$(OUTD)\$(name)s.lib : $(proj_obj)
	@$(lib) /out:$(OUTD)\$(name)s.lib $(proj_obj)

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	$(CC) -Fo$* msgtext.c

$(OUTD)/reswords.obj: reswords.c H/instruct.h H/special.h H/directve.h
	$(CC) -Fo$* reswords.c

######

clean:
	@erase $(OUTD)\*.dll
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
