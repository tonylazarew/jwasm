
# This makefile creates the JWasm Win32 or Win64 binary with PellesC. 
# Enter
#   pomake -f PellesC.mak
# to create the Win32 version. Or enter
#   pomake -f PellesC.mak amd64=1
# to create the Win64 version.
#
# Optionally, with 'pomake -f PellesC.mak DOS' one can additionally
# create a JWasm DOS binary (32-bit only. This needs the HXDEV package
# (see HXDIR below).

name = jwasm

# directory paths to adjust
# PODIR  - root directory for compiler, linker, include and lib files

!ifndef PODIR
PODIR  = \PellesC
!endif

!ifdef AMD64
TARGET=/Tamd64-coff
!endif

!ifdef DEBUG
OUTD=POCD
extra_c_flags = -Zi -DDEBUG_OUT $(TARGET)
LOPTD = /debug
!else
OUTD=POCR
extra_c_flags = -O2 -DNDEBUG $(TARGET)
LOPTD =
!endif

inc_dirs = -IH -I"$(PODIR)\include"

LINK = $(PODIR)\Bin\polink.exe
LIB = $(PODIR)\Bin\polib.exe

c_flags =-D__NT__ -Ze $(extra_c_flags)

lflagsw = $(LOPTD) /SUBSYSTEM:CONSOLE /map:$*.map

CC=$(PODIR)\bin\pocc.exe $(inc_dirs) $(c_flags)

.c{$(OUTD)}.obj:
	@$(CC) -J -Fo$*.obj $<

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
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   \
           $(OUTD)/apiemu.obj   $(OUTD)/dbgcv.obj    $(OUTD)/end.obj     \
           $(OUTD)/cpumodel.obj $(OUTD)/cmdline.obj  $(OUTD)/linnum.obj  \
           $(OUTD)/fastpass.obj
######

!ifdef DOS
# directory where HXDEV has been installed.
HXDIR=\hx
DOSTARG=$(OUTD)\$(name)d.exe
lflagsd = $(LOPTD) /NODEFAULTLIB /FIXED:NO /SUBSYSTEM:CONSOLE /map:$*.map /STUB:$(HXDIR)\bin\loadpex.bin /LIBPATH:$(HXDIR)\Lib dkrnl32s.lib imphlp.lib /STACK:0x40000,0x1000 /HEAP:0x1000,0x1000
!endif

all: $(OUTD) $(OUTD)\$(name).exe $(DOSTARG)

$(OUTD):
	@mkdir $(OUTD)

$(OUTD)\$(name).exe : $(OUTD)/main.obj $(OUTD)/$(name).lib
!ifdef AMD64
	$(LINK) $(lflagsw) $(OUTD)/main.obj $(OUTD)/$(name).lib /LIBPATH:$(PODIR)\Lib /LIBPATH:$(PODIR)\Lib\Win64 /OUT:$@
!else
	$(LINK) $(lflagsw) $(OUTD)/main.obj $(OUTD)/$(name).lib /LIBPATH:$(PODIR)\Lib /LIBPATH:$(PODIR)\Lib\Win /OUT:$@
!endif

$(OUTD)\$(name)d.exe : $(OUTD)/main.obj $(OUTD)/$(name).lib
	@$(LINK) $(lflagsd) $(HXDIR)\Lib\initw32.obj $(OUTD)/main.obj $(OUTD)/$(name).lib /LIBPATH:$(PODIR)\Lib crt.lib /OUT:$@
	@$(HXDIR)\bin\patchpe $*.exe

$(OUTD)\$(name).lib : $(proj_obj)
	@$(LIB) /out:$(OUTD)\$(name).lib $(proj_obj)

######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
