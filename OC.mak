
# this makefile (NMake) creates the JWasm Win32 binary with Orange C.

name = jwasm

# directory paths to adjust
# CCDIR  - root directory for compiler, linker, include and lib files

CCDIR  = d:\OrangeC

!ifndef DEBUG
DEBUG=0
!endif

!ifndef OUTD
!if $(DEBUG)
OUTD=OCD
!else
OUTD=OCR
!endif
!endif

inc_dirs  = -IH -I"$(CCDIR)\include"

TRMEM=0

linker = $(CCDIR)\bin\olink.exe

!if $(DEBUG)
extra_c_flags = -DDEBUG_OUT
!else
extra_c_flags = -DNDEBUG
!endif

c_flags =-D__NT__ $(extra_c_flags)

#lflags stuff
#########
LOPT =
!if $(DEBUG)
LOPTD =
!endif

lflagsw = $(LOPTD) $(LOPT)

CC=$(CCDIR)\bin\occ.exe /c /C+F -D__OCC__ $(inc_dirs) $(c_flags)

.c{$(OUTD)}.o:
	@set ORANGEC=$(CCDIR)
	@set PATH=$(CCDIR)\bin;%PATH%
	@$(CC) -o$* $<

proj_obj = $(OUTD)\main.o     $(OUTD)\assemble.o $(OUTD)\assume.o  \
           $(OUTD)\directiv.o $(OUTD)\posndir.o  $(OUTD)\segment.o \
           $(OUTD)\expreval.o $(OUTD)\memalloc.o $(OUTD)\errmsg.o  \
           $(OUTD)\macro.o    $(OUTD)\string.o   $(OUTD)\condasm.o \
           $(OUTD)\types.o    $(OUTD)\fpfixup.o  $(OUTD)\invoke.o  \
           $(OUTD)\equate.o   $(OUTD)\mangle.o   $(OUTD)\loop.o    \
           $(OUTD)\parser.o   $(OUTD)\tokenize.o $(OUTD)\input.o   \
           $(OUTD)\expans.o   $(OUTD)\symbols.o  $(OUTD)\labels.o  \
           $(OUTD)\fixup.o    $(OUTD)\codegen.o  $(OUTD)\data.o    \
           $(OUTD)\reswords.o $(OUTD)\branch.o   $(OUTD)\queue.o   \
           $(OUTD)\hll.o      $(OUTD)\proc.o     $(OUTD)\option.o  \
           $(OUTD)\omf.o      $(OUTD)\omfint.o   $(OUTD)\omffixup.o\
           $(OUTD)\coff.o     $(OUTD)\elf.o      $(OUTD)\bin.o     \
           $(OUTD)\listing.o  $(OUTD)\safeseh.o \
           $(OUTD)\context.o  $(OUTD)\extern.o   $(OUTD)\simsegm.o \
           $(OUTD)\backptch.o $(OUTD)\msgtext.o  $(OUTD)\tbyte.o   \
           $(OUTD)\dbgcv.o    $(OUTD)\end.o      $(OUTD)\cpumodel.o\
           $(OUTD)\cmdline.o  $(OUTD)\apiemu.o   $(OUTD)\linnum.o  \
           $(OUTD)\fastpass.o
######

ALL: $(OUTD) $(OUTD)\$(name).exe

$(OUTD):
	@mkdir $(OUTD)

$(OUTD)\$(name).exe : $(proj_obj)
	@set ORANGEC=$(CCDIR)
	@set PATH=$(CCDIR)\bin;%PATH%
	$(linker) /c /T:CON32 /m /o$(OUTD)\$(name) @<<
$(lflagsw) /L$(CCDIR)\Lib $(proj_obj) c0xpe.o "clwin.l" "climp.l"
<<

$(OUTD)/msgtext.o: msgtext.c H/msgdef.h H/usage.h H/globals.h
	@set ORANGEC=$(CCDIR)
	@set PATH=$(CCDIR)\bin;%PATH%
	@$(CC) -o$* msgtext.c

$(OUTD)/reswords.o: reswords.c H/instruct.h H/special.h H/directve.h
	@set ORANGEC=$(CCDIR)
	@set PATH=$(CCDIR)\bin;%PATH%
	@$(CC) -o$* reswords.c

######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.o
	@erase $(OUTD)\*.map
