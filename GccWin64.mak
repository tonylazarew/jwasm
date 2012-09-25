
# This makefile creates a Win64 binary of JWasm with MinGW-w64.
#  'make -f GccWin64.mak'

name = jwasm

ifndef DEBUG
DEBUG=0
endif

inc_dirs  = -IH

#cflags stuff

ifeq ($(DEBUG),1)
extra_c_flags = -DDEBUG_OUT -g
OUTD=MinGW-w64D
else
extra_c_flags = -DNDEBUG -O2
OUTD=MinGW-w64R
endif

c_flags = -D__NT__ $(extra_c_flags)

CC=x86_64-w64-mingw32-gcc.exe -c $(inc_dirs) $(c_flags)

$(OUTD)/%.obj: %.c
	$(CC) -o $(OUTD)/$*.obj $<

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
           $(OUTD)/dbgcv.obj    $(OUTD)/end.obj      $(OUTD)/cpumodel.obj\
           $(OUTD)/cmdline.obj  $(OUTD)/linnum.obj   $(OUTD)/fastpass.obj
######

TARGET1=$(OUTD)/$(name).exe

ALL: $(OUTD) $(TARGET1)

$(OUTD):
	mkdir $(OUTD)

$(OUTD)/$(name).exe : $(proj_obj)
	x86_64-w64-mingw32-gcc.exe $(proj_obj) -s -o $(OUTD)/$(name).exe -Wl,-Map,$(OUTD)/$(name).map

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h
	$(CC) -o $(OUTD)/msgtext.obj msgtext.c

$(OUTD)/reswords.obj: reswords.c H/instruct.h H/special.h H/directve.h
	$(CC) -o $(OUTD)/reswords.obj reswords.c

######

clean:
	@rm $(OUTD)/*.exe
	@rm $(OUTD)/*.obj
	@rm $(OUTD)/*.map
