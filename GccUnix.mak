
# This makefile creates the JWasm Elf binary for Linux/FreeBSD.

TARGET1=jwasm

ifndef DEBUG
DEBUG=0
endif

inc_dirs  = -IH

#cflags stuff

ifeq ($(DEBUG),0)
extra_c_flags = -DNDEBUG -O2
OUTD=GccUnixR
else
extra_c_flags = -DDEBUG_OUT -g
OUTD=GccUnixD
endif

c_flags =-D__UNIX__ $(extra_c_flags)

CC = gcc

.SUFFIXES:
.SUFFIXES: .c .o

proj_obj = $(OUTD)/main.o     $(OUTD)/assemble.o $(OUTD)/assume.o  \
           $(OUTD)/directiv.o $(OUTD)/posndir.o  $(OUTD)/segment.o \
           $(OUTD)/expreval.o $(OUTD)/memalloc.o $(OUTD)/errmsg.o  \
           $(OUTD)/macro.o    $(OUTD)/string.o   $(OUTD)/condasm.o \
           $(OUTD)/types.o    $(OUTD)/fpfixup.o  $(OUTD)/invoke.o  \
           $(OUTD)/equate.o   $(OUTD)/mangle.o   $(OUTD)/loop.o    \
           $(OUTD)/parser.o   $(OUTD)/tokenize.o $(OUTD)/input.o   \
           $(OUTD)/expans.o   $(OUTD)/symbols.o  $(OUTD)/labels.o  \
           $(OUTD)/fixup.o    $(OUTD)/codegen.o  $(OUTD)/data.o    \
           $(OUTD)/reswords.o $(OUTD)/branch.o   $(OUTD)/queue.o   \
           $(OUTD)/hll.o      $(OUTD)/proc.o     $(OUTD)/option.o  \
           $(OUTD)/omf.o      $(OUTD)/omfint.o   $(OUTD)/omffixup.o\
           $(OUTD)/coff.o     $(OUTD)/elf.o      $(OUTD)/bin.o     \
           $(OUTD)/listing.o  $(OUTD)/linnum.o  \
           $(OUTD)/context.o  $(OUTD)/extern.o   $(OUTD)/simsegm.o \
           $(OUTD)/apiemu.o   $(OUTD)/dbgcv.o    $(OUTD)/end.o     \
           $(OUTD)/backptch.o $(OUTD)/msgtext.o  $(OUTD)/tbyte.o   \
           $(OUTD)/cpumodel.o $(OUTD)/safeseh.o  $(OUTD)/cmdline.o \
           $(OUTD)/fastpass.o 
######

#.c.o:
#	$(CC) -c $(inc_dirs) $(c_flags) -o $(OUTD)/$*.o $<
$(OUTD)/%.o: %.c
	$(CC) -c $(inc_dirs) $(c_flags) -o $(OUTD)/$*.o $<

all:  $(OUTD) $(OUTD)/$(TARGET1)

$(OUTD):
	mkdir $(OUTD)

$(OUTD)/$(TARGET1) : $(proj_obj)
ifeq ($(DEBUG),0)
	$(CC) $(proj_obj) -s -o $@ -Wl,-Map,$(OUTD)/$(TARGET1).map
else
	$(CC) $(proj_obj) -o $@ -Wl,-Map,$(OUTD)/$(TARGET1).map
endif

$(OUTD)/msgtext.o: msgtext.c H/msgdef.h H/usage.h
	$(CC) -c $(inc_dirs) $(c_flags) -o $*.o msgtext.c

$(OUTD)/reswords.o: reswords.c H/instruct.h H/special.h H/directve.h
	$(CC) -c $(inc_dirs) $(c_flags) -o $*.o reswords.c

######

clean:
	rm $(OUTD)/$(TARGET1)
	rm $(OUTD)/*.o
	rm $(OUTD)/*.map

