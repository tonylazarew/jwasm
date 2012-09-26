
# this makefile (WMake) creates the Linux binary of JWasm.
# Open Watcom v1.7-v1.9 may be used.

name = jwasm

!ifndef DEBUG
DEBUG=0
!endif

!ifndef OUTD
!if $(DEBUG)
OUTD=OWLinuxD
!else
OUTD=OWLinuxR
!endif
!endif

# calling convention for compiler: s=Stack, r=Register
CCV=r

WATCOM=\Watcom

inc_dirs  = -IH -I$(WATCOM)\LH

# to track memory leaks, the Open Watcom TRMEM module can be included
TRMEM=0

LINK = $(WATCOM)\Binnt\wlink.exe

#cflags stuff
#########
extra_c_flags =
!if $(DEBUG)
extra_c_flags += -od -d2 -DDEBUG_OUT
!else
extra_c_flags += -ot -s -DNDEBUG
!endif

#lflags stuff
#########
!if $(DEBUG)
LOPTD = debug dwarf op symfile
!endif

CC = $(WATCOM)\Binnt\wcc386 -q -3$(CCV) -zc -bc -bt=linux $(inc_dirs) $(extra_c_flags) -fo$@

.c{$(OUTD)}.obj:
	$(CC) $<

proj_obj = $(OUTD)/main.obj     $(OUTD)/assemble.obj $(OUTD)/assume.obj  &
           $(OUTD)/directiv.obj $(OUTD)/posndir.obj  $(OUTD)/segment.obj &
           $(OUTD)/expreval.obj $(OUTD)/memalloc.obj $(OUTD)/errmsg.obj  &
           $(OUTD)/macro.obj    $(OUTD)/string.obj   $(OUTD)/condasm.obj &
           $(OUTD)/types.obj    $(OUTD)/fpfixup.obj  $(OUTD)/invoke.obj  &
           $(OUTD)/equate.obj   $(OUTD)/mangle.obj   $(OUTD)/loop.obj    &
           $(OUTD)/parser.obj   $(OUTD)/tokenize.obj $(OUTD)/input.obj   &
           $(OUTD)/expans.obj   $(OUTD)/symbols.obj  $(OUTD)/labels.obj  &
           $(OUTD)/fixup.obj    $(OUTD)/codegen.obj  $(OUTD)/data.obj    &
           $(OUTD)/reswords.obj $(OUTD)/branch.obj   $(OUTD)/queue.obj   &
           $(OUTD)/hll.obj      $(OUTD)/proc.obj     $(OUTD)/option.obj  &
           $(OUTD)/omf.obj      $(OUTD)/omfint.obj   $(OUTD)/omffixup.obj&
           $(OUTD)/coff.obj     $(OUTD)/elf.obj      $(OUTD)/bin.obj     &
           $(OUTD)/listing.obj  $(OUTD)/safeseh.obj &
           $(OUTD)/context.obj  $(OUTD)/extern.obj   $(OUTD)/simsegm.obj &
!if $(TRMEM)
           $(OUTD)/trmem.obj    &
!endif
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   &
           $(OUTD)/dbgcv.obj    $(OUTD)/end.obj      $(OUTD)/cpumodel.obj&
           $(OUTD)/cmdline.obj  $(OUTD)/linnum.obj   $(OUTD)/fastpass.obj
######

ALL: $(OUTD) $(OUTD)/$(name)

$(OUTD):
	@if not exist $(OUTD) mkdir $(OUTD)

$(OUTD)/$(name) : $(proj_obj)
	$(LINK) @<<
format elf runtime linux
$(LOPTD)
libpath $(WATCOM)/lib386
libpath $(WATCOM)/lib386/linux
op map=$^*, norelocs, quiet, stack=0x20000
file { $(proj_obj) }
name $@.
<<

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	$(CC) msgtext.c

$(OUTD)/reswords.obj: reswords.c H/instruct.h H/special.h H/directve.h
	$(CC) reswords.c

######

clean:
	@erase $(OUTD)\*.
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
