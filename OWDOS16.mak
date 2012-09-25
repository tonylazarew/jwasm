
# this makefile creates a DOS 16-bit real-mode version of JWasm (JWASMR.EXE).
# tools used:
# - Open Watcom v1.7a/v1.8/v1.9

name = JWasm

WATCOM = \Watcom

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTD=OWDOS16D
!else
OUTD=OWDOS16R
!endif

inc_dirs  = -IH -I$(WATCOM)\H

# to track memory leaks, the Open Watcom TRMEM module can be included.
# it's useful only if FASTMEM=0 is set, though, otherwise most allocs 
# won't use the C heap.
!ifndef TRMEM
TRMEM=0
!endif

LINK = $(WATCOM)\binnt\wlink.exe

#cflags stuff
#########
extra_c_flags =
!if $(DEBUG)
!if $(TRMEM)
extra_c_flags += -od -d2 -DDEBUG_OUT -DTRMEM
!else
extra_c_flags += -od -d2 -DDEBUG_OUT
!endif
!else
extra_c_flags += -obmilrs -s -DNDEBUG
!endif

#########

!if $(DEBUG)
LOPTD = debug dwarf op symfile
!endif

lflagsd = $(LOPTD) sys dos op map=$^*, stack=0x8400

CC=$(WATCOM)\binnt\wcc -q -0 -w3 -zc -ml -bc -bt=dos $(inc_dirs) $(extra_c_flags) -fo$@ -DFASTMEM=0 -DFASTPASS=0 -DCOFF_SUPPORT=0 -DELF_SUPPORT=0 -DAMD64_SUPPORT=0 -DSSE4SUPP=0 -DOWFC_SUPPORT=0 -DDLLIMPORT=0 -DAVXSUPP=0 -zt=12000

.c{$(OUTD)}.obj:
	@$(CC) $<

proj_obj = $(OUTD)/main.obj     $(OUTD)/assemble.obj $(OUTD)/input.obj   &
           $(OUTD)/tokenize.obj $(OUTD)/parser.obj   $(OUTD)/segment.obj &
           $(OUTD)/memalloc.obj $(OUTD)/cmdline.obj  $(OUTD)/condasm.obj &
           $(OUTD)/expans.obj   $(OUTD)/expreval.obj $(OUTD)/symbols.obj &
           $(OUTD)/codegen.obj  $(OUTD)/equate.obj   &
           $(OUTD)/directiv.obj $(OUTD)/assume.obj   $(OUTD)/posndir.obj &
           $(OUTD)/types.obj    $(OUTD)/invoke.obj   $(OUTD)/labels.obj  &
           $(OUTD)/errmsg.obj   $(OUTD)/macro.obj    $(OUTD)/string.obj  &
           $(OUTD)/mangle.obj   $(OUTD)/loop.obj     $(OUTD)/backptch.obj&
           $(OUTD)/fpfixup.obj  $(OUTD)/fixup.obj    $(OUTD)/data.obj    &
           $(OUTD)/reswords.obj $(OUTD)/branch.obj   $(OUTD)/queue.obj   &
           $(OUTD)/hll.obj      $(OUTD)/proc.obj     $(OUTD)/option.obj  &
           $(OUTD)/omf.obj      $(OUTD)/omfint.obj   $(OUTD)/omffixup.obj&
           $(OUTD)/coff.obj     $(OUTD)/elf.obj      $(OUTD)/bin.obj     &
           $(OUTD)/listing.obj  $(OUTD)/safeseh.obj &
           $(OUTD)/context.obj  $(OUTD)/extern.obj   $(OUTD)/simsegm.obj &
           $(OUTD)/msgtext.obj  $(OUTD)/linnum.obj   &
!if $(DEBUG)
!if $(TRMEM)
           $(OUTD)/trmem.obj    &
!endif
!endif
           $(OUTD)/tbyte.obj    $(OUTD)/cpumodel.obj&
           $(OUTD)/dbgcv.obj    $(OUTD)/end.obj      
######

TARGET=

ALL: $(OUTD) $(OUTD)/$(name)r.exe

$(OUTD):
	@if not exist $(OUTD) mkdir $(OUTD)

$(OUTD)/$(name)r.exe: $(OUTD)/$(name).lib $(OUTD)/main.obj
	@set LIB=$(WATCOM)\Lib286;$(WATCOM)\Lib286\DOS
	@$(LINK) @<<
$(lflagsd) file $(OUTD)/main.obj name $@ lib $(OUTD)/$(name).lib
<<

$(OUTD)/$(name).lib: $(proj_obj)
	@cd $(OUTD)
	@wlib -q -n $(name).lib $(proj_obj:$(OUTD)/=+)
	@cd ..

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	@$(CC) msgtext.c

$(OUTD)/reswords.obj: reswords.c H/instruct.h H/special.h H/directve.h
	@$(CC) reswords.c

######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
