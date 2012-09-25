
# this makefile creates the 32bit OS/2 binary of JWasm.
# tools used:
# - Open Watcom v1.7a/v1.8/v1.9

# 2011-07-09 -- rousseau at ecomstation.com -- fixed some stuff.
# - Removed a trailing space after the '&' in the object-list on the 
#   line with '$(OUTD)/omffixup.obj' that breaks wmake v1.9.
# - Added '.SYMBOLIC' to 'clean:' to supress dependency checking.
# - Added check for existence of files to 'clean:' to supress 
#   abort when files are not found.
# - Replaced 'erase' with 'del' in 'clean:' as this is the more common name.


name = JWasm

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTD=OWOS2D
!else
OUTD=OWOS2R
!endif

# calling convention for compiler: s=Stack, r=register
# r will create a slightly smaller binary
CCV=r

inc_dirs  = -IH

LINK = wlink.exe

#cflags stuff
#########
extra_c_flags =
!if $(DEBUG)
extra_c_flags += -od -d2 -DDEBUG_OUT
!else
extra_c_flags += -obmilrt -s -DNDEBUG
!endif

#########

LOPT = op quiet
!if $(DEBUG)
LOPTD = debug dwarf op symfile
!endif

lflagso = $(LOPTD) system os2v2 $(LOPT) op map=$^*

CC=wcc386 -q -3$(CCV) -bc -bt=os2 $(inc_dirs) $(extra_c_flags) -fo$@

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
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   &
           $(OUTD)/dbgcv.obj    $(OUTD)/end.obj      $(OUTD)/cpumodel.obj&
           $(OUTD)/cmdline.obj  $(OUTD)/linnum.obj   $(OUTD)/fastpass.obj
######

TARGET1=$(OUTD)/$(name).exe

ALL: $(OUTD) $(TARGET1)

$(OUTD):
	@if not exist $(OUTD) mkdir $(OUTD)

$(TARGET1): $(proj_obj)
	$(LINK) @<<
$(lflagso) file { $(proj_obj) } name $@ op stack=0x20000
<<

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	$(CC) msgtext.c

$(OUTD)/reswords.obj: reswords.c H/instruct.h H/special.h H/directve.h
	$(CC) reswords.c

######

clean: .SYMBOLIC
	@if exist $(OUTD)\*.exe del $(OUTD)\*.exe
	@if exist $(OUTD)\*.obj del $(OUTD)\*.obj
	@if exist $(OUTD)\*.map del $(OUTD)\*.map
