
# this makefile in OW WMake style creates JWASM.DLL (Win32)
# tools used:
# - Open Watcom v1.8/v1.9

# Open Watcom root directory

WATCOM = \Watcom

name = JWasm

!ifndef DEBUG
DEBUG=0
!endif

!ifndef DJGPP
DJGPP=0
!endif

!ifndef OUTD
!if $(DEBUG)
OUTD=OWWinDllD
!else
OUTD=OWWinDll
!endif
!endif

# calling convention for compiler: s=Stack, r=register
# r will create a slightly smaller binary
CCV=r

inc_dirs  = -IH -I$(WATCOM)\H

LINK = $(WATCOM)\binnt\wlink.exe

#cflags stuff
#########
extra_c_flags =
!if $(DEBUG)
extra_c_flags += -od -d2 -w3 -DDEBUG_OUT
!else
extra_c_flags += -ox -s -DNDEBUG
!endif

!if $(DJGPP)
extra_c_flags += -DDJGPP_SUPPORT=1
!endif
#########

LOPT = op quiet
!if $(DEBUG)
# for OW v1.8, the debug version needs user32.lib to resolve CharUpperA()
# without it, WD(W) will crash immediately.
LOPTD = debug dwarf op symfile lib user32.lib
!endif

lflagsw = $(LOPTD) format windows pe dll $(LOPT) op map=$^*, offset=0x5000000 export AssembleModule='_AssembleModule@4', ParseCmdline='_ParseCmdline@8', CmdlineFini='_CmdlineFini@0'

CC=$(WATCOM)\binnt\wcc386 -q -3$(CCV) -bd -zc -bt=nt $(inc_dirs) $(extra_c_flags) -fo$@

.c{$(OUTD)}.obj:
	$(CC) $<

proj_obj = $(OUTD)/assemble.obj $(OUTD)/assume.obj  &
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
           $(OUTD)/listing.obj  $(OUTD)/cmdline.obj &
           $(OUTD)/context.obj  $(OUTD)/extern.obj   $(OUTD)/simsegm.obj &
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   &
           $(OUTD)/apiemu.obj   $(OUTD)/dbgcv.obj    $(OUTD)/end.obj     &
           $(OUTD)/cpumodel.obj $(OUTD)/safeseh.obj  $(OUTD)/linnum.obj  &
           $(OUTD)/fastpass.obj
######

ALL: $(OUTD) $(OUTD)/$(name).dll

$(OUTD):
	@if not exist $(OUTD) mkdir $(OUTD)

$(OUTD)/$(name).dll: $(proj_obj)
	$(LINK) @<<
$(lflagsw) file { $(proj_obj) } name $@
Libpath $(WATCOM)\lib386 
Libpath $(WATCOM)\lib386\nt
Library kernel32, user32
<<

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	$(CC) msgtext.c

$(OUTD)/reswords.obj: reswords.c H/instruct.h H/special.h H/directve.h
	$(CC) reswords.c

######

clean:
	@erase $(OUTD)\*.dll
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
