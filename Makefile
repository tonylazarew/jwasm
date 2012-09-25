
# this makefile in OW WMake style creates JWASM.EXE (Win32) and optionally
# JWASMD.EXE (DOS).
# tools used:
# - Open Watcom v1.8/v1.9
# - HXDEV (only needed if DOS=1 is set below to create JWASMD.EXE)
#
# to create a debug version, run "WMake debug=1".
# to create a version with DJGPP support, run "WMake djgpp=1".

WIN=1
DOS=1

# Open Watcom root directory

WATCOM = \Watcom

# if DOS=1, HXDIR must contain the HX root directory

HXDIR = \HX

name = JWasm

!ifndef DEBUG
DEBUG=0
!endif

!ifndef DJGPP
DJGPP=0
!endif

# to track memory leaks, the Open Watcom TRMEM module can be included.
# it's useful only if FASTMEM=0 is set, though, otherwise most allocs 
# won't use the C heap.
!ifndef TRMEM
TRMEM=0
!endif

!ifndef OUTD
!if $(DEBUG)
OUTD=Debug
!else
OUTD=Release
!endif
!endif

# calling convention for compiler: s=Stack, r=register
# r will create a slightly smaller binary
CCV=r

inc_dirs  = -IH -I$(WATCOM)\H

!ifdef JWLINK
LINK = jwlink.exe
!else
LINK = $(WATCOM)\binnt\wlink.exe
!endif

#cflags stuff
#########
extra_c_flags =
!if $(DEBUG)
extra_c_flags += -od -d2 -w3 -DDEBUG_OUT
!else
#extra_c_flags += -obmilrt -s -DNDEBUG
extra_c_flags += -oxa -s -DNDEBUG
!endif

!if $(TRMEM)
extra_c_flags += -of -DTRMEM -DFASTMEM=0
!endif
!if $(DJGPP)
extra_c_flags += -DDJGPP_SUPPORT=1
!endif
#########

!if $(DEBUG)
# for OW v1.8, the debug version needs user32.lib to resolve CharUpperA()
# without it, WD(W) will crash immediately.
LOPTD = debug dwarf op symfile lib user32.lib
!else
LOPTD = 
!endif

CC=$(WATCOM)\binnt\wcc386 -q -3$(CCV) -zc -bc -bt=nt $(inc_dirs) $(extra_c_flags) -fo$@

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
           $(OUTD)/listing.obj  $(OUTD)/cmdline.obj &
           $(OUTD)/context.obj  $(OUTD)/extern.obj   $(OUTD)/simsegm.obj &
!if $(TRMEM)
           $(OUTD)/trmem.obj    &
!endif
           $(OUTD)/fastpass.obj $(OUTD)/backptch.obj $(OUTD)/tbyte.obj   &
           $(OUTD)/apiemu.obj   $(OUTD)/dbgcv.obj    $(OUTD)/end.obj     &
           $(OUTD)/cpumodel.obj $(OUTD)/safeseh.obj  $(OUTD)/linnum.obj  &
           $(OUTD)/msgtext.obj  
######

!if $(WIN)
TARGET1=$(OUTD)/$(name).exe
!endif
!if $(DOS)
TARGET2=$(OUTD)/$(name)d.exe
!endif

ALL: $(OUTD) $(TARGET1) $(TARGET2)

$(OUTD):
	@if not exist $(OUTD) mkdir $(OUTD)

$(OUTD)/$(name).exe: $(proj_obj)
	$(LINK) @<<
$(LOPTD)
format windows pe runtime console
file { $(proj_obj) } name $@
Libpath $(WATCOM)\lib386 
Libpath $(WATCOM)\lib386\nt
Library kernel32
op quiet, stack=0x20000, heapsize=0x100000, norelocs, map=$^* com stack=0x1000
<<
!if $(DEBUG)
	@if not exist TEST mkdir TEST
	copy $(OUTD)\$(name).exe TEST\*.* >NUL
	copy $(OUTD)\$(name).sym TEST\*.* >NUL
!endif

$(OUTD)/$(name)d.exe: $(proj_obj)
	$(LINK) @<<
$(LOPTD)
format windows pe runtime console
file { $(proj_obj) } name $@
Libpath $(WATCOM)\lib386 
Libpath $(WATCOM)\lib386\nt
Libpath $(HXDIR)\lib
Library imphlp.lib, dkrnl32s.lib 
Libfile cstrtwhx.obj 
op quiet, map=$^*, stub=$(HXDIR)\Bin\loadpex.bin, stack=0x40000, heapsize=0x100000
<<
#	$(HXDIR)\Bin\pestub.exe -x -z -n $@
	pestub.exe -x -z -n $@

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	$(CC) msgtext.c

$(OUTD)/reswords.obj: reswords.c H/instruct.h H/special.h H/directve.h H/opndcls.h H/instravx.h
	$(CC) reswords.c

######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
