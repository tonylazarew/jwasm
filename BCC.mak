
# this makefile (NMake) creates the JWasm Win32 binary with Borland Commandline Tools.

name = jwasm

BCDIR = \bc55

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTD=BCC32D
!else
OUTD=BCC32R
!endif

inc_dirs  = /IH /I"$(BCDIR)\include"

!if $(DEBUG)
extra_c_flags = -v -y -DDEBUG_OUT
!else
extra_c_flags = -O2 /DNDEBUG
!endif

c_flags =-q -WC -K -D__NT__ -w-8012 -w-8057 -w-8060 $(extra_c_flags)

CC = $(BCDIR)\bin\bcc32.exe -c $(inc_dirs) $(c_flags)
LINK = $(BCDIR)\Bin\ilink32.exe -s -Tpe -ap -Gn -c -L$(BCDIR)\Lib 

.c{$(OUTD)}.obj:
	@$(CC) -o$* $<

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

TARGET1=$(OUTD)\$(name).exe 

ALL: $(OUTD) $(TARGET1)

$(OUTD):
	@mkdir $(OUTD)

$(OUTD)\$(name).exe : $(proj_obj)
	@cd $(OUTD)
!if $(DEBUG)
	$(LINK) @<<
$(BCDIR)\Lib\c0x32.obj $(proj_obj:BCC32D/=+), $(name).exe, $(name).map, import32.lib cw32.lib
<<
!else
	$(LINK) @<<
$(BCDIR)\Lib\c0x32.obj $(proj_obj:BCC32R/=+), $(name).exe, $(name).map, import32.lib cw32.lib
<<
!endif
	@cd ..

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h
	@$(CC) /o$* msgtext.c

$(OUTD)/reswords.obj: reswords.c H/instruct.h H/special.h H/directve.h
	@$(CC) /o$* reswords.c

######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
