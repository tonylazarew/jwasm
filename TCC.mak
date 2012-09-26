
# This makefile (NMake) creates the JWasm Win32 binary with TCC.
# There are lots of warnings about unknown excape characters in msgdef.h!
# The resulting JWasm binary runs pretty slow.

name = jwasm
TCCDIR  = \tcc

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTD=TCCD
!else
OUTD=TCCR
!endif

inc_dirs  = -IH -I"$(TCCDIR)\include"

!if $(DEBUG)
extra_c_flags = -g -DDEBUG_OUT
!else
extra_c_flags = -DNDEBUG
!endif

CC=@$(TCCDIR)\tcc.exe $(inc_dirs) -D__NT__ $(extra_c_flags)

ALL: $(OUTD) $(OUTD)\$(name).exe

$(OUTD):
	@mkdir $(OUTD)

$(OUTD)\$(name).exe: *.c
	$(CC) -o $(OUTD)\$(name).exe *.c

clean:
	@erase $(OUTD)\$(name).exe
