@echo off
rem tools: jwasm, jwlink, jfc, ms link (COFF)
for %%f in (*.asm) do call :cmpbin %%f
for %%f in (*.asn) do call :cmpmz  %%f
for %%f in (*.aso) do call :cmperr %%f
for %%f in (*.asz) do call :lnkomf %%f
call :safeseh
call :coff
call :coffdbg
call :ifdef
exit

:cmpbin
jwasm -q -bin %1
..\tools\jfc %~n1.BIN %~n1.EXP
goto end

:cmpmz
jwasm -q -mz %1
..\tools\jfc %~n1.EXE %~n1.EXP
goto end

:cmperr
jwasm -q -omf %1 >NUL
..\tools\jfc %~n1.ERR %~n1.EXP
goto end

:lnkomf
jwasm -q -omf %1
if errorlevel 1 goto end
..\tools\jwlink op q,nofar format dos file %~n1.OBJ
..\tools\jfc %~n1.EXE %~n1.EXP
goto end

:safeseh
jwasm -q -coff -safeseh SAFESEH.ASC
if errorlevel 1 goto end
..\tools\link /nologo /SAFESEH SAFESEH.OBJ SAFESEH.LIB
..\tools\jfc -pe SAFESEH.EXE SAFESEH.EXP
goto end

:coff
rem weak externals
jwasm -q -coff EXTERN4.ASC
if errorlevel 1 goto end
..\tools\link /nologo /subsystem:console EXTERN4.OBJ
..\tools\jfc -pe EXTERN4.EXE EXTERN4.EXP

rem ALIAS segment attribute
jwasm -q -coff SEGALIAS.ASC
if errorlevel 1 goto end
..\tools\jfc -co SEGALIAS.OBJ SEGALIAS.EXP

rem 64-bit dll
jwasm -q -coff DLL64.ASC
if errorlevel 1 goto end
..\tools\jfc -co DLL64.OBJ DLL64.EXP

jwasm -q -coff SEH64.ASC
if errorlevel 1 goto end
..\tools\jfc -co SEH64.OBJ SEH64.EXP

jwasm -q -win64 MOV643.ASC
if errorlevel 1 goto end
..\tools\jfc -co MOV643.OBJ MOV643.EXP

jwasm -q -win64 PROC641.ASC
if errorlevel 1 goto end
..\tools\jfc -co PROC641.OBJ PROC641.EXP

rem option dllimport
jwasm -q -coff -Fd DLLIMP.ASC
if errorlevel 1 goto end
..\tools\jwlink2 format win pe f DLLIMP.OBJ op q,noreloc
..\tools\jfc -pe DLLIMP.EXE DLLIMP.EXP

goto end

:coffdbg
jwasm -q -Zi -coff DBGCV.ASC
if errorlevel 1 goto end
..\tools\jfc -co DBGCV.OBJ DBGCV.EXP
goto end

:ifdef
..\owdos16r\jwasmr -q -zlc -zld ISDEF.ASC
if errorlevel 1 goto end
..\tools\jfc ISDEF.OBJ ISDEF.EXP
goto end

:end
