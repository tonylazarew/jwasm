
;--- type expressions with EQ/NE operator

	.386
	.model flat

UINT typedef DWORD

	.code

	if (type(dword ptr [edx])) eq DWORD
		db 1
	endif
	if (type(dword ptr [edx])) eq REAL4
		db 0
	endif
	if (type(byte ptr [edx]))  eq BYTE
		db 2
	endif
	if (type(byte ptr [edx]))  eq SBYTE
		db 0
	endif
	if (type(sword ptr [edx])) eq SWORD
		db 3
	endif
	if (type(sword ptr [edx])) eq WORD
		db 0
	endif
	if (type(real4 ptr [edx])) ne REAL8
		db 4
	endif
	if (type(qword ptr [edx])) ne qword
		db 0
	endif
	if (type(UINT ptr [edx]))  eq UINT
		db 5
	endif
	if (type(DWORD ptr [edx])) eq UINT   ;this is TRUE!
		db 6
	endif
	if (type(SDWORD ptr [edx])) eq UINT
		db 0
	endif

	end
