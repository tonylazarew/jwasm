
	.x64
	.model flat, fastcall

printf proto :ptr byte, :vararg

	.data

szFmt db "%X",0

	.code

	invoke printf, addr szFmt
	invoke printf, addr szFmt, rax, edx, al, eax, ah
	ret

printf proc fmt:ptr byte, params:VARARG
	ret
printf endp

    END
