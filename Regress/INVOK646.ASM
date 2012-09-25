
;--- test 64-bit invoke with VARARG parameter

ifdef __JWASM__
	.x64
	.model flat, fastcall
endif

	.data
vq	dq 0
vd	dd 0
vw	dw 0
vb	db 0
	.code

p1 proc a1:VARARG
p1 endp

	invoke p1, 0
	ret

	invoke p1, 1,2,3,4
	invoke p1, vq,vd,vw,vb
	invoke p1, 1,2,3,4,5,6
	invoke p1, 1,2,3,4,vb,vw
	invoke p1, 1,2,3,4,addr vb,addr vw
	ret

	end
