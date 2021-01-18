[org 100h]
jmp start
stringRes: db 'Resumed'
printResum:
			pusha
			xor bx,bx
			mov ax, 0xb800
			mov es,ax
			xor ax,ax
			mov di,2500
			mov ah,0x07
			mov cx,7
			LOOP3:
				MOV AL,[cs:stringRes+bx]
				mov word[es:di],ax
				inc bx
				inc di
				inc di
			LOOP LOOP3
			popa
ret


; ;---------------------------------------------------------------
; ; SubServices 
; ;_______________________________________________________________
; ;***************************************************************

; add task		: 1600h
; end task		: 1700h
; pause task	: 1800h
; resume task	: 1900h
; remove task	: 2000h
start:

mov si,3
mov ax,1900h
int 80
;call print
mov ax,0x4c00
int 21h
