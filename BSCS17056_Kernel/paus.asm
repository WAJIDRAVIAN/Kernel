[org 100h]
jmp start
stringP: db 'Paused'
printPaus:
			pusha
			xor bx,bx
			mov ax, 0xb800
			mov es,ax
			xor ax,ax
			mov di,2500
			mov ah,0x07
			mov cx,6
			LOOP3:
				MOV AL,[cs:stringP+bx]
				mov word[es:di],ax
				inc bx
				inc di
				inc di
			LOOP LOOP3
			popa
ret

delay:
push cx
push ax
mov cx,0xffff
loopDelay:
mov ax, 0xf
innerdelay:
sub ax, 1
cmp ax, 0x0000
jne innerdelay
loop loopDelay
pop ax
pop cx
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
mov ax,1800h
int 80
;call print
mov ax,0x4c00
int 21h
