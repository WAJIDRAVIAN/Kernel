[org 100h]

jmp start
countt :  dw 0x0000
string :db 'Wajid Ali'
len equ 9
;cs, ip, ds, es
parameter: times 4 dw 0x0000
		tcs equ 00
		tip equ 02
		tds equ 04
		tes equ 06
print3:
	again:
			pusha
			mov ax, 0xb800
			mov es,ax
			mov di,230
			inc word[cs:countt]
			mov bx,[cs:countt]
			mov cx,4
			iloop2:
						mov ax,bx
						and ax,0xF000
						shr ax,12
						cmp ax,9
						ja skipp
								add ax, 0x0730
								jmp skipendd
						skipp:
											add ax,0x0737
										skipendd:
						mov word[es:di],ax
						inc di
						inc di
						shl bx,4
			loop iloop2
			popa
			jmp again
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
xor ax,ax
mov word[cs:countt],ax
mov [cs:parameter+tcs],cs
mov word[cs:parameter+tip],print3
mov [cs:parameter+tds],ds
mov [cs:parameter+tes],es
mov si,parameter
mov ax,0x1600
int 80
mov dx,start
add dx,15
shr dx,4
mov ax,0x3100
int 21h