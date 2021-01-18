				[org 100h]

				jmp start
				;*****************************************************************
				;	|:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
				;	|	#$	Assembly MultiTasking Kernel						$#
				;	|	#$														$#
				;	|	#$	By:													$#
				;	|	#$		Wajid Ali										$#
				;	|	#$		BSCS17056										$#
				;	|	#$		wajidh1997@gmail.com							$#
				;___|___#$______________________________________________________$#
				;.................................................................
				;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				; ;---------------------------------------------------------------
				; ; SubServices that have been used
				; ;_______________________________________________________________
				; ;***************************************************************
				; add task		: 1600h
				; end task		: 1700h
				; pause task	: 1800h
				; resume task	: 1900h
				; remove task	: 2000h

				;---------------------------------------------------------------
				;_______________________________________________________________
				;***************************************************************
				;SUBROUTINE OF INTERRUPT
				interrupt:
				call SAVESTATE
				call GETNEXT
				call printTaskNo ;printing taskno of current process

				call RESTORESTATE
				call wajid
				push ax
				mov al,0x20
				out 0x20,al
				pop ax
				iret
				;---------------------------------------------------------------
				;_______________________________________________________________
				;***************************************************************
				SAVESTATE:

				PUSH BX
				PUSH DS
				PUSH CS
				POP DS
				MOV BX, [CS:CURPROC]
				SHL BX,5
				MOV [cs:PCB+BX+AXSAVE],AX ; saving ax
				POP AX ; poping value of bx
				MOV [cs:PCB+BX+BXSAVE],AX ; saving bx
				MOV [cs:PCB+BX+CXSAVE],CX ;saving cx
				MOV [cs:PCB+BX+DXSAVE],DX
				MOV [cs:PCB+BX+ESSAVE],ES
				MOV [cs:PCB+BX+DSSAVE],DS
				MOV [cs:PCB+BX+SSSAVE],SS
				MOV [cs:PCB+BX+BPSAVE],BP
				MOV [cs:PCB+BX+SISAVE],SI
				MOV [cs:PCB+BX+DISAVE],DI
				POP AX
				MOV [cs:PCB+BX+DSSAVE],AX
				POP SI
				POP AX
				MOV [cs:PCB+BX+IPSAVE],AX
				POP AX
				MOV [cs:PCB+BX+CSSAVE],AX
				POP AX
				MOV [cs:PCB+BX+FLAGSSAVE],AX
				MOV [cs:PCB+BX+SPSAVE],SP
				PUSH SI

				RET
				;---------------------------------------------------------------
				;_______________________________________________________________
				;***************************************************************
				GETNEXT:
				getagain:
				MOV BX,[cs:CURPROC]
				SHL BX,5

				MOV AL,[cs:PCB+BX+NEXTSAVE]
				XOR AH,AH
				mov bx,ax
				shl bx,5 ;calculating next pcb
				cmp byte[cs:PCB+bx+pausres],0xff ; check if the next if on pause state
				je skipNext
				mov [cs:CURPROC],ax ;if next is on resume state, make it current
				jmp endget
				skipNext:
				;dec byte[cs:PCB+bx+pausres] 
				;mov al,[cs:PCB+bx+NEXTSAVE] ; else move next of next in current
				xor ah,ah
				mov [cs:CURPROC],ax
				jmp getagain
				endget:
				RET
				;---------------------------------------------------------------
				;_______________________________________________________________
				;***************************************************************
				RESTORESTATE:
				;debugging

				pop si ; POPING RETURN ADDRESS INTO SI

				mov bx,[cs:CURPROC]
				SHL BX,5 ; MULTYPLYING WITH 32
				MOV SS,[cs:PCB+BX+SSSAVE];
				MOV SP,[CS:PCB+BX+SPSAVE]

				PUSH WORD[cs:PCB+BX+FLAGSSAVE]; PUSHING  FLAGS TO  STACK
				PUSH WORD[cs:PCB+BX+CSSAVE]; PUSHING CS TO STACK
				PUSH WORD[cs:PCB+BX+IPSAVE]; PUSHING IP TO STACK
				PUSH SI ; PUSHING RETURN ADDRESS TO STACK
				MOV SI,[cs:PCB+BX+SISAVE];  RESTORING SI FROM PCB
				MOV AX,[cs:PCB+BX+AXSAVE] ; RESTORING AX FROM PCB
				MOV DX,[cs:PCB+BX+DXSAVE] ; RESTORING DX FROM PCB
				MOV CX,[cs:PCB+BX+CXSAVE] ;RESTORING CX FROM PCB
				MOV ES,[cs:PCB+BX+ESSAVE] ;
				MOV DS,[cs:PCB+BX+DSSAVE];
				MOV DI,[cs:PCB+BX+DISAVE];
				MOV BP,[cs:PCB+BX+BPSAVE]; RESTORING BP FROM PCB
				PUSH WORD[cs:PCB+BX+BXSAVE]; PUSHING BX TO STACK FROM PCB
				POP BX ; POPING BX INTO BX
				;inc word[cs:count]
				RET

				;---------------------------------------------------------------
				; To print message for invalid taskno
				;_______________________________________________________________
				;***************************************************************
				printInvalid:
							pusha
							xor bx,bx
							mov ax, 0xb800
							mov es,ax
							xor ax,ax
							mov di,2000
							mov ah,0x07
							mov cx,33
							LOOP3:
								MOV AL,[cs:string+bx]
								mov word[es:di],ax
								inc bx
								inc di
								inc di
							LOOP LOOP3
							popa
				ret
				;---------------------------------------------------------------
				; To print message for invalid subservice number
				;_______________________________________________________________
				;***************************************************************

				invalidSub:
							pusha
							xor bx,bx
							mov ax, 0xb800
							mov es,ax
							xor ax,ax
							mov di,2000
							mov ah,0x07
							mov cx,39
							LOOPSub:
								MOV AL,[cs:stringS+bx]
								mov word[es:di],ax
								inc bx
								inc di
								inc di
							LOOP LOOPSub
							popa
				ret

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
							LOOPPaus:
								MOV AL,[cs:stringP+bx]
								mov word[es:di],ax
								inc bx
								inc di
								inc di
							LOOP LOOPPaus
							popa
				ret


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
							LOOPRes:
								MOV AL,[cs:stringRes+bx]
								mov word[es:di],ax
								inc bx
								inc di
								inc di
							LOOP LOOPRes
							popa
				ret
				; ;---------------------------------------------------------------
				; ; To print Message that all pcb have been used
				; ;_______________________________________________________________
				; ;***************************************************************
				errorP:db 'All PCBs have been used, please wait until one of them is ended or remove one of them'
				printError:
							pusha
							xor bx,bx
							mov ax, 0xb800
							mov es,ax
							xor ax,ax
							mov di,3200
							mov ah,0x84
							mov cx,85
							LOOP7:
								MOV AL,[cs:errorP+bx]
								mov word[es:di],ax
								inc bx
								inc di
								inc di
							LOOP LOOP7
							popa
				ret


				; ;---------------------------------------------------------------
				; ; To print Message that pcb has been removed successfully
				; ;_______________________________________________________________
				; ;***************************************************************
				removeS:db 'Task has been removed successfully'
				removeMsg:
							pusha
							xor bx,bx
							mov ax, 0xb800
							mov es,ax
							xor ax,ax
							mov di,3200
							mov ah,0x84
							mov cx,34
							LOOP8:
								MOV AL,[cs:removeS+bx]
								mov word[es:di],ax
								inc bx
								inc di
								inc di
							LOOP LOOP8
							popa
				ret

				; ;---------------------------------------------------------------
				; ; To print Message that pcb has been removed successfully
				; ;_______________________________________________________________
				; ;***************************************************************
				enddS:db 'Task has been ended successfully'
				endMsg:
							pusha
							xor bx,bx
							mov ax, 0xb800
							mov es,ax
							xor ax,ax
							mov di,3200
							mov ah,0x84
							mov cx,32
							LOOP9:
								MOV AL,[cs:enddS+bx]
								mov word[es:di],ax
								inc bx
								inc di
								inc di
							LOOP LOOP9
							popa
				ret


				wajid:
							pusha
							xor bx,bx
							mov ax, 0xb800
							mov es,ax
							xor ax,ax
							mov di,2130
							mov ah,0xf4
							mov cx,20
							LOOP5:
								MOV AL,[cs:stringW+bx]
								mov word[es:di],ax
								inc bx
								inc di
								inc di
							LOOP LOOP5
							popa
				ret
				;---------------------------------------------------------------
				;_______________________________________________________________
				;***************************************************************
				print2:
							pusha
							mov ax, 0xb800
							mov es,ax
							xor di,di
							mov bx,[cs:count]
							mov cx,4
							iloop:
										mov ax,bx
										and ax,0xF000
										shr ax,12
										cmp ax,9
										ja skip
												add ax, 0x0730
												jmp skipend
										skip:
															add ax,0x0737
														skipend:
										mov word[es:di],ax
										add di,2
										shl bx,4
							loop iloop
							popa
				ret
				;;********************************************************
				;;_______________________________________________________________
				printTaskNo:
							pusha
							mov ax, 0xb800
							mov es,ax
							mov di,148
							mov bx,[cs:CURPROC]
							shl bx,5
							mov al,[cs:PCB+bx+taskno]
							xor ah,ah
							mov bx,ax
							mov cx,4
							iloop0:
										mov ax,bx
										and ax,0xF000
										shr ax,12
										cmp ax,9
										ja skip0
												add ax, 0x0730
												jmp skipend0
										skip0:
															add ax,0x0737
														skipend0:
										mov word[es:di],ax
										add di,2
										shl bx,4
										push cx
											mov cx,6500
											lop:
											loop lop
										pop cx
							loop iloop0
							popa
				ret

				;---------------------------------------------------------------
				;ADD or Remove PCB interrupt
				;_______________________________________________________________
				;***************************************************************
				 addremovPCB:
						cmp ax,1600h ; To add a new PCB
						jne remov	 ; If not equal jump to remove PCB
							call addPCB ; call addPCB subroutine
						jmp endd ; after adding pcb, jump to end of this subroutine
					 remov:
						cmp ax,1700h ; To End Task from the after the completion of the task
						jne pauseR   ; if not equal jump too Pause PCB
						call removePCB ; Call subroutine to end task
						jmp endd ; after ending, go to last of this subroutine
					pauseR:
						cmp ax,1800h ;To Pause a Task
						jne resume ;if not equal, jump to resume a paused task
						call pausePCB ; call pausePCB to pause a task
						jmp endd ; jump to last instruction of this PCB
					resume:
						cmp ax,0x1900
						jne removOut
						call resumePCB
						jmp endd
					removOut:
						cmp ax,2000h
						jne invalid
						call removePCBOut
						jmp endd
					invalid:
						call invalidSub ;if invalid subservice number comes 
						endd:
					 iret
				;74373001
				; ;---------------------------------------------------------------
				; ; Remove PCB
				; ;_______________________________________________________________
				; ;***************************************************************
				 removePCB:
					pusha
					mov di, [cs:CURPROC] ; getting current process
					shl di,5 ;calculating current pcb's starting address in di
					mov si,[cs:lastPCB]
					shl si,5 ;calculating  lastPCB's starting address in si
					mov al,[cs:PCB+si+taskno] ; getting taskno of lastPCB
					mov byte[cs:PCB+di+taskno],al ; moving taskno of lastPCB in current pcb
					mov al,[cs:PCB+si+pausres] ; getting paus-resume of lastPCB
					mov byte[cs:PCB+di+pausres],al ; moving paus-resume of lastPCB in current pcb

					mov cx,14
					mov bx,4
					loop5: ;saving lastPCB's data in current pcb
						mov ax,[cs:PCB+si+bx] ;getting value of lastPCB into ax
						mov [cs:PCB+di+bx],ax ; saving value of lastPCB in current pcb
						inc bx
					loop loop5
					mov al,[cs:PCB+si+NEXTSAVE] ; getting next of lastPCB in al
					mov bl,[cs:PCB+si+PREVIOUSSAVE] ; getting previous of lastPCB in bl
					xor bh,bh
					xor ah,ah
					shl bx,5 ; calculating 2nd lastPCB's starting address
					mov byte[cs:PCB+bx+NEXTSAVE],al ;next of lastPCB is now next of 2nd lastPCB
					dec word[cs:nextPCB]
					dec word[cs:lastPCB]
					call endMsg ; to print message that task has been ended successfully
					popa
					pop word [cs:tempaddress]
					call GETNEXT
					call RESTORESTATE
					push word [cs:tempaddress]
				 ret
				 
				 
				; ;---------------------------------------------------------------
				; ; Remove PCB From Outside
				; ;_______________________________________________________________
				; ;***************************************************************
				 removePCBOut:
					pusha
					mov bx,32
					mov cx,15 ; for 15 PCBs excluding 0th pcb
					loopOut:
					mov ax,si
					cmp al,byte[cs:PCB+bx+taskno] ;searching and comparing task number
					je milgyaOut
					add bx,32
					loop loopOut
					;cmp bx,480
					jmp notFoundO ;if required task is not running then exit
					milgyaOut:
					mov di,bx
					mov si,[cs:lastPCB]
					shl si,5 ;calculating  lastPCB's starting address in si
					mov al,[cs:PCB+si+taskno] ; getting taskno of lastPCB
					mov byte[cs:PCB+di+taskno],al ; moving taskno of lastPCB in current pcb
					mov al,[cs:PCB+si+pausres] ; getting paus-resume of lastPCB
					mov byte[cs:PCB+di+pausres],al ; moving paus-resume of lastPCB in current pcb

					mov cx,14
					mov bx,4
					loopOut1: ;saving lastPCB's data in current pcb
						mov ax,[cs:PCB+si+bx] ;getting value of lastPCB into ax
						mov [cs:PCB+di+bx],ax ; saving value of lastPCB in current pcb
						inc bx
					loop loopOut1
					mov al,[cs:PCB+si+NEXTSAVE] ; getting next of lastPCB in al
					mov bl,[cs:PCB+si+PREVIOUSSAVE] ; getting previous of lastPCB in bl
					xor bh,bh
					xor ah,ah
					shl bx,5 ; calculating 2nd lastPCB's starting address
					mov byte[cs:PCB+bx+NEXTSAVE],al ;next of lastPCB is now next of 2nd lastPCB
					dec word[cs:nextPCB]
					dec word[cs:lastPCB]
					call removeMsg ; to print message that task has been successfully removed
					jmp enddOut ;jumping over printInvalid
					notFoundO:
					call printInvalid
					enddOut:
					popa
					; pop word [cs:tempaddress]
					; call GETNEXT
					; call RESTORESTATE
					; push word [cs:tempaddress]
				 ret
				 ; ;---------------------------------------------------------------
				; ; Pause pcb
				; ;_______________________________________________________________
				; ;***************************************************************

				 
				pausePCB:
					push bx
					push cx
					; mov [cs:count],si
					; call print2
					mov bx,32
					mov cx,15
					loopP:
					mov ax,si
					cmp al,byte[cs:PCB+bx+taskno] ;searching and comparing task number
					je milgyaP
					add bx,32
					loop loopP
					;cmp bx,480
					jmp notFound
					milgyaP:
					mov byte[cs:PCB+bx+pausres],0xff
					call printPaus
					jmp enddPaus
					notFound:
					call printInvalid
					enddPaus:
					pop cx
					pop bx
				ret

				tasknoInitialize:
					pusha 
					mov cx,16
					mov bx,0
					loop4:
					shl bx,5
					mov byte[cs:PCB+bx+taskno], 0xff
					mov byte[cs:PCB+bx+pausres],0x00
					inc bx
					loop loop4
					popa
				ret
				; ;---------------------------------------------------------------
				; ; Resume pcb
				; ;_______________________________________________________________
				; ;***************************************************************

				resumePCB:
					pusha
					;mov [cs:count],si
					;call print2
					;push cx
					mov bx,32
					mov cx,15
					loopR:
					; shl bx,5

					mov ax,[cs:PCB+bx+taskno]
					; mov [cs:count],ax
					; call print2
					mov ax,si
					cmp al,byte[cs:PCB+bx+taskno] ;searching and comparing task number
					je found
					add bx,32
					loop loopR
					jmp nfound
					found:
					xor ax,ax
					mov byte[cs:PCB+bx+pausres],al
					call printResum
					jmp enddRes
					nfound:
					call printInvalid
					;pop cx
					enddRes:
					popa
				ret

				; ;---------------------------------------------------------------
				; ; initialize new pcb
				; ;_______________________________________________________________
				; ;***************************************************************
				addPCB:
					push ax 
					push bx 
					push cx 
					push di
					push dx
					mov bx, [cs:nextPCB] ; read next available PCB index
					cmp bx, 16 ; if all PCBs are used
					je near exit		;then exit
					;mov cl, 5 
					shl bx, 5 ; multiply by 32 for PCB start
					mov ax, [si+tcs]	; read code segment parameter

					mov [cs:PCB+bx+CSSAVE],ax ; save in PCB space for cs

					mov ax, [si+tip]	;read offset parameter

					mov [cs:PCB+bx+IPSAVE],ax ;save in PCB space for ip
					mov ax, [si+tds]	; read data segment parameter
					mov [cs:PCB+bx+DSSAVE], ax ; save in PCB space for ds
					mov ax, [si+tes] ;read extra segment parameter
					mov [cs:PCB+bx+ESSAVE], ax ;save in PCB space for es
					;mov ax, [si+tcs]
					mov [cs:PCB+bx+SSSAVE], cs ; set stack to our segment
					mov ax, [cs:nextPCB] ; read this PCB index
					;mov cl, 9
					shl ax, 9	; multiply by 512
					add ax, 512
					add ax, MYSTACK ; end of stack for this thread 
					mov [cs:PCB+bx+SPSAVE], ax ;save di in PCB space for sp

				;	mov ax, [cs:si+tip] ; read parameter for subroutine
				;	sub di, 2 ;decrement thread stack pointer
				;	mov [cs:di], ax ;pushing param on thread stack
				;	sub di, 4 ; space for far return address
					mov word [cs:PCB+bx+FLAGSSAVE], 0x0200 ; initialize flags
					mov al, [cs:PCB+NEXTSAVE] ;read next of 0th thread in ax
					xor ah,ah
					mov byte[cs:PCB+bx+NEXTSAVE], al ;set as next of new thread
					mov byte[cs:PCB+bx+PREVIOUSSAVE],ah ; set 0 as previous of new thread
					mov ax, [cs:nextPCB] ; read new thread index
					xor ah,ah
					push bx
					xor bx,bx
					mov bl,[cs:PCB+NEXTSAVE] ;read next of 0th thread
					shl bx,5
					mov byte[cs:PCB+bx+PREVIOUSSAVE],al ; set new thread as previous of current thread
					mov byte[cs:PCB+NEXTSAVE], al ; set as next of 0th thread
					pop bx
					mov byte[cs:PCB+taskno+bx],al
					inc word[cs:nextPCB] ; this PCB is now used
					inc word[cs:lastPCB] ; making this pcb as lastPCB
				;	mov word[CURPROC],al
				jmp endexit
				exit:
					call printError
						 endexit:
					 pop dx
					 pop di 
					 pop cx 
					 pop bx 
					 pop ax

				 ret
				;---------------------------------------------------------------
				;_______________________________________________________________
				;***************************************************************
						stringW:db 'Wajid Haneef Khokhar'
						tempaddress: dw 0x0000
						lastPCB: dw 0x0000
						count :  dw 0x0000
						stringS:db 'You have sent invalid subservice number'
						string:db'You have sent invalid task number'
						
						nextPCB: dw 0x0000
						CURPROC: dw 0X0000
				;		MYSTACK: TIMES 16*256 DW
						MYSTACK: times 4096 dw 0x0000
						;next,previous,dummy,ax,bx,cx,dx,es,ds,cs,ss,sp,ip,bp,si,di,flags
				;		PCB: times 16*16 dw
						
						
						PCB: times 256 dw 0x000
						NEXTSAVE EQU 00
						PREVIOUSSAVE EQU 01
						taskno EQU 02
						pausres equ 03
						AXSAVE EQU 04
						BXSAVE EQU 06
						CXSAVE EQU 08
						DXSAVE EQU 10
						ESSAVE EQU 12
						DSSAVE EQU 14
						CSSAVE  equ 16
						SSSAVE EQU 18
						SPSAVE EQU 20
						IPSAVE EQU 22
						BPSAVE EQU 24
						SISAVE EQU 26
						DISAVE EQU 28
						FLAGSSAVE EQU 30
						tcs equ 00
						tip equ 02
						tds equ 04
						tes equ 06
						
				;---------------------------------------------------------------
				;_______________________________________________________________
				;***************************************************************
				start:
				;debugging
				;call print
				call tasknoInitialize
				xor ax,ax
				mov word[cs:count],ax
				mov word[cs:lastPCB],ax
				mov word[cs:nextPCB],0x0001
				mov es,ax
				mov byte[cs:PCB+NEXTSAVE],0x00
				mov byte[cs:PCB+taskno],0x00
				mov byte[cs:PCB+pausres],0x00
				mov byte[cs:PCB+PREVIOUSSAVE],0x00

				 mov word [es:80*4], addremovPCB ; 80*4
				 mov [es:80*4+2], cs ; hook int 80  ; 80*4+2

				cli
				mov word[es:8*4],interrupt
				mov word[es:8*4+2],cs
				sti

				mov dx, start
				add dx, 15
				mov cl, 4
				shr dx, cl
				mov ax, 0x3100
				int 21h