org 0x7e00
jmp 0x0000:main
    tela db 'COOKIE CLICKER', 13
	nicks db 'pfbc2jmbj2ldcs', 13
    telaLento db '/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-COOKIE CLICKER-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/', 0
    comecar db 'Pressione ESPACO',13

printtela:
	lodsb

	mov ah, 0xe
	mov bh, 0
	int 10h

	cmp al, 13
	jne printtela
ret

delay:
	mov bp, 500
	mov dx, 500
	delay2:
		dec bp
		nop
		jnz delay2
	dec dx
	jnz delay2

ret
delayDeTela:
	mov bp, 800
	mov dx, 800
	delayTela:
		dec bp
		nop
		jnz delayTela
	dec dx
	jnz delayTela

ret
limpaTela:
;; Limpa a tela dos caracteres colocados pela BIOS
	; Set the cursor to top left-most corner of screen
	mov dx, 0 
    mov bh, 0      
    mov ah, 0x2
    int 0x10

    ; print 2000 blanck chars to clean  
    mov cx, 2000 
    mov bh, 0
    mov al, 0x20 ; blank char
    mov ah, 0x9
    int 0x10
    
    ;Reset cursor to ton left-most corner of screen
    mov dx, 0 
    mov bh, 0      
    mov ah, 0x2
    int 0x10
ret

PrintarString: 
	lodsb
	cmp al,0
	je fim

	mov ah, 0xe
	int 10h	

	call delay 
	
	jmp PrintarString
fim:
ret

putchar:
    mov ah, 0x0e
    int 10h
ret

endl:
	mov al, 0x0a          ; line feed
	call putchar
	mov al, 0x0d          ; carriage return
	call putchar
ret

main:
    xor ax, ax
    mov es, ax
    mov ds, ax
    mov ah, 0
	mov al, 12h
	int 10h
;Colorindo a tela de azul.
    mov ah, 0xb  
	mov bh, 0     
	mov bl, 1   
	int 10h	

	mov ah, 02h
	mov bh, 00h
	mov dh, 08h
	mov dl, 34
	int 10h

	mov bl, 13
	mov si, tela
	call printtela
	call delay

	;Colorindo a tela de amarelo.
	mov ah, 0xb  
	mov bh, 0     
	mov bl, 14  
	int 10h

	mov ah, 02h
	mov bh, 00h
	mov dh, 08h
	mov dl, 34
	int 10h

	mov bl, 5
	mov si, tela
	call printtela
	call delay

	;Colorindo a tela de vermelho.
	mov ah, 0xb  
	mov bh, 0     
	mov bl, 4   
	int 10h

	mov ah, 02h
	mov bh, 00h
	mov dh, 08h
	mov dl, 34
	int 10h

	mov bl, 14
	mov si, tela
	call printtela
	call delay

	;Colorindo a tela de verde.
	mov ah, 0xb  
	mov bh, 0     
	mov bl, 4  
	int 10h

	mov ah, 02h
	mov bh, 00h
	mov dh, 08h
	mov dl, 34
	int 10h

	mov bl, 15
	mov si, tela
	call printtela
    
    call delay
    call limpaTela
    
   
    mov ah, 0xb  
	mov bh, 0     
	mov bl, 12   
	int 10h
    mov bl, 1
	mov ah, 02h
	mov bh, 00h
	mov dh, 08h
	mov dl, 34
	int 10h
	mov si, nicks
	call PrintarString
	call endl

    
    call delay

	mov  dl, 1   ;Column
 	mov  dh, 1   ;Row
  	mov  bh, 0    ;Display page
  	mov  ah, 02h  ;SetCursorPosition
  	int  10h
	  
    mov  dl, 33
    mov  dh, 15
	mov  bh, 0
	mov  ah, 02h
	int  10h
    mov si, comecar
    call printtela


enterr:
  	mov ah, 0x00
  	int 16h
	cmp al, 32
	je fimm
	jmp enterr

fimm:  

	call delayDeTela

jogo:
;Setando a posição do disco onde kernel.asm foi armazenado(ES:BX = [0x500:0x0])
	mov ax,0x860		;0x860<<1 + 0 = 0x8600
	mov es,ax
	xor bx,bx		;Zerando o offset

;Setando a posição da Ram onde o jogo será lido
	mov ah, 0x02	;comando de ler setor do disco
	mov al,8		;quantidade de blocos ocupados por jogo
	mov dl,0		;drive floppy

;Usaremos as seguintes posições na memoria:
	mov ch,0		;trilha 0
	mov cl,7		;setor 7
	mov dh,0		;cabeca 0
	int 13h
	jc jogo	;em caso de erro, tenta de novo

break:	
	jmp 0x8600 
  	
exit: