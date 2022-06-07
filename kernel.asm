org 0x8600
jmp 0x0000:start

letraJ db 'J', 0
letraK db 'K', 0
letraF db 'F', 0
letraG db 'G', 0
letraH db 'H', 0
strCIn db 'CIn', 0
strCAC db 'CAC', 0
strAll db 'All', 0
strCCEN db 'CCEN', 0
strCCS db 'CCS', 0

printString:
  lodsb
  mov ah, 0xe
  mov bh, 0
  mov bl, 0xf
  int 10h

  cmp al, 0
  jne printString
  ret

string_Posicao:
  mov ah, 02h  ;Setando o cursor
  mov bh, 0    ;Pagina 0
  int 10h
  call printString
ret

imprimeTexto:
	mov dh, 2; linha
	mov dl, 13; coluna
	mov si, letraF
	call string_Posicao
	mov dh, 2; linha
	mov dl, 18; coluna
	mov si, letraG
	call string_Posicao
	mov dh, 2; linha
	mov dl, 23; coluna
	mov si, letraH
	call string_Posicao
	mov dh, 2; linha
	mov dl, 28; coluna
	mov si, letraJ
	call string_Posicao
	mov dh, 2; linha
	mov dl, 33; coluna
	mov si, letraK
	call string_Posicao
  mov dh, 1; linha
	mov dl, 13; coluna
	mov si, strCAC
	call string_Posicao
	mov dh, 1; linha
	mov dl, 18; coluna
	mov si, strAll
	call string_Posicao
	mov dh, 1; linha
	mov dl, 23; coluna
	mov si, strCCEN
	call string_Posicao
	mov dh, 1; linha
	mov dl, 28; coluna
	mov si, strCCS
	call string_Posicao
	mov dh, 1; linha
	mov dl, 33; coluna
	mov si, strCIn
	call string_Posicao
	ret

data:
  ;Teste do contador
  valor_seg times 20 db 0
  instrucao1 db 'segundos: ', 0
  zero_str db '0', 0
  erro_ne db 'Nao houve recursos suficientes', 0
  sucesso db 'Compra efetuada com sucesso', 0
  erro_cantbuy db 'Nao pode mais comprar esse', 0
  pesps db 'p/seg', 0

  cookies db ' cookies', 0
  leite db ' leite', 0
  vacas db ' vacas', 0
  fazendas db ' fazendas', 0

  ;Unidades de numero
  counter dw 0,0,0,0
  increase dw 1,0,0,0
  quatro dw 4
  umVal dw 1
  doisVal dw 2
  zero dw 0
  flag_texto dw 0
  distancia dw 0 
  flag dw 0
  
  ;Valores normais
  CAC db 'CAC: (ck)', 0
  inc_CAC dw 2,0,0,0 
  inc_value_CAC dw 10,0,0,0
  inc_qt_CAC dw 11
  flag_CAC dw 0

  AREAII db 'AREAII: (lt)', 0
  inc_AREAII dw 0,1,0,0
  inc_value_AREAII dw 0,1,0,0
  inc_qt_AREAII dw 8
  flag_AREAII dw 0

  CCEN db 'CCEN: (lt)', 0
  inc_CCEN dw 0,5,0,0
  inc_value_CCEN dw 0,10,0,0
  inc_qt_CCEN dw 6
  flag_CCEN dw 0

  CCS db 'CCS: (vc)', 0
  inc_CCS dw 0,0,2,0
  inc_value_CCS dw 0,0,10,0 
  inc_qt_CCS dw 5
  flag_CCS dw 0

  CIn db 'CIn: (fz)', 0
  inc_CIn dw 0,0,0,1
  inc_value_CIn dw 0,0,0,4
  inc_qt_CIn dw 4
  flag_CIn dw 0

  money equ 0
  um equ 1

;------- LIBRARY
  print_zero:
    mov si,zero_str
    call prints
    ret
  
  reverse:              ; mov si, string
    mov di, si
    xor cx, cx          ; zerar contador
    .loop1:             ; botar string na stack
      lodsb
      cmp al, 0
      je .endloop1
      inc cl
      push ax
      jmp .loop1
    .endloop1:
    .loop2:             ; remover string da stack        
      pop ax
      stosb
      loop .loop2
    ret

  tostring:              ; mov ax, int / mov di, string
    push di
    .loop1:
      cmp ax, 0
      je .endloop1
      xor dx, dx
      mov bx, 10
      div bx            ; ax = 9999 -> ax = 999, dx = 9
      xchg ax, dx       ; swap ax, dx
      add ax, 48        ; 9 + '0' = '9'
      stosb
      xchg ax, dx
      jmp .loop1
    .endloop1:
    pop si
    cmp si, di
    jne .done
    mov al, 48
    stosb
    .done:
    mov al, 0
    stosb
    call reverse
    ret

  putchar:
    mov ah, 0x0e
    int 10h
    ret

  putcharNum:
    mov  dl, 3   ;Column
    add  dl, [distancia]
    mov  dh, 22   ;Row
    mov  bh, 0    ;Display page
    mov  ah, 02h  ;SetCursorPosition
    int  10h
    call putchar
    ret
  
  putcharErro:
    mov  dl, 3    ;Column
    mov  dh, 22   ;Row
    add dl, [distancia]
    mov  bh, 0    ;Display page
    mov  ah, 02h  ;SetCursorPosition
    int  10h
    call putchar
    ret

  putcharLoja:
    mov  dl, 0   ;Column
    add dl, [distancia]
    mov  bh, 0    ;Display page
    mov  ah, 02h  ;SetCursorPosition
    int  10h
    call putchar
    ret


  prints:             ; mov si, string
    .loop:
      lodsb           ; bota character em al 
      cmp al, 0
      je .endloop
      call putchar
      jmp .loop
    .endloop:
    ret

  printsErro:             ; mov si, string
    .loop:

      push ax
      mov ax, [distancia]
      inc ax
      mov [distancia], ax
      pop ax

      lodsb           ; bota character em al 
      cmp al, 0
      je .endloop
      call putcharErro
      jmp .loop
    .endloop:
    ret

  printsCCEN:             ; mov si, string
    .loop:
      push ax
      mov ax, [distancia]
      inc ax
      mov [distancia], ax
      pop ax
      lodsb           ; bota character em al 
      cmp al, 0
      je .endloop
      mov  dh, 18   ;Row
      call putcharLoja
      jmp .loop
    .endloop:

    ret

  printsCCS:             ; mov si, string
    .loop:
      push ax
      mov ax, [distancia]
      inc ax
      mov [distancia], ax
      pop ax
      lodsb           ; bota character em al 
      cmp al, 0
      je .endloop
      mov  dh, 19   ;Row
      call putcharLoja
      jmp .loop
    .endloop:

    ret

  printsCAC:             ; mov si, string
    .loop:
      push ax
      mov ax, [distancia]
      inc ax
      mov [distancia], ax
      pop ax
      lodsb           ; bota character em al 
      cmp al, 0
      je .endloop
      mov  dh, 16   ;Row
      call putcharLoja
      jmp .loop
    .endloop:

    ret

  printsAREAII:             ; mov si, string
    .loop:
      push ax
      mov ax, [distancia]
      inc ax
      mov [distancia], ax
      pop ax
      lodsb           ; bota character em al 
      cmp al, 0
      je .endloop
      mov  dh, 17   ;Row
      call putcharLoja
      jmp .loop
    .endloop:

    ret

  printsCIn:             ; mov si, string
    .loop:
      push ax
      mov ax, [distancia]
      inc ax
      mov [distancia], ax
      pop ax
      lodsb           ; bota character em al 
      cmp al, 0
      je .endloop
      mov  dh, 20   ;Row
      call putcharLoja
      jmp .loop
    .endloop:

    ret

  printsItem:
    mov dh, 5
    .loop:
      push ax
      mov ax, [distancia]
      inc ax
      mov [distancia], ax
      pop ax
      lodsb           ; bota character em al 
      cmp al, 0
      je .endloop
      mov  dh, 9   ;Row
      call putchar
      jmp .loop
    .endloop:
    ret

  printsNum:             ; mov si, string
      push ax
      mov ax, [distancia]
      inc ax
      mov [distancia], ax
      pop ax
    .loop:
      lodsb           ; bota character em al 
      cmp al, 0
      je .endloop
      call putcharNum
      jmp .loop
    .endloop:
    ret

  endl:
    mov al, 0x0a          ; line feed
    call putchar
    mov al, 0x0d          ; carriage return
    call putchar
    ret

  delay100ms:              ; 0.1 SEC DELAY
    mov cx, 01h
    mov dx, 86a0h
    mov ah, 86h
    int 15h
    ret

  delay:              
    mov cx, 01h
    mov dx, 0D76h
    mov ah, 86h
    int 15h
    ret

imprimir_numero: ; Coloca o numero em cx

  push ax
  push bx
  
  push bx
  mov bx, 0
  mov [distancia], bx
  pop bx
  
  end_pz:
  
  mov ax,cx
  push bx
  mov di,valor_seg
  call tostring
  pop bx

  mov si,valor_seg
  call prints

  pop bx
  pop ax 
  ret

incremente_counter: ; Coloca em si
  push bx
  mov ax,[si]
  cmp ax,9999
  jge counter_2f
  jmp end_1

  counter_2f:
    mov ax,0
    mov [si],ax
    mov ax,[si + 2]
    inc ax
    cmp ax,9999
    mov [si + 2],ax
    jge counter_3f
    jmp end_1
  counter_3f:
    mov ax,0
    mov [si + 2],ax
    mov ax,[si + 4]
    inc ax
    cmp ax,9999
    mov [si + 4],ax
    jge counter_4f
    jmp end_1
  counter_4f:
    mov ax,0
    mov [si + 4],ax
    mov ax,[si + 6]
    inc ax
    cmp ax,9999
    mov [si + 6],ax
  end_1:
  pop bx
  ret
get_click:
  push ax
  push bx
  push cx

  mov ah,1
  int 16h
  jz not_pressed  ; Verifica se tem algum botao sendo pressionado
  mov ah,0
  int 16h

  cmp al,32 ; Se tiver algum, verifica se é o espaco
  je pressed_space 

  cmp al,98
  je pressed_buy

  jmp not_pressed ; Caso nenhuma das teclas seja a correta

  pressed_space:  
    
    call aumentar_pesquisas_clique
    jmp not_pressed

  pressed_buy:
    call buy
    jmp not_pressed 

  not_pressed: ; Se nao, ele continua a execucao do código
    pop cx
    pop bx
    pop ax
  after_pop:
    ret

aumentar_pesquisas_clique:
    push ax
    mov ax,[counter]
    add ax,[increase]
    mov [counter],ax

    mov ax,[counter + 2]
    add ax,[increase + 2]
    mov [counter + 2],ax

    mov ax,[counter + 4]
    add ax,[increase + 4]
    mov [counter + 4],ax

    mov ax,[counter + 6]
    add ax,[increase + 6]
    mov [counter + 6],ax

    mov si,counter
    call incremente_counter
    push ax

  call setCursor
  
  xor ax, ax
  mov ah, 0Bh   ;setando o ah para especificar a prox interrupção
  xor bx, bx
  mov bh, 01h
  mov bl, 02h
  int 10h       ;interrupção para limpar o terminal (não ficar printando várias linhas a mesma coisa)

  mov ah, 02h  ;Setando o cursor
  mov bh, 0    ;Pagina 0
  mov dl, 1    ;Column
  mov dh, 5    ;Row
  int 10h

  mov cx,[counter]
    call imprimir_numero
    
    mov ah, 02h  ;Setando o cursor
    mov bh, 0    ;Pagina 0
    mov dl, 5    ;Column
    mov dh, 5    ;Row
    int 10h

    mov si, cookies
    call printsItem


    call endl
    mov ah, 02h  ;Setando o cursor
    mov bh, 0    ;Pagina 0
    mov dl, 1    ;Column
    mov dh, 6    ;Row
    int 10h

  mov cx,[counter + 2]
    call imprimir_numero
    
    mov ah, 02h  ;Setando o cursor
    mov bh, 0    ;Pagina 0
    mov dl, 5    ;Column
    mov dh, 6    ;Row
    int 10h

    mov si, leite
    call printsItem

    call endl
    mov ah, 02h  ;Setando o cursor
    mov bh, 0    ;Pagina 0
    mov dl, 1    ;Column
    mov dh, 7    ;Row
    int 10h

  mov cx,[counter + 4]
    call imprimir_numero

    mov ah, 02h  ;Setando o cursor
    mov bh, 0    ;Pagina 0
    mov dl, 5    ;Column
    mov dh, 7    ;Row
    int 10h

    mov si, vacas
    call printsItem

    call endl
    mov ah, 02h  ;Setando o cursor
    mov bh, 0    ;Pagina 0
    mov dl, 1    ;Column
    mov dh, 8    ;Row
    int 10h

  mov cx,[counter+6]
    call imprimir_numero

    mov ah, 02h  ;Setando o cursor
    mov bh, 0    ;Pagina 0
    mov dl, 5    ;Column
    mov dh, 8    ;Row
    int 10h

    mov si, fazendas
    call printsItem

  call endl


  pop ax
  pop ax
  ret

compare: ; flag em dx =, 1 foi suficiente

  mov ax,[counter + bx]
  cmp ax,[si + bx] ; Compara primeira unidade
  jge enough

  jmp notenough 

  enough:
    mov ax,[si + bx]
    add [si + bx],ax
    sub [counter+bx],ax
    mov ax,[di + bx]
    add [di + bx],ax
    add [increase + bx],ax
    mov cx, 1
    jmp end_9

  notenough:
    push ax
    xor ax, ax
    mov [distancia], ax
    inc ax
    mov [flag_texto], ax
    mov si, erro_ne
    push bx
    mov bx, 2
    call printsErro 
    pop bx
    pop ax

    xor cx,cx
  end_9:
  ret


compraFeita:
  push ax
  xor ax, ax
  mov di, ax
  mov [distancia], ax
  inc ax
  mov [flag_texto], ax
  mov si, sucesso
  call printsErro 
  pop ax
  ret

noMore:
  push ax
  xor ax, ax
  mov di, ax
  mov [distancia], ax
  pop ax
  mov si, erro_cantbuy
  call printsErro
  ret

buy:
  mov ah,0
  int 16h
  mov cl,al

  push dx
  xor dx, dx
  mov [flag], dx
  pop dx
  
  cmp cl,107  
  je  m_5
  cmp cl,103
  je m_2 
  cmp cl,104
  je m_3
  cmp cl,106
  je m_4
  cmp cl,102
  jne pop_all 

  m_1:
    mov di,inc_CAC
    mov si,inc_value_CAC
    mov bx,[inc_qt_CAC]
    cmp bx,0
    je cant_buy_more
    xor bx,bx
    call compare ; Chama a funcao que compara as pesquisas e compra blocos
    mov bx, 2
    cmp cx,1
    jne pop_all

    call compraFeita

    sub [inc_qt_CAC],cx
    add [flag_CAC],cx

    jmp pop_all

  m_2:
    mov di,inc_AREAII
    mov si,inc_value_AREAII
    mov bx,[inc_qt_AREAII]
    cmp bx,0
    je cant_buy_more
    mov bx,2
    call compare ; Chama a funcao que compara as pesquisas e compra blocos

    cmp cx,1
    jne pop_all

    call compraFeita

    sub [inc_qt_AREAII],cx
    add [flag_AREAII],cx

    jmp pop_all
  m_3:
    mov di,inc_CCEN
    mov si,inc_value_CCEN
    mov bx,[inc_qt_CCEN]
    cmp bx,0
    je cant_buy_more
    mov bx,2
    call compare ; Chama a funcao que compara as pesquisas e compra blocos

    cmp cx,1
    jne pop_all

    call compraFeita

    sub [inc_qt_CCEN],cx
    add [flag_CCEN],cx

    jmp pop_all

  m_4:
    mov di,inc_CCS
    mov si,inc_value_CCS
    mov bx,[inc_qt_CCS]
    cmp bx,0
    je cant_buy_more
    mov bx,4
    call compare ; Chama a funcao que compara as pesquisas e compra blocos

    cmp cx,1
    jne pop_all

    call compraFeita

    sub [inc_qt_CCS],cx
    add [flag_CCS],cx

    jmp pop_all

  m_5:
    mov di,inc_CIn
    mov si,inc_value_CIn
    mov bx,[inc_qt_CIn]
    cmp bx,0
    je cant_buy_more
    mov bx,6
    call compare ; Chama a funcao que compara as pesquisas e compra blocos

    cmp cx,1
    jne pop_all 

    call compraFeita

    sub [inc_qt_CIn],cx
    add [flag_CIn],cx

    jmp pop_all

  cant_buy_more:
    pusha
    mov ax, 2
    mov cx, 2
    mov dx, 2
    mov bx, 2
    call noMore
    popa

  pop_all:
  ret

setCursor:
  mov  dl, 1   ;Column
  mov  dh, 1   ;Row
  mov  bh, 0    ;Display page
  mov  ah, 02h  ;SetCursorPosition
  int  10h
  ret

bought:
  ; Empilhando:

  print1:
    xor dx, dx
    cmp dx,[flag_CAC]
    je print2
    ; Aqui printa o que ja foi comprado
    mov si,CAC
    xor ax, ax
    mov [distancia], ax
    call printsCAC
    
    mov ax,[inc_CAC]
    mov di,valor_seg
    call tostring
    mov si,valor_seg
    call prints
    mov si,pesps
    call prints
    call endl
    call setCursor

  print2:
    xor dx, dx
    cmp dx,[flag_AREAII] 
    je print3
    mov si,AREAII
    xor ax, ax
    mov [distancia], ax
    call printsAREAII

    mov ax,[inc_AREAII + 2]
    mov di,valor_seg
    call tostring
    mov si,valor_seg
    call prints
    mov si,pesps
    call prints
    call endl
    call setCursor

  print3:
    xor dx, dx
    cmp dx,[flag_CCEN]
    je print4
    mov si,CCEN
    xor ax, ax
    mov [distancia], ax
    call printsCCEN

    mov ax,[inc_CCEN + 2]
    mov di,valor_seg
    call tostring
    mov si,valor_seg
    call prints
    mov si,pesps
    call prints
    call endl

    call setCursor

  print4:
    xor dx, dx
    cmp dx,[flag_CCS]
    je print5
    mov si,CCS
    xor ax, ax
    mov [distancia], ax
    call printsCCS

    mov ax,[inc_CCS + 4]
    mov di,valor_seg
    call tostring
    mov si,valor_seg
    call prints
    mov si,pesps
    call prints
    call endl
    call setCursor

  print5:
    xor dx, dx
    cmp dx,[flag_CIn]
    je pop_
    mov si,CIn
    xor ax, ax
    mov [distancia], ax
    call printsCIn

    mov ax,[inc_CIn + 6]
    mov di,valor_seg
    call tostring
    mov si,valor_seg
    call prints
    mov si,pesps
    call prints
    call endl
    call setCursor

  pop_:
  ret

;------video
  brown:
    mov ah, 0ch
    mov bh, 0
    mov al, 6 ; cor
    int 10h
    ret

  black:
    mov ah, 0ch
    mov bh, 0
    mov al, 0 ; cor
    int 10h
    ret

  blue:
    mov ah, 0ch
    mov bh, 0
    mov al, 11 ; cor
    int 10h
    ret

  red:
    mov ah, 0ch
    mov bh, 0
    mov al, 4 ; cor
    int 10h
    ret

  darkBlue:
    mov ah, 0ch
    mov bh, 0
    mov al, 9 ; cor
    int 10h
    ret

  background:
    push ax
    push bx
    push dx
    mov ax, 0
    mov ds, ax

    mov ah, 0
    mov bh, 13h
    int 10h

    mov ah, 0xb
    mov bh, 0
    mov bl, 4
    int 10h
    pop dx
    pop bx
    pop ax
    ret

  circle:
    mov dx, 0
    mov cx, 0

    .jp_blue:
      cmp dx, 480
      jg .detalhes
      mov cx, 0
      .jp_loop_blue:
        call blue
        inc cx
        cmp cx, 640
        jl .jp_loop_blue
        inc dx
        jmp .jp_blue

    .detalhes:
      mov dx, 0
      mov cx, 0
      .detalhes_loop:
        cmp dx, 480
        jg .barra
        mov cx, 0
        .jp_loop_detalhes:
          call darkBlue
          push ax
          mov ax, [quatro]
          add cx, ax
          pop ax
          cmp cx, 640
          jl .jp_loop_detalhes
          inc dx
          jmp .detalhes_loop

    .barra:
      mov dx, 1
      mov cx, 100

      .barra_brown:
        cmp dx, 27
        jg .cin
        mov cx, 100
        .jp_loop_brown:
          call brown
          inc cx
          cmp cx, 260
          jl .jp_loop_brown
          inc dx
          jmp .barra_brown
    
    .cin:
      mov dx, 1
      mov cx, 260

      .cin_red:
        cmp dx, 27
        jg .cookie_brown
        mov cx, 260
        .jp_cin_red:
          call red
          inc cx
          cmp cx, 300
          jl .jp_cin_red
          inc dx
          jmp .cin_red


          

    .cookie_brown:
      mov cx, 480
      mov dx, 110
      call brown_filled_circle

    .jp_end:
      ret

  smallCircle:
    .cookie_dot:
      call black_circle

    .jp_end:
      ret

  mediumCircle:
    .cookie_dot:
      call black_medium_circle

    .jp_end:
      ret

  black_circle:  ; cx linha dx coluna (centro)
    push cx
    push dx

    mov ax, -3
    push ax

    .loop_circle:
      pop ax
      cmp ax, 3
      je .done
      inc ax
      push ax

      mov bx, -3
      push bx
      jmp .loop_circle2

    .loop_circle2:
      pop bx
      cmp bx, 3
      je .loop_circle
      inc bx
      push bx

      jmp .check

    .check:
      pop bx
      pop ax
      push ax
      push bx

      mov cx, ax
      mov dx, bx

      mul ax
      xchg ax, bx
      mul ax
      add ax, bx

      cmp ax, 9
      jg .loop_circle2

      pop bx
      pop ax
      pop dx
      pop cx
      push cx
      push dx
      push ax
      push bx

      add cx, ax
      add dx, bx

      call black

      jmp .loop_circle2

    .done:
      pop dx
      pop cx
      ret

  brown_filled_circle:  ; cx linha dx coluna (centro)
    push cx
    push dx

    mov ax, -50
    push ax

    .loop_circle:
      pop ax
      cmp ax, 50
      je .done
      inc ax
      push ax

      mov bx, -50
      push bx
      jmp .loop_circle2

    .loop_circle2:
      pop bx
      cmp bx, 50
      je .loop_circle
      inc bx
      push bx

      jmp .check

    .check:
      pop bx
      pop ax
      push ax
      push bx

      mov cx, ax
      mov dx, bx

      mul ax
      xchg ax, bx
      mul ax
      add ax, bx

      cmp ax, 2500
      jg .loop_circle2

      pop bx
      pop ax
      pop dx
      pop cx
      push cx
      push dx
      push ax
      push bx

      add cx, ax
      add dx, bx

      call brown

      jmp .loop_circle2

    .done:
      pop dx
      pop cx
      ret

  black_medium_circle:  ; cx linha dx coluna (centro)
    push cx
    push dx

    mov ax, -8
    push ax

    .loop_circle:
      pop ax
      cmp ax, 8
      je .done
      inc ax
      push ax

      mov bx, -8
      push bx
      jmp .loop_circle2

    .loop_circle2:
      pop bx
      cmp bx, 8
      je .loop_circle
      inc bx
      push bx

      jmp .check

    .check:
      pop bx
      pop ax
      push ax
      push bx

      mov cx, ax
      mov dx, bx

      mul ax
      xchg ax, bx
      mul ax
      add ax, bx

      cmp ax, 64
      jg .loop_circle2

      pop bx
      pop ax
      pop dx
      pop cx
      push cx
      push dx
      push ax
      push bx

      add cx, ax
      add dx, bx

      call black

      jmp .loop_circle2

    .done:
      pop dx
      pop cx
      ret

  video:
    mov ah, 00h
    mov al, 13h
    int 10h
    ret

  draw_cookie:
    
    ;call border

    call circle
    
    mov cx, 130
    mov dx, 120
    call smallCircle

    mov cx, 140
    mov dx, 100
    call smallCircle

    mov cx, 160
    mov dx, 140
    call smallCircle

    mov cx, 170
    mov dx, 145
    call smallCircle

    mov cx, 180
    mov dx, 90
    call smallCircle

    mov cx, 160
    mov dx, 80
    call mediumCircle

    mov cx, 175
    mov dx, 120
    call mediumCircle

    ret

    %macro drawSquare 4
    mov cx, %1
    .draw_rows:
      mov dx, %2
      int 10h
      mov dx, %4
      int 10h
      inc cx
      cmp cx, %3
      je .end_column
      jmp .draw_rows
    .end_column:
      mov dx, %2
    .draw_columns:
      mov cx, %1
      int 10h
      mov cx, %3
      int 10h
      inc dx
      cmp dx, %4
      jne .draw_columns
    %endmacro

    %macro drawer 1
    mov ah, 0ch 
    mov al, 0
    mov bh, 0
    %endmacro

  box_app1:
    drawSquare 100, 1, 140, 28; esq, cim, dir, bai

  box_app2:
    drawSquare 140, 1, 180, 28

  box_app3:
    drawSquare 180, 1, 220, 28

  box_app4:
    drawSquare 220, 1, 260, 28

  box_app5: 
    drawSquare 260, 1, 300, 28

    ret

  draw_box_app:
    drawer blue
    call box_app1
    ret

  clearText:
    push dx
    push cx
    mov dx, 175
    mov cx, 0
    .jp_blue_clear:
      cmp dx, 185
      jg .endClear
      mov cx, 30
      .jp_loop_blue_clear:
        call darkBlue
        inc cx
        cmp cx, 280
        jl .jp_loop_blue_clear
        inc dx
        jmp .jp_blue_clear
    .endClear:
    pop cx
    pop dx
    ret

start:
  xor ax, ax
  mov cx, ax
  mov bx, ax
  mov es, ax
  mov ds, ax
  mov dx, ax

  pusha

  call video
  call draw_cookie
  call draw_box_app
  call imprimeTexto
  call clearText
  
  popa

  loopcounter:
    call delay                               
    inc bx           
    call get_click ; Pega informacao do aperto de botoes
    mov cx, 10                                
    cmp bx, cx                                
    je game ; Atualiza game a cada segundo                                  
    jne continue 
                                
      ; ---------------- GAME MECHANICS CODE             
    continue:
      jmp loopcounter 
  ; --------------------------- Framecounter ENDs

    game:
      ; ---------- REINICIA TELA E PRINTA SEG

      
      push ax

      xor ax, ax
      cmp ax, [flag_CAC]
      je keep_moving_bro
      call aumentar_pesquisas_clique

      keep_moving_bro:
      pop ax
      inc ax

      pusha
      
      xor ax, ax
      mov ah, 0Bh   ;setando o ah para especificar a prox interrupção
      xor bx, bx
      mov bh, 01h
      mov bl, 02h
      int 10h       ;interrupção para limpar o terminal (não ficar printando várias linhas a mesma coisa)

      mov ax, [flag_texto]
      cmp ax, [zero]
      je ignora
      cmp ax, [umVal]
      je estadoUm
      cmp ax, [doisVal]
      je estadoDois
      call clearText
      jmp follow

    estadoUm:
      inc ax
      mov [flag_texto], ax
      jmp ignora

    estadoDois:
      inc ax
      mov [flag_texto], ax
      jmp ignora
      
    follow:
      xor ax, ax
      mov [zero], ax

    ignora:
      popa
      ; ---------- REINICIA TELA E PRINTA SEG
      
      push ax
      ; ---------- GAME SCREEN
      
      ; Printando valor
      call bought
      ;call aumentar_pesquisas_clique

      xor bx, bx 
      jmp loopcounter
  ; ------------------------- Game Loop END

jmp 0x7e00