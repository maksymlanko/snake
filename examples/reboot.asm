[org 0x100]                 	; Origin directive, set start address for the program (used in .COM files)

circular_len equ 100			; max snake len / 2
section	.data
	cur_x			DW 5		; our x position
	cur_y			DW 5		; our y position
	mov_x			DW 5		; our movement in x
	mov_y			DW 0		; our movement in y
	cur_size		DW 5		; square length
	cur_len			DW 2
	game_tick		DW 1
	apple_exists	DW 0		; flag for checking if apple already exists
	apple_x			DW 0		; apple x coordinate
	apple_y			DW 0 		; apple y coordinate
	seed			DW 0x1337		; seed for generating apple coords
	snake			DW 100 dup(10)
 

section	.text
start:
	call init_screen			; change to video mode
	call create_snake			; start snake positions
game_loop:
	call clear_screen
	call check_input
	call move_square
	call save_new_position
	call draw_snake
	call draw_apple

	;call draw_apple
	call delay					; jump to main loop
	jmp game_loop

init_screen:
	; Set video mode to 320x200 pixels with 256 colors
	mov ah, 00h                 ; AH=00h - Set the function number for 'Set Video Mode'
	mov al, 13h                 ; AL=13h - Set the mode to 13h (320x200, 256 color mode)
	int 10h                     ; Call interrupt 10h (Video BIOS services), apply the video mode
	ret

save_new_position:
	; get modulus
	mov dx, 0					; DIV XX opcode: divide DX:AX by XX and store result in AX, modulus in DX
	mov ax, [game_tick]			; load current game_tick
	mov cx, 50					; load max game_tick (100 positions / 4 position size = 25 max game_ticks)
	div cx						; perform division
	mov si, dx					; save modulus in si
	; because in 16bit you can't do lea [arr + 2 * counter]
	shl si, 2					; saved x is 2 bytes, y is another 2 bytes, so we want index * 4

	mov ax, [cur_x]				; load x position
	mov bx, [cur_y]
	lea di, [snake+si]			; load position snake[2i]
	mov [di], ax				; save x in snake[2i]
	add di, 2					; advance index by 2
	mov [di], bx				; save y in snake[2i+1]
	ret

create_snake:
	mov ax, [cur_x]				; load starting x position
	mov bx, [cur_y]				
	; using SI and DI because CX and DX arent valid destination registers for LEA in x86 asm
	lea si, [snake+0]			; load first address of circular buffer
	mov [si], ax				; store in first position of circular buffer

	add si, 2
	mov [si], bx
	ret

draw_snake:
	; get modulus
	mov dx, 0					; DIV XX opcode: divide DX:AX by XX and store result in AX, modulus in DX
	mov ax, [game_tick]			; load current game_tick
	mov cx, 50					; load max game_tick (100 positions / 4 position size = 25 max game_ticks)
	div cx						; perform division
	mov si, dx					; save modulus in si
	shl si, 2

	mov cx, 0
snake_loop:
	push word 4					; red color
	lea di, [snake + si+2]		; get y
	push word [di] 
	sub di, 2					; get x
	push word [di]

	call draw_square			; draw our position
	add sp, 6

	inc cx
	sub si, 4
	; if index goes to negative values, change it to last position
	cmp si, -4
	jne snake_finished
	mov si, 196
snake_finished:
	cmp cx, [cur_len]
	jne snake_loop
	ret

get_random:
	mov ax, [seed]				; load seed value
	mov bx, 0xcafe				; load multiply constanst
	mul bx						; multiply them (saved in DX:AX)
	; add ax, 0xdead				; add constant to result
	add ax, [game_tick]			; add kind of random value
	mov [seed], ax				; save new seed value
	ret
	; mov ax, [game_tick]			; load game_tick value
	; shl ax, 2					; multiply by 4

check_apple:
	mov ax, [apple_exists]		; return saved in AX register
	cmp ax, 0
	jne skip_generate_coords
	call get_random
	push word 40				; 200pixels /5pixels per square
	push word ax				; random value
	call get_modulus			; get random % 40
	add sp, 4					; clean up stack
	push word ax;
	push word ax;
	; push word 100;

skip_generate_coords:
	call draw_apple
	ret

get_modulus:
	push bp					; save previous bp
	mov bp, sp				; set base pointer


	mov dx, 0					; DIV XX opcode: divide DX:AX by XX and store result in AX, modulus in DX
	mov ax, [bp+4]				; get random value passed as arg1
	mov cx, [bp+6]				; get value to use as divider (arg2)
	div cx						; perform division
	mov ax, dx					; save modulus in si
	; because in 16bit you can't do lea [arr + 2 * counter]
	pop bp


draw_apple:
	push word 2					; green color
	call get_random				; get random value
	; mov ax, [game_tick]

	; push word ax
	; push word ax
	; push word 100				; y coordinate
	; push word 100				; x coordinate

	call draw_square			; draw apple
	add sp, 6					; clean up stack
	ret

check_input:
	mov ah, 1h					; see if key was pressed
	int 16h						; call interrupt
	jnz read_key				; if received a key read it
	ret

read_key:
	mov ah, 0h					; read key int
	int 16h						; call int

switch_left:
	cmp al, 61h					; see if key was 'a' in ascii
	jne switch_down				; if key is not 'a' exit
	mov word [mov_x], -5		; set x velocity to -5
	mov word [mov_y], 0			; set y velocity to 0
	ret

switch_down:
	cmp al, 73h					; see if key was 's' in ascii
	jne switch_right			; if key is not 's' exit
	mov word [mov_x], 0			; set x velocity to -5
	mov word [mov_y], 5			; set x velocity to -5
	ret

switch_right:
	cmp al, 64h					; see if key was 'd' in ascii
	jne switch_up
	mov word [mov_x], 5
	mov word [mov_y], 0
	ret

switch_up:
	cmp al, 77h					; see if key was 'w' in ascii
	jne exit_check_input				; if no relevant key was pressed, ignore key press
	mov word [mov_x], 0
	mov word [mov_y], -5
exit_check_input:
	ret

draw_square:
	push bp						; save bp
	mov bp, sp					; set bp to sp
	
	push cx
	mov cx, [bp+4]				; cx = x
	mov dx, [bp+6]				; dx = y

draw_x:
	; draw pixels at coordinates (x->x+5 , y) with red color
	mov ah, 0ch                 ; AH=0Ch - Set the function number for 'Put Pixel'
	mov bh, 0                   ; BH=0 - Use display page 0
	mov al, [bp+8]              ; AL=4 - Set the color index (color number 4 from the palette)
	int 10h                     ; Call interrupt 10h (Video BIOS services), plot the pixel
	inc cx						; Move x to the right by 1 pixel
	mov ax, cx					; save x of next pixel to be drawn
	sub ax, [bp+4]				; ax = drawn_pixel - pos x
	cmp ax, [cur_size]			; if ax == square length
	jne draw_x					; Draw another pixel of the x line
draw_y:
	mov cx, [bp+4]				; Reset the x value
	inc dx						; Move y down
	mov ax, dx					; save y of next pixel to be drawn
	sub ax, [bp+6]				; ax = drawn_pixel - pos y
	cmp ax, [cur_size]			; if ax == square length
	jne draw_x					; Draw another line

	pop cx
	pop bp
	ret

clear_screen:
	; Set all pixels in video memory to color index 0 (black) by repeating stosb 'cx' times
	cld						 	; Set direction to forward (0)
    mov ax, 0A000h           	; Set segment address to video memory
    mov es, ax               	; Load ES with video memory segment
    xor di, di               	; Set di to start of video memory
    mov cx, 64000            	; There are 320x200 pixels, each pixel takes 1 byte
    mov al, 0                	; Color index for black (clear to black)
    rep stosb                	; Stores AL into ES:DI and increments DI, rep + stosb is like memset()
	ret

delay:
	; Delay for CX:DX microseconds (CX and DX are 16 bit)
	mov ah, 86h					; Set flag to wait CX:DX time
	xor cx, cx					; Set cx to 0
	;mov dx, 16393				; around 1/60 of second
	mov dx, 16666				; around 1/60 of second
	int 15h						; Call wait interrupt
	mov bx, [game_tick]
	inc bx

	; dont make it 0 because of out of bounds snake[] access in draw_snake
	cmp bx, 199
	jne	skip_reset_tick
	mov bx, 99

skip_reset_tick:
	mov [game_tick], bx
	ret

move_square:
calc_x:
	cmp word [mov_x], 0			; is there movement in x
	je calc_y
	mov bx, [cur_x]				; load our x position

check_end_x:
	cmp bx, 320					; check if position = screen end
	jne check_start_x
	cmp word [mov_x], 5			; check if moving forward
	jl update_x				; if vel < 5 don't reset x
	mov bx, -5					; if x=320 and vel >= 5 -> reset x
check_start_x:
	cmp bx, 0					; check if position = screen start
	jne update_x
	cmp word [mov_x], -5		
	jg update_x
	mov bx, 325

update_x:
	add bx, [mov_x]				; add velocity to our x position
	mov [cur_x], bx				; update our position
	ret 

calc_y:
	cmp word [mov_y], 0			; is there movement in y
	jne after_check
	ret
after_check:
	mov bx, [cur_y]

check_end_y:
	cmp bx, 195
	jne check_start_y
	cmp word [mov_y], 5			; check if moving down
	jl exit_key
	mov bx, -5					; set bx to before screen start
	jmp update_y
check_start_y:
	cmp bx, 0
	jne update_y
	cmp word [mov_y], -5
	jg update_y
	mov bx, 195

update_y:
	add bx, [mov_y]
	mov [cur_y], bx
	ret

exit_key:
	; Keep the program running (exit on key press)
	mov ah, 0                   ; AH=0 - Function number for 'Check Keystroke'
	int 16h                     ; Call interrupt 16h (Keyboard BIOS services), wait for key press
	mov ax, 4C00h               ; Set function for 'Terminate with return code'
	int 21h                     ; Call interrupt 21h (DOS services), terminate the program