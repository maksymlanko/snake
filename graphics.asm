[org 0x100]                 	; Origin directive, set start address for the program (used in .COM files)
section	.data
	cur_x			DW 5		; our x position
	cur_y			DW 5		; our y position
	mov_x			DW 5		; our movement in x
	mov_y			DW 0		; our movement in y
	cur_size		DW 5		; square length

section	.text
start:
	call init_screen			; change to video mode
	jmp delay					; jump to main loop

init_screen:
	; Set video mode to 320x200 pixels with 256 colors
	mov ah, 00h                 ; AH=00h - Set the function number for 'Set Video Mode'
	mov al, 13h                 ; AL=13h - Set the mode to 13h (320x200, 256 color mode)
	int 10h                     ; Call interrupt 10h (Video BIOS services), apply the video mode
	ret

check_input:
	mov ah, 1h					; see if key was pressed
	int 16h						; call interrupt
	jz clear_screen				; if received a key read it

	mov ah, 0h					; read key int
	int 16h						; call int

switch_left:
	cmp al, 61h					; see if key was 'a' in ascii
	jne switch_right			; if key is not 'a' exit
	mov word [mov_x], -5		; set x velocity to -5
	mov word [mov_y], 0			; set y velocity to 0
	jmp clear_screen			; continue main loop

switch_right:
	cmp al, 73h					; see if key was 's' in ascii
	jne exit_key				; if key is not 's' exit
	mov word [mov_x], 0		; set x velocity to -5
	mov word [mov_y], 5			; set x velocity to -5
	
	jmp clear_screen			; continue main loop

draw_square:
	push bp						; save bp
	mov bp, sp					; set bp to sp
	mov cx, [bp+4]				; cx = x
	mov dx, [bp+6]				; dx = y

draw_x:
	; draw pixels at coordinates (x->x+5 , y) with red color
	mov ah, 0ch                 ; AH=0Ch - Set the function number for 'Put Pixel'
	mov bh, 0                   ; BH=0 - Use display page 0
	mov al, 4                   ; AL=4 - Set the color index (color number 4 from the palette)
	int 10h                     ; Call interrupt 10h (Video BIOS services), plot the pixel
	inc cx						; Move x to the right by 1 pixel
	mov ax, cx					; save x of next pixel to be drawn
	sub ax, [cur_x]				; ax = drawn_pixel - pos x
	cmp ax, [cur_size]			; if ax == square length
	jne draw_x					; Draw another pixel of the x line
draw_y:
	mov cx, [cur_x]				; Reset the x value
	inc dx						; Move y down
	mov ax, dx					; save y of next pixel to be drawn
	sub ax, [cur_y]				; ax = drawn_pixel - pos y
	cmp ax, [cur_size]			; if ax == square length
	jne draw_x					; Draw another line

	jmp delay


clear_screen:
	; Set all pixels in video memory to color index 0 (black) by repeating stosb 'cx' times
	cld						 	; Set direction to forward (0)
    mov ax, 0A000h           	; Set segment address to video memory
    mov es, ax               	; Load ES with video memory segment
    xor di, di               	; Set di to start of video memory
    mov cx, 64000            	; There are 320x200 pixels, each pixel takes 1 byte
    mov al, 0                	; Color index for black (clear to black)
    rep stosb                	; Stores AL into ES:DI and increments DI, rep + stosb is like memset()
	push word [cur_y]			; arg2 = y
	push word [cur_x]			; arg1 = x
	call draw_square			; draw our position
	add esp, 4					; cleanup stack

delay:
	; Delay for CX:DX microseconds (CX and DX are 16 bit)
	mov ah, 86h					; Set flag to wait CX:DX time
	xor cx, cx					; Set cx to 0
	;mov dx, 16393				; around 1/60 of second
	mov dx, 16666				; around 1/60 of second
	int 15h						; Call wait interrupt

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
	jmp check_input				; clear screen

calc_y:
	cmp word [mov_y], 0			; is there movement in y
	je check_input
	mov bx, [cur_y]

check_end_y:
	cmp bx, 195
	jne check_start_y
	cmp word [mov_y], 5			; check if moving down
	; jl check_start_y
	jl exit_key
	mov bx, -5					; set bx to before screen start
	jmp update_y
check_start_y:
	cmp bx, 0
	jne update_y
	cmp word [mov_y], -5
	jg update_y
	mov bx, 645

update_y:
	add bx, [mov_y]
	mov [cur_y], bx
	jmp check_input

exit_key:
	; Keep the program running (exit on key press)
	mov ah, 0                   ; AH=0 - Function number for 'Check Keystroke'
	int 16h                     ; Call interrupt 16h (Keyboard BIOS services), wait for key press
	mov ax, 4C00h               ; Set function for 'Terminate with return code'
	int 21h                     ; Call interrupt 21h (DOS services), terminate the program