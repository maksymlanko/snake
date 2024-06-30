	; graphics.asm
	; Set video mode and draw a pixel in DOS using BIOS interrupts

	[org 0x100]                 ; Origin directive, set start address for the program (used in .COM files)
start:
	; Set video mode to 320x200 pixels with 256 colors
	mov ah, 00h                 ; AH=00h - Set the function number for 'Set Video Mode'
	mov al, 13h                 ; AL=13h - Set the mode to 13h (320x200, 256 color mode)
	int 10h                     ; Call interrupt 10h (Video BIOS services), apply the video mode

draw_square:	
	mov cx, 5
	mov dx, 5
draw_x:
	; Write pixels on the screen at coordinates (5-10,5) with color index 4
	mov ah, 0ch                 ; AH=0Ch - Set the function number for 'Put Pixel'
	mov bh, 0                   ; BH=0 - Use display page 0
	mov al, 4                   ; AL=4 - Set the color index (color number 4 from the palette)
	int 10h                     ; Call interrupt 10h (Video BIOS services), plot the pixel
	inc cx						; Move x to the right
	cmp cx, 10					; Size 10-5
	jne draw_x					; Draw another pixel of the x line
draw_y:
	inc dx						; Move y down
	mov cx, 5					; Reset the x value
	cmp dx, 10					; Size 10-5
	jne draw_x					; Draw another line

	; Keep the program running (exit on key press)
	mov ah, 0                   ; AH=0 - Function number for 'Check Keystroke'
	int 16h                     ; Call interrupt 16h (Keyboard BIOS services), wait for key press
	mov ax, 4C00h               ; Set function for 'Terminate with return code'
	int 21h                     ; Call interrupt 21h (DOS services), terminate the program

