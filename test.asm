	; graphics.asm
	; Set video mode and draw a pixel in DOS using BIOS interrupts

	[org 0x100]                 ; Origin directive, set start address for the program (used in .COM files)
section	.data
	cur_x			DW 5
	cur_y			DW 5
	cur_size		DW 5

section	.text
start:
	; Set video mode to 320x200 pixels with 256 colors
	mov ah, 00h                 ; AH=00h - Set the function number for 'Set Video Mode'
	; mov al, 13h                 ; AL=13h - Set the mode to 13h (320x200, 256 color mode)
	mov al, 03h                 ; AL=13h - Set the mode to 13h (320x200, 256 color mode)
	int 10h                     ; Call interrupt 10h (Video BIOS services), apply the video mode

top_of_screen:
	; Set cursor position 	AH=02h 	BH = Page Number, DH = Row, DL = Column 
	mov ah, 0x02 					; set cursor position interrupt
	mov bh, 0x00					; page number 0
	mov dh, 0x00					; row number 0
	mov dl, 0x00 					; col number 0

clear:
	mov ah, 07h						; scroll down window
	mov al, 00h						; set to clear window
	;mov bh, 43H					; set background color to red
	mov bh, 0x14					; set background color as first byte and foreground as second byte
	mov bl, 0x00					; set page to be scrolled
	mov cx, 0x00					; set row and col start value
	mov dh, 0x24					; set row value of ending point
	mov dl, 0x79					; set col value of ending point
	int 10h							; call interrupt
	;jmp halt

print:
	mov ah, 0Eh						; set interrupt to teletype output
	mov al, 'a'						; set to char 'a'
	mov bh, 0x00 					; page 0
	mov bl, 0x03 						; set color to 3
	int 10h							; call interrupt
	;jmp halt

; halt:
; 	jmp halt


exit_key:
	; Keep the program running (exit on key press)
	mov ah, 0                   ; AH=0 - Function number for 'Check Keystroke'
	int 16h                     ; Call interrupt 16h (Keyboard BIOS services), wait for key press
	mov ax, 4C00h               ; Set function for 'Terminate with return code'
	int 21h                     ; Call interrupt 21h (DOS services), terminate the program

