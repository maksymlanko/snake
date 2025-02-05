[org 0x100]                 ; Origin for COM file

section .data
    array   db  1, 2, 3, 4, 5, 0    ; Word-sized array elements

section .text
	global start

start:
    call calculate_sum
    add al, 16              ; add 0x10 to the result
    jmp end                 

calculate_sum:
    mov  ax, 0              ; Initialize accumulator
    mov  cx, 0              ; Initialize counter
    mov  dx, 0 			  	; Initialize read value
    
sum:
    mov  bx, cx             ; Copy counter to bx
    mov  dl, [array + bx]   ; Load array element
    cmp  dl, 0            	; Check for end of array
    je   finish              	; If zero, we're done
    add  ax, dx            	; Add to sum
    inc  cx                	; Increment counter
    jmp  sum               	; Continue loop

finish:
    ret                     ; return

end:
    ; Result is in AX
    mov ah, 4ch
    int 21h

