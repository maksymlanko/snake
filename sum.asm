[org 0x100]                 ; Origin for COM file
section .text
	global start

start:
    call clear_regs         ; set all GP regs to 0
    
    push word 5
    push word 7
    call sum2
    add sp, 4               ; cleanup args for sum

    call exit

sum2:
    push bp                 ; save bp
    mov bp, sp              ; set base to be sp

    mov bx, [bp+4]
    mov ax, [bp+6]
    add ax, bx
    ret

clear_regs:
    mov ax, 0
    mov bx, 0
    mov cx, 0
    mov dx, 0
    ret

exit:
    mov ah, 4ch
    int 21h