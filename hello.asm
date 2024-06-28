global _start

section .data
    msg db "Hello, world!", 0x0a
    len equ $ - msg

section .text
_start:
    mov eax, 4      ; sys_write
    mov ebx, 1      ; stdout
    mov ecx, msg    ; set msg
    mov edx, len    ; set msg len
    int 0x80        ; call kernel
    mov eax, 1      ; sys_exit
    mov ebx, 43      ; return 0
    int 0x80
