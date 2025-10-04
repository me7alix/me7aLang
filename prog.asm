section .text
global main
main:
    push rbp
    mov rbp, rsp
    mov eax, 5
    mov dword [rbp - 4], eax
    mov eax, 1
    mov dword [rbp - 8], eax
    mov eax, 2
    mov dword [rbp - 12], eax
    mov eax, 0
    mov dword [rbp - 16], eax
    mov eax, 0
    mov dword [rbp - 20], eax
.L0:
    mov eax, dword [rbp - 20]
    mov ecx, dword [rbp - 4]
    cmp eax, ecx
    setne al
    movzx eax, al
    mov dword [rbp - 24], eax
    cmp dword [rbp - 24], 0
    je .L1
    mov eax, dword [rbp - 12]
    mov dword [rbp - 16], eax
    mov eax, dword [rbp - 12]
    mov ecx, dword [rbp - 8]
    add eax, ecx
    mov dword [rbp - 28], eax
    mov eax, dword [rbp - 28]
    mov dword [rbp - 12], eax
    mov eax, dword [rbp - 16]
    mov dword [rbp - 8], eax
    mov eax, dword [rbp - 20]
    mov ecx, 1
    add eax, ecx
    mov dword [rbp - 32], eax
    mov eax, dword [rbp - 32]
    mov dword [rbp - 20], eax
    jmp .L0
.L1:
    mov eax, dword [rbp - 12]
    mov rsp, rbp
    pop rbp
    ret

    mov rsp, rbp
    pop rbp
    ret

