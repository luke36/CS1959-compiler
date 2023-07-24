    .global _decode_literal
_decode_literal:
    movq $-2, %rax
    movq %r8, %r9
loop_entry:
    movzbq 0(%r8), %rcx
    addq $1, %r8
    cmpq $1, %rcx
    je case_pair
    cmpq $0, %rcx
    je case_trivial
    cmpq $2, %rcx
    je case_vector
    cmpq $4, %rcx
    je case_empty_vector
default:
    movq %rcx, %rsi
    jmp go_up
case_pair:
    movq %rdx, %rsi
    addq $16, %rdx
    addq $1, %rsi
    movq $-2, -1(%rsi)
    movq %rax, 7(%rsi)
    movq %rsi, %rax
    jmp loop_entry
case_trivial:
    movq 0(%r8), %rsi
    addq $8, %r8
    jmp go_up
case_vector:
    movq 0(%r8), %rdi
    addq $8, %r8
    movq %rdx, %rsi
    addq %rdi, %rdx
    addq $8, %rdx
    addq $3, %rsi
    movq $0, -3(%rsi)
    movq %rax, -3(%rsi, %rdi)
    movq %rsi, %rax
    jmp loop_entry
case_empty_vector:
    movq %rdx, %rsi
    addq $8, %rdx
    addq $3, %rsi
    movq $0, -3(%rsi)
    cmpq $-2, %rax
    jne go_up
    movq %rsi, %rax
    jmp *%r15
go_up:
    movq %rax, %r10
    andq $7, %r10
    cmpq $1, %r10
    jne pa_is_vector
pa_is_pair:
    cmpq $-2, -1(%rax)
    jne is_cdr
is_car:
    movq %rsi, -1(%rax)
    jmp loop_entry
is_cdr:
    movq 7(%rax), %rbx
    movq %rsi, 7(%rax)
    jmp return_or_up
pa_is_vector:
    cmpb $3, 0(%r8)
    je is_last_element
not_last_element:
    movq -3(%rax), %r10
    movq %rsi, 5(%rax, %r10)
    addq $8, -3(%rax)
    jmp loop_entry
is_last_element:
    addq $1, %r8
    movq -3(%rax), %r10
    movq 5(%rax, %r10), %rbx
    movq %rsi, 5(%rax, %r10)
    addq $8, -3(%rax)
return_or_up:
    cmpq $-2, %rbx
    je return
    movq %rax, %rsi
    movq %rbx, %rax
    jmp go_up
return:
    movq %rax, 0(%r9)
    jmp *%r15

    .global _symbol_to_address
_symbol_to_address:
    sarq $3, %rdi
    leaq _symbol_dump(%rip), %rax
    addq %rdi, %rax
    ret
