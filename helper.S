    .globl _scheme_decode_literal
_scheme_decode_literal:
    movq $-2, %rax
DL_LOOP_ENTRY:
    movzbq 0(%r8), %rcx
    addq $1, %r8
    cmpq $1, %rcx
    je DL_CASE_PAIR
    cmpq $0, %rcx
    je DL_CASE_TRIVIAL
    cmpq $2, %rcx
    je DL_CASE_VECTOR
    cmpq $4, %rcx
    je DL_CASE_EMPTY_VECTOR
DL_DEFAULT:
    movq %rcx, %rsi
    jmp DL_GO_UP
DL_CASE_PAIR:
    movq %r11, %rsi
    addq $16, %r11
    addq $1, %rsi
    movq $-2, -1(%rsi)
    movq %rax, 7(%rsi)
    movq %rsi, %rax
    jmp DL_LOOP_ENTRY
DL_CASE_TRIVIAL:
    movq 0(%r8), %rsi
    addq $8, %r8
    jmp DL_GO_UP
DL_CASE_VECTOR:
    movq 0(%r8), %rdi
    addq $8, %r8
    movq %r11, %rsi
    addq %rdi, %r11
    addq $8, %r11
    addq $3, %rsi
    movq $0, -3(%rsi)
    movq %rax, -3(%rsi, %rdi)
    movq %rsi, %rax
    jmp DL_LOOP_ENTRY
DL_CASE_EMPTY_VECTOR:
    movq %r11, %rsi
    addq $8, %r11
    addq $3, %rsi
    movq $0, -3(%rsi)
    cmpq $-2, %rax
    jne DL_GO_UP
    movq %rsi, %rax
    jmp *%r15
DL_GO_UP:
    movq %rax, %r10
    andq $7, %r10
    cmpq $1, %r10
    jne DL_PA_IS_VECTOR
DL_PA_IS_PAIR:
    cmpq $-2, -1(%rax)
    jne DL_IS_CDR
DL_IS_CAR:
    movq %rsi, -1(%rax)
    jmp DL_LOOP_ENTRY
DL_IS_CDR:
    movq 7(%rax), %rbx
    movq %rsi, 7(%rax)
    jmp DL_DL_RETURN_OR_UP
DL_PA_IS_VECTOR:
    cmpb $3, 0(%r8)
    je DL_IS_LAST_ELEMENT
DL_NOT_LAST_ELEMENT:
    movq -3(%rax), %r10
    movq %rsi, 5(%rax, %r10)
    addq $8, -3(%rax)
    jmp DL_LOOP_ENTRY
DL_IS_LAST_ELEMENT:
    addq $1, %r8
    movq -3(%rax), %r10
    movq 5(%rax, %r10), %rbx
    movq %rsi, 5(%rax, %r10)
    addq $8, -3(%rax)
DL_DL_RETURN_OR_UP:
    cmpq $-2, %rbx
    je DL_RETURN
    movq %rax, %rsi
    movq %rbx, %rax
    jmp DL_GO_UP
DL_RETURN:
    jmp *%r15

    .globl _scheme_symbol_to_address
_scheme_symbol_to_address:
    subq $4, %rdi
    leaq _scheme_symbol_dump(%rip), %rax
    addq %rdi, %rax
    ret

    .globl _scheme_call_with_current_continuation
_scheme_call_with_current_continuation:
CALLCC_CHECK_OVERFLOW:
    movq %r8, %r12
    leaq 2(%r11), %r8
    addq $24, %r11
    movq %rbp, %r9
    subq %r14, %r9
    addq %r9, %r11
#ifdef COLLECTION
    cmpq %r13, %r11
    jle CALLCC_MAKE_CONT
CALLCC_COLLECT:
    leaq 0(%rbp), %rdi
    leaq 8(%rbp), %rsi
    movq %r15, %rdx
    movq %rbp, %rcx
    movq %r11, %r9
    call collect
    movq 0(%rbp), %r11
    movq 8(%rbp), %r13
    movq %rax, %r8
CALLCC_MAKE_CONT:
#endif
    leaq _scheme_invoke_continuation(%rip), %r10
    movq %r10, -2(%r8)
    movq %r15, 6(%r8)
    movq %rbp, %rcx
    subq %r14, %rcx
    movq %rcx, 14(%r8)
    movq $0, %rbx
CALLCC_LOOP_ENTRY:
    cmpq %rbx, %rcx
    je CALLCC_APPLY
CALLCC_DO_COPY:
    movq 0(%rbx, %r14), %r10
    movq %r10, 22(%r8, %rbx)
    addq $8, %rbx
    jmp CALLCC_LOOP_ENTRY
CALLCC_APPLY:
    movq %r8, %r9
    jmp *-2(%r12)

    .quad -1
_scheme_invoke_continuation:
    movq 14(%r8), %rax
    movq %r14, %rbp
    addq %rax, %rbp
IC_LOOP_ENTRY:
    cmpq $0, %rax
    je IC_RETURN
IC_DO_COPY:
    movq 22(%rax, %r8), %r10
    movq %r10, 0(%r14, %rax)
    subq $8, %rax
    jmp IC_LOOP_ENTRY
IC_RETURN:
    movq %r9, %rax
    jmp *6(%r8)

    .globl _scheme_collect
_scheme_collect:
    leaq 0(%rbp), %rdi
    leaq 8(%rbp), %rsi
    movq %r15, %rdx
    movq %rbp, %rcx
    movq %r11, %r9
    call collect
    movq 0(%rbp), %r11
    movq 8(%rbp), %r13
    jmp *%r15

    .globl _scheme_inspect
_scheme_inspect:
    movq %r8, %rdi
    movq %r11, %r12
    call inspect
    movq %r12, %r11
    jmp *%r15

    .globl _scheme_write
_scheme_write:
    movq %r8, %rdi
    movq %r11, %r12
    call write_ptr
    movq %r12, %r11
    jmp *%r15

    .globl _scheme_display
_scheme_display:
    movq %r8, %rdi
    movq %r11, %r12
    call display_ptr
    movq %r12, %r11
    jmp *%r15

    .globl _scheme_read_char
_scheme_read_char:
    call getchar
    shlq $8, %rax
    addq $254, %rax
    jmp *%r15
