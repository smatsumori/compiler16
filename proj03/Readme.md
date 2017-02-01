## 冪乗計算について
stack[2 5]	(2の5乗)
popq %rax	(乗数の部分を格納)
movq $1 %rbx	(結果)

LOOP:
cmpq $1	%rax	
je BREAK

subq $1	%rax	(rax = rax - 1)
imulq %rsp %rbx		(rbx = rbx * rsp) rsp is 2
jmp LOOP

BERAK:
movq %rbx (%rsp) 	(return statement)
