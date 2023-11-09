The program is written in F#. It takes assembly and converts it to rows of a program array to be pasted in programs.scala.

Some notes:
MOV cannot use a negative number
No comments
No spaces except to seperate opcode registers and immediate values.
Registers are written as: R#
An example:

NOP
MOV R10 100
MOV R1 0
MOV R2 2
ADD R1 R1 R2
JNE R1 R10 4
HALT