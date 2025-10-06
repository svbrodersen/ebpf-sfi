mov r0, 0
mov r1, 5
ja +2
add r0, r1
sub r1, 1
jgt r1, 0, +-3
exit
