mov r3, 10
jeq r3, 0, +2
stb [r3+2], 0x2a
jeq r0, 0, +2
jeq r0, 0, +-1
ldxb r0, [r3+2]
exit
