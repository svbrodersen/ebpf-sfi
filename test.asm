mov64 r3, 10
jeq r3, 0, +7
mov64 r11, r3
sub64 r11, r1
add64 r11, 2
and64 r11, r2
add64 r11, r1
stb [r11], 42
jeq r0, 0, +2
jeq r0, 0, +-7
mov64 r11, r3
sub64 r11, r1
add64 r11, 2
and64 r11, r2
add64 r11, r1
ldxb r0, [r11]
exit
