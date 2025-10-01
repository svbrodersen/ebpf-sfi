sub64 r2, 1
mov64 r3, 10
jeq r3, 0, +14
mov64 r9, r3
add64 r9, 2
mov64 r11, r9
sub64 r9, r10
and64 r9, 511
add64 r9, r10
jeq r9, r11, +6
sub64 r11, r1
and64 r11, r2
add64 r11, r1
stb [r11], 42
ja +2
stb [r9], 42
jeq r0, 0, +2
jeq r0, 0, +-14
mov64 r9, r3
add64 r9, 2
mov64 r11, r9
sub64 r9, r10
and64 r9, 511
add64 r9, r10
jeq r9, r11, +6
sub64 r11, r1
and64 r11, r2
add64 r11, r1
ldxb r0, [r11]
ja +2
ldxb r0, [r9]
mov64 r9, r3
mov64 r11, r9
sub64 r9, r10
and64 r9, 511
add64 r9, r10
jeq r9, r11, +6
sub64 r11, r1
and64 r11, r2
add64 r11, r1
ldxb r0, [r11]
ja +2
ldxb r0, [r9]
exit
