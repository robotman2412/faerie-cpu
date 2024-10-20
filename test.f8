
    .equ x, 0
    .equ y, 1
    .equ z, 2

    .org 0xc000
entry:
    li  0
    st  [x]
    st  [z]
    li  1
    st  [y]
loop:
    ld  [x]
    add [y]
    bcs [entry]
    st  [z]
    ld  [x]
    st  [y]
    ld  [z]
    st  [x]
    j   [loop]
