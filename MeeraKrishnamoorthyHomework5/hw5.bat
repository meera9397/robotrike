asm86 keypad.asm m1 ep db
asm86 display.asm m1 ep db
asm86 main.asm m1 ep db
asm86 converts.asm m1 ep db
asm86 segtable.asm m1 ep db
asm86 eh.asm m1 ep db
asm86 timer.asm m1 ep db
asm86 cs.asm m1 ep db

link86 keypad.obj, display.obj, main.obj, converts.obj, segtable.obj, hw54test.obj to hw51.lnk
link86 eh.obj, timer.obj, cs.obj to hw52.lnk
link86 hw51.lnk, hw52.lnk to hw5.lnk

loc86 hw5.lnk ad(sm(code(1000h), data(400h), stack(7000h))) noic