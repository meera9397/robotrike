asm86 display.asm m1 ep db
asm86 eh_ti.asm m1 ep db
asm86 main.asm m1 ep db
asm86 converts.asm m1 ep db
asm86 segtable.asm m1 ep db

link86 display.obj, eh_ti.obj, main.obj, converts.obj, segtable.obj, hw4test.obj to hw4.lnk

loc86 hw4.lnk ad(sm(code(1000h), data(400h), stack(7000h))) noic