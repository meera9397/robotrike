asm86 serial.asm m1 ep db
asm86 main.asm m1 ep db
asm86 eh.asm m1 ep db
asm86 int2.asm m1 ep db
asm86 cs.asm m1 ep db
asm86 queues.asm m1 ep db

link86 serial.obj, hw7test.obj, main.obj, eh.obj, int2.obj, cs.obj, queues.obj to hw7.lnk

loc86 hw7.lnk ad(sm(code(1000h), data(400h), stack(7000h))) noic