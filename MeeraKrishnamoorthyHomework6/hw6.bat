asm86 motors.asm m1 ep db
asm86 trigtbl.asm m1 ep db
asm86 forcet.asm m1 ep db
asm86 motorb.asm m1 ep db
asm86 main.asm m1 ep db
asm86 eh.asm m1 ep db
asm86 timer.asm m1 ep db
asm86 cs.asm m1 ep db

link86 motors.obj, trigtbl.obj, forcet.obj, motorb.obj, hw6test.obj to hw61.lnk
link86 hw61.lnk, main.obj, eh.obj, timer.obj, cs.obj to hw6.lnk

loc86 hw6.lnk ad(sm(code(1000h), data(400h), stack(7000h))) noic