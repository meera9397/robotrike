asm86 main9.asm m1 ep db
asm86 mfunc.asm m1 ep db

asm86 converts.asm m1 ep db
asm86 queues.asm m1 ep db

asm86 keypad.asm m1 ep db
asm86 display.asm m1 ep db

asm86 motors.asm m1 ep db
asm86 trigtbl.asm m1 ep db
asm86 forcet.asm m1 ep db
asm86 motorb.asm m1 ep db

asm86 serial.asm m1 ep db
asm86 int2.asm m1 ep db

asm86 allEH.asm m1 ep db
asm86 timer.asm m1 ep db
asm86 cs.asm m1 ep db

link86 main9.obj, mfunc.obj, converts.obj, queues.obj, keypad.obj, display.obj to hw91.lnk
link86 motors.obj, trigtbl.obj, forcet.obj, motorb.obj, serial.obj, int2.obj to hw92.lnk
link86 allEH.obj, timer.obj, cs.obj, motorb.obj to hw93.lnk