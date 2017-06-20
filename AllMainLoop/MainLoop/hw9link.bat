link86 converts.obj, queues.obj, display.obj, segtable.obj to hw91.lnk
link86 motors.obj, trigtbl.obj, forcet.obj, motorb.obj to hw92.lnk
link86 int2.obj, timer.obj, cs.obj to hw93.lnk
link86 hw91.lnk, hw92.lnk, hw93.lnk to hw94.lnk
link86 allEH.obj, main9.obj, mfunc.obj, keypad.obj, serial.obj, hw94.lnk to hw9.lnk

loc86 hw9.lnk ad(sm(code(1000h), data(400h), stack(7000h))) noic