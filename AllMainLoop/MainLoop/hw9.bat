rem Remote Unit Bat File
rem load main loop and functions for main loop
asm86 main9.asm m1 ep db
asm86chk main9.asm
asm86 mfunc.asm m1 ep db
asm86chk mfunc.asm

rem load functions that convert from hex/dec to ASCII
asm86 converts.asm m1 ep db
asm86chk converts.asm

rem load functions to generate queues
asm86 queues.asm m1 ep db
asm86chk queues.asm

rem load keypad and display functions
asm86 keypad.asm m1 ep db
asm86chk keypad.asm
asm86 display.asm m1 ep db
asm86chk display.asm

rem load serial related functions
asm86 serial.asm m1 ep db
asm86chk serial.asm
asm86 int2.asm m1 ep db
asm86chk int2.asm

rem load event handler related functions
asm86 allEH.asm m1 ep db
asm86chk allEH.asm

rem load function to initalize timer
asm86 timer.asm m1 ep db
asm86chk timer.asm

rem load function to initialize chip select
asm86 cs.asm m1 ep db
asm86chk cs.asm

rem load segment table for display
asm86 segtab14.asm m1 ep db
asm86chk segtab14.asm

rem link all files together
link86 converts.obj, queues.obj, display.obj, segtab14.obj to hw91.lnk
link86 int2.obj, timer.obj, cs.obj to hw93.lnk
link86 hw91.lnk, hw93.lnk to hw94.lnk
link86 allEH.obj, main9.obj, mfunc.obj, keypad.obj, serial.obj, hw94.lnk to hw9.lnk

loc86 hw9.lnk ad(sm(code(1000h), data(400h), stack(7000h))) noic