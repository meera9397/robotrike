rem Motor Unit Bat File
rem load main loop/ main loop related functions
asm86 main10.asm m1 ep db
asm86chk main10.asm
asm86 mfunc2.asm m1 ep db
asm86chk mfunc2.asm

rem load functions that convert from hex/dec to ASCII
asm86 converts.asm m1 ep db
asm86chk converts.asm

rem load functions to generate queues
asm86 queues.asm m1 ep db
asm86chk queues.asm

rem load motor functions
asm86 motors.asm m1 ep db
asm86 trigtbl.asm m1 ep db
asm86 forcet.asm m1 ep db
asm86 motorb.asm m1 ep db

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

rem load functions to parse serial data
asm86 parser.asm m1 ep db
asm86chk parser.asm

link86 converts.obj, queues.obj to hw91.lnk
link86 motors.obj, trigtbl.obj, forcet.obj, motorb.obj, parser.obj to hw92.lnk
link86 int2.obj, timer.obj, cs.obj to hw93.lnk
link86 hw91.lnk, hw92.lnk, hw93.lnk to hw94.lnk
link86 allEH.obj, main10.obj, mfunc2.obj, serial.obj, hw94.lnk to hw9.lnk

loc86 hw9.lnk ad(sm(code(1000h), data(400h), stack(7000h))) noic