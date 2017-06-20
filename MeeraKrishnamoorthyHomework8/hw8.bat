asm86 parser.asm m1 ep db
asm86 parser.asm m1 ep db

link86 parser.obj, main.obj, hw8test.obj to hw8.lnk

loc86 hw8.lnk ad(sm(code(1000h), data(400h), stack(7000h))) noic