asm86 macros.asm m1 ep db
asm86 main.asm m1 ep db
link86 macros.obj, main.obj to macros.lnk
loc86 macros.lnk ad(sm(code(1000h), data(400h), stack(7000h))) noic