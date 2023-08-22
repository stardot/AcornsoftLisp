CFLAGS=	-Wall -g -O3
LISP406_real=	../ROMs/LISP406

lisp:	tlisp IMAGE reltab.bin combine
	./combine -i a600 -r b800 tlisp IMAGE reltab.bin >lisp

compare:	lisp
	cmp $(LISP406_real) lisp && echo success

tlisp:	lisp*.a
	acme -DLISVAL='$$8000' -r tlisp.rep -o tlisp lisp.a

tlisphigh:	lisp*.a
	acme -DLISVAL='$$8100' -o tlisphigh lisp.a

IMAGE:	init.lsp tlisp
	../bbc/run-bbc -c -i init.lsp tlisp

reltab.bin:	tlisp tlisphigh build-reltab
	./build-reltab -b -e a400 -h d700 tlisp tlisphigh >reltab.bin

reltab.asm:	tlisp tlisphigh build-reltab
	./build-reltab -a -e a400 -h d700 tlisp tlisphigh >reltab.asm

build-reltab:	build-reltab.o
combine:	combine.o

clean:
	rm *.o build-reltab combine tlisp* IMAGE reltab.bin lisp

