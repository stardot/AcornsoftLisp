ROM=	LISP407
ROMlc=	LISP407lc

CFLAGS=	-Wall -g -O3

all:	$(ROM) $(ROMlc)

$(ROM):	tlisp IMAGE reltab.bin combine
	./combine -i a600 -r b800 tlisp IMAGE reltab.bin >$(ROM)

$(ROMlc):	$(ROM) convert-to-lc
	./convert-to-lc <$(ROM) >$(ROMlc)

tlisp:	lisp*.a
	acme -DLISVAL='$$8000' -r tlisp.rep -o tlisp lisp.a

tlisphigh:	lisp*.a
	acme -DLISVAL='$$8100' -o tlisphigh lisp.a

# You probably don't have the run-bbc program.  Instead you need to
# run tlisp on a BBC computer or emulator, and then execute the
# commands in init.lsp.

IMAGE:	init.lsp tlisp
	run-bbc -c -i init.lsp tlisp

reltab.bin:	tlisp tlisphigh build-reltab
	./build-reltab -b -e a400 -h d700 tlisp tlisphigh >reltab.bin

reltab.asm:	tlisp tlisphigh build-reltab
	./build-reltab -a -e a400 -h d700 tlisp tlisphigh >reltab.asm

build-reltab:	build-reltab.o

combine:	combine.o

convert-to-lc:	convert-to-lc.o

clean:
	rm *.o build-reltab combine convert-to-lc tlisp* IMAGE reltab.bin $(ROM) $(ROMlc)
