LISP406_real=	../ROMs/LISP406
SOURCE=		LISP406_acme.asm

compare:	LISP406_replica
	cmp $(LISP406_real) LISP406_replica && echo success

clean:
	rm *.o low.{asm,bin} high.{asm,bin} image.{asm,bin} reloc.{asm,bin}

LISP406_replica:	$(SOURCE) image.asm reltab.asm
	acme -r LISP406_replica.rep -o LISP406_replica $(SOURCE)

image.asm:	image-to-assembler
	./image-to-assembler 2600 a600 <$(LISP406_real) >image.asm

reltab.asm:	low.bin high.bin build-reltab
	./build-reltab >reltab.asm

low.bin:	low.asm
	acme -o low.bin low.asm

low.asm:	$(SOURCE)
	sed '/^unused3/q' <$(SOURCE) >low.asm

high.asm:	low.asm
#note the tab character in the following line
	sed 's/=[ 	]*LISVAL/= LISVAL+256/' <low.asm  >high.asm

high.bin:	high.asm
	@echo Expect some warnings here
	acme -o high.bin high.asm

image-to-assembler: image-to-assembler.c

build-reltab:	build-reltab.c
