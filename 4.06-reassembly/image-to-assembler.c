/*
 * Convert an Acornsoft Lisp heap image into ACME assembler code
 * suitable for including in the source.  The image can be a ROM
 * or the result of running (SAVE 'filename) in Lisp.
 *
 * The first argument is the offset in the file where the image
 * appears.  It should be 2600 for the LISP 4.06 ROM (sorresponding to
 * address A600), and 0 for a saved image.
 *
 * The second argment is the address where it will be included in
 * the assembler code, normally A600.  This is needed because the
 * program prodduces readable labels rather than hex addresses, and
 * they need to be adjusted according to where they are assembled.
 *
 * The third argument specifies an addres for the image to be relocated
 * to before converting.  This makes no functional difference, as the
 * image is relocated anyway in the running Lisp.  But it allows us
 * to produce code matching the original source.
 *
 * To generate an exact copy of the image in the Lisp 4.06 ROM, use
 *   image-to-assembler 2600 a600 < the-rom > image.asm
 *
 * To generate code corresponding to the original source, use
 *   image-to-assembler 2600 a600 a600 < the-rom > image.asm
 * Then edit image.asm to remove everything from VERSION onwards,
 * as that comes from Lisp code that is loaded in after assembly.
 *
 * (C) Richard Tobin 2023
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

uint16_t UNDEFINED, IMBOT, AREVAL, IMLEN;
#define MIN_IMAGE_SIZE 15	/* must be room for UNDEFINED */

enum type {CHARF=0x00, NUMF=0x04, SUBRF=0x08, FSUBRF=0x0c, LISTF=0x80};
#define is_NIL(addr) (((addr) & 0xff00) == 0)

uint8_t memory[64*1024];
uint8_t memory2[64*1024];	/* used for relocating */
char *label[65536] = {0};

void read_image(int image_off);
void relocate_image(uint16_t reloc_addr);
void assign_labels(void);
void print_image(uint16_t asm_addr);

void print_objects(uint16_t shift);
void print_addr(uint16_t addr, uint16_t shift);

char *get_name(uint16_t symbol);

void relocate_word(uint16_t addr, uint16_t shift);

uint16_t get_byte(uint16_t addr);
void set_byte(uint16_t addr, uint8_t value);
uint16_t get_word(uint16_t addr);
void set_word(uint16_t addr, uint16_t value);

int object_size(uint16_t obj);
uint16_t next_object(uint16_t obj);

int main(int argc, char **argv)
{
    int image_off, asm_addr, reloc_addr = -1;
    
    if(argc < 3 || argc > 4)
    {
	fprintf(stderr, "usage: %s image-offset assembly-address [relocation-address]\n", argv[1]);
	return 2;
    }

    image_off = strtoul(argv[1], 0, 16);
    asm_addr = strtoul(argv[2], 0, 16);
    if(argc > 3)
	reloc_addr = strtoul(argv[3], 0, 16);
    
    read_image(image_off);

    if(reloc_addr != -1)
	relocate_image(reloc_addr);

    assign_labels();

    print_image(asm_addr);
}

/* Read an image into its expected place im memory */

void read_image(int image_off)
{
    static uint8_t rom[64*1024];
    int nread;

    /* read the image file */
    
    nread = fread(rom, 1, sizeof(rom), stdin);
    if(nread < MIN_IMAGE_SIZE || nread > 60*1024)
    {
	fprintf(stderr, "implausibly sized image file\n");
	exit(1);
    }

    if(image_off + MIN_IMAGE_SIZE > nread)
    {
	fprintf(stderr, "no room for an image in image file\n");
	exit(1);
    }
    
    /* verify that there is an image at the offset */

    if(strncmp((char *)rom + image_off + 8, "UNDEFINED", 9) != 0 &&
       strncmp((char *)rom + image_off + 8, "undefined", 9) != 0)
    {
	fprintf(stderr, "didn't find UNDEFINED symbol at start of image\n");;
	exit(1);
    }

    /* image started 2 bytes before UNDEFINED's address (which is its
       own value) and end at the address at the start of the image */

    UNDEFINED = rom[image_off + 4] + (rom[image_off + 5] << 8);
    IMBOT = UNDEFINED - 2;
    AREVAL = rom[image_off] + (rom[image_off + 1] << 8);
    IMLEN = AREVAL - IMBOT;

    if(image_off + IMLEN > nread)
    {
	fprintf(stderr, "image runs off end of image file\n");
	exit(1);
    }

    fprintf(stderr, "image had IMBOT=%04x, length=0x%x\n", IMBOT, IMLEN);

    /* install the image in memory */

    memcpy(memory+IMBOT, rom+image_off, IMLEN);
}

void relocate_image(uint16_t reloc_addr)
{
    uint16_t obj, shift;

    shift = reloc_addr - IMBOT;

    /* copy the objects */
    
    for(obj=UNDEFINED; obj<AREVAL; obj=next_object(obj))
    {
	int type = get_byte(obj);
	uint16_t newobj = obj + shift; /* may wrap around, not a problem */

	/* copy the object */
	
	memcpy(memory2 + newobj, memory + obj, object_size(obj));

	/* fix its pointers */
	
	switch(type)
	{
	case CHARF:
	case SUBRF:
	case FSUBRF:
	    relocate_word(obj+2, shift);
	    relocate_word(obj+4, shift);
	    break;
	case LISTF:
	    relocate_word(obj+1, shift);
	    relocate_word(obj+3, shift);
	    break;
	default:
	    break;
	}
    }

    memcpy(memory, memory2, sizeof(memory));

    /* update image parameters */
    
    UNDEFINED = UNDEFINED + shift;
    IMBOT = IMBOT + shift;
    AREVAL = AREVAL + shift;
}

/* assign labels to memory locations: s_XXX for a symbol called XXX,
   v_XXX for its value (unless it's another symbol), p_XXX for its plist */

void assign_labels(void)
{
    uint16_t obj, value, plist;

    for(obj=UNDEFINED; obj<AREVAL; obj=next_object(obj))
    {
	int type = get_byte(obj);
	uint16_t value, plist;
	char *name, *l;

	if(type != CHARF)
	    continue;

	name = get_name(obj);
	value = get_word(obj+2);
	plist = get_word(obj+4);
	
	if(!label[obj] || label[obj][0] == 's')
	{
	    l = malloc(2+strlen(name)+1);
	    sprintf(l, "s_%s", name);
	    label[obj] = l;
	}
	if(!is_NIL(value) && !label[(uint16_t)(value)])
	{
	    l = malloc(2+strlen(name)+1);
	    sprintf(l, "v_%s", name);
	    label[(uint16_t)(value)] = l;
	}
	if(is_NIL(plist) && !label[(uint16_t)(plist)])
	{
	    l = malloc(2+strlen(name)+1);
	    sprintf(l, "p_%s", name);
	    label[(uint16_t)(plist)] = l;
	}	
    }
}

/* output the image as assembler suitable for the given address */

void print_image(uint16_t asm_addr)
{
    uint16_t obj, shift;

    /* we are assembling the objects at this much higher in memory, so
       we will have to adjust the labels back to the right value */
    
    shift = asm_addr - IMBOT;

    /* print the boilerplate around the objects */
    
    printf("IMAGE\n");
    printf("AREEXT\n");
    if(shift)
	printf("    !word ATOP - $%04X\n", shift);
    else
	printf("    !word ATOP\n");
    printf("WSBOT\n");

    /* print each object */
    
    print_objects(shift);

    printf("ATOP\n");
}

/* print the objects, with labels adjusted for the assembly address */

void print_objects(uint16_t shift)
{
    uint16_t obj;
	
    for(obj=UNDEFINED; obj<AREVAL; obj=next_object(obj))
    {
	uint8_t type = get_byte(obj);
	int len, value, i;
	
	printf("%s\n", label[obj] ? label[obj] : "");
	
	switch(get_byte(obj))
	{
	case CHARF:
	    len = get_byte(obj+1);
	    printf("    !byte CHARF, $%02X\n", len);
	    print_addr(get_word(obj+2), shift);
	    print_addr(get_word(obj+4), shift);
	    if(len == 7 && get_byte(obj+6) < ' ')
		printf("    !byte $%02X\n", get_byte(obj+6));
	    else
	    {
		printf("    !text \"");
		for(i=6; i<len; i++)
		    putchar(get_byte(obj+i));
		printf("\"\n");
	    }
	    break;
	case NUMF:
	    len = get_byte(obj+1);
	    printf("    !byte NUMF, $%02X\n", len);
	    printf("    !word $%04X\n", get_word(obj+2));
	    break;
	case SUBRF:
	    printf("    !byte SUBRF, $%02X\n", get_byte(obj+1));
	    print_addr(get_word(obj+2), shift);
	    print_addr(get_word(obj+4), shift);
	    break;
	case FSUBRF:
	    printf("    !byte FSUBRF, $%02X\n", get_byte(obj+1));
	    print_addr(get_word(obj+2), shift);
	    print_addr(get_word(obj+4), shift);
	    break;
	case LISTF:
	    len = 5;
	    printf("    !byte LISTF\n");
	    print_addr(get_word(obj+1), shift);
	    print_addr(get_word(obj+3), shift);
	    break;
	default:
	    fprintf(stderr, "bogus object type %d at %04x\n", get_byte(obj), obj);
	    exit(1);
	}
    }
}

void relocate_word(uint16_t addr, uint16_t shift)
{
    uint16_t value = get_word(addr);
    if(!is_NIL(value))
	set_word(addr+shift, value + shift);
}

void print_addr(uint16_t addr, uint16_t shift)
{
    char *l = label[addr];

    if(is_NIL(addr))
    {
	printf("    !word $%04X\n", addr);
	return;
    }
    
    if(l)
	if(shift == 0)
	    printf("    !word %s\n", l);
	else
	    printf("    !word %s - $%04X\n", l, shift);
    else
	printf("    !word $%04X\n", addr);
}
    
/* get s symbol's name, adjusted if necessary to be usable as a label */

char *get_name(uint16_t symbol)
{
    int len = get_byte(symbol+1), i;
    static char buf[512];
    char *p = buf;

    for(i=6; i<len; i++)
    {
	int c = get_byte(symbol + i);
	if(isalnum(c))
	    *p++ = c;
	else
	{
	    sprintf(p, "%02X", c);
	    p += 2;
	}
    }
    *p = 0;

    return buf;
}


/* Read from memory */

uint16_t get_byte(uint16_t addr)
{
    return memory[addr];
}

uint16_t get_word(uint16_t addr)
{
    return get_byte(addr+1) << 8 | get_byte(addr);
}

/* Write to memory2 */

void set_byte(uint16_t addr, uint8_t value)
{
    memory2[addr] = value;
}

void set_word(uint16_t addr, uint16_t value)
{
    memory2[addr] = (value & 0xff);
    memory2[addr+1] = (value >> 8);
}


int object_size(uint16_t obj)
{
    switch(get_byte(obj))
    {
    case CHARF:
    case NUMF:
	return get_byte(obj+1);
    case SUBRF:
    case FSUBRF:
	return 6;
    case LISTF:
	return 5;
    default:
	fprintf(stderr, "bogus object type %d at %04x\n", get_byte(obj), obj);
	exit(1);
    }
}

uint16_t next_object(uint16_t obj)
{
    return obj + object_size(obj);
}
