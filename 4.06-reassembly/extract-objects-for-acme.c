#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

char *label[65536];

uint16_t ROM_BASE=0x8000;
#define ROM_SIZE ((size_t)0x4000)
uint16_t IMAGE=0xa600;
uint16_t UNDEFINED, OFFSET, ATOP;

enum type {CHARF=0x00, NUMF=0x04, SUBRF=0x08, FSUBRF=0x0c, LISTF=0x80};

#define is_NIL(addr) (((addr) & 0xff00) == 0)

static uint8_t image[ROM_SIZE];

uint16_t get_byte(uint16_t addr)
{
    return image[addr - ROM_BASE];
}

void set_byte(uint16_t addr, uint8_t value)
{
    image[addr - ROM_BASE] = value;
}

uint16_t get_word(uint16_t addr)
{
    return get_byte(addr+1) << 8 | get_byte(addr);
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

void find_labels(void)
{
    uint16_t obj, value, plist;

    for(obj=UNDEFINED; obj<ATOP; obj=next_object(obj))
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
	if(!is_NIL(value) && !label[(uint16_t)(value+OFFSET)])
	{
	    l = malloc(2+strlen(name)+1);
	    sprintf(l, "v_%s", name);
	    label[(uint16_t)(value+OFFSET)] = l;
	}
	if(is_NIL(plist) && !label[(uint16_t)(plist+OFFSET)])
	{
	    l = malloc(2+strlen(name)+1);
	    sprintf(l, "p_%s", name);
	    label[(uint16_t)(plist+OFFSET)] = l;
	}	
    }
}

void determine_offset(void)
{
    /* UNDEFINED is the first object, located at IMAGE+2,
     * and its value is itself.
     * So from its value we can tell where the heap was locaed when
     * the image was saved, and thus the offset from ROM addresses
     * to addresses in the objects.
     */

    UNDEFINED = IMAGE+2;
    fprintf(stderr, "UNDEFINED = %04X\n", UNDEFINED);
    fprintf(stderr, "saved with UNDEFINED at %04X\n", get_word(UNDEFINED+2));
    OFFSET = UNDEFINED - get_word(UNDEFINED+2);
    fprintf(stderr, "OFFSET = %04X\n", OFFSET);

    /* ATOP is stored at AREEXT (= IMAGE), and needs adjusting by OFFSET */
    ATOP = get_word(IMAGE) + OFFSET;
    fprintf(stderr, "ATOP = %04X\n", ATOP);
}

void print_addr(uint16_t addr, int relocate)
{
    if(is_NIL(addr))
	printf("    !word $%04X\n", addr);
    else if(!label[(uint16_t)(addr+OFFSET)])
    {
	if(relocate)
	    printf("    !word $%04X\n", addr+OFFSET);
	else
	    printf("    !word $%04X\n", addr);
    }
    else
    {
	if(relocate)
	    printf("    !word %s\n", label[(uint16_t)(addr+OFFSET)]);
	else
	    printf("    !word %s - $%04X\n", label[(uint16_t)(addr+OFFSET)], OFFSET);
    }
}

void print_objects(int relocate)
{
    uint16_t obj;

    for(obj=UNDEFINED; obj<ATOP; obj=next_object(obj))
    {
	uint8_t type = get_byte(obj);
	int len, value, i;
	
	printf("%s\n", label[obj] ? label[obj] : "");
	
	switch(get_byte(obj))
	{
	case CHARF:
	    len = get_byte(obj+1);
	    printf("    !byte CHARF, $%02X\n", len);
	    print_addr(get_word(obj+2), relocate);
	    print_addr(get_word(obj+4), relocate);
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
	    print_addr(get_word(obj+2), relocate);
	    print_addr(get_word(obj+4), relocate);
	    break;
	case FSUBRF:
	    printf("    !byte FSUBRF, $%02X\n", get_byte(obj+1));
	    print_addr(get_word(obj+2), relocate);
	    print_addr(get_word(obj+4), relocate);
	    break;
	case LISTF:
	    len = 5;
	    printf("    !byte LISTF\n");
	    print_addr(get_word(obj+1), relocate);
	    print_addr(get_word(obj+3), relocate);
	    break;
	default:
	    fprintf(stderr, "bogus object type %d at %04x\n", get_byte(obj), obj);
	    exit(1);
	}
    }
}    
    
int main(int argc, char **argv)
{
    size_t size;
    uint16_t ATOP, addr;
    int obj, len, nargs, i;
    uint8_t type;
    int relocate = 0, saved_image = 0;
    char *prog = argv[0];
    
    while(argc > 1)
    {
	if(strcmp(argv[1], "-r") == 0)
	{
	    relocate = 1;
	    argv++;
	    argc--;
	}
	else if(strcmp(argv[1], "-s") == 0)
	{
	    saved_image = 1;
	    argv++;
	    argc--;
	}
	else
	{
	    fprintf(stderr, "usage: %s [-r] [-s]\n", prog);
	    return 2;
	}
    }
    
    size = fread(image, 1, ROM_SIZE, stdin);
    if(!saved_image && size != ROM_SIZE)
    {
	fprintf(stderr, "didn't read %zu bytes from ROM\n", ROM_SIZE);
	return 1;
    }

    if(saved_image)
    {
	ROM_BASE = 0;
	IMAGE=0;
    }
    
    determine_offset();

    find_labels();

    printf("IMAGE\n");
    printf("AREEXT\n");
    if(relocate)
	printf("    !word ATOP\n");
    else
	printf(" !word ATOP - $%04X\n", OFFSET);
    printf("WSBOT\n");

    print_objects(relocate);

    printf("ATOP\n");
}
