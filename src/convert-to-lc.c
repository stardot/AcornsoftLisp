#include <stdio.h>
#include <stdlib.h>

#define ROM_BASE 0x8000
#define ROM_SIZE ((size_t)0x4000)
#define IMAGE 0xa600

enum type {CHARF=0x00, NUMF=0x04, SUBRF=0x08, FSUBRF=0x0c, LISTF=0x80};

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

int main(int argc, char **argv)
{
    size_t size;
    uint16_t ATOP;
    int obj, len, i, nsymbols=0, found_nil=0;
    uint8_t type;
    
    size = fread(image, 1, ROM_SIZE, stdin);
    if(size != ROM_SIZE)
    {
	fprintf(stderr, "didn't read %zu bytes from ROM\n", ROM_SIZE);
	return 1;
    }

    /* IMAGE+5 tells us where the heap was located when the end
       of it (stored at IMAGE) was calculated */
    ATOP = IMAGE + get_word(IMAGE) - (get_byte(IMAGE+5) << 8);

    for(obj = IMAGE+2; obj < ATOP; obj += len)
    {
	type = get_byte(obj);
	switch(type)
	{
	case CHARF:
	    nsymbols++;
	    len = get_byte(obj+1);
	    for(i=6; i<len; i++)
	    {
		int c = get_byte(obj+i);
		if(c >= 'A' && c <= 'Z')
		    c += ('a' - 'A');
		set_byte(obj+i, c);
	    }
	    break;
	case NUMF:
	    len = 4;
	    break;
	case SUBRF:
	case FSUBRF:
	    len = 6;
	    break;
	case LISTF:
	    len = 5;
	    break;
	default:
	    fprintf(stderr, "bad object type %u at %04x\n", type, obj);
	    return 1;
	}
    }

    /* Look for NIL in the code */

    for(i=ROM_BASE; i<IMAGE; i++)
    {
	if(get_byte(i) == 'N' &&
	   get_byte(i+1) == 'I' &&
	   get_byte(i+2) == 'L'+128)
	{
	    fprintf(stderr, "found NIL at %04x\n", i);
	    if(found_nil)
		fprintf(stderr, "warning, found NIL more than once!\n");
	    set_byte(i, 'n');
	    set_byte(i+1, 'i');
	    set_byte(i+2, 'l'+128);
	    found_nil = 1;
	}
    }

    if(!found_nil)
	fprintf(stderr, "warning, didn't find NIL\n");
		
    size = fwrite(image, 1, ROM_SIZE, stdout);
    if(size != ROM_SIZE)
    {
	fprintf(stderr, "didn't write %zu bytes to new ROM\n", ROM_SIZE);
	return 1;
    }

    fprintf(stderr, "converted %d symbols\n", nsymbols);
}

	   
