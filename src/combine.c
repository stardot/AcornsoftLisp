/*
 * Combine the parts of the ROM image: the assembled code (including the vector table
 * but not the in-assembler image), the image, and the tube relocation table.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define ROM_ADDR 0x8000
#define ROM_SIZE 0x4000
#define ROM_END (ROM_ADDR + ROM_SIZE)

void usage(char *prog)
{
    fprintf(stderr, "usage: %s -i image-addr -r reloc-table-addr tlisp image reloc-table\n", prog);
    exit(2);
}

int main(int argc, char **argv)
{
    char *prog = argv[0];
    FILE *tlisp, *image, *rtable;
    uint16_t imaddr = 0, rtaddr = 0;
    int nread, i;
    static uint8_t rom[ROM_SIZE];
    
    while(argc > 1 && argv[1][0] == '-')
    {
	if(strcmp(argv[1], "-i") == 0)
	{
	    if(argc < 3)
		usage(prog);
	    imaddr = strtoul(argv[2], 0, 16);
	    argv += 2;
	    argc -= 2;
	}
	else if(strcmp(argv[1], "-r") == 0)
	{
	    if(argc < 3)
		usage(prog);
	    rtaddr = strtoul(argv[2], 0, 16);
	    argv += 2;
	    argc -= 2;
	}
	else
	    usage(prog);
    }

    if(argc != 4 || imaddr == 0 || rtaddr == 0)
	usage(prog);

    /* read the code and sanity check */
    
    tlisp = fopen(argv[1], "r");
    if(!tlisp)
    {
	perror(argv[1]);
	return 1;
    }

    nread = fread(rom, 1, ROM_SIZE, tlisp);
    fprintf(stderr, "read 0x%04x bytes of tlisp\n", nread);
    if(nread < 0x1000 || nread > 0x3000)
    {
	fprintf(stderr, "tlisp size implausible: 0x%04x\n", nread);
	return 1;
    }

    if((rom[imaddr + 8 - ROM_ADDR] & 0xdf) != 'U')
    {
	fprintf(stderr, "UNDEFINED not at expected place in tlisp\n");
	return 1;
    }

    /* read the image and sanity check */
    
    image = fopen(argv[2], "r");
    if(!image)
    {
	perror(argv[2]);
	return 1;
    }

    nread = fread(rom + imaddr - ROM_ADDR, 1, ROM_END-imaddr, image);
    fprintf(stderr, "read 0x%04x bytes of image\n", nread);
    if(nread < 0x100 || nread > rtaddr-imaddr)
    {
	fprintf(stderr, "image size implausible: 0x%04x\n", nread);
	return 1;
    }

    if((rom[imaddr + 8 - ROM_ADDR] & 0xdf) != 'U')
    {
	fprintf(stderr, "UNDEFINED not at expected place in image\n");
	return 1;
    }

    /* fill the space between the image and the relocation table */

    for(i=imaddr+nread; i<rtaddr; i++)
	rom[i - ROM_ADDR] = 0;

    /* read the relocation table and sanity check */
    
    rtable = fopen(argv[3], "r");
    if(!image)
    {
	perror(argv[3]);
	return 1;
    }

    nread = fread(rom + rtaddr - ROM_ADDR, 1, ROM_END-rtaddr, rtable);
    fprintf(stderr, "read 0x%04x bytes of relocation table\n", nread);
    if(nread < 0x100 || nread > 0x700)
    {
	fprintf(stderr, "tube relocation table size implausible: 0x%04x\n", nread);
	return 1;
    }

    /* fill the space between the relocation table and the end of the ROM */

    for(i=rtaddr+nread; i<ROM_SIZE; i++)
	rom[i - ROM_ADDR] = 0;

    /* write out the ROM */

    fwrite(rom, 1, ROM_SIZE, stdout);
}
