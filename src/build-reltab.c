/*
 * Build the relocation table for running Acornsoft Lisp at a high address
 * (usually D700) in the tube.
 *
 * First, assemble two versions, one at the usual location (low.bin) and
 * the other at a different location (high.bin).  This program will then
 * find the bytes that have changed and make a table either in binary ready
 * to be inserted at the appropriate location in the ROM, or in text
 * suitable to be included in the ACME assembler source.
 *
 * (C) Richard Tobin 2023
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define ROM_ADDR 0x8000

void usage(char *prog)
{
    fprintf(stderr, "usage: %s -a|-b -e end-addr -h tube-start low.bin high.bin\n", prog);
    exit(2);
}

int main(int argc, char **argv)
{
    char *prog = argv[0];
    int binary = -1;
    uint16_t end = 0, tube = 0;
    FILE *low, *high;
    uint16_t addr, offset;
    int a, b;

    while(argc > 1 && argv[1][0] == '-')
    {
	if(strcmp(argv[1], "-a") == 0)
	{
	    binary = 0;
	    argv++;
	    argc--;
	}
	else if(strcmp(argv[1], "-b") == 0)
	{
	    binary = 1;
	    argv++;
	    argc--;
	}
	else if(strcmp(argv[1], "-e") == 0)
	{
	    if(argc < 3)
		usage(prog);
	    end = strtoul(argv[2], 0, 16);
	    argv += 2;
	    argc -= 2;
	}
	else if(strcmp(argv[1], "-h") == 0)
	{
	    if(argc < 3)
		usage(prog);
	    tube = strtoul(argv[2], 0, 16);
	    argv += 2;
	    argc -= 2;
	}
	else
	    usage(prog);
    }

    if(argc != 3 || binary == -1 || end == 0 || tube == 0)
	usage(prog);
    
    offset = tube - ROM_ADDR;

    low = fopen(argv[1], "r");
    if(!low)
    {
	perror(argv[1]);
	return 1;
    }
    high = fopen(argv[2], "r");
    if(!high)
    {
	perror(argv[2]);
	return 1;
    }
		 
    for(addr=ROM_ADDR; addr<end; addr++)
    {
	a = getc(low);
	b = getc(high);
	if(a != b && a < (end >> 8))
	{
	    if(binary)
	    {
		putchar((addr+offset) & 0xff);
		putchar((addr+offset) >> 8);
	    }
	    else
		printf("    !word $%04x + $%04x\n", addr, tube - ROM_ADDR);
	}
    }

    if(binary)
    {
	putchar(0);
	putchar(0);
    }
    else
	printf("    !word 0\n");
}
