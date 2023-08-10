/*
 * Build the relocation table for running Acorsoft Lisp at D700 in the tube.
 *
 * First, assemble to versions, one at the usual location (low.bin) and
 * the other at a different location (high.bin).  This program will then
 * find the bytes that have changed and make a table suitable to be
 * included in the (ACME) assembler program.
 *
 * (C) Richard Tobin 2023
 */

#include <stdio.h>

int main(void)
{
    FILE *low = fopen("low.bin", "r");
    FILE *high = fopen("high.bin", "r");
    int a, b;
    int addr = 0x8000;
    
    while(addr <= 0xa400)	/* this shouldn't be built-in */
    {
	a = getc(low);
	b = getc(high);
	if(a != b && a < 0xa4)
	    printf("  !word MOVE_OFFSET + $%04x\n", addr);
	addr++;
    }

    printf("    !word 0\n");
}
