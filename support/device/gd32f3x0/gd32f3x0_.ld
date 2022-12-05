STACK_SIZE = DEFINED(STACK_SIZE) ? STACK_SIZE : 0x0400;

ENTRY(Reset_Handler)

MEMORY
{
	RAM (xrw)       : ORIGIN = 0x20000000, LENGTH = 8K
	FLASH (rx)      : ORIGIN = 0x8000000, LENGTH = 64K
}

SECTIONS
{
	.text :
	{
		KEEP(*(.vectors))
		*(.text*)
		*(.rodata*)
		. = ALIGN(4);
	} >FLASH
	.data :
	{
		_sdata = .;
		*(.data*)
		. = ALIGN(4);
		_edata = .;
	} >RAM AT >FLASH
	_rdata = LOADADDR(.data);
	/*
	.dflash : 
	{
		*(.dflash*)
		. = ALIGN(4);
	} >DFLASH
	*/
	.bss (NOLOAD) :
	{
		_sbss = .;
		*(.bss*)
		*(COMMON)
		. = ALIGN(4);
		_ebss = .;
	} >RAM
	.stack (NOLOAD) :
	{
		. = ALIGN(8);
		. = . + STACK_SIZE;
		. = ALIGN(8);
		_estack = .;
	} >RAM
	_eheap = ORIGIN(RAM) + LENGTH(RAM);
}
