;**********************************************************************
;*                                                                    *
;*                         FARPOINT SOFTWARE                          *
;*                         -----------------                          *
;*                     physmem.asm * version 1.11                     *
;*                                                                    *
;*                                                                    *
;*  A device driver whose purpose is to provide a means for an        *
;*  application to access physical memory addresses by calling        *
;*  PhysToUVirt for it.                                               *
;*                                                                    *
;*                                                                    *
;*  The calling sequence to this driver is as follows (Large Model):  *
;*                                                                    *
;*    ULONG ulPhys;                                                   *
;*    void *pvVirt;                                                   *
;*    USHORT action;                                                  *
;*    HFILE handle;                                                   *
;*                                                                    *
;*    /* ulPhys contains the physical address and pvVirt is the       *
;*        variable to receive the virtual address               */    *
;*                                                                    *
;*    DosOpen (&"PHYSMEM$", &handle, &action, 0L, 0, 1, 0x0040, 0L);  *
;*    DosDevIOCtl (&pvVirt, &ulPhys, 0x40, 0x80, handle);             *
;*                                                                    *
;*    /*                                                              *
;*        .                                                           *
;*        .                                                           *
;*        .                                                           *
;*       Use the memory via pvVirt                                    *
;*        .                                                           *
;*        .                                                           *
;*        .                                                           *
;*                   */                                               *
;*                                                                    *
;*    DosDevIOCtl (&pvVirt, &ulPhys, 0x41, 0x80, handle);             *
;*    DosClose (handle);                                              *
;*                                                                    *
;* ------------------------------------------------------------------ *
;*                                                                    *
;* Other functions:                                                   *
;*                                                                    *
;*    "DosDevIOCtl (&pvVirt, &ulPhys, 0x42, 0x80, handle);"           *
;*                                                                    *
;*      returns with pointers set to point to the Global              *
;*       Descriptor Table.                                            *
;*                                                                    *
;*                                                                    *
;*    "DosDevIOCtl (&pvVirt, &ulPhys, 0x43, 0x80, handle);"           *
;*                                                                    *
;*      returns with pointers set to point to the Interrupt           *
;*       Descriptor Table.                                            *
;*                                                                    *
;**********************************************************************

.286p
.SEQ

	extrn	DosWrite:far

;-------------------------------------------

ADATA	SEGMENT	PARA PUBLIC 'AUTO'

nexthdr		dd	0FFFFFFFFh        ;pointer to next device driver
devattr		dw	8140h             ;attribute flags
stratof		dw	offset strategy   ;offset of strategy routine entry
reserv1		dw	0
devname		db	'PHYSMEM$'        ;device name for "DosOpen"
reserv2		db	8 dup (0)

devhelp		dd	0         ;this is where we save the DevHelp pointer

gdt_phys	dw	3 dup (0)         ;scratch memory for GDT
idt_phys	dw	3 dup (0)         ;scratch memory for IDT

end_of_data	label	byte              ;the rest isn't needed after init

initmsg		db	0Dh,0Ah
		db	'Absolute Physical Memory Access Device Driver',0Dh,0Ah
		db	'Version 1.11 * Copyright (c) 1988, Farpoint Software',0Dh,0Ah
		db	0Dh,0Ah
initmsglen	equ	$-offset initmsg
byteswritten	dw	0

ADATA	ENDS

DGROUP	GROUP	ADATA

;-------------------------------------------

CODE	SEGMENT	PARA	'CODE'

	ASSUME	CS:CODE,DS:ADATA

strategy	proc	far

;examine command code in req packet

	mov	al,es:[bx+2]
	cmp	al,0                   ;"initialize" command
	je	initializej
	cmp	al,16                  ;"IOCtl" command
	je	generic_ioctl

;if none of the above, execute default stuff

	mov	word ptr es:[bx+3],0100h    ;set the "done" flag
	ret

initializej:	jmp	initialize

; * * * generic IOCtl command * * *

generic_ioctl:

	;check the function code:

	mov	al,es:[bx+14]
	cmp	al,40h                 ;translate physical to virtual
	je	acquire
	cmp	al,41h                 ;release virtual selector acquired
		                       ; previously with 40h, 42h, or 43h
	je	releasej
	cmp	al,42h                 ;get global descriptor table
	je	acquire_gdt
	cmp	al,43h                 ;get local descriptor table
	je	acquire_idt

	;return error if not recognized

	mov	word ptr es:[bx+3],0C10Ch
	ret

releasej:
	jmp	release

acquire:
	push	es
	push	bx
	push	ds
	lds	si,dword ptr es:[bx+15]    ;read physical address from
		                           ; the request packet
	mov	ax,[si+2]
	mov	bx,[si]
	pop	ds
	mov	cx,0
	mov	dh,0                   ;request type 0, get selector for
		                       ; readable / executable segment
	mov	dl,17h             ;index for PhysToUVirt
	call	devhelp
	jnc	acquire_good

	;error exit

	pop	bx
	pop	es
	mov	word ptr es:[bx+3],0C113h
	ret

	;no error, return virtual address

acquire_good:
	mov	ax,es
	mov	dx,bx
	pop	bx
	pop	es
	push	ds
	lds	si,dword ptr es:[bx+19]    ;put result into request packet
	mov	[si+2],ax
	mov	[si],dx
	pop	ds

	mov	word ptr es:[bx+3],0100h    ;set the "done" flag
	ret

acquire_gdt:

	sgdt	fword ptr gdt_phys     ;store GDT register - this instruction
		                       ; can only be executed in ring zero
	push	es
	les	si,dword ptr es:[bx+15]    ;put physical address into packet
	mov	ax,gdt_phys+2
	mov	es:[si],ax
	mov	ax,gdt_phys+4
	and	ax,00FFh
	mov	es:[si+2],ax
	pop	es

	jmp	acquire                ;proceed with translation to virtual

acquire_idt:

	sidt	fword ptr idt_phys     ;store IDT register - this instruction
		                       ; can only be executed in ring zero
	push	es
	les	si,dword ptr es:[bx+15]    ;put physical address into packet
	mov	ax,idt_phys+2
	mov	es:[si],ax
	mov	ax,idt_phys+4
	and	ax,00FFh
	mov	es:[si+2],ax
	pop	es

	jmp	acquire                ;proceed with translation to virtual

release:
	push	es
	push	bx
	push	ds
	lds	si,dword ptr es:[bx+19]    ;get virtual address to release
	mov	ax,[si+2]
	pop	ds
	mov	bx,0
	mov	cx,0
	mov	dh,2                   ;request type 2, free selector
	mov	dl,17h             ;index for PhysToUVirt
	call	devhelp
	jnc	release_good

	;error exit

	pop	bx
	pop	es
	mov	word ptr es:[bx+3],0C113h
	ret

	;no error

release_good:
	pop	bx
	pop	es
	mov	word ptr es:[bx+3],0100h    ;set the "done" flag
	ret

end_of_code	label	byte           ;code after this point is needed
			               ; only at initialization time

; * * * initialization command * * *

initialize:

	;save "DevHlp" call address

	mov	ax,es:[bx+14]
	mov	word ptr devhelp,ax
	mov	ax,es:[bx+16]
	mov	word ptr devhelp+2,ax

	;display message

	push	1
	push	ds
	push	offset initmsg
	push	initmsglen
	push	ds
	push	offset byteswritten
	call	DosWrite

	;set ending offsets

	mov	word ptr es:[bx+14],offset end_of_code
	mov	word ptr es:[bx+16],offset end_of_data

	;set other req packet fields

	mov	word ptr es:[bx+18],0
	mov	word ptr es:[bx+20],0

	;set status and exit

	mov	word ptr es:[bx+3],0100h    ;"done"
	ret

strategy	endp

CODE	ENDS

	end
