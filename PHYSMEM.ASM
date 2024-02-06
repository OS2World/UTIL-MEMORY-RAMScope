;**********************************************************************
;*                                                                    *
;*                         FARPOINT SOFTWARE                          *
;*                  modified to work under 32-bit OS/2                *
;*                         -----------------                          *
;*                     physmem.asm * version 1.12                     *
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

.686p

	extrn	DosWrite:far16

;-------------------------------------------

ADATA	SEGMENT	PARA PUBLIC USE16 'DATA'
PUBLIC nexthdr
PUBLIC gdt_phys
PUBLIC idt_phys

nexthdr		dd	0FFFFFFFFh        ;pointer to next device driver
devattr		dw	8140h             ;attribute flags
stratof		dw	offset strategy   ;offset of strategy routine entry
reserv1		dw	0
devname		db	'PHYSMEM$'        ;device name for "DosOpen"
reserv2		db	8 dup (0)

devhelp		dd	0         ;this is where we save the DevHelp pointer

; align memory location for "sgdt" as suggested in Intel SW Developer manual
ALIGN 4
dummy1    dw  1 dup (0)
gdt_phys	dw	3 dup (0)         ;scratch memory for GDT
; align memory location for "sidt" as suggested in Intel SW Developer manual
ALIGN 4
dummy2    dw  1 dup (0)
idt_phys	dw	3 dup (0)         ;scratch memory for IDT
; a pagelist with one pagerange entry: 1 DWORD of physical base addr, 1 DWORD for pagerange length
pagelist  dd  2 dup (0)

end_of_data	label	byte              ;the rest isn't needed after init

initmsg		db	0Dh,0Ah
		db	'Absolute Physical Memory Access Device Driver',0Dh,0Ah
		db	'Version 1.12 * Copyright (c) 1988, Farpoint Software',0Dh,0Ah
    db  'modified to work under 32-bit OS/2',0Dh,0Ah
		db	0Dh,0Ah
initmsglen	equ	$-offset initmsg
byteswritten	dw	0

ADATA	ENDS

DGROUP	GROUP	ADATA

;-------------------------------------------

CODE	SEGMENT	PARA PUBLIC USE16	'CODE'

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
  ; PhysToUVirt under 32-bit OS/2 does not seem to handle well
  ; a length of 0 (meaning: 65536 bytes)
  ; we therefore map this maximum minus one page length: 65536-4096 = 61440 bytes
  ; for the case where the requested physical address does not align on a page boundary
  mov cx,61440
	je	acquire
	cmp	al,41h                 ;release virtual selector acquired
		                         ; previously with 40h, 42h, or 43h
	je	releasej
	cmp	al,42h                 ;get global descriptor table
	je	acquire_gdt
	cmp	al,43h                 ;get interrupt descriptor table
	je	acquire_idt

	;return error if not recognized

	mov	word ptr es:[bx+3],0C10Ch
	ret

releasej:
	jmp	release

acquire:
	push	es
	push	bx
  push  si
	push	ds
	lds	si,dword ptr es:[bx+15]    ;read physical address from
		                             ; the request packet
	mov	ax,[si+2]
	mov	bx,[si]
	pop	ds
	mov	dh,0                        ;request type 0, get selector for
		                              ; readable / executable segment
	mov	dl,17h                      ;index for PhysToUVirt
	call	devhelp
	jnc	acquire_good

	;error exit

  pop si
	pop	bx
	pop	es
	mov	word ptr es:[bx+3],0C113h
	ret

	;no error, return virtual address

acquire_good:
	mov	ax,es
	mov	dx,bx
  pop si
	pop	bx
	pop	es

  push  si
	push	ds
	lds	si,dword ptr es:[bx+19]    ;put result into request packet
	mov	[si+2],ax
	mov	[si],dx
	pop	ds
  pop si

	mov	word ptr es:[bx+3],0100h    ;set the "done" flag
	ret

acquire_gdt:

  db 66h
	sgdt	fword ptr gdt_phys     ;store GDT register - this instruction
		                           ; can only be executed in ring zero

  ; OS/2 32-bit uses paging:
  ; determine the physical address from the linear address
  ; subsequently, we only map 256 bytes
  ; (that's what the main executable displays in the window)
  ; and therefore one pagerange entry in the pagelist is sufficient
  ; be aware that the GDT might be physically NON-CONTIGUOUS in memory
  ; which might lead to wrong conclusions when viewing it in the
  ; application
	push	es
  push ebx
  push esi
  push edi
  mov   ax,SEG pagelist
  lea   esi,pagelist
  mov   dl,5Bh
  call devhelp                  ; VirtToLin
  mov  edi,eax
  mov   eax,dword ptr [gdt_phys+2]
  mov  ecx,256
  push ecx
  mov  dl,5Eh                   ; LinToPageList
  call devhelp
  pop  ecx

	les	si,dword ptr es:[bx+15]    ;put physical address into packet
  mov eax,dword ptr [pagelist]
  mov dword ptr es:[si],eax

  pop edi
  pop esi
  pop ebx
	pop	es

	jmp	acquire                ;proceed with translation to virtual

acquire_idt:

  db 66h
	sidt	fword ptr idt_phys     ;store IDT register - this instruction
		                       ; can only be executed in ring zero

  ; OS/2 32-bit uses paging:
  ; determine the physical address from the linear address
  ; subsequently, we only map 256 bytes
  ; (that's what the main executable displays in the window)
  ; and therefore one pagerange entry in the pagelist is sufficient
  ; be aware that the IDT might be physically NON-CONTIGUOUS in memory
  ; which might lead to wrong conclusions when viewing it in the
  ; application
	push	es
  push ebx
  push esi
  push edi
  mov   ax,SEG pagelist
  lea   esi,pagelist
  mov   dl,5Bh
  call devhelp                  ; VirtToLin
  mov  edi,eax
  mov  eax,dword ptr [idt_phys+2]
  mov  ecx,256
  push ecx
  mov  dl,5Eh                   ; LinToPageList
  call devhelp
  pop  ecx

	les	si,dword ptr es:[bx+15]    ;put physical address into packet
  mov eax,dword ptr [pagelist]
  mov dword ptr es:[si],eax

  pop edi
  pop esi
  pop ebx
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

	mov	word ptr es:[bx+14],offset end_of_code-1
	mov	word ptr es:[bx+16],offset end_of_data-1

	;set other req packet fields

	mov	word ptr es:[bx+18],0
	mov	word ptr es:[bx+20],0

	;set status and exit

	mov	word ptr es:[bx+3],0100h    ;"done"
	ret

strategy	endp

CODE	ENDS

	end
