;;;
;;; PLL: Partition Linux Loader
;;;
;;; (c) 2022-2025 Scott Jennings - All rights reserved as per GPLv3+:
;;;

	%define VERSION	'0.1.0'

;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;;; 
;;;  Some code adapted from Dr Gareth Owen (www.ghowen.me) and Sebastian Plotz.
;;;  Rewritten, for desktop and embedded use

;;; Expects a valid GUID Partition Table (GPT), with a valid linux
;;; kernel image in each of up to the first four partitions (no filesystems)
;;;
;;; A kernel command line is stored in LBA0, and is preloaded
;;; with 'PLL=v.v.v-xx.n', where 'v' are version numbers, and 'xx' is replaced
;;; at boot time with the BIOS number of the boot device. This command
;;; line option must exist and must be first.
;;;
;;; One of three possible boot scenarios are selected by the '.' located
;;; between the 'xx' and the 'n' in the 'PLL=' command line option.
;;; These three cases are '.', '+', and anything else:
;;;
;;;   In the case of '.':
;;; 
;;; At boot time, state of the keyboard shift keys determine which partition
;;; the kernel will be loaded from:
;;; 	no shift keys are down: partition 1 is selected.
;;; 	a 'shift' key is down:  partition 2 is selected.
;;; 	a 'ctrl' key is down:	partition 3 is selected.
;;; 	an 'alt' key is down:	partition 4 is selected.
;;; for each of these cases, the 'n' in the 'PLL=' option will be replaced
;;; with the partition the kernel was actualled loaded from.
;;;
;;;   In the case of '+':
;;;
;;; The kernel will be loaded from the partion number following the '+'
;;; (at the 'n' position) in the command option..
;;;
;;;   In all other cases:
;;;
;;; The partition number following the character (which replaced the
;;; '.')  will be 'xor'ed with 0b00000011 ('1' xor '2') so that a '1'
;;; will become a '2', and vice versa. The kernel will be loaded from
;;; the resulting partition number. Then, the character in the '.'
;;; position, will be overwritten with '+' on disk in LBA0.
;;; (the number following the '+' is unchanged on disk, or the
;;; command line)
;;;
;;; This is effectively a 'one time boot'. If the command line is not
;;; subsequently changed, the next boot will revert to the previous
;;; partition number. Applications can look at the PLL option in the
;;; command line in /proc/cmdline to check this character and determine
;;; if we have booted in a 'one time boot' condition.
;;;
;;;
;;; Loading of an external initmpfs is *not* supported.
;;; When needed, the initmpfs should be compiled into the kernel.
;;;
;;; 
;;; This code assembles using NASM.
;;;
;;; FYI: search for "doggy" to find potentially unresolved issues.
;;; 	 search for "dog" to find potential issues.
;;; 
;;; Memory Usage/Map:
;;; 0x?????? - 0x07bff	stack
;;; 0x007c00 - 0x007dff	This is where the BIOS loads LBA0
;;; 		the following two lines presume a max 8 partition GPT:
;;; 		2LBAs hold the MBR(LBA0)+GPT, 2LBAs hold the 8partition tables
;;; 		2LBAs hold additional PLL code (bank-two second stage)
;;; 0x007e00 - 0x008dff PLL loads the first 8 LBAs(4k) of the boot drive here
;;;   0x8600 - 0x8dff  (1K of second stage PLL code space)
	
;;; 		the following two lines presume a max 128 partition GPT:
;;; 		2LBAs hold the MBR+GPT, 32LBAs hold the 128partition tables
;;; 		2LBAs hold additional PLL code (bank-two second stage)
;;; 0x007e00 - 0x00cbff PLL loads the first 38 LBAs(19k) of the boot drive here
;;;   0xc400 - 0xcbff  (1K of second stage PLL code space)
	
;;; 0x010000 - 0x017fff	PLL loads the Real mode kernel header here (32Kb)
;;; 0x018000 - 0x01dfff	kernel heap				; size: 24Kb
;;; 0x01e000 - 0x01ffff	command line copied to here from LBA0	; size: 8Kb
;;; 0x020000 - 0x02ffff	disk read buffer for high moves		; size: 64Kb

;;; 0x100000 - +kernlen	protected-mode kernel

	;; progress reporting:
	%define OURNAME1	'P'	; the boot loader has started
	%define OURNAME2	'L'	; A20 and UNREAL achieved
	%define OURNAME3	'L'	; GPT loaded
	%define PART1		'1'	; booting kernel in partition 1
	%define PART2		'2'	; booting kernel in partition 2
	%define PART3		'3'	; booting kernel in partition 3
	%define PART4		'4'	; booting kernel in partition 4
	%define OURNAME4	':'	; kernel realmode code loaded
	;; then the kernel version is reported (if enabled)
	;; followed by one '.' for each group of logical block groups loaded
	%define	KLAUNCH		'*' ;report kernel real mode code being executed
	;; these are all fatal errors:
	%define	A20ERR		'A' ;failed to enable A20
	%define WRITEERR	'W' ;failed to write boot sector (ONETIMEBOOT)
	%define	HDDERR		'R' ;failed to read from disk via int13 ah=0x42
	%define	HDDERRNOEXT	'r' ;failed to read from disk via int13 ah=0x02
	%define GPTSIGERR	'G' ;GPT signature not found
	%define	KSIGERR		'K' ;kernel signature not found
	%define	KPROTOERR	'P' ;kernel protocol version incompatible
	%define	KLOADHIERR	'H' ;kernel not to be loaded hi

 	%define SECTORSPERGROUP	127 ;max sectors per BIOS read call

	%define ORIGIN	0x7c00	;where the BIOS loads us to
	%define	STACK	ORIGIN	;also where we put the stack pointer
	%define	PPT	446	;offset into the MBR of the Protective PartTable
	%define MBREND	PPT	;THIS IS OUR COMPLETE CODESPACE IN THE MBR

;;; these are passed upon nasm invocation (by PLLinstall)
;;; if maximum 8 partitions in GPT:
;;; 	%define	BANKTWOSTART	4 ; bank two starts at LBA4
;;; 	%define BANKTWOSIZE	2 ; length of bank two in logical blocks
;;; if maximum 128 partitions in GPT:
;;; 	%define	BANKTWOSTART	34 ; bank two starts at LBA34
;;; 	%define BANKTWOSIZE	2 ; length of bank two in logical blocks
	
;;; ============================================================
	
;;; This is the beginning of the MBR code
;;; This is offset 0 in LBA0 of the boot disk.
;;; The BIOS loads it to 0x0000:0x7c00 (ORIGIN)

	[BITS 16]
	[map all PLL.map]

	org	ORIGIN

	xor	ax, ax
	mov	ss, ax
	mov	sp, STACK	; setup stack (ORIGIN)
	mov	ds, ax
;;; 	mov	es, ax	  	;pointless as check_a20 will crash it
	cld
;;; save the boot device number passed from the BIOS in dl
 	push	dx	; we read it in the stack at byte 0:0x7bfe (svd3byts)

	;; check if the BIOS supports 'extended' calls.
	mov	word bx, 0x55aa
;;;	mov	dl, [boot_drive] ;already in dl from bios
	mov	ah, 0x41 	;see if extentions available
	int	0x13
	jc	short no_ext	; carry on return = no extentions

	xor	cx, cx
	cmp	bx, 0xaa55	; bx = 0xaa55 = extentions available
	je	short yes_ext

no_ext:
	;;; Read Drive Parameters (ah=0x08) to get the sectors per heads per cylinder
	mov	ah, 0x08
	int	0x13
	and	cl, 0x3f	;[secs_per_head]
 	inc	dh		;[hds_per_cyl]
	mov	ch, dh

yes_ext:
	push	cx		;save secs_per_head/hds_per_cyl to stack

	mov	al, OURNAME1	; the boot loader has started, reading GPT
	call	print_al

;;; load the MBR(LBA0), GPT{LBA1-?}, and our second bank of code space
;;; which is a minimum 2 sectors between the GPT and the first partition
;;; note that disk access table was preloaded by the assembler for this
	call	disk_read	; load GPT and second bank of code into 0:MBR

	mov	al, GPTSIGERR
 	cmp	dword [GPT_sig], 'EFI '	;GPT signature
	jne	short err_al
	cmp	dword [GPT_sig+4], 'PART' ;GPT signature
	jne	short err_al

	mov	al, OURNAME2	;GPT loaded, jumping into second bank of code
	call	print_al

	jmp	second_bank
;;;	*****

;;;------------------------------------------------
;;; error handling
;;;------------------------------------------------

err_al:
	call	print_al
	mov	al, '!'
	call	print_al

err:	jmp	short $		;hang forever (ints active)
;;	---------------

;;;---------------------------------------------------------
;;;	read sectors via extended BIOS call ah=0x42 int0x13
;;; 	[xferLBA]    = LBA to start reading from (48bits)
;;; 	[xferbuffer] = location in ram to read to (32bits)
;;; 	[xferblocks] = number of logical blocks to read (16bits)
;;;---------------------------------------------------------

;;; typically invoked thus:
;;; mov	dword [xferbuffer], 0x10000000	;where to put the data
;;; mov	word [xferblocks], 64		;blocks/sectors to read
;;; call disk_read

disk_read:
	;; if no BIOS extentions available, use the other read routine
	cmp	byte [secs_per_head], 0
	jne	short read_no_ext

	;; DS:SI points to da_pack
	mov	si, da_pack
	mov	ah, 0x42
	mov	dl, [boot_drive]
 	push	word [xferblocks]
	int	0x13
 	pop	bx

	mov	al, HDDERR
	jc	short err_al

	ret
;;; 	---

;;;-------------------------------------------------------------------
;;;
;;; READ UP TO 128 LOGICAL BLOCKS USING BIOS INT13 AH=0x02 Cyl/Hds/Sec
;;;
;;; on call to INT13:
;;;  AH=02, AL=sectors to read, CH=low 8 bits of cylinder number,
;;;  CL = sector number 1-63 (bits 0-5) high two bits of cylinder (bits 6-7)
;;;  DH = head number, DL = drive number (bit 7 set for hard disk),
;;;  ES:BX -> data buffer
;;;
;;; on return:  CF set on error
;;;
;;; systems without BIOS extentions are very old, and usually use
;;; much smaller storage devices:
;;; 
;;; with a 16bit LBA address, we get to the first 33.5MB of HDD
;;; ( (2**16)*512 = 33554432 ) which should be enough for two kernels,
;;; one in each of the first two partitions
;;; 
;;; (might not be too hard to extend this to 32bit LBA if needed)
;;; (right now, reads beyond 33.5MB will wrap around)
;;;-------------------------------------------------------------------

read_no_ext:
	mov	ax, [xferLBA]
	div	byte [secs_per_head]
	xor	cx, cx
	mov	cl, ah
 	inc	cl		;sectors start with 1
	xor	ah, ah
	div	byte [hds_per_cyl]
	mov	dh, ah		;head
	mov	ch, al		;cyl

;;; now ch=cyl, dh=head, cl=sector (bits 6+7 are 0)

	mov	bx, [xferbuffer]
	mov	es, [xferbuffer+2]
	mov	dl, [boot_drive]
	mov	al, [xferblocks]
	mov	ah, 0x02	;read drive sectors command
	int	0x13
	mov	al, HDDERRNOEXT
	jc	short err_al

	ret
;;;	---

;;;-----------------------------------------
;;; 		Print AL as one character
;;;-----------------------------------------

print_al:
	mov	ah, 0xe		; 2
	mov	bx, 7		;+3
	int	0x10		;+2
	ret			;+1=8
;;;	---
	

;;;-------------------------------------------------
;;;		start of initialized data in LBA0
;;;-------------------------------------------------

;;;--------------------------------------------------------------
;;; 		Disk Access Packet   for int 13h ah=42h disk reads
;;;--------------------------------------------------------------

;;; ( pre-loaded to read all sectors up to the second code bank end )
da_pack:	db	da_pack_end-$	;(constant) length of this packet
		db	0	;(constant)
;;; 	number of blocks to transfer max 127 on some BIOS
xferblocks:	dw	(BANKTWOSTART+BANKTWOSIZE)
xferbuffer:	dw	MBR	;offset
		dw	0	;segment
xferLBA:	dd	0	;48bit LBA
		dd	0
da_pack_end:

;;; this is the start of the command line handed to the kernel by PLL
cmdline:
	db	"PLL="
	db	VERSION
	db	"-"
cmdlinedev:
	db	"xx"		;BIOS disk number we booted from (hex)
;;; if cmdlineonce is '.', the boot partition is decided by shift keys:
;;; default=part1 shift=part2 ctrl=part3 alt=part4
;;; 
;;; If cmdlineonce is '+', the boot partition number (in ascii) is the
;;; value at cmdlinepart.
;;; 
;;; If cmdlineonce is anything other than '.' or '+', then it will be
;;; changed to '+' and written back to disk, then the boot partion will
;;; the value at cmdlinepart xored with 0x00000011 ( ascii '1' xor '2' ).
;;; The value of cmdlinepart on disk is not changed.
;;; The effect is a one time boot of the "other" of partions 1 and 2
;;; note: if cmdlineonce is not '.' or '+', and cmdlinepart is not
;;; '1' or '2', the results may not be what you want.
	
cmdlineonce:
	db	"."
;;; this is the partition number to load the boot kernel from (in ascii)
cmdlinepart:
	db	"1 "		;partiton number we loaded the kernel from
;;; this is the start of the user supplied command line
commandline:
	db	"", 0
;;; "quiet loglevel=0 tsx_async_abort=full,nosmt mds=full,nosmt l1tf=full,force nosmt=force kvm.nx_huge_pages=force ipv6.disable=1"
;;; 	times	256-($-commandline)	db	0

	%assign	cmdlinelen	MBREND-(commandline-$$)
	%assign cmdlineaddr	commandline-$$
	%assign cmdlineend	MBREND
	%warning cmdlinelen bytes available for command line at cmdlineaddr to cmdlineend
	
;;; ---------------------------------------------------
;;;
;;; Assuming that the object code is copied to the boot disk using
;;; "dd if=PLL of=/dev/sd?", and that said disk was previously
;;; partitioned as GPT, then a "protective partition table"
;;; and the 0xaa55 signature are already there. Therefore, we
;;; stop at MBREND to avoid overwriting the "PPT"
;;;
;;; We load MBR+GPT+SecondBank sectors to this addres in segment 0x0000
	times	(0x200)-($-$$)		db	0
MBR:
	times	(BANKTWOSTART*0x200)		db	0 ; bump to second bank

;;;=====================================================
;;;	HERE IS THE START OF THE SECOND CODE BANK 
;;;=====================================================
BANKTWO:
;;; this is where we store the first LBA of
;;; the partition we will load the kernel from
boot_FLBA:	dd	0

second_bank:
;;; -------------------------------------------------------------------
;;; The kernel is large, and must be loaded high so we must enable the
;;; high address bit: A20, and enter protected mode to get access to
;;; the upper memory address space.
;;; 
;;; Some systems start with A20 already enabled, so we can check that
;;; and avoid enabling code which could be problematic.
;;; 
;;; There are multiple methods of enabling A20 and each have their
;;; issues. Some recommend trying all of them in order of increasing
;;; risk, and checking for success as you go. There can be delays on
;;; some systems before A20 is actually enabled.
;;; -------------------------------------------------------------------

;;; NOTE: CALLS TO check_a20 CRASH ES!

	call	check_a20_no_delay
	jne	short a20_is_enabled	;A20 was already enabled, good to go.

;;; first try telling BIOS to enable A20

	mov	ax, 0x2401 ; A20 line enable via BIOS
	int	0x15
	call	check_a20_no_delay
	jne	short a20_is_enabled

;;; the "fast A20 gate" method (io data at 0x92)
;;; quick and dirty version
;;; note: bit 0 of 0x92 is RESET, no touchie
 	in	al, 0x92
	or	al, 2
	out	0x92, al	;Enable A20
	call	check_a20_delay
	jne	short a20_is_enabled

;;; this method rarely works
;;; the "0xee" method (read/write strobe to 0xee) (al content irrelevent)
;;;	out	0xef, al		;RESET (for information only, DON'T)
	in	al, 0xee		;Enable A20
;;;	out	0xee, al		;Disable A20 (for information only)
	call	check_a20_delay
	jne	short a20_is_enabled

;;; the A20 by keyboard method can hang on systems without a keyboard controller
	cli
	mov	ah, 0xAD	;disable keyboard
	call	a20kbout
	mov	ah, 0xD0	;read from input
	call	a20kbout
.wait:
	in	al, 0x64
	test	al, 1
	jz	short .wait

	in	al, 0x60
	push	ax
	mov	ah, 0xD1	;write to output
	call	a20kbout
	pop	ax
	or	al, 2
	out	0x60, al	;enable A20

	mov	ah, 0xAE	;enable keyboard
	call	a20kbout
	sti

	call	check_a20_delay
	je	err_al

;;;---------------------------------------------------------------------
;;;	A20 IS ENABLED		ds=0x0000 es=0 *if* no check_a20 calls
;;;---------------------------------------------------------------------
a20_is_enabled:
;;;	check_a20 crashed es, use ds override
	cli
	lgdt	[ds:gdt_desc]	;load the global/interupt descriptor table

	mov	eax, cr0
	or	al, 00000001b	; set protected mode bit
	mov	cr0, eax	; enter protected mode
	jmp	short $+2

	mov	bx, 0x8 	; first descriptor in GDT
	mov	ds, bx
	mov	es, bx
	mov	gs, bx

	and	al, 11111110b	; clear protected mode bit
	mov	cr0, eax	; back to real mode

;;; 	- now limits are removed but seg regs still work as normal

	xor	ax, ax 		; restore segment values
 	mov	ds, ax
	mov	gs, ax
	mov	es, ax		; segment to load MBR and GPT into
	sti

;;;---------------------------------------------------
;;;	now in UNREAL MODE		ds=es=0x0000
;;;---------------------------------------------------

	mov	al, OURNAME3	; A20 and UNREAL achieved
	call	print_al

;;; record the boot device in the cmdline for posterity
	mov	ah, [boot_drive]
	mov	al, ah
	shr	al, 4
	and	ax, 0000111100001111b
	or	ax, 0x3030
	cmp	al, '9'
	jng	short bdhi

	add	al, 'a' - '9' - 1

bdhi:	cmp	ah, '9'
	jng	short bdlo

	add	ah, 'a' - '9' - 1

bdlo:	mov	[cmdlinedev], ax

;;; determine which partion to load the kernel from
	mov	si, cmdlinepart
	mov	al, [si]	;partition to boot from, in ascii
	dec	si
	mov	ah, [si]	;one-time flag ( '.' or '+', or once-time-boot )
	cmp	ah, '.'
 	je	short shift_boot

	cmp 	ah, '+'
	je	short normal_boot

;;; this is a "one time boot".

	push	ax	       ;save boot partition in al
	
	mov	byte [cmdlineonce+0x200], '+' ;clear the one-time flag
;;; 	... and write MBR sector back to disk

;;; we only every write to LBA0, so just use int13 ah=03 (not 'extended' call)
;;; note that at present, failure to write the change back to disk will not
;;; stop the kernel from being run, but 'one time boot' will become forever,
;;; since the MBR (LBA0) will be unchanged.  The error is reported, if you're
;;; fast enough to see it before the kernel blanks the screen.

;;; to make the error 'hard', change the 'jnc .write_ok' to 'jc err_al', and
;;; remove the 'call print_al'

	mov	bx, MBR		; offset
	xor	cx, cx		;0 sec/trk
	mov	dx, cx		;0 hd
	inc	cl		;first sector is 1
	mov	dl, [boot_drive]
	mov	ax, 0x0301	;write one sector
	int	0x13		;write the modified MBR sector back
	mov	al, WRITEERR
 	jnc	.write_ok
;;;	jc	err_al

	call	print_al	;dog this error is not fatal, should it be?

.write_ok:
	pop	ax		;restore boot partition

	xor	al, '1' ^ '2'	;boot the "other" partition this once

;;; normal boot, boots the partition stored in cmdlinepart
	
normal_boot:
	and	al, 0111b
	dec	al
	jz	short bootpart1

	dec	al
	jz	short bootpart2

	dec	al
	jz	short bootpart3

	jmp	short bootpart4
;;;	-------------

;;; shift boot selects the boot partition based on shift keys held down
shift_boot:
;;;-----------------------------------------------------------
;;; so, if a "shift" key is down, it selects the boot partition:
;;; L or R SHIFT boots partition two.
;;; L or R CTRL boots partition three.
;;; L or R ALT boots partition four.
;;; (else partition one)
;;; ----------------------------------------------------------
	mov	ah, 0x12
	int	0x16
	push	ax
	mov	ah, 0x01	;dog sort of delay maybe
	int	0x16
	mov	ah, 0x12
	int	0x16
	pop	bx
	or	ax, bx

	rcr	ah, 1
	jc	short ctrl_down
	
	rcr	ah, 1
	jc	short alt_down

	rcr	ah, 1
	jc	short ctrl_down

	rcr	ah, 1
	jc 	short alt_down

	rcr	al, 1
	jc	short shift_down

	rcr	al, 1
	jc	short shift_down

	rcr	al, 1
	jc	short ctrl_down

	rcr	al, 1
	jc	short alt_down

bootpart1:
	mov	al, PART1
	mov	ebx, [part1_FLBA]
	jmp	short bootpart
;;;	---

shift_down:
bootpart2:
	mov	al, PART2
	mov	ebx, [part2_FLBA]
	jmp	short bootpart
;;;	---
	
ctrl_down:
bootpart3:
	mov	al, PART3
	mov	ebx, [part3_FLBA]
	jmp	short bootpart
;;;	---

alt_down:
bootpart4:
	mov	al, PART4
	mov	ebx, [part4_FLBA]

bootpart:
	mov	[boot_FLBA], ebx
	mov	[xferLBA], ebx
	mov	[cmdlinepart], al	;record the boot partion in the cmdline
	call	print_al		;and announce it

;;; load the kernel header and real mode code to 0x1000:0
	push	0x1000
	pop	es		; es=0x1000 ds=0x0000

	mov	dword [xferbuffer], 0x10000000	;where to put the kernel header 0x1000:0
	mov	word [xferblocks], 64		;maximum kernel header size of 32K (64 sectors)
	call	disk_read

	mov	al, OURNAME4	; kernel realmode code loaded
	call	print_al

	push	ds		;0x0000
	push	es		;0x1000
	pop	ds		; ds = es = 0x1000, 0x0000 on stack

	cmp	dword [0x202], 'HdrS' ;signature
	mov	al, KSIGERR
	jne	err_al

;;; 	we have found the kernel real mode header signature

	cmp	word [0x206], 0x204	; kernel boot protocol version
	mov	al, KPROTOERR
	jb	err_al			; must be protocol v2.04 or greater
	
	mov	al, KLOADHIERR
	test	byte [0x211], 1 	; loadflags: boot protocol option flags
 	jz	err_al			; error if not loaded high: 0x100000

	mov	si, [0x20e]	; pointer to kernel version string
	add	si, 0x200	; skip over the kernel boot sector
	call	print_string	;print the kernel version string
;;;
;;; 	obligatory kernel setup ( ds = es = 0x1000, and 0x0000 on stack )
;;;
;;; 	mov	word [0x1fa], 0xffff	;modify obligatory vid mode? doggy
	
	;; set kernel quiet mode?
;;;	mov	word [0x210], 0x81e1	; set LOADHI+USEHEAP+type_of_loader
	mov	word [0x210], 0x81ff	; set LOADHI+USEHEAP+type_of_loader
;;; 	mov	word [0x210], 0xa1e1	; set LOADHI+USEHEAP+type_of_loader+quite-mode

;;; 	if you want to play with an external initramfs, do it here:
;;;	mov	dword [0x218], 0x0000 ;set ramdisk_image
;;;	mov	dword [0x21C], 0x0000 ;set ramdisk_size

;;; "Set this field to the offset (from the beginning of the real-mode code)"
;;; "of the end of the setup stack/heap, minus 0x0200"
	mov	word [0x224], ( 0xe000 - 0x200 )	 ;set heap_end_ptr

;;;	mov	byte [0x226], 0x00	 ;set ext_loader_ver doggy
;;;	mov	byte [0x227], 0x01	 ;set ext_loader_type / bootloader id 11

;;; "can be anywhere between the end of the setup heap and 0xA0000"
;;; "The kernel command line should not be located below the real-mode code,"
;;; "nor should it be located in high memory."

;;; FYI: it was working to leave cmdline  at 0:cmdline (no copy), but kernel says keep it above the heap
	mov	dword [0x228], 0x1e000 ; set cmd_line_ptr

;;;	qword [0x258] prefered load address
	pop	ds		       ; ds=0x0000 es=0x1000(kernel realmode segment)
;;; copy the kernel command line from LBA0 to the kernel command line buffer
	mov	si, cmdline
	mov	di, 0xe000
cmdcopy:
	lodsb
	stosb
	test	al, al		; string is null terminated
	jnz	short cmdcopy

    ; the protected mode part must be loaded at 0x100000
    ; load 128or64 sectors at a time to 0x2000, then copy to 0x100000

;;; load the kernel protected mode code to 0x100000

	xor	eax, eax
 	mov 	al, [es:0x1f1] ; no of blocks in the kernel realmode header
 	or	al, al
 	jnz	short has_size

	mov	al, 4		;default size

has_size:
 	add	eax, [boot_FLBA] ;compute first LBA of protected mode code
	inc	eax	;plus one for the legacy boot sector, eax=LBA to load
	mov	[xferLBA], eax	;starting LBA of kernel protected mode code

 	mov	eax, [es:0x1f4] ;num of 16byte pages of protected code to load
	xor	edx, edx
;;; /32 (pages per 512 byte LBA) to get logical blocks (sectors),
;;; then by SECTORSPERGROUP to get number of "blocks"
	mov	cx, ( 32 * SECTORSPERGROUP )
 	div	ecx
	inc	ax		; +1 potentially partial group
	mov	cx, ax

	call	load_high

wait_shift_keys_up:
;;; 	mov	ah, 0x02
;;; 	int	0x16
;;;	or	al, ah
;;; 	and	al, 00001111b
;;; 	jnz	short wait_shift_keys_up

	mov	al, KLAUNCH	; starting the Kernel
	call	print_al

	cli
	mov	ax, 0x1000
	mov	ds, ax
	mov	es, ax
	mov	fs, ax
	mov	gs, ax
	mov	ss, ax
	mov	sp, 0xe000
 	jmp	0x1020:0	;and away we go!
;;; 	----------------

;;;---------------------------------------------------------
	
;;; if 'UNDEFINED', upon return, will jne if A20 is enabled,
;;;   je if A20 is likely disabled.
;;; else upon return, will je if A20 is enabled, jne if A20 is likely disabled
;;; crashes ax, es, and maybe cx
check_a20_delay:
;;; wait to see if it eventually enables
 	xor	cx, cx	; arbitrary delay loop value
.delay_loop:
	call	check_a20_no_delay
	jne	short return

	dec	cx
	jnz	short .delay_loop

check_a20_no_delay:
 %ifndef	UNDEFINED
	xor	ax, ax
 	dec	ax		; 1byte smaller than 'not'
	mov	es, ax		; 0xffff
	cmp	word [es:0x7e0e], 0xaa55 ; boot sector ID mirrored in himem?
	mov	al, A20ERR

return:	ret
;;;	---

 %else	;another method, more sure, bigger.
	
	xor	ax, ax
;;;	mov	ds, ax
	dec	ax ; ax = 0xFFFF
	mov	es, ax
 
	mov	si, 0x0500
	mov	di, 0x0510

	mov	byte [ds:si], 0x3A ;write one
	mov	byte [es:di], 0xA3 ;write two

	cmp 	byte [ds:si], 0x3A
	jne	short checka20.ret	;write two clobber write one?

	cmp	byte [es:di], 0xA3 ;write two fail to write?

checka20.ret:
	mov	al, A20ERR
	ret
 %endif	;UNDEFINED

;;;----------------------------------------------------------------
;;;	    interface to keyboard controller to enable A20
;;; caution: this can hang on systems without a keyboard controller
;;;----------------------------------------------------------------
a20kbout:
.wait:
        in      al, 0x64
        test    al, 2
        jnz     short .wait

	mov	al, ah
	out     0x64, al
        ret
;;;	---

;;;------------------------------------------------------------------------
;;; On Entry:	[xferLBA]=starting LBA, cx=number of groups to move
;;;		[highmove_addr]=destination
;;; 
;;; load a group (128or64) of logical blocks, starting from LBA=eax
;;; into the buffer at 0x2000, then copy them to [highmove_addr]
;;; repeat for cx number of block groups
;;;------------------------------------------------------------------------

load_high:
	mov	word [xferbuffer+2], 0x2000 ;input buffer seg to read group into
	xor	ax, ax
	mov	[xferbuffer], ax
	mov	es, ax		 ;es=0x0000 ds=0x0000
	mov	word [xferblocks], SECTORSPERGROUP

.loadloop:
	push	cx	;save the remaining groups count
	call	disk_read	;read group of blocks into disk read buffer

	mov	esi, 0x20000	;the disk read buffer segment
	mov	edi, [highmove_addr] ;the current pointer to highmem address
;;; 	mov	cx, (SECTORSPERGROUP*512/4) ;this works, but...
	mov	ecx, (SECTORSPERGROUP*512/4)
;;; following instruction needs ds = es = 0x0000
	a32	rep movsd	;move read buffer to high memory
	mov	[highmove_addr], edi ;save the current highmem pointer
	mov	al, '.'
	call	print_al	;report the progress
	add	dword [xferLBA], SECTORSPERGROUP
	pop	cx		; restore the group count
	dec	cx
	jnz	short .loadloop

 	ret
;;;	-----

;;;---------------------------------------------------------
;;; on entry ds:si points to NULL terminated string to print
;;;---------------------------------------------------------
ps_loop:	
	call	print_al	; 3

print_string:
	lodsb			;+1
	or	al, al		;+2
	jnz	short ps_loop	;+2

	ret			;+1=9
;;; 	---

;;;---------------------------------------------------
;;; on entry dx contains value to print as hexadecimal
;;; ( this is orphan code, but really useful for debugging )
;;;---------------------------------------------------

print_dx_hex:
	call	pdxh
	mov	al, ' '
	jmp	print_al
;;;	---

pdxh:
	call	.dh
	
	mov	dh, dl
.dh:
	mov	al, dh
	shr	al, 4
	call	.all

	mov	al, dh
.all:
	and	al, 0x0f
	or	al, 0x30
	cmp	al, '9'
	jng	print_al

	add	al, 'a' - '9' - 1
	jmp	print_al
;;; 	----------------

;;;---------------------------------------------------------
;;; on entry ss:sp points to NULL terminated string to print
;;; ( this is orphan code, somewhat useful for debugging )
;;;---------------------------------------------------------

print_string_following:	
	pop	si
	call	print_string
	push	si
	ret
;;; 	---

;;;-----------------------------------------------------------------------
;;;	global descriptor table needed to set up protected mode
;;;-----------------------------------------------------------------------
gdt_desc:
	dw	gdt_end - gdt - 1
	dd	gdt

;;; access byte:
;;; [present, priv[2] (0=highest), 1, Execbit, Direction=0, rw=1, accessed=0]

;;; flags:
;;; Granuality (0=limitinbytes, 1=limitin4kbs), Sz= [0=16bit, 1=32bit], 0, 0
gdt:
	dq	0 ; first entry 0
;flat data segment
	dw	0FFFFh ; limit[0:15] (aka 4gb)
	dw	0      ; base[0:15]
 	db	0      ; base[16:23]
 	db	10010010b  ; access byte 
	db	11001111b    ; [7..4]= flags [3..0] = limit[16:19]
	db	0 ; base[24:31]
gdt_end:

;;;-----------------------------------------------------------------------
;;;		High Memory Pointer for copying protected mode kernel code
;;;-----------------------------------------------------------------------

highmove_addr:	dd	0x100000 ;initialized to starting destination address
;;;highmove_addr:	dd	0x200000
;;;highmove_addr:	dd	0x7fab000; this is the address qemu loads initramfs at
;;;highmove_addr:	dd	0xfffff000

	%assign MBRADDR		MBR-$$
	%warning "MBR" is at MBRADDR

 	%assign	BANKTWOADDR	BANKTWO-$$
 	%assign	BANKTWOLEN	$-BANKTWO
	%warning "bank_two" is at BANKTWOADDR, and is BANKTWOLEN bytes long

;;;---------------------------------------------------------
;;;
;;;	end of code, we reset to MBR to point to entries in the MBR/GPT
;;; 
;;;---------------------------------------------------------
	absolute	MBR
;;; LBA0:
;;; We load the first disk sectors to this addres in segment 0x0000
	times	0x200-($-MBR)	resb	1 ; bump to LBA1
;;; LBA1:
;;; the GPT (GUID Partition Table) header
GPT_header:
GPT_sig:		resb	8
GPT_rev:		resb	4
GPT_hdrsize:		resb	4
GPT_hdrCRC32:		resb	4
GPT_rsrvd:		resb	4
GPT_current_LBA:	resw	4
GPT_other_LBA:		resw	4
GPT_first_usable_LBA:	resw	4 ;this is the starting LBA of our second bank
GPT_last_usable_LBA:	resw	4
GPT_GUID:		resw	8
GPT_PARTS_LBA:		resw	4
GPT_PART_ENTRIES:	resw	2 ;check this many for smallest FLBA
GPT_PART_SIZE:		resw	2
GPT_PARTSARRAY_CRC32:	resw	2

	times	(2*0x200)-($-MBR)	resb	1 ; bump to LBA2

;;; the GPT (GUID Partition Table) entries (LBA2=first four partitions)
;;; LBA2:
GPT_entries:
part1_type:	resw	8
part1_UUID:	resw	8
part1_FLBA:	resw	4	;0x20 GPT part1 First LBA
part1_LLBA:	resw	4	;0x28 GPT part1 Last LBA
part1_FLAGS:	resw	4
part1_NAME:	resb	72
;;; 	times	0x80+0x20-($-GPT_entries)	resb	1
part2_type:	resw	8
part2_UUID:	resw	8
part2_FLBA:	resw	4	;0x20 GPT part1 First LBA
part2_LLBA:	resw	4	;0x28 GPT part1 Last LBA
part2_FLAGS:	resw	4
part2_NAME:	resb	72
;;; 	times	(2*0x80)+0x20-($-GPT_entries)	resb	1
part3_type:	resw	8
part3_UUID:	resw	8
part3_FLBA:	resw	4	;0x20 GPT part1 First LBA
part3_LLBA:	resw	4	;0x28 GPT part1 Last LBA
part3_FLAGS:	resw	4
part3_NAME:	resb	72
;;; 	times	(3*0x80)+0x20-($-GPT_entries)	resb	1
part4_type:	resw	8
part4_UUID:	resw	8
part4_FLBA:	resw	4	;0x20 GPT part1 First LBA
part4_LLBA:	resw	4	;0x28 GPT part1 Last LBA
part4_FLAGS:	resw	4
part4_NAME:	resb	72

;;; LBA3: the next four GPT partition entries (if present)

;;;	times	(4*0x200)-($-MBR)	resb	1 ; bump past end of LBA3

;;; to save code space these are pushed on the stack early
;;; and then read in place on the stack when needed
;;; (push instructions are usually small, so we saved six bytes)

;;; this has to be hand changed when the data pushed to stack changes
;;; presently one words pushed
	absolute STACK - (2*2)
;;; if following are 0, then there are bios exentions available
secs_per_head:		;BIOS provided boot disk parameter
	resb	1
hds_per_cyl:		;BIOS provided boot disk parameter
	resb	1
boot_drive:		;BIOS provided boot disk device number
	resb	1
	resb	1
	
;;; ============================================================
