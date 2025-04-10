# PLL

Partition Linux Loader -- a tiny linux kernel boot loader

This code presently fits in three 512B sectors. PLL's minimalist
design makes it very robust, and easy to audit. PLL does not need nor
use any filesystems, so it cannot be disabled through changes to, or
corruption of filesystems. PLL boots by loading a kernel directly out
of the selected dedicated partition.

This is my daily boot loader on my desktop, and all of my servers.  It
was written to address difficulties that emerged periodically with
[lilo][1] and [grub][2], which were difficult to diagnose, especially with
regard to servers running on older hardware.

PLL does not need nor use [UEFI][3]; it presumes a [GPT][4] for simplicity.
I chose [GPT][4] because it supports numerous partitions, as well as
partition names and [UUID][5]s.

PLL loads and boots a kernel directly from a partition.  The partitioun
contain only the kernel, no filesystem. (though the kernel can of course
contain an [initmpfs][6])

PLL supports a [kernel command line][7] in the [boot sector][8], and
the ability to boot from an alternative partition “one time only”
(reverting to the previous kernel on the next boot), and the ability
to select which partition to load the kernel from by holding shift
keys down at boot time.

PLL does not load a separate [initmpfs][6] file. When one is needed, it
must be compiled into the kernel.

PLL compiles to three sectors at present: The [boot
sector][8] or Master Boot Record (MBR) (Logical Block Address Zero
[LBA0]), with the remaining two sectors sitting between the [GPT][4]
and the physically first partition. Most partition editors provide
this space by default, though I prefer to micro-manage my partiton
locations.  The [execline][10] install script `PLLinstall` will
compile PLL appropriately for your [GPT][4], and write the PLL sectors
to the disk if instructed to do so.

## `PLL=v.v.v-xx.n` Selecting the kernel load partition

The kernel command line stored in LBA0, is preloaded with
`PLL=v.v.v-xx.n`, where `v` are version numbers, and `xx` is replaced
at boot time with the BIOS number of the boot device. This command
line option must exist and must be first.

One of three possible boot scenarios are selected by what character
follows the `xx` in the `PLL=` option (`.` by default)  
These three cases are `.`, `+`, and any other character:

### In the case of `.`
 
At boot time, state of the keyboard shift keys determine which
partition the kernel will be loaded from:

* no shift keys are down: partition 1 is selected.
* a 'shift' key is down:  partition 2 is selected.
* a 'ctrl' key is down:	partition 3 is selected.
* an 'alt' key is down:	partition 4 is selected.

for each of these cases, the `n` in the `PLL=` option will be replaced
with the partition the kernel was actualled loaded from.

### In the case of `+`

The kernel will be loaded from the partion number following the `+`
(at the `n` position) in the `PLL=` command option..

### In all other cases

The partition number following the character (which replaced the `.`)
will be 'xor'ed with 0b00000011 (which is an ASCII `1` xored with an
ASCII `2`) so that a `1` will become a `2`, and vice versa. The kernel
will be loaded from the resulting partition number. Then, the
character in the `.` position, will be overwritten with `+` on disk
in LBA0.  (the number following the `+` is unchanged on either the
disk [LBA0], or the command line option) Note that this is the *only*
condition where PLL will write to disk.

This is effectively a “one time boot”. If the command line is not
subsequently changed, the next boot will revert to the previous
partition number. Applications can look at the PLL option in the
command line in `/proc/cmdline` to check this character and determine
if we have booted in a “one time boot” condition.

## Dependencies

* PLL is compiled by [nasm][9].

* The [execline][10] install script uses execlineb from [skarnet.org][10],
hexdump ([util-linux][11]), dd, and test ([core-utils][12]).

* The easiest way to edit the kernel command line is with [hexedit][17].

## To Use

* install [execline][10] via your package manager
* install [nasm][9] via your package manager
* clone PLL from codeberg/github/gitflic into `/usr/src/PLL`  
or if gentoo, use the [ebuild][16] in my [overlay][15]
* use a partition editor (fdisk, parted, etc) to install a [GPT][4]
(GUID partiton table) to your target disk (e.g. /dev/sdb):  
	`$ fdisk /dev/sdb`  
	create at least one partition large enough to contain your kernel.
(e.g. `/dev/sdb1` )
* compile a suitable kernel for your hardware  
if an initmpfs is needed, compile it into the kernel
* write your kernel into the partiton:  
	`$ dd if=/boot/vmlinuz of=/dev/sdb1`
* run the PLLinstall script to write the boot sectors:  
	`$ cd /usr/src/PLL`  
	`$ ./PLLinstall /dev/sdb`  
this will read the [GPT][4], compile PLL to fit, and
(if invoked with --write) write the sectors to the disk

* The disk should now be bootable by a [legacy boot BIOS][13]

## “[enhanced BIOS][14]” calls.

If your BIOS supports [enhanced BIOS][14] calls, PLL will detect this
and use Logical Block Addresses [LBA][18] for disk read/write. If not,
PLL will use the older “Cylinder/Head/Sector” ([CHS][19]) addressing.
This later code is currently using only a 16 bit [LBA][18], so only the
top 33.5MB of the disk are accessible to load a kernel from. Linux
kernels are not usually that big, so this has not been a problem on
older hardware with limited RAM.  On systems with gigabytes of RAM,
loading a kernel with a very large embedded initmpfs could be
desirable, but such systems usually support [LBA][18] addressing
through [enhanced BIOS][14] calls.  If this limitation becomes a
problem, a larger [LBA][18] could be supported with more complex code,
but it would never achieve the reach of [LBA][18], which is 48bits.

## other useful references

[int-13][20]  
[int-16][21]

[1]: https://www.joonet.de/lilo/
[2]: https://www.gnu.org/software/grub/
[3]: https://en.wikipedia.org/wiki/UEFI
[4]: https://en.wikipedia.org/wiki/GUID_Partition_Table
[5]: https://en.wikipedia.org/wiki/UUID
[6]: https://www.kernel.org/doc/html/latest/filesystems/ramfs-rootfs-initramfs.html#what-is-initramfs
[7]: https://www.kernel.org/doc/html/v4.14/admin-guide/kernel-parameters.html
[8]: https://en.wikipedia.org/wiki/Volume_boot_record
[9]: https://www.nasm.us/
[10]: https://www.skarnet.org/software/execline/
[11]: https://github.com/util-linux/util-linux
[12]: https://www.gnu.org/software/coreutils/
[13]: https://en.wikipedia.org/wiki/UEFI_CSM#CSM_booting
[14]: https://en.wikipedia.org/wiki/Enhanced_BIOS#Enhanced_BIOS
[15]: https://codeberg.org/smj/smj-gentoo
[16]: https://codeberg.org/smj/smj-gentoo/src/branch/main/sys-boot/pll
[17]: http://rigaux.org/hexedit.html
[18]: https://en.wikipedia.org/wiki/Logical_block_addressing
[19]: https://en.wikipedia.org/wiki/Cylinder-head-sector
[20]: https://www.ctyme.com/intr/int-13.htm
[21]: https://www.ctyme.com/intr/int-16.htm
