# PLL

Partition Linux Loader -- a tiny linux kernel boot loader

A working, work in progress...

This code presently fits in three 512B sectors. PLL's minimalist
design makes it very robust, and easy to audit. PLL does not need nor
use any filesystems, so it cannot be disabled through changes to, or
corruption of filesystems. PLL boots by loading a kernel directly out
of the selected dedicated partition.

This is my daily boot loader on my desktop, and all of my servers.  It
was written to address difficulties that emerged periodically with
[lilo][1] and [grub][2], which were difficult to diagnose, especially with
regard to older hardware.

PLL does not need nor use [UEFI][3]; it presumes a [GPT][4] for simplicity.
I chose [GPT][4] because it supports numerous partitions, as well as
partition names and [UUID][5]s.

PLL loads and boots kernels directly from a partition.  The partition
contain only the kernel, no filesystem. (though the kernel can of course
contain an [initramfs][6])

PLL supports a [kernel command line][7] in the [boot sector][8], the ability to
boot alternative partitions “one time only” (reverting to the previous
kernel on the next boot), and the ability to select which partition to
load the kernel from by holding shift keys down at boot time.

No separate [initramfs][6] file is loaded, so compile it into the kernel
when needed.

PLL compiles to less than three sectors at present: The [boot sector][8]
(Logical Block Address Zero [LBA0]), and the remainder which must fit
between the [GPT][4] and the physically first partition. Most partition
editors provide this space by default, though I prefer to micro-manage
my partiton locations.  The [execline][10] install script will compile PLL
appropriately for your [GPT][4], and write the PLL sectors to the disk if
instructed to do so.

## Dependencies

* PLL is compiled by [nasm][9].

* The [execline][10] install script uses: execlineb from [skarnet.org][10],
hexdump ([util-linux][11]), dd, and test ([core-utils][12]).

## To Use

* install [execline][10] via your package manager
* install [nasm][9] via your package manager
* clone PLL from codeberg into `/usr/src/PLL`  
or if gentoo, use the [ebuild][16] in my [overlay][15]
* use a partition editor (fdisk, parted, etc) to install a [GPT][4]
(GUID partiton table) to your target disk (e.g. /dev/sdb):  
	`$ fdisk /dev/sdb`  
	create at least one partition large enough to contain your kernel.
(e.g. `/dev/sdb1` )
* compile a suitable kernel for your hardware  
if an initramfs is needed, compile it into the kernel
* write your kernel into the partiton:  
	`$ dd if=/boot/vmlinuz of=/dev/sdb1`
* run the PLLinstall script to write the boot sectors:  
	`$ cd /usr/src/PLL`  
	`$ ./PLLinstall /dev/sdb`  
this will read the [GPT][4], compile PLL to fit, and
(if invoked with --write) write the sectors to the disk

* The disk should now be bootable by a [legacy boot BIOS][13]

## ToDo

* Presently, PLL requires that your BIOS supports “[enhanced BIOS][14]” calls.  
A previous version booted without those extentions, and was
designed to fit entirely within LBA0, but did not always work on
very large disks or partition tables. Now that PLL has outgrown
a single sector, I intend to re-enstate the ability to boot
without [enhanced BIOS][14] calls on machines that do not support them.

* Add utility to edit the [command line][7]  
I currently use [hexedit][17]:  
`$ hexedit /dev/sdb`

* After the above is implemented, code and comments need a thorough
clean up.

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
