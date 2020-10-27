<img src="https://semissioncontrol.github.io/assets/logo/SEMC%20Logo%20Wide.png"/>

This is semcOS. With the Open Source community, we plan to write Mission Control software for major Space Agencies. A distro built from the ground up to be fast and reliable, semcOS is hard-coded to just work.

We use BusyBox, thanks to its small size and super speed. Plans are to modify some of BusyBox's core systems to be more efficient and cohesive with our goals. 

## Installation

#### The following guide is deprecated 0 nevertheless, I'm keeping it up temporarily - until we have an official installer. Check out `handy/install` for a more up-to-date script on generating this distro.

This guide assumes that you have GRUB already installed.

Grab any USB install image. Log in, and:

```
fdisk /dev/sda
```

*If* you want to remove all partitions on your device, type:
```
o
```

Now, create a new partition:

```
p
n
```

Now, let's decide - do you want Semc OS install to take up the whole disk, or just a part? If the whole, then your `<Decided Action>` is `<Enter>`. If not, then your `<Decided Action>` is, for example, `+100G` (for 100 GB).

```
<Enter>
<Enter>
<Enter>
<Decided Action>
```

```
p
w
```

Note the partition you created.

Now, 

```
mkfs.ext4 <partition>
```

For example, if you have one partition:

```
mkfs.ext4 /dev/sda1
```

Now:

```
mount <partition> /mnt
cd /mnt
wget https://github.com/semissioncontrol/semcOS/releases/download/v0.0.2-alpha/semcos-0.0.2-alpha-x86_64.tar.xz
tar cfJ semcos-0.0.2-alpha-x86_64.tar.xz
nano -w boot/grub/grub.cfg
```

And now, change the line that says "/dev/sda6" to your `<partition>`.

Update GRUB, reboot, and you're in! 


## Further information
This software is licensed under the `Mozilla Public License 2.0`. Currently available for `x86_64` systems.

We are currently going through LICENSE fixes. More on that soon!
