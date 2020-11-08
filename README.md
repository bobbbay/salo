<img src="https://semissioncontrol.github.io/assets/logo/SEMC%20Logo%20Wide.png"/>

Space. The Final Frontier. Where man hesitates to go, for it is one of the most dangerous realms that can be found. Not a world, but a multitude of worlds, accessible through only one tool — the space rocket. But tools of great power rely on systems of even more tremendously large strength. These systems are the ones that are created here, at SEMC. The Space Exploration Mission Control organization strives to create powerful tools and services, Open Sourced.

And this is SEMC OS. A from-scratch distro written to be fast and reliable. Why? Because large space agencies lack one. There should be a unification when it comes to softwares that control the future of humanity - and this is where SEMC OS comes in. Hooked? Read on!

we use the [quantum package manager](https://github.com/quantum-package-manager/quantum-lua): Quantum is primarily a non-root pkg manager that installs things to a users home directory.
This feature was originally created so that computers running SEMCOS wouldn't have death by package manager, but then we split off into a seperate project.

## Installation

```bash
git clone https://github.com/semissioncontrol/semcOS
cd semcOS
./semcos devel init x86_64
./semcos devel install /dev/sdXY
```

Replacing `/dev/sdXY` with the partition you'd like to install SEMC OS onto. 

<!--

#### The following guide is deprecated - furthermore, you're a weird to be looking at the source of this README. Nevertheless, I'm keeping the instructions up temporarily - until we have an official installer. Check out `handy/install` for a more up-to-date script on generating this distro.

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

-->

## Contribution

[Rockets Still Not Fueled](https://github.com/semissioncontrol/semcOS/search?utf8=%E2%9C%93&q=TODO) | [Small Obstacles for Takeoff](https://github.com/semissioncontrol/semcOS/issues?q=is%3Aopen+is%3Aissue+label%3A%22Good+First+Issue%22) | [etc contents](https://github.com/semcos-packages/etc)

#### Useful Resources

 - [Build a Custom Minimal Linux Distribution from Source by Petros Koutoupis](https://www.linuxjournal.com/content/diy-build-custom-minimal-linux-distribution-source)
 - [site:linuxfromscratch.org "install zlib"](http://google.com/search?q=site:linuxfromscratch.org%20%22install%20zlib%22) will search for the LFS instuctions for installing ZLib - which are awesome!

## Further information

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/.

SEMC OS Authors, Top Contributors and Nexus Aurora High members retain the rights of SEMC OS. Contributors mean each individual or legal entity that creates, contributes to the creation of, or owns this software.

You can find our packages repositories [here](https://github.com/semcos-packages/) and [here](https://github.com/quantum-package-manager/repo). 
