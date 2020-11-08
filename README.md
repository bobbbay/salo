<img src="https://semissioncontrol.github.io/assets/logo/SEMC%20Logo%20Wide.png"/>

Space. The Final Frontier. Where man hesitates to go, for it is one of the most dangerous realms that can be found. Not a world, but a multitude of worlds, accessible through only one tool — the space rocket. But tools of great power rely on systems of even more tremendously large strength. These systems are the ones that are created here, at SEMC. The Space Exploration Mission Control organization strives to create powerful tools and services, Open Sourced.

And this is SEMC OS. A from-scratch distro written to be fast and reliable. Why? Because large space agencies lack one. There should be a unification when it comes to softwares that control the future of humanity - and this is where SEMC OS comes in. Hooked? Read on!

We use BusyBox's packages. Plans are to modify some of BusyBox's core systems to be more efficient and cohesive with our goals. 

## Installation

```bash
git clone https://github.com/semissioncontrol/semcOS
cd semcOS
./semcos devel init x86_64
./semcos devel install /dev/sdXY
```

Replacing `/dev/sdXY` with the partition you'd like to install SEMC OS onto. 

## Development

Once you're in the SEMC OS Git repo, and have run the install commands, you'll be given a `build` directory. This is what you can use to install and produce in the future.

Let's run though (pretend) installing Zlib on SEMC OS. Unzip the Zlib tarball, `cd` into that directory, and run the following commands:

```sh
export SEMCOS="/home/you/path/to/semcOS/build"
sed -i 's/-O3/-Os/g' configure
./configure --prefix=/usr --shared
make && make DESTDIR=${SEMCOS}/ install
```

What's happening there? Essentially, we're setting a variable `SEMCOS`, which stores the `build` directory of our SEMC OS system. Then, we run a quick `sed` command to fix some known errors with Zlib. You can generally find these sorts of patches at [Linux From Scratch](http://www.linuxfromscratch.org/lfs/view/stable/). Many packages won't need this command.

Then, we run `configure`. Useful `configure` instructions can also be found at Linux From Scratch. Although `./configure` *could* work for the most part, it's best to look into which flags should be enabled/disabled. 

Finally, we run two commands in one - `make`, which compiles everything, and `make install`, which installs our compiled files. Be careful though - `make install` without any flags will install it onto your system, while `make DESTDIR=${SEMCOS}/ install` will install Zlib to our SEMC OS build directory. This is really just setting the Destination Directory to `/home/you/path/to/semcOS/build`. 

Note that it's regular if `make` takes a long time to run. You can usually find, yet again, SBU times (how long it takes for the package to compile relative to your system) at Linux From Scratch as well. 

A wiki is in the works.

## Post-development

So, we just installed Zlib to our system. Now, let's run some commands to set this up for a Pull Request!

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

You can find our packages repositories [here](https://github.com/semcos-packages/). 
