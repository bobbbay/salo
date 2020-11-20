<img src="https://semissioncontrol.github.io/assets/logo/SEMC%20Logo%20Wide.png"/>

Space. The Final Frontier. Where man hesitates to go, for it is one of the most dangerous realms that can be found. Not a world, but a multitude of worlds, accessible through only one tool — the space rocket. But tools of great power rely on systems of even more tremendously large strength. These systems are the ones that are created here, at SEMC. The Space Exploration Mission Control organization strives to create powerful tools and services, Open Sourced.

And this is SEMC OS. A from-scratch distro written to be fast and reliable. Why? Because large space agencies lack one. There should be a unification when it comes to softwares that control the future of humanity - and this is where SEMC OS comes in. Hooked? Read on!

We use BusyBox's packages. Plans are to modify some of BusyBox's core systems to be more efficient and cohesive with our goals. 

### TODO

 - [ ] Add support for gcc
 - [ ] Add support for wireless networks
   - [ ] Add netplug to installer
   - [ ] Make sure mdev can recognize our devices (wlan0)
 - [ ] Create a "package manager" that can be invoked from SEMC OS
   - [ ] We can now add defaults - a cool vimrc is an example
   - [ ] Write buildfiles for everything that is being installed in the installer
     - [x] `/etc`
     - [x] For cross-compiler
       - [x] Kernel headers
       - [x] GNU Binutils
       - [x] Static GCC
       - [x] Glibc
       - [x] Final GCC
     - [x] Busybox
     - [ ] Kernel itself
     - [ ] CLFS Bootscripts
     - [ ] Zlib
     - [ ] Bash
     - [ ] Make
     - [ ] IANA-ETC
     - [ ] Sysstat
   - [ ] Make this have dependencies - Kernel needs kernel headers first
   - [ ] Make sure we are storing things in `/builds`
 - [ ] Support more programming languages
   - [x] Go
   - [x] Fortran
   - [ ] Lua
   - [ ] Lua ffi with C

### BUGS

 - [ ] Wireless does not work - see [#25](https://github.com/semissioncontrol/semcos/issues/25)
 - [ ] gcc can act up - heck, we need to install it correctly, see [#26](https://github.com/semissioncontrol/semcos/issues/26)

## Installation

```bash
# mkdir /mnt/semcos/
# mount /dev/sdXY /mnt/semcos
# handy/install
```

This will insall SEMC OS onto `/dev/sdXY` (replace `XY` with your partition). This *does* take a while, due to it being compiled from scratch. On an average modern PC, this would take ~30 minutes. 

You can also set Makeflags, if you know how to, to speed up the process.

## Development

Let's run though (pretend) installing Zlib on SEMC OS. Add the following to the `handy/install` file (it's already there :wink:). 

```sh
export SEMCOS="/mnt/semcos"

wget zlibtarball.com # change this to a link to the tarball!
tar xvf zlib*.tar.*z
cd zlib*/

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

```
./semcos devel commit handy/install
```

What this'll do is add all files, throw in a tarball, and start a commit. `--signoff` is enabled by default - please do not delete the `Signed-off-by: ` line. Git Signoff was introduced due to the [SCO lawsuit](http://en.wikipedia.org/wiki/SCO_v._IBM), and binds you to the legal statements mentioned [here](https://developercertificate.org/). In short, you're verifying that the code you wrote was yours, or more specifically that you have not infringed someone else's Intelectual Property with the code you have contributed. 

Done? Create a Pull Request, and you're done!

## Contribution

[Rockets Still Not Fueled](https://github.com/semissioncontrol/semcOS/search?utf8=%E2%9C%93&q=TODO) | [Small Obstacles for Takeoff](https://github.com/semissioncontrol/semcOS/issues?q=is%3Aopen+is%3Aissue+label%3A%22Good+First+Issue%22)

#### Useful Resources

 - [Build a Custom Minimal Linux Distribution from Source by Petros Koutoupis](https://www.linuxjournal.com/content/diy-build-custom-minimal-linux-distribution-source)
 - [site:linuxfromscratch.org "install zlib"](http://google.com/search?q=site:linuxfromscratch.org%20%22install%20zlib%22) will search for the LFS instuctions for installing ZLib - which are awesome!

## Further information

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/.

SEMC OS Authors, Top Contributors and Nexus Aurora High members retain the rights of SEMC OS. Contributors mean each individual or legal entity that creates, contributes to the creation of, or owns this software.
