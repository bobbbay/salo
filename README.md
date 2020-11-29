<img src="https://semissioncontrol.github.io/assets/logo/SEMC%20Logo%20Wide.png"/>

Space. The Final Frontier. Where man hesitates to go, for it is one of the most dangerous realms that can be found. Not a world, but a multitude of worlds, accessible through only one tool — the space rocket. But tools of great power rely on systems of even more tremendously large strength. These systems are the ones that are created here, at SEMC. The Space Exploration Mission Control organization strives to create powerful tools and services, Open Sourced.

And this is SEMC OS. A from-scratch distro written to be fast and reliable. Why? Because large space agencies lack one. There should be a unification when it comes to softwares that control the future of humanity - and this is where SEMC OS comes in. Hooked? Read on!

We use BusyBox's packages. Plans are to modify some of BusyBox's core systems to be more efficient and cohesive with our goals. 

### TODO

 - [x] Completely rewrite installation process!
 - [ ] Completely rewrite installation process 2.0, featuring C + Qt
 - [ ] Meet the [DistroWatch requirments](https://distrowatch.com/dwres.php?resource=faq#newdistro) for [application](https://distrowatch.com/dwres.php?resource=submit)
 - [x] More functional installer
 - [ ] Add support for gcc
 - [ ] Add support for wireless networks
   - [ ] Add netplug to installer
   - [ ] Make sure mdev can recognize our devices (wlan0)
 - [x] Add aliases, such as `q=quantum`, to default `/etc/profile`
 - [ ] Tell the CLFS Bootscripts to set the hostname to `semc` immediately
 - [x] Install git
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
     - [x] CLFS Bootscripts
     - [x] Zlib
     - [x] Bash
     - [x] Make
     - [x] IANA-ETC
     - [x] Sysstat
   - [ ] Make this have dependencies - Kernel needs kernel headers first
   - [x] Make sure we are storing things in `/builds`
 - [ ] Support more programming languages
   - [x] Go
   - [x] Fortran
   - [ ] Lua
   - [ ] Lua ffi with C
 - [ ] Get a working version of [Quantum](http://github.com/quantum-package-manager/) onto SEMC OS
 - [ ] Make sure c++ works correctly - last time there were complaints about it acting up
 - [ ] Close up [#7](https://github.com/semissioncontrol/semcos/issues/7), large refactoring. This is very close to completion!
 - [ ] Add support for Wayland and Sway (contact Mr. Petro)
 - [ ] Get better documentation - maybe use the GitHub built-in wiki function?
 - [ ] Test if docker can work on this machine (needed for ESS)

### Known Bugs

 - [x] Startup scripts are not run - newest version does not contain this issue
 - [x] On boot, I get an error of `/dev/tty{1..6} not found` - newest version does not contain this issue
 - [ ] My hostname is `(none)`
 - [ ] Wireless does not work - see [#25](https://github.com/semissioncontrol/semcos/issues/25)
 - [ ] gcc can act up - heck, we need to install it correctly, see [#26](https://github.com/semissioncontrol/semcos/issues/26)

### Plans for the near future

We'd like to hit the Distrowatch Requirments, as shown:

> New distributions should be installable on their own without requiring another distribution to be in place to bootstrap them. There should be an ISO or IMG file users can download, we do not list projects which are available only on cloud services (ie Amazon Linux) or as Docker containers. Also, new projects should have their own website. Distributions which only have a project page on build services like SUSE Studio will not be featured. Or that are only mentioned on a page of a larger company website as a promo or demo tool will not be listed.

TL;DR:

 - [ ] Installable without requiring another distribution in place to bootstrap them
 - [ ] ISO or IMG file users can download
 - [x] Not solely avaulable on cloud services or as Docker containers
 - [ ] Have their own website
 - [x] Not a project page on build services (e.x. SUSE Studio)
 - [x] Not ony mentioned as a demo or promo tool

If we can get this, we'll have a path paved out for us. The future after this is uncertain, like many things :sparkles:

As a side note, we currently have a working bootstrap script. This'll allow us to create our distribution from another distribution. This is the `bin/semcos` file. Future plans contain a graphical installer, which would replace the bootstrap script, on a Live CD, which would replace the need for an existing distribution. This would knowck off the first two requirments on the Distrowatch list. 

## Installation

```bash
# git clone https://github.com/semissioncontrol/semcos
# cd semcos
# mkdir /mnt/semcos/
# mount /dev/sdXY /mnt/semcos
# bin/semcos check
# bin/semcos
```

Replacing `/dev/sdXY` with the device partition you'd like to install to (for example, `/dev/sda1`). `bin/semcos check` will give you warnings about your installation - make sure you read them up! You won't be prevented to continue with installation though - that choice is up to you. 

The compilation takes some time, due to it being largely from scratch. Soon there will be a non cross compiled option. At the moment, on an average modern PC, this could take ~30 minutes. 

> Disclaimer: Compilation may not work on WSL or WSL2. Proceed at your own debugging.

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

```sh
git add bin/semcos
git commit -m "My commit" --signoff
```

What this'll do is add all files and start a commit. Please try enabling `--signoff`, and don't delete the `Signed-off-by: ` line. Git Signoff was introduced due to the [SCO lawsuit](http://en.wikipedia.org/wiki/SCO_v._IBM), and binds you to the legal statements mentioned [here](https://developercertificate.org/). In short, you're verifying that the code you wrote was yours, or more specifically that you have not infringed someone else's Intelectual Property with the code you have contributed. 

Done? Create a Pull Request, and you're done!

## Other executables
If you check out `bin/`, you'll see a few scripts. Generally, you'll be looking for `semcos-stable` or `semcos`, but here is the documentation for all scripts:

#### `semcos-featureful`

The experimental (and theoretically not-working) script that has more "features" - also known as customizablity. Do not confuse this with the resulting system - the resulting system will be the same as `semcos`. See [Extra](#extra) for more details.

#### `semcos-bare`

The theoretical script that does not build a cross-compiler, resulting in faster build time, but restricting you to target only your own CPU archictecture.

## Extra
<!-- TODO: better title -->

The current installer accepts environment variables. There are a few variables that can be set.

#### `THREADS=32 bin/semcos`
Specifies the amount of threads for the installer to use. Default: `$(nproc) - 1`.

#### `NOROOT=y bin/semcos-featureful`
Asks the script to not force running as root. Beware! Default: `n`.

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
