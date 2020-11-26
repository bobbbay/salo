---
name: "Bug \U0001F41B"
about: Caterpillars are cool. Please make sure you've read over the wiki before submitting.
title: This thing doesn't work (please replace title)
labels: Bug
assignees: ''

---

**Tell us your story**

I just booted up my newly installer system (no errors during installation). I boot up, and am greeted by an error:

```
error: /dev/tty1 not found
error: /dev/tty2 not found
error: /dev/tty3 not found
error: /dev/tty4 not found
error: /dev/tty5 not found
error: /dev/tty6 not found
```

I actually get a loop of the same message again and again. I've tried creating the `/dev` directory, to no avail.

**System information**

 - Installed via bootable USB
 - System:
   - Ryzen 9 5900X
   - RTX 3080
   - 3000W Power Supply

**I've tried...**

I tried creating the `/dev` directory, as well as re-installing the system. I also tried unplugging the computer from the power, and then re-plugging it in.

**Additional context**
I'm running the `fanhub` version of SEMC OS.
