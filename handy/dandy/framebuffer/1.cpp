// Using /dev/fb0 to manipulate pixels -- super important.
// The following code was written by the team of Orion Lawlor at
// https://bit.ly/30VYOUy (published Google Doc). Modifications and
// further explanations to come.

/*
Linux framebuffer example program, originally from the Qt Qtopia Core examples:
https://cep.xray.aps.anl.gov/software/qt4-x11-4.2.2/qtopiacore-testingframebuffer.html

Compile with:
    gcc framebuffer.cpp -o fb
Run with:
    ./fb

On my 4k laptop, this only works inside an old-school terminal,
and it seems to think I'm at 800x600 even when running X in 4k.
*/

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <linux/fb.h>
#include <sys/mman.h>
#include <sys/ioctl.h>

// This class matches how the pixels are actually stored in memory
struct pixel {
    unsigned char b; // blue channel, 0 is black, 255 is bright
    unsigned char g; // green
    unsigned char r; // red
    unsigned char a; // alpha transparency (unused)
};

int main()
{
    // Open the framebuffer device file
    int fbfd = open("/dev/fb0", O_RDWR);
    if (fbfd == -1) {
        perror("Error: cannot open framebuffer device");
        exit(1);
    }
    printf("The framebuffer device was opened successfully.\n");

    // Get fixed screen information
    struct fb_fix_screeninfo finfo;
    if (ioctl(fbfd, FBIOGET_FSCREENINFO, &finfo) == -1) {
        perror("Error reading fixed information");
        exit(2);
    }

    // Get variable screen information
    struct fb_var_screeninfo vinfo;
    if (ioctl(fbfd, FBIOGET_VSCREENINFO, &vinfo) == -1) {
        perror("Error reading variable information");
        exit(3);
    }

    printf("%dx%d, %dbpp\n", vinfo.xres, vinfo.yres, vinfo.bits_per_pixel);
    if (vinfo.bits_per_pixel!=32) {
        printf("Sorry, we only support 32 bit framebuffer\n");
        exit(4);
    }

    // Figure out the size of the screen in bytes
    long screensize = vinfo.xres * vinfo.yres * vinfo.bits_per_pixel / 8;

    // Map the device to memory
    pixel *framebuffer = (pixel *)mmap(0, screensize,
        PROT_READ | PROT_WRITE, MAP_SHARED, fbfd, 0);

    if ((long)framebuffer == -1) {
        perror("Error: failed to map framebuffer device to memory");
        exit(5);
    }

    // Figure out where in memory to put the pixel
    for (int y = 100; y < 500; y++)
        for (int x = 100; x < 500; x++) {
            long index = (x+vinfo.xoffset) +
                         (y+vinfo.yoffset) * finfo.line_length / sizeof(pixel);
            pixel &p = framebuffer[index];
            p.r=x/2; // red == x
            p.g = 255 - p.g; // invert green channel
            p.b=y/2; // green == y
        }

    // Clean up
    munmap(framebuffer, screensize);
    close(fbfd);
    return 0;
}
