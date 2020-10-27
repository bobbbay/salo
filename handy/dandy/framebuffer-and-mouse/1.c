#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <linux/fb.h>
#include <sys/mman.h>

int main(int argc, char** argv)
{
    int fd, bytes;
    unsigned char data[3];

    const char *pDevice = "/dev/input/mice";

    // Open Mouse
    fd = open(pDevice, O_RDWR);
    if(fd == -1)
    {
        printf("ERROR Opening %s\n", pDevice);
        return -1;
    }

    int left, middle, right;
    int ax = 0;
    int ay = 0;
    signed char x, y;
    while(1)
    {
        // Read Mouse
        bytes = read(fd, data, sizeof(data));

        if(bytes > 0)
        {
            left = data[0] & 0x1;
            right = data[0] & 0x2;
            middle = data[0] & 0x4;

            x = data[1];
            y = data[2];
            //printf("x=%d, y=%d, left=%d, middle=%d, right=%d\n", x, y, left, middle, right);
        }

        ax += x;
        ay += -y; // y is reversed initially

        framebuff(x, y, ax, ay);
    }
    return 0;
}

int framebuff(int mx, int my, int ax, int ay) // Mouse X, Mouse Y, Absolute X, Absolute Y
{
    struct fb_var_screeninfo vinfo;
    struct fb_fix_screeninfo finfo;
    long int screensize = 0;
    char *fbp = 0;
    int x = 0, y = 0;
    long int location = 0;

    int fbfd = open("/dev/fb0", O_RDWR);
    if (fbfd == -1) {
        perror("opening /dev/fb0");
        return -1;
    }

    // Get fixed screen information
    if (ioctl(fbfd, FBIOGET_FSCREENINFO, &finfo)) {
        printf("Error reading fixed information.\n");
        return -2;
    }

    // Get variable screen information
    if (ioctl(fbfd, FBIOGET_VSCREENINFO, &vinfo)) {
        printf("Error reading variable information.\n");
        return -3;
    }

    //printf("%dx%d, %dbpp\n", vinfo.xres, vinfo.yres, vinfo.bits_per_pixel );

    // Figure out the size of the screen in bytes
    screensize = vinfo.xres * vinfo.yres * vinfo.bits_per_pixel / 8;

    // Map the device to memory
    fbp = (char *)mmap(0, screensize, PROT_READ | PROT_WRITE, MAP_SHARED, fbfd, 0);
    if ((int)fbp == -1) {
        printf("Error: failed to map framebuffer device to memory.\n");
        return -4;
    }
    //printf("The framebuffer device was mapped to memory successfully.\n");

    x = 0;
    y = 0; // Where we are going to put the pixel

    // Figure out where in memory to put the pixel
    for ( y = 0; y < vinfo.yres; y++ ) {
        for ( x = 0; x < vinfo.xres; x++ ) {

            location = (x+vinfo.xoffset) * (vinfo.bits_per_pixel/8) + (y+vinfo.yoffset) * finfo.line_length;

            if ( vinfo.bits_per_pixel == 32 ) {
                if (ax == x || ax + 1 == x || ax + 2 == x && ay == y || ay + 1 == y || ay + 2 == y) { // PLEASE fix this
                    *(fbp + location) = 0; // b
                    *(fbp + location + 1) = 0; // g
                    *(fbp + location + 2) = 255; // r
                    *(fbp + location + 3) = 0; // a
                } else {
                    *(fbp + location) = 255;
                    *(fbp + location + 1) = 0;
                    *(fbp + location + 2) = 0;
                    *(fbp + location + 3) = 0;
                }
            } else  { //assume 16bpp
                printf("16bpp resolution is not supported, but will be soon! Get in touch to push it up the to-do list.");
                /*
                int b = 10;
                int g = mx/6;
                int r = 31-my/16;
                unsigned short int t = r<<11 | g << 5 | b;
                *((unsigned short int*)(fbp + location)) = t;
                */
            }

        }
    }
    munmap(fbp, screensize);
    close(fbfd);
    return 0;
}
