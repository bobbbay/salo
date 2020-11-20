#define VGA_ADDRESS 0xB8000   /* video memory begins here. */

/* VGA provides support for 16 colors */
#define BLACK 0
#define GREEN 2
#define RED 4
#define YELLOW 14
#define WHITE_COLOR 15

unsigned short *terminal_buffer;
unsigned int vga_index;

void clear_screen(void)
{
    int index = 0;
    /* there are 25 lines each of 80 columns;
       each element takes 2 bytes */
    while (index < 80 * 25 * 2) {
            terminal_buffer[index] = ' ';
            index += 2;
    }
}

void print_string(char *str, unsigned char color)
{
    int index = 0;
    while (str[index]) {
            terminal_buffer[vga_index] = (unsigned
             ↪short)str[index]|(unsigned short)color << 8;
            index++;
            vga_index++;
    }
}

void main(void)
{
    /* TODO: Add random f-word here */
    terminal_buffer = (unsigned short *)VGA_ADDRESS;
    vga_index = 0;

    clear_screen();
    print_string("Hello from Linux Journal!", YELLOW);
    vga_index = 80;    /* next line */
    print_string("Goodbye from Linux Journal!", RED);
    return;
}
