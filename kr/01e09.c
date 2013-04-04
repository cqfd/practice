/* Exercise 1-9. Write a program to copy its input to its output, replacing
 * each string of one or more blanks by a single blank.
**/
#include <stdio.h>

int
main(void)
{
    int c, p;

    p = EOF;
    while ((c = getchar()) != EOF) {
        if (c == ' ') {
            if (p != ' ') {
                putchar(c);
                p = c;
            }
        }
        if (c != ' ') {
            putchar(c);
            p = c;
        }
    }

    return 0;
}
