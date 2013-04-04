/* Exercise 1-20. Write a program detab that replaces tabs in the input with
 * the proper number of blanks to space to the next tab stop. Assume a fixed
 * set of tab stops, say every n columns. Should n be a variable or
 * symbolic parameter?
**/
#include <stdio.h>

#define SPACE '.'
#define TABSTOP 8

#define IN 1
#define OUT 0

/* Replaces tabs with the correct number of spaces. Works on lines of arbitrary
 * length (or at least those with less than INT_MAX/TABSTOP consecutive tabs).
**/
int
main(void)
{
    int c, pos, spaces, state, tabs;

    state = OUT;
    pos = tabs = 0;
    while ((c = getchar()) != EOF) {
        if (c == '\n') {
            putchar(c);
            state = OUT;
            pos = tabs = 0;
        } else if (c == '\t') {
            if (state == IN) {
                state = OUT;
                tabs = 0;
            }

            ++tabs;
        } else {
            if (c != ' ' && state == OUT) {
                spaces = tabs * TABSTOP - pos % TABSTOP;
                while (spaces > 0) {
                    putchar(SPACE);
                    --spaces;
                }

                pos = 0;
                state = IN;
            }

            putchar(c == ' ' ? SPACE : c);
            ++pos;
        }
    }

    return 0;
}
