/* Exercise 1-8. Write a program to count blanks, tabs, and newlines.
**/
#include <stdio.h>

int
main(void)
{
    int c;
    double b, t, nl;

    b = t = nl = 0.0;
    while ((c = getchar()) != EOF) {
        if (c == ' ') {
            ++b;
        }

        if (c == '\t') {
            ++t;
        }

        if (c == '\n') {
            ++nl;
        }
    }

    printf("blanks = %.0f, tabs = %.0f, newlines = %.0f\n", b, t, nl);

    return 0;
}
