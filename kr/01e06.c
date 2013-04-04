/* Exercise 1-6. Verify that the expression getchar() != EOF is 0 or 1.
**/
#include <stdio.h>

int
main(void)
{
    int c;

    /* Note: <ctrl>-D/Z is EOF, <return> is not */
    while ((c = getchar()) != EOF) {
        printf("=> %d\n", c != EOF);
    }

    printf("=> %d\n", c != EOF);


    return 0;
}
