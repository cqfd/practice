/* Exercise 1-2. Experiment to find out what happens when printf's argument
 * string contains \c, where c is some character not listed above.
 *
 * Answer: The compiler warns of an "unknown escape sequence."
**/
#include <stdio.h>

int
main(void)
{
    printf("\h");

    return 0;
}
