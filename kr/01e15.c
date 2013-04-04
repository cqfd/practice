/* Exercise 1-15. Rewrite the temperature conversion program of Section 1.2 to
 * use a function for conversion.
**/
#include <stdio.h>

int
convert(int fahr);

int
main(void)
{
    int fahr, celsius;
    int lower, upper, step;

    lower = 0;
    upper = 300;
    step = 20;

    fahr = lower;
    while (fahr <= upper) {
        celsius = convert(fahr);
        printf("%d\t%d\n", fahr, celsius);
        fahr = fahr + step;
    }

    return 0;
}

int
convert(int fahr)
{
    return 5 * (fahr - 32) / 9;
}
