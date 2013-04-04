/* Exercise 1-19. Write a function reverse(s) that reverses the character
 * string s. Use it to write a program that reverses its input a line
 * at a time.
**/
#include <stdio.h>

#define MAXCHARS 1000

/* function prototypes */
int
getline(char s[], int lim);

int
getlength(char s[]);

void
reverse(char s[]);

/* reverses input line by line */
int
main(void)
{
    int len;
    char line[MAXCHARS];

    len = 0;
    while ((len = getline(line, MAXCHARS)) > 0) {
        reverse(line);
        printf("%s", line);
    }

    return 0;
}

/* gets the length of a string */
int
getlength(char s[])
{
    int len;

    len = 0;
    while (s[len] != '\0') {
        ++len;
    }

    return len;
}

/* read a line into s, return length */
int
getline(char s[], int lim)
{
    int c, i;

    for (i = 0; i < lim-1 && (c = getchar()) != EOF && c != '\n'; ++i) {
        s[i] = c;
    }

    if (c == '\n') {
        s[i] = c;
        ++i;
    }

    s[i] = '\0';

    return i;
}

/* reverses a string */
void
reverse(char s[])
{
    int c, i, j, len;

    len = getlength(s);

    for (i=0, j=len-1; i < j; ++i, --j) {
        c = s[i];
        s[i] = s[j];
        s[j] = c;
    }
}
