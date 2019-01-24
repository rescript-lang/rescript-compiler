#include <stdio.h>
#include <ctype.h>

#define LINE_LENGTH 1024

char line[LINE_LENGTH];

int isprefix(s, pref)
     char * s;
     char * pref;
{
  while (1) {
    if (*pref == 0) return 1;
    if (*s == 0) return 0;
    if (*s != *pref) return 0;
    s++;
    pref++;
  }
}

int main(argc, argv)
     int argc;
     char * argv [];
{
  unsigned char * p;
  int c;
  int inquote;
  int inverb;
  int inverbatim;

  inverbatim = 0;
  inquote = 0;

  while(fgets(line, LINE_LENGTH, stdin) != NULL) {
    if (inverbatim) {
      fputs(line, stdout);
      if (isprefix(line, "\\end{verbatim")
          || isprefix(line, "\\end{alltt}")) inverbatim = 0;
      continue;
    }
    if (isprefix(line, "\\begin{verbatim")
        || isprefix(line, "\\begin{alltt}")) {
      fputs(line, stdout);
      inverbatim = 1;
      continue;
    }
    inverb = 0;
    for (p = (unsigned char *) line; *p != 0; p++) {
      c = *p;
      if (inverb) {
        if (c == inverb) inverb = 0;
        putchar(c);
        continue;
      }
      switch(c) {
      case '"':
        if (inquote) {
          fputs("\001", stdout);
          inquote = 0;
        } else {
          fputs("\\verb\001", stdout);
          inquote = 1;
        }
        break;
      case '\\':
        if (isprefix(p, "\\verb") && p[5] != 0 && !isalpha(p[5])) {
          inverb = p[5];
          p = p + 5;
          fputs("\\verb", stdout);
          putchar(inverb);
        } else if (inquote) {
          if (p[1] == '"' || p[1] == '\\') {
            c = p[1];
            p++;
          }
          putchar(c);
        } else {
          putchar('\\');
        }
        break;
      default:
        putchar(c);
      }
    }
  }
  return 0;
}
