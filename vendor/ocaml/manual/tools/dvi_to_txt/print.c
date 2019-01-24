#include <stdio.h>
#include "output.h"

/* Low-level output functions */

void null()
{
}

void print_FF()
{
  putchar('\014');
}

void plain_line(txt, style, len)
     char * txt, * style;
     int len;
{
  fwrite(txt, 1, len, stdout);
  putchar('\n');
}

void printer_line(txt, style, len)
     char * txt, * style;
     int len;
{
  for (/*nothing*/; len > 0; len--, txt++, style++) {
    putchar(*txt);
    switch(*style) {
    case ITALICS:
      putchar('\b'); putchar('_'); break;
    case BOLD:
      putchar('\b'); putchar(*txt); break;
    case MONOSPACED:
      if (standout_tt) { putchar('\b'); putchar(*txt); }
      break;
    }
  }
  putchar('\n');
}

