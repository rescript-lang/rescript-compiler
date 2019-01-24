#include <stdio.h>
#include "output.h"

/* Rich Text Format */

void begin_rtf_document()
{
  printf("{\\rtf1\\ansi\\deff0\n");
  printf("{\\fonttbl{\\f0\\fmodern Courier;}}\n");
  printf("\\f0\\fs20\n");
}

void end_rtf_document()
{
  printf("}\n");
}

void end_rtf_page()
{
  printf("\\page\n");
}

void rtf_line(txt, style, len)
     char * txt, * style;
     int len;
{
  int currstyle;

  for (currstyle = PLAIN; len > 0; len--, txt++, style++) {
    if (*txt != ' ') {
      switch(*style) {
      case PLAIN:
        if (currstyle != PLAIN) {
          putchar('}');
          currstyle = PLAIN;
        }
        break;
      case ITALICS:
        if (currstyle != ITALICS) {
          if (currstyle != PLAIN) putchar('}');
          printf("{\\i ");
          currstyle = ITALICS;
        }
        break;
      case BOLD:
        if (currstyle != BOLD) {
          if (currstyle != PLAIN) putchar('}');
          printf("{\\b ");
          currstyle = BOLD;
        }
        break;
      case MONOSPACED:
        if (standout_tt) {
          if (currstyle != BOLD) {
            if (currstyle != PLAIN) putchar('}');
            printf("{\\b ");
            currstyle = BOLD;
          }
        } else {
          if (currstyle != PLAIN) {
            putchar('}');
            currstyle = PLAIN;
          }
        }
        break;
      }
    }
    switch(*txt) {
    case '\\':
    case '{':
    case '}':
      putchar('\\'); putchar(*txt); break;
    default:
      putchar(*txt); break;
    }
  }
  if (currstyle != PLAIN) putchar('}');
  printf("\\par\n");
}

