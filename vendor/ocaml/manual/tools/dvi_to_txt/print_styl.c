#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "output.h"

/* Macintosh STYL tables */

extern char * input_name;

static FILE * text;
static FILE * styl;
static int partnum = 0;
static int currstyle;
static int currstart;
static int currpos;

static void output_current_style()
{
  int style_code;

  switch(currstyle) {
  case PLAIN:
    style_code = 0; break;
  case ITALICS:
    style_code = 2; break;
  case BOLD:
    style_code = 1 + 32; break; /* bold condensed */
  case MONOSPACED:
    style_code = standout_tt ? 1 + 32 : 0; break;
  }
  fprintf(styl, "%d %d Monaco %d 9 0 0 0\n", currstart, currpos, style_code);
}


static void output_style_change(newstyle)
     int newstyle;
{
  if (!standout_tt && (newstyle == PLAIN && currstyle == MONOSPACED ||
                       newstyle == MONOSPACED && currstyle == PLAIN)) {
    currstyle = newstyle;
    return;
  }
  output_current_style();
  currstyle = newstyle;
  currstart = currpos;
}

void begin_styl_page()
{
  char name[1024], buffer[1024];
  int n;

  strcpy(name, input_name);
  n = strlen(name);
  if (n >= 4 && strcmp(name + n - 4, ".dvi") == 0) name[n - 4] = 0;
  partnum++;
  sprintf(buffer, "%s.%03d.txt", name, partnum);
  text = fopen(buffer, "w");
  if (text == NULL) { perror(buffer); exit(2); }
  sprintf(buffer, "%s.%03d.stl", name, partnum);
  styl = fopen(buffer, "w");
  if (styl == NULL) { perror(buffer); exit(2); }
  currstyle = PLAIN;
  currstart = 0;
  currpos = 0;
}

void end_styl_page()
{
  output_current_style();
  fclose(text);
  fclose(styl);
}

void styl_line(txt, style, len)
     char * txt, * style;
     int len;
{
  for (/*nothing*/; len > 0; len--, txt++, style++, currpos++) {
    putc(*txt, text);
    if (*txt != ' ' && *style != currstyle) {
      output_style_change(*style);
    }
  }
  putc('\n', text);
  currpos++;
}


    
