#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "output.h"

void null(), print_FF(), plain_line(), printer_line();
void begin_rtf_document(), end_rtf_document(), end_rtf_page(), rtf_line();
void begin_styl_page(), end_styl_page(), styl_line();

struct output_device {
  void (*begin_document)();
  void (*end_document)();
  void (*begin_page)();
  void (*end_page)();
  void (*line)();
} device[] = {
  null, null, null, print_FF, plain_line,
  null, null, null, print_FF, printer_line,
  begin_rtf_document, end_rtf_document, null, end_rtf_page, rtf_line,
  null, null, begin_styl_page, end_styl_page, styl_line
};

#define SIZEX 160

struct line {
  int ypos;
  int len;
  char * contents;
  char * styles;
  struct line * next_in_bucket;
};

#define NBUCKETS 101

struct line * screenlines[NBUCKETS];

int numlines;

char * xmalloc(size)
     int size;
{
  char * res = (char *) malloc(size);
  if (res == NULL) {
    fprintf(stderr, "Out of memory\n");
    exit(2);
  }
  return res;
}

char * xrealloc(ptr, size)
     char * ptr;
     int size;
{
  char * res = (char *) realloc(ptr, size);
  if (res == NULL) {
    fprintf(stderr, "Out of memory\n");
    exit(2);
  }
  return res;
}

void begin_document()
{
  device[output_device].begin_document();
}

void end_document()
{
  device[output_device].end_document();
}

void clear_page()
{
  int i;

  for (i = 0; i < NBUCKETS; i++) screenlines[i] = NULL;
  numlines = 0;
}

void out(x, y, c, style)
     int x, y;
     char c;
     char style;
{
  unsigned int h;
  struct line * line;

  h = ((unsigned int) y) % NBUCKETS;
  line = screenlines[h];
  while (line != NULL && line->ypos != y) line = line->next_in_bucket;
  if (line == NULL) {
    line = (struct line *) xmalloc(sizeof(struct line));
    line->ypos = y;
    line->len = 80;
    line->contents = (char *) xmalloc(line->len);
    memset(line->contents, ' ', line->len);
    line->styles = (char *) xmalloc(line->len);
    memset(line->styles, PLAIN, line->len);
    line->next_in_bucket = screenlines[h];
    screenlines[h] = line;
    numlines++;
  }
  x = x / scalex;
  if (x < 0) return;
  while (x >= line->len) {
    int newlen = 2 * line->len;
    line->contents = (char *) xrealloc(line->contents, newlen);
    memset(line->contents + line->len, ' ', newlen - line->len);
    line->styles = (char *) xrealloc(line->styles, newlen);
    memset(line->styles + line->len, PLAIN, newlen - line->len);
    line->len = newlen;
  }
  line->contents[x] = c;
  line->styles[x] = style;
}

static void free_bucket(l)
     struct line * l;
{
  if (l != NULL) {
    free(l->contents);
    free(l->styles);
    free_bucket(l->next_in_bucket);
    free(l);
  }
}

static void free_buckets()
{
  int i;
  for (i = 0; i < NBUCKETS; i++) free_bucket(screenlines[i]);
}

static int compare_lines(l1, l2)
     struct line ** l1, ** l2;
{
  return (**l1).ypos - (**l2).ypos;
}

void output_page()
{
  struct line ** lines;
  struct line * l;
  int i, j, k, y;
  char * p, * q, * style_p, * style_q, * s;

  device[output_device].begin_page();

  /* First, sort the lines by y coordinate */
  lines = (struct line **) malloc(numlines * sizeof(struct line *));
  if (lines == NULL) {
    printf("*** Out of memory ***\n\014");
    free_buckets();
    return;
  }
  j = 0;
  for (i = 0; i < NBUCKETS; i++)
    for (l = screenlines[i]; l != NULL; l = l->next_in_bucket)
      lines[j++] = l;
  qsort(lines, numlines, sizeof(struct line *), compare_lines);
      
  /* Output the lines */ 

  y = 0;
  for (i = 0; i < numlines; i++) {
    /* Emit blank lines to reach the current line ypos */
    while (lines[i]->ypos - y >= 3 * scaley / 2) {
      device[output_device].line(NULL, NULL, 0);
      y += scaley;
    }
    /* If next line is close to current line, attempt to merge them */
    while (i + 1 < numlines &&
           lines[i+1]->ypos - lines[i]->ypos < scaley) {
      p = lines[i]->contents;
      q = lines[i+1]->contents;
      style_p = lines[i]->styles;
      style_q = lines[i+1]->styles;
      for (j = lines[i]->len; j < lines[i+1]->len; j++)
        if (q[j] != ' ') goto cannot_merge;
      for (j = lines[i+1]->len; j < lines[i]->len; j++)
        if (p[j] != ' ') goto cannot_merge;
      k = lines[i]->len;
      if (k > lines[i+1]->len) k = lines[i+1]->len;
      for (j = 0; j < k; j++)
        if (p[j] != ' ' && q[j] != ' ') goto cannot_merge;
      /* Seems OK, do the merging */
      for (j = 0; j < k; j++)
        if (p[j] != ' ') {
          q[j] = p[j];
          style_q[j] = style_p[j];
        }
      /* Now consider next line */
      i++;
    }
  cannot_merge:
    /* Now print the current line */
    p = lines[i]->contents;
    q = p + lines[i]->len;
    while (q >= p && *--q == ' ') /*nothing*/;
    device[output_device].line(p, lines[i]->styles, q-p+1);
    /* Go on with next line */
    y = lines[i]->ypos;
  }

  device[output_device].end_page();
  free(lines);
  free_buckets();
}

