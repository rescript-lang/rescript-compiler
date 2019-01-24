#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "io.h"
#include "dvi.h"
#include "output.h"

#define SEEK_CUR 1

int h, v, w, x, y, z, sp;
int currfont;
int encoding;
int style;

#define FONT_NAME_SIZE 31
#define NUM_FONTS 256

struct {
  char name[FONT_NAME_SIZE+1];
  int encoding;
  int style;
} font[NUM_FONTS];

#define TYPEWRITER 0
#define ROMAN 1
#define MATH_ITALIC 2
#define MATH_SYMBOL 3
#define MATH_EXTENSION 4
#define LINE_SEGMENTS 5
#define CIRCLE_SEGMENTS 6
#define LATEX_SYMBOLS 7

char * transcode[] = {
/* 0.......+.......1.......+.......2.......+.......3.......+.......4.......+.......5.......+.......6.......+.......7.......+....... */
/* TYPEWRITER */
  "GDTLXPSUPYO##################### !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~#",
/* ROMAN */
  "GDTLXPSUPYO*****               0'!\"#$%&'()*+,-./0123456789:;!=??@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"]^.`abcdefghijklmnopqrstuvwxyz--\"~#",
/* MATH_ITALIC */
  "GDTLXPSUPYOabgdezhtiklmnxpystupxyoeuorsp----`'><0123456789.,</>*dABCDEFGHIJKLMNOPQRSTUVWXYZ#####labcdefghijklmnopqrstuvwxyzij###",
/* MATH_SYMBOL */
  "-.x*/###+-x/.ooo==##<><>==##<><><>||####<>||####'#####/|###0RIT##ABCDEFGHIJKLMNOPQRSTUVWXYZ###########{}<>||||\\|################",
/* MATH_EXTENSION */
  "()[]||||{}<>||##()[]||||{}<>||##()[]||||{}<>||##########################################################[]||||{}################",
/* LINE_SEGMENTS */
  "/|||||  _ / /   _/ //   _ / /   __// /  _   /                   \\|||||  \\ \\ \\   _\\ \\\\   _ \\ \\   __\\\\ \\  _   \\                   ",
/* CIRCLE_SEGMENTS */
  "                                                                                                                                ",
/* LATEX_SYMBOLS */
    " <<>>                                           U#O0      ~~[]                                                                "
};

#define STACK_SIZE 100

struct { int sh, sv, sw, sx, sy, sz; } stack[STACK_SIZE];

struct known_font_struct {
  char * prefix;
  int encoding, style;
} known_fonts[] = {
  "docrm", ROMAN, PLAIN,
  "doctt", TYPEWRITER, MONOSPACED,
  "docit", ROMAN, ITALICS,
  "docbf", ROMAN, BOLD,
  "docmi", MATH_ITALIC, PLAIN,
  "cmsy", MATH_SYMBOL, PLAIN,
  "cmex", MATH_EXTENSION, PLAIN,
  "line", LINE_SEGMENTS, PLAIN,
  "lcircle", CIRCLE_SEGMENTS, PLAIN,
  "lasy", LATEX_SYMBOLS, PLAIN
};

void fontdef(input, fontnum)
     FILE * input;
     int fontnum;
{
  int a, l, i;

  assert(fontnum >= 0 && fontnum < NUM_FONTS);
  fseek(input, 12, SEEK_CUR);   /* skip c, s and d parameters */
  a = get8u(input);
  l = get8u(input);
  assert(l < FONT_NAME_SIZE);
  fseek(input, a, SEEK_CUR);    /* skip the "area" part */
  fread(font[fontnum].name, 1, l, input); /* read the font name */
  font[fontnum].name[l] = 0;
  for (i = 0;
       i < sizeof(known_fonts) / sizeof(struct known_font_struct);
       i++) {
    if (strncmp(font[fontnum].name, known_fonts[i].prefix,
                strlen(known_fonts[i].prefix)) == 0) {
      font[fontnum].encoding = known_fonts[i].encoding;
      font[fontnum].style = known_fonts[i].style;
      return;
    }
  }
  fprintf(stderr, "Warning: unknown font `%s'\n", font[fontnum].name);
  font[fontnum].encoding = ROMAN;
  font[fontnum].style = PLAIN;
}

void setfont(fontnum)
     int fontnum;
{
  currfont = fontnum;
  encoding = font[fontnum].encoding;
  style = font[fontnum].style;
}

int outchar(c)
     int c;
{
  if (c < 0 || c > 127)
    out(h, v, '#', PLAIN);
  else
    out(h, v, transcode[encoding][c], style);
  return scalex;
}

void outrule(height, width)
     int height, width;
{
  char c;
  int dx, dy;

  if (height <= 0 || width <= 0) return;
  c = height >= width ? '|' : '-';
  dy = 0;
  do {
    dx = 0;
    do {
      out(h + dx, v - dy, c, PLAIN);
      dx += scalex;
    } while (dx <= width);
    dy += scaley;
  } while (dy < height);
}

void interprete(input)
     FILE * input;
{
  int c, n, height, width, mag;

  sp = 0;
  c = get8u(input);
  n = get8u(input);
  if (c != PRE || n != 2) {
    fprintf(stderr, "File does not start with DVI preamble.\n");
    exit(2);
  }
  (void) get32s(input);
  (void) get32s(input);
  mag = get32s(input);
  scalex = SCALEX * mag / 1000;
  scaley = SCALEY * mag / 1000;
  n = get8u(input);
  fseek(input, n, SEEK_CUR);    /* skip comment */

  begin_document();

  while (1) {
    c = get8u(input);
    if (c >= SET_CHAR_0 && c <= SET_CHAR_127)
      h += outchar(c);
    else if (c >= FNT_NUM_0 && c <= FNT_NUM_63)
      setfont(c - FNT_NUM_0);
    else switch(c) {
    case SET1:
      h += outchar(get8u(input)); break;
    case SET2:
      h += outchar(get16u(input)); break;
    case SET3:
      h += outchar(get24u(input)); break;
    case SET4:
      h += outchar(get32s(input)); break;
    case SET_RULE:
      height = get32s(input);
      width = get32s(input);
      outrule(height, width);
      h += width;
      break;
    case PUT1:
      (void) outchar(get8u(input)); break;
    case PUT2:
      (void) outchar(get16u(input)); break;
    case PUT3:
      (void) outchar(get24u(input)); break;
    case PUT4:
      (void) outchar(get32s(input)); break;
    case PUT_RULE:
      height = get32s(input);
      width = get32s(input);
      outrule(height, width);
      break;
    case NOP:
      break;
    case BOP:
      clear_page();
      h = v = w = x = y = z = 0;
      sp = 0;
      fseek(input, 44, SEEK_CUR); /* skip c0...c9 and ptr to previous page */
      break;
    case EOP:
      output_page();
      break;
    case PUSH:
      assert(sp < STACK_SIZE);
      stack[sp].sh = h; stack[sp].sv = v; stack[sp].sw = w;
      stack[sp].sx = x; stack[sp].sy = y; stack[sp].sz = z;
      sp++;
      break;
    case POP:
      assert(sp > 0);
      sp--;
      h = stack[sp].sh; v = stack[sp].sv; w = stack[sp].sw;
      x = stack[sp].sx; y = stack[sp].sy; z = stack[sp].sz;
      break;
    case RIGHT1:
      h += get8s(input); break;
    case RIGHT2:
      h += get16s(input); break;
    case RIGHT3:
      h += get24s(input); break;
    case RIGHT4:
      h += get32s(input); break;
    case W0:
      h += w; break;
    case W1:
      w = get8s(input); h += w; break;
    case W2:
      w = get16s(input); h += w; break;
    case W3:
      w = get24s(input); h += w; break;
    case W4:
      w = get32s(input); h += w; break;
    case X0:
      h += x; break;
    case X1:
      x = get8s(input); h += x; break;
    case X2:
      x = get16s(input); h += x; break;
    case X3:
      x = get24s(input); h += x; break;
    case X4:
      x = get32s(input); h += x; break;
    case DOWN1:
      v += get8s(input); break;
    case DOWN2:
      v += get16s(input); break;
    case DOWN3:
      v += get24s(input); break;
    case DOWN4:
      v += get32s(input); break;
    case Y0:
      v += y; break;
    case Y1:
      y = get8s(input); v += y; break;
    case Y2:
      y = get16s(input); v += y; break;
    case Y3:
      y = get24s(input); v += y; break;
    case Y4:
      y = get32s(input); v += y; break;
    case Z0:
      v += z; break;
    case Z1:
      z = get8s(input); v += z; break;
    case Z2:
      z = get16s(input); v += z; break;
    case Z3:
      z = get24s(input); v += z; break;
    case Z4:
      z = get32s(input); v += z; break;
    case FNT1:
      setfont(get8u(input)); break;
    case FNT2:
      setfont(get16u(input)); break;
    case FNT3:
      setfont(get24u(input)); break;
    case FNT4:
      setfont(get32s(input)); break;
    case XXX1:
      n = get8u(input); fseek(input, n, SEEK_CUR); break;
    case XXX2:
      n = get16u(input); fseek(input, n, SEEK_CUR); break;
    case XXX3:
      n = get24u(input); fseek(input, n, SEEK_CUR); break;
    case XXX4:
      n = get32s(input); fseek(input, n, SEEK_CUR); break;
    case FNT_DEF1:
      fontdef(input, get8u(input)); break;
    case FNT_DEF2:
      fontdef(input, get16u(input)); break;
    case FNT_DEF3:
      fontdef(input, get24u(input)); break;
    case FNT_DEF4:
      fontdef(input, get32s(input)); break;
    case POST:
      end_document(); return;
    default:
      assert(0);
    }
  }
}
