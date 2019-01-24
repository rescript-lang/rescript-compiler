#include <stdio.h>
#include "io.h"

int get16u(input)
     FILE * input;
{
  int b1 = getc(input);
  int b2 = getc(input);
  return (b1 << 8) + b2;
}
int get16s(input)
     FILE * input;
{
  int b1 = (schar) getc(input);
  int b2 = getc(input);
  return (b1 << 8) + b2;
}
int get24u(input)
     FILE * input;
{
  int b1 = getc(input);
  int b2 = getc(input);
  int b3 = getc(input);
  return (b1 << 16) + (b2 << 8) + b3;
}
int get24s(input)
     FILE * input;
{
  int b1 = (schar) getc(input);
  int b2 = getc(input);
  int b3 = getc(input);
  return (b1 << 16) + (b2 << 8) + b3;
}
int get32s(input)
     FILE * input;
{
  int b1 = (schar) getc(input);
  int b2 = getc(input);
  int b3 = getc(input);
  int b4 = getc(input);
  return (b1 << 24) + (b2 << 16) + (b3 << 8) + b4;
}

