#include <stdio.h>
#include "output.h"

void interprete(FILE *input);

char * input_name;

int main(argc, argv)
     int argc;
     char ** argv;
{
  FILE * f;
  int i;

  output_device = OUTPUT_PLAIN;
  standout_tt = 0;
  for (i = 1; i < argc && argv[i][0] == '-'; i++) {
    switch(argv[i][1]) {
    case 'p':
      output_device = OUTPUT_PRINTER; break;
    case 'r':
      output_device = OUTPUT_RTF; break;
    case 's':
      output_device = OUTPUT_STYL; break;
    case 't':
      standout_tt = 1; break;
    default:
      fprintf(stderr, "Unknown option `%s', ignored\n", argv[i]);
    }
  }
  if (i >= argc) {
    input_name = "unknown.dvi";
    interprete(stdin);
  } else {
    for (/*nothing*/; i < argc; i++) {
      f = fopen(argv[i], "r");
      if (f == NULL) {
        perror(argv[i]);
        continue;
      }
      input_name = argv[i];
      interprete(f);
      fclose(f);
    }
  }
  return 0;
}
