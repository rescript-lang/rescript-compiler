#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#if defined(_WIN32)
#include <fcntl.h>
#include <io.h>
#endif

/* A tool to read data from standard input and send it to standard
   output or standard error. */

void copyline(FILE * in, FILE * out)
{
  int c;
  do {
    c = getc(in);
    if (c == EOF) {
      fputs("<end of file>\n", out);
      break;
    }
    putc(c, out);
  } while (c != '\n');
  fflush(out);
}

/* Command language:
     i2o       copy one line from stdin to stdout
     i2e       copy one line from stdin to stderr
     o <txt>   write <txt> plus newline to stdout
     e <txt>   write <txt> plus newline to stderr
     v <var>   write value of environment variable <env> to stdout
*/

int main(int argc, char ** argv)
{
  int i;
  char * cmd;
#if defined(_WIN32)
  _setmode(_fileno(stdin), _O_BINARY);
  _setmode(_fileno(stdout), _O_BINARY);
  _setmode(_fileno(stderr), _O_BINARY);
#endif
  i = 1;
  while (i < argc) {
    cmd = argv[i];
    if (strcmp(cmd, "i2o") == 0) {
      copyline(stdin, stdout);
      i++;
    } else if (strcmp(cmd, "i2e") == 0) {
      copyline(stdin, stderr);
      i++;
    } else if (strcmp(cmd, "o") == 0 && i + 1 < argc) {
      fputs(argv[i + 1], stdout);
      fputc('\n', stdout);
      fflush(stdout);
      i += 2;
    } else if (strcmp(cmd, "e") == 0 && i + 1 < argc) {
      fputs(argv[i + 1], stderr);
      fputc('\n', stderr);
      fflush(stderr);
      i += 2;
    } else if (strcmp(cmd, "v") == 0 && i + 1 < argc) {
      char * v = getenv(argv[i + 1]);
      fputs((v == NULL ? "<no such variable>" : v), stdout);
      fputc('\n', stdout);
      fflush(stdout);
      i += 2;
    } else {
      fputs("<bad argument>\n", stderr);
      return 2;
    }
  }
  return 0;
}
