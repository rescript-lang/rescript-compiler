#include <stdio.h>
#include <ctype.h>

char * transl[256];

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
  int inverbatim_like;
  int incaml;
  int inverbatim = 0;
  char *verbatim_end_in = "";
  char *verbatim_end_out = "";

  for (c = 0; c < 256; c++) transl[c] = NULL;
#ifdef TIE_BLANKS
  transl[' '] = "~";
  transl['\n'] = "~";
#else
  transl[' '] = "\\ ";
  transl['\n'] = "\\ ";
#endif
  transl['{'] = "{\\char123}";
  transl['}'] = "{\\char125}";
  transl['^'] = "{\\char94}";
  transl['_'] = "{\\char95}";
  transl['\\'] = "{\\char92}";
  transl['~'] = "{\\char126}";
  transl['$'] = "\\$";
  transl['&'] = "{\\char38}";
  transl['#'] = "\\#";
  transl['%'] = "\\%";
  transl['\''] = "{\\textquotesingle}";
  transl['`'] = "{\\textasciigrave}";
  inverbatim_like = 0;
  incaml = 0;
  inquote = 0;
  inverbatim = 0;

  puts ("% THIS FILE IS GENERATED.\n");

  while(fgets(line, LINE_LENGTH, stdin) != NULL) {
    if (inverbatim_like) {
      fputs(line, stdout);
      if (isprefix(line, "\\end{caml_")
          || isprefix(line, "\\end{rawhtml}")) inverbatim_like = 0;
      continue;
    }
    if (incaml) {
      fputs(line, stdout);
      if (isprefix(line, "\\endcamlexample")) incaml = 0;
      continue;
    }
    if (inverbatim){
      if (isprefix (line, verbatim_end_in)){
        fputs (verbatim_end_out, stdout);
        inverbatim = 0;
      }else{
        for (p = (unsigned char *) line; *p != 0; p++){
          c = *p;
          if (c == ' ' || c == '\n' || transl[c] == NULL){
            putchar (c);
          }else{
            fputs (transl[c], stdout);
          }
        }
      }
      continue;
    }
    if (isprefix(line, "\\begin{caml_")
        || isprefix(line, "\\begin{rawhtml}")) {
      fputs(line, stdout);
      inverbatim_like = 1;
      continue;
    }
    if (isprefix(line, "\\camlexample")) {
      fputs(line, stdout);
      incaml = 1;
      continue;
    }
    if (isprefix (line, "\\begin{verbatim}")){
      fputs ("\\begin{machineenv}", stdout);
      inverbatim = 1;
      verbatim_end_in = "\\end{verbatim}";
      verbatim_end_out = "\\end{machineenv}";
      continue;
    }
    if (isprefix (line, "\\begin{ocamldoccode}")){
      fputs ("\\begin{ocamldoccode}", stdout);
      inverbatim = 1;
      verbatim_end_in = "\\end{ocamldoccode}";
      verbatim_end_out = "\\end{ocamldoccode}";
      continue;
    }
    inverb = 0;
    for (p = (unsigned char *) line; *p != 0; p++) {
      c = *p;
      if (inverb) {
        if (c == inverb){
          inverb = 0;
        }else if (c == '\'' || c == '`'){
          fprintf (stderr, "Warning: %c found in \\verb\n", c);
        }
        putchar(c);
        continue;
      }
      switch(c) {
      case '"':
        if (inquote) {
          fputs("}}", stdout);
          inquote = 0;
        } else {
          fputs("{\\machine{", stdout);
          inquote = 1;
        }
        break;
      case '\\':
        if (inquote) {
          if (p[1] == '"' || p[1] == '\\') {
            c = p[1];
            p++;
          }
          if (transl[c] != NULL)
            fputs(transl[c], stdout);
          else
            putchar(c);
        } else if (isprefix(p, "\\verb") && p[5] != 0 && !isalpha(p[5])) {
          inverb = p[5];
          p = p + 5;
          fputs("\\verb", stdout);
          putchar(inverb);
        } else {
          putchar('\\');
        }
        break;
      default:
        if (inquote && transl[c] != NULL)
          fputs(transl[c], stdout);
        else
          putchar(c);
      }
    }
  }
  return 0;
}
