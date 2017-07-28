/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* Based on public-domain code from Berkeley Yacc */

#include "defs.h"

char *header[] =
{
  "open Parsing;;",
  "let _ = parse_error;;", /* avoid warning 33 (PR#5719) */
  0
};

char *define_tables[] =
{
  "let yytables =",
  "  { Parsing.actions=yyact;",
  "    Parsing.transl_const=yytransl_const;",
  "    Parsing.transl_block=yytransl_block;",
  "    Parsing.lhs=yylhs;",
  "    Parsing.len=yylen;",
  "    Parsing.defred=yydefred;",
  "    Parsing.dgoto=yydgoto;",
  "    Parsing.sindex=yysindex;",
  "    Parsing.rindex=yyrindex;",
  "    Parsing.gindex=yygindex;",
  "    Parsing.tablesize=yytablesize;",
  "    Parsing.table=yytable;",
  "    Parsing.check=yycheck;",
  "    Parsing.error_function=parse_error;",
  "    Parsing.names_const=yynames_const;",
  "    Parsing.names_block=yynames_block }",
  0
};

void write_section(char **section)
{
    register int i;
    register FILE *fp;

    fp = code_file;
    for (i = 0; section[i]; ++i)
    {
        ++outline;
        fprintf(fp, "%s\n", section[i]);
    }
}
