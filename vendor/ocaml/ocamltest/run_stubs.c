/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Sebastien Hinderer, projet Gallium, INRIA Paris            */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Stubs to let OCaml programs use the run library */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/types.h>
#include <string.h>

#include "run.h"

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/io.h"
#include "caml/osdeps.h"

/* cstringvect: inspired by similar function in otherlibs/unix/cstringv.c */
static array cstringvect(value arg)
{
  array res;
  mlsize_t size, i;

  size = Wosize_val(arg);
  res = (array) caml_stat_alloc((size + 1) * sizeof(char_os *));
  for (i = 0; i < size; i++)
    res[i] = caml_stat_strdup_to_os(String_val(Field(arg, i)));
  res[size] = NULL;
  return res;
}

static void free_cstringvect(array v)
{
  char_os **p;
  for (p = v; *p != NULL; p++)
    caml_stat_free(*p);
  caml_stat_free(v);
}

static void logToChannel(void *voidchannel, const char *fmt, va_list ap)
{
  struct channel *channel = (struct channel *) voidchannel;
  int length, initialTextLength = 512;
  char *text = malloc(512);
  if (text == NULL) return;
  length = vsnprintf(text, initialTextLength, fmt, ap);
  if (length <= 0)
  {
    free(text);
    return;
  }
  if (length > initialTextLength)
  {
    free(text);
    text = malloc(length);
    if (text == NULL) return;
    if (vsnprintf(text, length, fmt, ap) != length) goto end;
  }
  caml_putblock(channel, text, length);
  caml_flush(channel);
end:
  free(text);
}

CAMLprim value caml_run_command(value caml_settings)
{
  int res;
  command_settings settings;

  CAMLparam1(caml_settings);
  settings.program = caml_stat_strdup_to_os(String_val(Field(caml_settings, 0)));
  settings.argv = cstringvect(Field(caml_settings, 1));
  /* settings.envp = cstringvect(Field(caml_settings, 2)); */
  settings.stdin_filename = caml_stat_strdup_to_os(String_val(Field(caml_settings, 2)));
  settings.stdout_filename = caml_stat_strdup_to_os(String_val(Field(caml_settings, 4)));
  settings.stderr_filename = caml_stat_strdup_to_os(String_val(Field(caml_settings, 4)));
  settings.append = Bool_val(Field(caml_settings, 5));
  settings.timeout = Int_val(Field(caml_settings, 6));
  settings.logger = logToChannel;
  settings.loggerData = Channel(Field(caml_settings, 7));
  res = run_command(&settings);
  caml_stat_free(settings.program);
  free_cstringvect(settings.argv);
  caml_stat_free(settings.stdin_filename);
  caml_stat_free(settings.stdout_filename);
  caml_stat_free(settings.stderr_filename);
  CAMLreturn(Val_int(res));
}
