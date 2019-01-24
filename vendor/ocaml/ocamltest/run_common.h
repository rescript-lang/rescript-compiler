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

/* Private definitions shared by both Unix and Windows process runners */

#ifndef __RUN_COMMON_H__
#define __RUN_COMMON_H__

/* is_defined(str) returns 1 iff str points to a non-empty string */
/* Otherwise returns 0 */
static int is_defined(const char_os *str)
{
  return (str != NULL) && (*str != 0);
}

static void defaultLogger(void *where, const char *format, va_list ap)
{
  vfprintf(stderr, format, ap);
}

static void mylog(Logger *logger, void *loggerData, char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  logger(loggerData, fmt, ap);
  va_end(ap);
}

static void error_with_location(
  const char *file, int line,
  const command_settings *settings,
  const char *msg, ...)
{
  va_list ap;
  Logger *logger = (settings->logger != NULL) ? settings->logger
                                              : defaultLogger;
  void *loggerData = settings->loggerData;
  va_start(ap, msg);
  mylog(logger, loggerData, "%s:%d: ", file, line);
  logger(loggerData, msg, ap);
  mylog(logger, loggerData, "\n");
  va_end(ap);
}



#endif /* __RUN_COMMON_H__ */
