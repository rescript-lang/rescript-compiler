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

/* Run programs with rediretions and timeouts under Unix */

#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>
#include <signal.h>

#include "run.h"
#include "run_common.h"

#define COREFILENAME "core"

static volatile int timeout_expired = 0;

#define error(msg, ...) \
error_with_location(__FILE__, __LINE__, settings, msg, ## __VA_ARGS__)

/*
  Note: the ## __VA_ARGS__ construct is gcc specific.
  For a more portable (but also more complex) solution, see
  http://stackoverflow.com/questions/20818800/variadic-macro-and-trailing-comma
*/

static void myperror_with_location(
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
  mylog(logger, loggerData, ": %s\n", strerror(errno));
  va_end(ap);
}

#define myperror(msg, ...) \
myperror_with_location(__FILE__, __LINE__, settings, msg, ## __VA_ARGS__)

/* Same remark as for the error macro. */

static void open_error_with_location(
  const char *file, int line,
  const command_settings *settings,
  const char *msg)
{
  myperror_with_location(file, line, settings, "Can not open %s", msg);
}

#define open_error(filename) \
open_error_with_location(__FILE__, __LINE__, settings, filename)

static void realpath_error_with_location(
  const char *file, int line,
  const command_settings *settings,
  const char *msg)
{
  myperror_with_location(file, line, settings, "realpath(\"%s\") failed", msg);
}

#define realpath_error(filename) \
realpath_error_with_location(__FILE__, __LINE__, settings, filename)

static void handle_alarm(int sig)
{
  timeout_expired = 1;
}

static int paths_same_file(
  const command_settings *settings, const char * path1, const char * path2)
{
  int same_file = 0;
#ifdef __GLIBC__
  char *realpath1, *realpath2;
  realpath1 = realpath(path1, NULL);
  if (realpath1 == NULL)
    realpath_error(path1);
  realpath2 = realpath(path2, NULL);
  if ( (realpath2 == NULL)  && (errno != ENOENT) )
  {
    free(realpath1);
    realpath_error(path2);
  }
#else
  char realpath1[PATH_MAX], realpath2[PATH_MAX];
  if (realpath(path1, realpath1) == NULL)
    realpath_error(path1);
    if ((realpath(path2, realpath2) == NULL) && (errno != ENOENT))
      realpath_error(path2);
#endif /* __GLIBC__ */
  if (strcmp(realpath1, realpath2) == 0)
    same_file = 1;
#ifdef __GLIBC__
  free(realpath1);
  free(realpath2);
#endif /* __GLIBC__ */
  return same_file;
}

static int run_command_child(const command_settings *settings)
{
  int res;
  int stdin_fd = -1, stdout_fd = -1, stderr_fd = -1; /* -1 = no redir */
  int inputFlags = O_RDONLY;
  int outputFlags =
    O_CREAT | O_WRONLY | (settings->append ? O_APPEND : O_TRUNC);
  int inputMode = 0400, outputMode = 0666;

  if (setpgid(0, 0) == -1) myperror("setpgid");

  if (is_defined(settings->stdin_filename))
  {
    stdin_fd = open(settings->stdin_filename, inputFlags, inputMode);
    if (stdin_fd < 0)
      open_error(settings->stdin_filename);
    if ( dup2(stdin_fd, STDIN_FILENO) == -1 )
      myperror("dup2 for stdin");
  }

  if (is_defined(settings->stdout_filename))
  {
    stdout_fd = open(settings->stdout_filename, outputFlags, outputMode);
    if (stdout_fd < 0)
      open_error(settings->stdout_filename);
    if ( dup2(stdout_fd, STDOUT_FILENO) == -1 )
      myperror("dup2 for stdout");
  }

  if (is_defined(settings->stderr_filename))
  {
    if (stdout_fd != -1)
    {
      if (paths_same_file(
        settings, settings->stdout_filename,settings->stderr_filename))
        stderr_fd = stdout_fd;
    }
    if (stderr_fd == -1)
    {
      stderr_fd = open(settings->stderr_filename, outputFlags, outputMode);
      if (stderr_fd == -1) open_error(settings->stderr_filename);
    }
    if ( dup2(stderr_fd, STDERR_FILENO) == -1 )
      myperror("dup2 for stderr");
  }

  res = execvp(settings->program, settings->argv); /* , settings->envp); */

  myperror("Cannot execute %s", settings->program);
  return res;
}

/* Handles the termination of a process. Arguments:
 * The pid of the terminated process
 * Its termination status as returned by wait(2)
 * A string giving a prefix for the core file name.
   (the file will be called prefix.pid.core but may come from a
   diffferent process)
 * Returns the code to return if this is the child process
 */
static int handle_process_termination(
  const command_settings *settings,
  pid_t pid, int status, const char *corefilename_prefix)
{
  int signal, core = 0;
  char *corestr;

  if (WIFEXITED(status)) return WEXITSTATUS(status);

  if ( !WIFSIGNALED(status) )
    error("Process %d neither terminated normally nor received a" \
          "signal!?", pid);

  /* From here we know that the process terminated due to a signal */
  signal = WTERMSIG(status);
#ifdef WCOREDUMP
  core = WCOREDUMP(status);
#endif /* WCOREDUMP */
  corestr = core ? "" : "no ";
  fprintf(stderr,
    "Process %d got signal %d(%s), %score dumped\n",
    pid, signal, strsignal(signal), corestr
  );

  if (core)
  {
    if ( access(COREFILENAME, F_OK) == -1)
      fprintf(stderr, "Could not find core file.\n");
    else {
      char corefile[strlen(corefilename_prefix) + 128];
      snprintf(corefile, sizeof(corefile),
        "%s.%d.core", corefilename_prefix, pid);
      if ( rename(COREFILENAME, corefile) == -1)
        fprintf(stderr, "The core file exists but could not be renamed.\n");
      else
        fprintf(stderr,"The core file has been renamed to %s\n", corefile);
    }
  }

  return -signal;
}

static int run_command_parent(const command_settings *settings, pid_t child_pid)
{
  int waiting = 1, status, code, child_code = 0;
  pid_t pid;

  if (settings->timeout>0)
  {
    struct sigaction action;
    action.sa_handler = handle_alarm;
    sigemptyset(&action.sa_mask);
    action.sa_flags = SA_RESETHAND;
    if (sigaction(SIGALRM, &action, NULL) == -1) myperror("sigaction");
    if (alarm(settings->timeout) == -1) myperror("alarm");
  }

  while (waiting)
  {
    pid = wait(&status);
    if (pid == -1)
    {
      switch (errno)
      {
        case EINTR:
          if ((settings->timeout > 0) && (timeout_expired))
          {
            timeout_expired = 0;
            fprintf(stderr, "Timeout expired, killing all child processes");
            if (kill(-child_pid, SIGKILL) == -1) myperror("kill");
          };
          break;
        case ECHILD:
          waiting = 0;
          break;
        default:
          myperror("wait");
      }
    } else { /* Got a pid */
      code = handle_process_termination(
        settings, pid, status, settings->program);
      if (pid == child_pid) child_code = code;
    }
  }

  return child_code;
}

int run_command(const command_settings *settings)
{
  pid_t child_pid = fork();
  if (child_pid == -1) myperror("fork");
  if (child_pid == 0) return run_command_child(settings);
  else return run_command_parent(settings, child_pid);
}
