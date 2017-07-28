/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Contributed by Sylvain Le Gall for Lexifi                          */
/*                                                                     */
/*  Copyright 2008 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include "winworker.h"
#include <stdio.h>
#include "windbug.h"
#include "winlist.h"

/* This constant define the maximum number of objects that
 * can be handle by a SELECTDATA.
 * It takes the following parameters into account:
 * - limitation on number of objects is mostly due to limitation
 *   a WaitForMultipleObjects
 * - there is always an event "hStop" to watch
 *
 * This lead to pick the following value as the biggest possible
 * value
 */
#define MAXIMUM_SELECT_OBJECTS (MAXIMUM_WAIT_OBJECTS - 1)

/* Manage set of handle */
typedef struct _SELECTHANDLESET {
  LPHANDLE lpHdl;
  DWORD    nMax;
  DWORD    nLast;
} SELECTHANDLESET;

typedef SELECTHANDLESET *LPSELECTHANDLESET;

void handle_set_init (LPSELECTHANDLESET hds, LPHANDLE lpHdl, DWORD max)
{
  DWORD i;

  hds->lpHdl = lpHdl;
  hds->nMax  = max;
  hds->nLast = 0;

  /* Set to invalid value every entry of the handle */
  for (i = 0; i < hds->nMax; i++)
  {
    hds->lpHdl[i] = INVALID_HANDLE_VALUE;
  };
}

void handle_set_add (LPSELECTHANDLESET hds, HANDLE hdl)
{
  LPSELECTHANDLESET res;

  if (hds->nLast < hds->nMax)
  {
    hds->lpHdl[hds->nLast] = hdl;
    hds->nLast++;
  }

  DEBUG_PRINT("Adding handle %x to set %x", hdl, hds);
}

BOOL handle_set_mem (LPSELECTHANDLESET hds, HANDLE hdl)
{
  BOOL  res;
  DWORD i;

  res = FALSE;
  for (i = 0; !res && i < hds->nLast; i++)
  {
    res = (hds->lpHdl[i] == hdl);
  }

  return res;
}

void handle_set_reset (LPSELECTHANDLESET hds)
{
  DWORD i;

  for (i = 0; i < hds->nMax; i++)
  {
    hds->lpHdl[i] = INVALID_HANDLE_VALUE;
  }
  hds->nMax  = 0;
  hds->nLast = 0;
  hds->lpHdl = NULL;
}

/* Data structure for handling select */

typedef enum _SELECTHANDLETYPE {
  SELECT_HANDLE_NONE = 0,
  SELECT_HANDLE_DISK,
  SELECT_HANDLE_CONSOLE,
  SELECT_HANDLE_PIPE,
  SELECT_HANDLE_SOCKET,
} SELECTHANDLETYPE;

typedef enum _SELECTMODE {
  SELECT_MODE_NONE = 0,
  SELECT_MODE_READ = 1,
  SELECT_MODE_WRITE = 2,
  SELECT_MODE_EXCEPT = 4,
} SELECTMODE;

typedef enum _SELECTSTATE {
  SELECT_STATE_NONE = 0,
  SELECT_STATE_INITFAILED,
  SELECT_STATE_ERROR,
  SELECT_STATE_SIGNALED
} SELECTSTATE;

typedef enum _SELECTTYPE {
  SELECT_TYPE_NONE = 0,
  SELECT_TYPE_STATIC,       /* Result is known without running anything */
  SELECT_TYPE_CONSOLE_READ, /* Reading data on console */
  SELECT_TYPE_PIPE_READ,    /* Reading data on pipe */
  SELECT_TYPE_SOCKET        /* Classic select */
} SELECTTYPE;

/* Data structure for results */
typedef struct _SELECTRESULT {
  LIST       lst;
  SELECTMODE EMode;
  int        lpOrigIdx;
} SELECTRESULT;

typedef SELECTRESULT *LPSELECTRESULT;

/* Data structure for query */
typedef struct _SELECTQUERY {
  LIST         lst;
  SELECTMODE   EMode;
  HANDLE       hFileDescr;
  int          lpOrigIdx;
  unsigned int uFlagsFd; /* Copy of filedescr->flags_fd */
} SELECTQUERY;

typedef SELECTQUERY *LPSELECTQUERY;

typedef struct _SELECTDATA {
  LIST             lst;
  SELECTTYPE       EType;
  /* Sockets may generate a result for all three lists from one single query object
   */
  SELECTRESULT     aResults[MAXIMUM_SELECT_OBJECTS * 3];
  DWORD            nResultsCount;
  /* Data following are dedicated to APC like call, they
     will be initialized if required.
     */
  WORKERFUNC       funcWorker;
  SELECTQUERY      aQueries[MAXIMUM_SELECT_OBJECTS];
  DWORD            nQueriesCount;
  SELECTSTATE      EState;
  DWORD            nError;
  LPWORKER         lpWorker;
} SELECTDATA;

typedef SELECTDATA *LPSELECTDATA;

/* Get error status if associated condition is false */
static BOOL check_error(LPSELECTDATA lpSelectData, BOOL bFailed)
{
  if (bFailed && lpSelectData->nError == 0)
  {
    lpSelectData->EState = SELECT_STATE_ERROR;
    lpSelectData->nError = GetLastError();
  }
  return bFailed;
}

/* Create data associated with a  select operation */
LPSELECTDATA select_data_new (LPSELECTDATA lpSelectData, SELECTTYPE EType)
{
  /* Allocate the data structure */
  LPSELECTDATA res;
  DWORD        i;

  res = (LPSELECTDATA)caml_stat_alloc(sizeof(SELECTDATA));

  /* Init common data */
  list_init((LPLIST)res);
  list_next_set((LPLIST)res, (LPLIST)lpSelectData);
  res->EType         = EType;
  res->nResultsCount = 0;


  /* Data following are dedicated to APC like call, they
     will be initialized if required. For now they are set to
     invalid values.
     */
  res->funcWorker    = NULL;
  res->nQueriesCount = 0;
  res->EState        = SELECT_STATE_NONE;
  res->nError        = 0;
  res->lpWorker  = NULL;

  return res;
}

/* Free select data */
void select_data_free (LPSELECTDATA lpSelectData)
{
  DWORD i;

  DEBUG_PRINT("Freeing data of %x", lpSelectData);

  /* Free APC related data, if they exists */
  if (lpSelectData->lpWorker != NULL)
  {
    worker_job_finish(lpSelectData->lpWorker);
    lpSelectData->lpWorker = NULL;
  };

  /* Make sure results/queries cannot be accessed */
  lpSelectData->nResultsCount = 0;
  lpSelectData->nQueriesCount = 0;

  caml_stat_free(lpSelectData);
}

/* Add a result to select data, return zero if something goes wrong. */
DWORD select_data_result_add (LPSELECTDATA lpSelectData, SELECTMODE EMode, int lpOrigIdx)
{
  DWORD res;
  DWORD i;

  res = 0;
  if (lpSelectData->nResultsCount < MAXIMUM_SELECT_OBJECTS * 3)
  {
    i = lpSelectData->nResultsCount;
    lpSelectData->aResults[i].EMode  = EMode;
    lpSelectData->aResults[i].lpOrigIdx = lpOrigIdx;
    lpSelectData->nResultsCount++;
    res = 1;
  }

  return res;
}

/* Add a query to select data, return zero if something goes wrong */
DWORD select_data_query_add (LPSELECTDATA lpSelectData,
                             SELECTMODE EMode,
                             HANDLE hFileDescr,
                             int lpOrigIdx,
                             unsigned int uFlagsFd)
{
  DWORD res;
  DWORD i;

  res = 0;
  if (lpSelectData->nQueriesCount < MAXIMUM_SELECT_OBJECTS)
  {
    i = lpSelectData->nQueriesCount;
    lpSelectData->aQueries[i].EMode      = EMode;
    lpSelectData->aQueries[i].hFileDescr = hFileDescr;
    lpSelectData->aQueries[i].lpOrigIdx  = lpOrigIdx;
    lpSelectData->aQueries[i].uFlagsFd   = uFlagsFd;
    lpSelectData->nQueriesCount++;
    res = 1;
  }

  return res;
}

/* Search for a job that has available query slots and that match provided type.
 * If none is found, create a new one. Return the corresponding SELECTDATA, and
 * update provided SELECTDATA head, if required.
 */
LPSELECTDATA select_data_job_search (LPSELECTDATA *lppSelectData, SELECTTYPE EType)
{
  LPSELECTDATA res;

  res = NULL;

  /* Search for job */
  DEBUG_PRINT("Searching an available job for type %d", EType);
  res = *lppSelectData;
  while (
      res != NULL
      && !(
        res->EType == EType
        && res->nQueriesCount < MAXIMUM_SELECT_OBJECTS
        )
      )
  {
    res = LIST_NEXT(LPSELECTDATA, res);
  }

  /* No matching job found, create one */
  if (res == NULL)
  {
    DEBUG_PRINT("No job for type %d found, create one", EType);
    res = select_data_new(*lppSelectData, EType);
    *lppSelectData = res;
  }

  return res;
}

/***********************/
/*      Console        */
/***********************/

void read_console_poll(HANDLE hStop, void *_data)
{
  HANDLE events[2];
  INPUT_RECORD record;
  DWORD waitRes;
  DWORD n;
  LPSELECTDATA  lpSelectData;
  LPSELECTQUERY lpQuery;

  DEBUG_PRINT("Waiting for data on console");

  record;
  waitRes = 0;
  n = 0;
  lpSelectData = (LPSELECTDATA)_data;
  lpQuery = &(lpSelectData->aQueries[0]);

  events[0] = hStop;
  events[1] = lpQuery->hFileDescr;
  while (lpSelectData->EState == SELECT_STATE_NONE)
  {
    waitRes = WaitForMultipleObjects(2, events, FALSE, INFINITE);
    if (waitRes == WAIT_OBJECT_0 || check_error(lpSelectData, waitRes == WAIT_FAILED))
    {
      /* stop worker event or error */
      break;
    }
    /* console event */
    if (check_error(lpSelectData, PeekConsoleInput(lpQuery->hFileDescr, &record, 1, &n) == 0))
    {
      break;
    }
    /* check for ASCII keypress only */
    if (record.EventType == KEY_EVENT &&
      record.Event.KeyEvent.bKeyDown &&
      record.Event.KeyEvent.uChar.AsciiChar != 0)
    {
      select_data_result_add(lpSelectData, lpQuery->EMode, lpQuery->lpOrigIdx);
      lpSelectData->EState = SELECT_STATE_SIGNALED;
      break;
    }
    else
    {
      /* discard everything else and try again */
      if (check_error(lpSelectData, ReadConsoleInput(lpQuery->hFileDescr, &record, 1, &n) == 0))
      {
        break;
      }
    }
  };
}

/* Add a function to monitor console input */
LPSELECTDATA read_console_poll_add (LPSELECTDATA lpSelectData,
                                    SELECTMODE EMode,
                                    HANDLE hFileDescr,
                                    int lpOrigIdx,
                                    unsigned int uFlagsFd)
{
  LPSELECTDATA res;

  res = select_data_new(lpSelectData, SELECT_TYPE_CONSOLE_READ);
  res->funcWorker = read_console_poll;
  select_data_query_add(res, SELECT_MODE_READ, hFileDescr, lpOrigIdx, uFlagsFd);

  return res;
}

/***********************/
/*        Pipe         */
/***********************/

/* Monitor a pipe for input */
void read_pipe_poll (HANDLE hStop, void *_data)
{
  DWORD         res;
  DWORD         event;
  DWORD         n;
  LPSELECTQUERY iterQuery;
  LPSELECTDATA  lpSelectData;
  DWORD         i;
  DWORD         wait;

  /* Poll pipe */
  event = 0;
  n = 0;
  lpSelectData = (LPSELECTDATA)_data;
  wait = 1;

  DEBUG_PRINT("Checking data pipe");
  while (lpSelectData->EState == SELECT_STATE_NONE)
  {
    for (i = 0; i < lpSelectData->nQueriesCount; i++)
    {
      iterQuery = &(lpSelectData->aQueries[i]);
      res = PeekNamedPipe(
          iterQuery->hFileDescr,
          NULL,
          0,
          NULL,
          &n,
          NULL);
      if (check_error(lpSelectData,
            (res == 0) &&
            (GetLastError() != ERROR_BROKEN_PIPE)))
      {
        break;
      };

      if ((n > 0) || (res == 0))
      {
        lpSelectData->EState = SELECT_STATE_SIGNALED;
        select_data_result_add(lpSelectData, iterQuery->EMode, iterQuery->lpOrigIdx);
      };
    };

    /* Alas, nothing except polling seems to work for pipes.
       Check the state & stop_worker_event every 10 ms
     */
    if (lpSelectData->EState == SELECT_STATE_NONE)
    {
      event = WaitForSingleObject(hStop, wait);

      /* Fast start: begin to wait 1, 2, 4, 8 and then 10 ms.
       * If we are working with the output of a program there is
       * a chance that one of the 4 first calls succeed.
       */
      wait = 2 * wait;
      if (wait > 10)
      {
        wait = 10;
      };
      if (event == WAIT_OBJECT_0 || check_error(lpSelectData, event == WAIT_FAILED))
      {
        break;
      }
    }
  }
  DEBUG_PRINT("Finish checking data on pipe");
}

/* Add a function to monitor pipe input */
LPSELECTDATA read_pipe_poll_add (LPSELECTDATA lpSelectData,
                                 SELECTMODE EMode,
                                 HANDLE hFileDescr,
                                 int lpOrigIdx,
                                 unsigned int uFlagsFd)
{
  LPSELECTDATA res;
  LPSELECTDATA hd;

  hd = lpSelectData;
  /* Polling pipe is a non blocking operation by default. This means that each
     worker can handle many pipe. We begin to try to find a worker that is
     polling pipe, but for which there is under the limit of pipe per worker.
     */
  DEBUG_PRINT("Searching an available worker handling pipe");
  res = select_data_job_search(&hd, SELECT_TYPE_PIPE_READ);

  /* Add a new pipe to poll */
  res->funcWorker = read_pipe_poll;
  select_data_query_add(res, EMode, hFileDescr, lpOrigIdx, uFlagsFd);

  return hd;
}

/***********************/
/*       Socket        */
/***********************/

/* Monitor socket */
void socket_poll (HANDLE hStop, void *_data)
{
  LPSELECTDATA   lpSelectData;
  LPSELECTQUERY    iterQuery;
  HANDLE           aEvents[MAXIMUM_SELECT_OBJECTS];
  DWORD            nEvents;
  long             maskEvents;
  DWORD            i;
  u_long           iMode;
  SELECTMODE       mode;
  WSANETWORKEVENTS events;

  lpSelectData = (LPSELECTDATA)_data;

  DEBUG_PRINT("Worker has %d queries to service", lpSelectData->nQueriesCount);
  for (nEvents = 0; nEvents < lpSelectData->nQueriesCount; nEvents++)
  {
    iterQuery = &(lpSelectData->aQueries[nEvents]);
    aEvents[nEvents] = CreateEvent(NULL, TRUE, FALSE, NULL);
    maskEvents = 0;
    mode = iterQuery->EMode;
    if ((mode & SELECT_MODE_READ) != 0)
    {
      DEBUG_PRINT("Polling read for %d", iterQuery->hFileDescr);
      maskEvents |= FD_READ | FD_ACCEPT | FD_CLOSE;
    }
    if ((mode & SELECT_MODE_WRITE) != 0)
    {
      DEBUG_PRINT("Polling write for %d", iterQuery->hFileDescr);
      maskEvents |= FD_WRITE | FD_CONNECT | FD_CLOSE;
    }
    if ((mode & SELECT_MODE_EXCEPT) != 0)
    {
      DEBUG_PRINT("Polling exceptions for %d", iterQuery->hFileDescr);
      maskEvents |= FD_OOB;
    }

    check_error(lpSelectData,
        WSAEventSelect(
          (SOCKET)(iterQuery->hFileDescr),
          aEvents[nEvents],
          maskEvents) == SOCKET_ERROR);
  }

  /* Add stop event */
  aEvents[nEvents]  = hStop;
  nEvents++;

  if (lpSelectData->nError == 0)
  {
    check_error(lpSelectData,
        WaitForMultipleObjects(
          nEvents,
          aEvents,
          FALSE,
          INFINITE) == WAIT_FAILED);
  };

  if (lpSelectData->nError == 0)
  {
    for (i = 0; i < lpSelectData->nQueriesCount; i++)
    {
      iterQuery = &(lpSelectData->aQueries[i]);
      if (WaitForSingleObject(aEvents[i], 0) == WAIT_OBJECT_0)
      {
        DEBUG_PRINT("Socket %d has pending events", (i - 1));
        if (iterQuery != NULL)
        {
          /* Find out what kind of events were raised
           */
          if (WSAEnumNetworkEvents((SOCKET)(iterQuery->hFileDescr), aEvents[i], &events) == 0)
          {
            if ((iterQuery->EMode & SELECT_MODE_READ) != 0 && (events.lNetworkEvents & (FD_READ | FD_ACCEPT | FD_CLOSE)) != 0)
            {
              select_data_result_add(lpSelectData, SELECT_MODE_READ, iterQuery->lpOrigIdx);
            }
            if ((iterQuery->EMode & SELECT_MODE_WRITE) != 0 && (events.lNetworkEvents & (FD_WRITE | FD_CONNECT | FD_CLOSE)) != 0)
            {
              select_data_result_add(lpSelectData, SELECT_MODE_WRITE, iterQuery->lpOrigIdx);
            }
            if ((iterQuery->EMode & SELECT_MODE_EXCEPT) != 0 && (events.lNetworkEvents & FD_OOB) != 0)
            {
              select_data_result_add(lpSelectData, SELECT_MODE_EXCEPT, iterQuery->lpOrigIdx);
            }
          }
        }
      }
      /* WSAEventSelect() automatically sets socket to nonblocking mode.
         Restore the blocking one. */
      if (iterQuery->uFlagsFd & FLAGS_FD_IS_BLOCKING)
      {
        DEBUG_PRINT("Restore a blocking socket");
        iMode = 0;
        check_error(lpSelectData,
          WSAEventSelect((SOCKET)(iterQuery->hFileDescr), aEvents[i], 0) != 0 ||
          ioctlsocket((SOCKET)(iterQuery->hFileDescr), FIONBIO, &iMode) != 0);
      }
      else
      {
        check_error(lpSelectData,
          WSAEventSelect((SOCKET)(iterQuery->hFileDescr), aEvents[i], 0) != 0);
      };

      CloseHandle(aEvents[i]);
      aEvents[i] = INVALID_HANDLE_VALUE;
    }
  }
}

/* Add a function to monitor socket */
LPSELECTDATA socket_poll_add (LPSELECTDATA lpSelectData,
                              SELECTMODE EMode,
                              HANDLE hFileDescr,
                              int lpOrigIdx,
                              unsigned int uFlagsFd)
{
  LPSELECTDATA res;
  LPSELECTDATA candidate;
  long i;
  LPSELECTQUERY aQueries;

  res = lpSelectData;
  candidate = NULL;
  aQueries = NULL;

  /* Polling socket can be done mulitple handle at the same time. You just
     need one worker to use it. Try to find if there is already a worker
     handling this kind of request.
     Only one event can be associated with a given socket which means that if a socket
     is in more than one of the fd_sets then we have to find that particular query and update
     EMode with the additional flag.
     */
  DEBUG_PRINT("Scanning list of worker to find one that already handle socket");
  /* Search for job */
  DEBUG_PRINT("Searching for an available job for type %d for descriptor %d", SELECT_TYPE_SOCKET, hFileDescr);
  while (res != NULL)
  {
    if (res->EType == SELECT_TYPE_SOCKET)
    {
      i = res->nQueriesCount - 1;
      aQueries = res->aQueries;
      while (i >= 0 && aQueries[i].hFileDescr != hFileDescr)
      {
        i--;
      }
      /* If we didn't find the socket but this worker has available slots, store it
       */
      if (i < 0)
      {
        if ( res->nQueriesCount < MAXIMUM_SELECT_OBJECTS)
        {
          candidate = res;
        }
        res = LIST_NEXT(LPSELECTDATA, res);
      }
      else
      {
        /* Previous socket query located -- we're finished
         */
        aQueries = &aQueries[i];
        break;
      }
    }
    else
    {
      res = LIST_NEXT(LPSELECTDATA, res);
    }
  }

  if (res == NULL)
  {
    res = candidate;

    /* No matching job found, create one */
    if (res == NULL)
    {
      DEBUG_PRINT("No job for type %d found, create one", SELECT_TYPE_SOCKET);
      res = select_data_new(lpSelectData, SELECT_TYPE_SOCKET);
      res->funcWorker = socket_poll;
      res->nQueriesCount = 1;
      aQueries = &res->aQueries[0];
    }
    else
    {
      aQueries = &(res->aQueries[res->nQueriesCount++]);
    }
    aQueries->EMode = EMode;
    aQueries->hFileDescr = hFileDescr;
    aQueries->lpOrigIdx = lpOrigIdx;
    aQueries->uFlagsFd = uFlagsFd;
    DEBUG_PRINT("Socket %x added", hFileDescr);
  }
  else
  {
    aQueries->EMode |= EMode;
    DEBUG_PRINT("Socket %x updated to %d", hFileDescr, aQueries->EMode);
  }

  return res;
}

/***********************/
/*       Static        */
/***********************/

/* Add a static result */
LPSELECTDATA static_poll_add (LPSELECTDATA lpSelectData,
                              SELECTMODE EMode,
                              HANDLE hFileDescr,
                              int lpOrigIdx,
                              unsigned int uFlagsFd)
{
  LPSELECTDATA res;
  LPSELECTDATA hd;

  /* Look for an already initialized static element */
  hd = lpSelectData;
  res = select_data_job_search(&hd, SELECT_TYPE_STATIC);

  /* Add a new query/result */
  select_data_query_add(res, EMode, hFileDescr, lpOrigIdx, uFlagsFd);
  select_data_result_add(res, EMode, lpOrigIdx);

  return hd;
}

/********************************/
/* Generic select data handling */
/********************************/

/* Guess handle type */
static SELECTHANDLETYPE get_handle_type(value fd)
{
  DWORD            mode;
  SELECTHANDLETYPE res;

  CAMLparam1(fd);

  mode = 0;
  res = SELECT_HANDLE_NONE;

  if (Descr_kind_val(fd) == KIND_SOCKET)
  {
    res = SELECT_HANDLE_SOCKET;
  }
  else
  {
    switch(GetFileType(Handle_val(fd)))
    {
      case FILE_TYPE_DISK:
        res = SELECT_HANDLE_DISK;
        break;

      case FILE_TYPE_CHAR: /* character file or a console */
        if (GetConsoleMode(Handle_val(fd), &mode) != 0)
        {
          res = SELECT_HANDLE_CONSOLE;
        }
        else
        {
          res = SELECT_HANDLE_NONE;
        };
        break;

      case FILE_TYPE_PIPE: /* a named or an anonymous pipe (socket already handled) */
        res = SELECT_HANDLE_PIPE;
        break;
    };
  };

  CAMLreturnT(SELECTHANDLETYPE, res);
}

/* Choose what to do with given data */
LPSELECTDATA select_data_dispatch (LPSELECTDATA lpSelectData, SELECTMODE EMode, value fd, int lpOrigIdx)
{
  LPSELECTDATA    res;
  HANDLE          hFileDescr;
  struct sockaddr sa;
  int             sa_len;
  BOOL            alreadyAdded;
  unsigned int    uFlagsFd;

  CAMLparam1(fd);

  res          = lpSelectData;
  hFileDescr   = Handle_val(fd);
  sa_len       = sizeof(sa);
  alreadyAdded = FALSE;
  uFlagsFd     = Flags_fd_val(fd);

  DEBUG_PRINT("Begin dispatching handle %x", hFileDescr);

  DEBUG_PRINT("Waiting for %d on handle %x", EMode, hFileDescr);

  /* There is only 2 way to have except mode: transmission of OOB data through
     a socket TCP/IP and through a strange interaction with a TTY.
     With windows, we only consider the TCP/IP except condition
  */
  switch(get_handle_type(fd))
  {
    case SELECT_HANDLE_DISK:
      DEBUG_PRINT("Handle %x is a disk handle", hFileDescr);
      /* Disk is always ready in read/write operation */
      if (EMode == SELECT_MODE_READ || EMode == SELECT_MODE_WRITE)
      {
        res = static_poll_add(res, EMode, hFileDescr, lpOrigIdx, uFlagsFd);
      };
      break;

    case SELECT_HANDLE_CONSOLE:
      DEBUG_PRINT("Handle %x is a console handle", hFileDescr);
      /* Console is always ready in write operation, need to check for read. */
      if (EMode == SELECT_MODE_READ)
      {
        res = read_console_poll_add(res, EMode, hFileDescr, lpOrigIdx, uFlagsFd);
      }
      else if (EMode == SELECT_MODE_WRITE)
      {
        res = static_poll_add(res, EMode, hFileDescr, lpOrigIdx, uFlagsFd);
      };
      break;

    case SELECT_HANDLE_PIPE:
      DEBUG_PRINT("Handle %x is a pipe handle", hFileDescr);
      /* Console is always ready in write operation, need to check for read. */
      if (EMode == SELECT_MODE_READ)
      {
        DEBUG_PRINT("Need to check availability of data on pipe");
        res = read_pipe_poll_add(res, EMode, hFileDescr, lpOrigIdx, uFlagsFd);
      }
      else if (EMode == SELECT_MODE_WRITE)
      {
        DEBUG_PRINT("No need to check availability of data on pipe, write operation always possible");
        res = static_poll_add(res, EMode, hFileDescr, lpOrigIdx, uFlagsFd);
      };
      break;

    case SELECT_HANDLE_SOCKET:
      DEBUG_PRINT("Handle %x is a socket handle", hFileDescr);
      if (getsockname((SOCKET)hFileDescr, &sa, &sa_len) == SOCKET_ERROR)
      {
        if (WSAGetLastError() == WSAEINVAL)
        {
          /* Socket is not bound */
          DEBUG_PRINT("Socket is not connected");
          if (EMode == SELECT_MODE_WRITE || EMode == SELECT_MODE_READ)
          {
            res = static_poll_add(res, EMode, hFileDescr, lpOrigIdx, uFlagsFd);
            alreadyAdded = TRUE;
          }
        }
      }
      if (!alreadyAdded)
      {
        res = socket_poll_add(res, EMode, hFileDescr, lpOrigIdx, uFlagsFd);
      }
      break;

    default:
      DEBUG_PRINT("Handle %x is unknown", hFileDescr);
      win32_maperr(ERROR_INVALID_HANDLE);
      uerror("select", Nothing);
      break;
  };

  DEBUG_PRINT("Finish dispatching handle %x", hFileDescr);

  CAMLreturnT(LPSELECTDATA, res);
}

static DWORD caml_list_length (value lst)
{
  DWORD res;

  CAMLparam1 (lst);
  CAMLlocal1 (l);

  for (res = 0, l = lst; l != Val_int(0); l = Field(l, 1), res++)
  { }

  CAMLreturnT(DWORD, res);
}

static value find_handle(LPSELECTRESULT iterResult, value readfds, value writefds, value exceptfds)
{
  CAMLparam3(readfds, writefds, exceptfds);
  CAMLlocal2(result, list);
  int i;

  switch( iterResult->EMode )
  {
    case SELECT_MODE_READ:
      list = readfds;
      break;
    case SELECT_MODE_WRITE:
      list = writefds;
      break;
    case SELECT_MODE_EXCEPT:
      list = exceptfds;
      break;
  };

  for(i=0; list != Val_unit && i < iterResult->lpOrigIdx; ++i )
  {
    list = Field(list, 1);
  }

  if (list == Val_unit)
    failwith ("select.c: original file handle not found");

  result = Field(list, 0);

  CAMLreturn( result );
}

#define MAX(a, b) ((a) > (b) ? (a) : (b))

/* Convert fdlist to an fd_set if all the handles in fdlist are sockets and return 0.
 * Returns 1 if a non-socket value is encountered.
 */
static int fdlist_to_fdset(value fdlist, fd_set *fdset)
{
  value l, c;
  FD_ZERO(fdset);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    c = Field(l, 0);
    if (Descr_kind_val(c) == KIND_SOCKET) {
      FD_SET(Socket_val(c), fdset);
    } else {
      DEBUG_PRINT("Non socket value encountered");
      return 0;
    }
  }
  return 1;
}

static value fdset_to_fdlist(value fdlist, fd_set *fdset)
{
  value res = Val_int(0);
  Begin_roots2(fdlist, res)
    for (/*nothing*/; fdlist != Val_int(0); fdlist = Field(fdlist, 1)) {
      value s = Field(fdlist, 0);
      if (FD_ISSET(Socket_val(s), fdset)) {
        value newres = alloc_small(2, 0);
        Field(newres, 0) = s;
        Field(newres, 1) = res;
        res = newres;
      }
    }
  End_roots();
  return res;
}

CAMLprim value unix_select(value readfds, value writefds, value exceptfds, value timeout)
{
  /* Event associated to handle */
  DWORD   nEventsCount;
  DWORD   nEventsMax;
  HANDLE *lpEventsDone;

  /* Data for all handles */
  LPSELECTDATA lpSelectData;
  LPSELECTDATA iterSelectData;

  /* Iterator for results */
  LPSELECTRESULT iterResult;

  /* Iterator */
  DWORD i;

  /* Error status */
  DWORD err;

  /* Time to wait */
  DWORD milliseconds;

  /* Is there static select data */
  BOOL  hasStaticData = FALSE;

  /* Wait return */
  DWORD waitRet;

  /* Set of handle */
  SELECTHANDLESET hds;
  DWORD           hdsMax;
  LPHANDLE        hdsData;

  /* Length of each list */
  DWORD readfds_len;
  DWORD writefds_len;
  DWORD exceptfds_len;

  CAMLparam4 (readfds, writefds, exceptfds, timeout);
  CAMLlocal5 (read_list, write_list, except_list, res, l);
  CAMLlocal1 (fd);

  fd_set read, write, except;
  double tm;
  struct timeval tv;
  struct timeval * tvp;

  DEBUG_PRINT("in select");

  err = 0;
  tm = Double_val(timeout);
  if (readfds == Val_int(0) && writefds == Val_int(0) && exceptfds == Val_int(0)) {
    DEBUG_PRINT("nothing to do");
    if ( tm > 0.0 ) {
      enter_blocking_section();
      Sleep( (int)(tm * 1000));
      leave_blocking_section();
    }
    read_list = write_list = except_list = Val_int(0);
  } else {
    if (fdlist_to_fdset(readfds, &read) && fdlist_to_fdset(writefds, &write) && fdlist_to_fdset(exceptfds, &except)) {
      DEBUG_PRINT("only sockets to select on, using classic select");
      if (tm < 0.0) {
        tvp = (struct timeval *) NULL;
      } else {
        tv.tv_sec = (int) tm;
        tv.tv_usec = (int) (1e6 * (tm - (int) tm));
        tvp = &tv;
      }
      enter_blocking_section();
      if (select(FD_SETSIZE, &read, &write, &except, tvp) == -1) {
        err = WSAGetLastError();
        DEBUG_PRINT("Error %ld occurred", err);
      }
      leave_blocking_section();
      if (err) {
        DEBUG_PRINT("Error %ld occurred", err);
        win32_maperr(err);
        uerror("select", Nothing);
      }
      read_list = fdset_to_fdlist(readfds, &read);
      write_list = fdset_to_fdlist(writefds, &write);
      except_list = fdset_to_fdlist(exceptfds, &except);
    } else {
      nEventsCount   = 0;
      nEventsMax     = 0;
      lpEventsDone   = NULL;
      lpSelectData   = NULL;
      iterSelectData = NULL;
      iterResult     = NULL;
      hasStaticData  = 0;
      waitRet        = 0;
      readfds_len    = caml_list_length(readfds);
      writefds_len   = caml_list_length(writefds);
      exceptfds_len  = caml_list_length(exceptfds);
      hdsMax         = MAX(readfds_len, MAX(writefds_len, exceptfds_len));

      hdsData = (HANDLE *)caml_stat_alloc(sizeof(HANDLE) * hdsMax);

      if (tm >= 0.0)
        {
          milliseconds = 1000 * tm;
          DEBUG_PRINT("Will wait %d ms", milliseconds);
        }
      else
        {
          milliseconds = INFINITE;
        }


      /* Create list of select data, based on the different list of fd to watch */
      DEBUG_PRINT("Dispatch read fd");
      handle_set_init(&hds, hdsData, hdsMax);
      i=0;
      for (l = readfds; l != Val_int(0); l = Field(l, 1))
        {
          fd = Field(l, 0);
          if (!handle_set_mem(&hds, Handle_val(fd)))
            {
              handle_set_add(&hds, Handle_val(fd));
              lpSelectData = select_data_dispatch(lpSelectData, SELECT_MODE_READ, fd, i++);
            }
          else
            {
              DEBUG_PRINT("Discarding handle %x which is already monitor for read", Handle_val(fd));
            }
        }
      handle_set_reset(&hds);

      DEBUG_PRINT("Dispatch write fd");
      handle_set_init(&hds, hdsData, hdsMax);
      i=0;
      for (l = writefds; l != Val_int(0); l = Field(l, 1))
        {
          fd = Field(l, 0);
          if (!handle_set_mem(&hds, Handle_val(fd)))
            {
              handle_set_add(&hds, Handle_val(fd));
              lpSelectData = select_data_dispatch(lpSelectData, SELECT_MODE_WRITE, fd, i++);
            }
          else
            {
              DEBUG_PRINT("Discarding handle %x which is already monitor for write", Handle_val(fd));
            }
        }
      handle_set_reset(&hds);

      DEBUG_PRINT("Dispatch exceptional fd");
      handle_set_init(&hds, hdsData, hdsMax);
      i=0;
      for (l = exceptfds; l != Val_int(0); l = Field(l, 1))
        {
          fd = Field(l, 0);
          if (!handle_set_mem(&hds, Handle_val(fd)))
            {
              handle_set_add(&hds, Handle_val(fd));
              lpSelectData = select_data_dispatch(lpSelectData, SELECT_MODE_EXCEPT, fd, i++);
            }
          else
            {
              DEBUG_PRINT("Discarding handle %x which is already monitor for exceptional", Handle_val(fd));
            }
        }
      handle_set_reset(&hds);

      /* Building the list of handle to wait for */
      DEBUG_PRINT("Building events done array");
      nEventsMax   = list_length((LPLIST)lpSelectData);
      nEventsCount = 0;
      lpEventsDone = (HANDLE *)caml_stat_alloc(sizeof(HANDLE) * nEventsMax);

      iterSelectData = lpSelectData;
      while (iterSelectData != NULL)
        {
          /* Check if it is static data. If this is the case, launch everything
           * but don't wait for events. It helps to test if there are events on
           * any other fd (which are not static), knowing that there is at least
           * one result (the static data).
           */
          if (iterSelectData->EType == SELECT_TYPE_STATIC)
            {
              hasStaticData = TRUE;
            };

          /* Execute APC */
          if (iterSelectData->funcWorker != NULL)
            {
              iterSelectData->lpWorker =
                worker_job_submit(
                                  iterSelectData->funcWorker,
                                  (void *)iterSelectData);
              DEBUG_PRINT("Job submitted to worker %x", iterSelectData->lpWorker);
              lpEventsDone[nEventsCount] = worker_job_event_done(iterSelectData->lpWorker);
              nEventsCount++;
            };
          iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
        };

      DEBUG_PRINT("Need to watch %d workers", nEventsCount);

      /* Processing select itself */
      enter_blocking_section();
      /* There are worker started, waiting to be monitored */
      if (nEventsCount > 0)
        {
          /* Waiting for event */
          if (err == 0 && !hasStaticData)
            {
              DEBUG_PRINT("Waiting for one select worker to be done");
              switch (WaitForMultipleObjects(nEventsCount, lpEventsDone, FALSE, milliseconds))
                {
                case WAIT_FAILED:
                  err = GetLastError();
                  break;

                case WAIT_TIMEOUT:
                  DEBUG_PRINT("Select timeout");
                  break;

                default:
                  DEBUG_PRINT("One worker is done");
                  break;
                };
            }

          /* Ordering stop to every worker */
          DEBUG_PRINT("Sending stop signal to every select workers");
          iterSelectData = lpSelectData;
          while (iterSelectData != NULL)
            {
              if (iterSelectData->lpWorker != NULL)
                {
                  worker_job_stop(iterSelectData->lpWorker);
                };
              iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
            };

          DEBUG_PRINT("Waiting for every select worker to be done");
          switch (WaitForMultipleObjects(nEventsCount, lpEventsDone, TRUE, INFINITE))
            {
            case WAIT_FAILED:
              err = GetLastError();
              break;

            default:
              DEBUG_PRINT("Every worker is done");
              break;
            }
        }
      /* Nothing to monitor but some time to wait. */
      else if (!hasStaticData)
        {
          Sleep(milliseconds);
        }
      leave_blocking_section();

      DEBUG_PRINT("Error status: %d (0 is ok)", err);
      /* Build results */
      if (err == 0)
        {
          DEBUG_PRINT("Building result");
          read_list = Val_unit;
          write_list = Val_unit;
          except_list = Val_unit;

          iterSelectData = lpSelectData;
          while (iterSelectData != NULL)
            {
              for (i = 0; i < iterSelectData->nResultsCount; i++)
                {
                  iterResult = &(iterSelectData->aResults[i]);
                  l = alloc_small(2, 0);
                  Store_field(l, 0, find_handle(iterResult, readfds, writefds, exceptfds));
                  switch (iterResult->EMode)
                    {
                    case SELECT_MODE_READ:
                      Store_field(l, 1, read_list);
                      read_list = l;
                      break;
                    case SELECT_MODE_WRITE:
                      Store_field(l, 1, write_list);
                      write_list = l;
                      break;
                    case SELECT_MODE_EXCEPT:
                      Store_field(l, 1, except_list);
                      except_list = l;
                      break;
                    }
                }
              /* We try to only process the first error, bypass other errors */
              if (err == 0 && iterSelectData->EState == SELECT_STATE_ERROR)
                {
                  err = iterSelectData->nError;
                }
              iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
            }
        }

      /* Free resources */
      DEBUG_PRINT("Free selectdata resources");
      iterSelectData = lpSelectData;
      while (iterSelectData != NULL)
        {
          lpSelectData = iterSelectData;
          iterSelectData = LIST_NEXT(LPSELECTDATA, iterSelectData);
          select_data_free(lpSelectData);
        }
      lpSelectData = NULL;

      /* Free allocated events/handle set array */
      DEBUG_PRINT("Free local allocated resources");
      caml_stat_free(lpEventsDone);
      caml_stat_free(hdsData);

      DEBUG_PRINT("Raise error if required");
      if (err != 0)
        {
          win32_maperr(err);
          uerror("select", Nothing);
        }
    }
  }

  DEBUG_PRINT("Build final result");
  res = alloc_small(3, 0);
  Store_field(res, 0, read_list);
  Store_field(res, 1, write_list);
  Store_field(res, 2, except_list);

  DEBUG_PRINT("out select");

  CAMLreturn(res);
}
