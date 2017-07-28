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

#ifndef _WINWORKER_H
#define _WINWORKER_H

#define _WIN32_WINNT 0x0400
#include "unixsupport.h"
#include <windows.h>

/* Pool of worker threads.
 *
 * These functions help to manage a pool of worker thread and submit task to
 * the pool. It helps to reduce the number of thread creation.
 *
 * Each worker are started in alertable wait state and jobs are submitted as
 * APC (asynchronous procedure call).
 */

/* Data associated with submitted job */
typedef struct _WORKER WORKER;
typedef WORKER *LPWORKER;

/* Function type of submitted job:
 * void worker_call (HANDLE hStop, void *data)
 *
 * This function will be called using the data following:
 * - hStop must be watched for change, since it represents an external command
 *   to stop the call. This event is shared through the WORKER structure, which
 *   can be access throuhg worker_job_event_done.
 * - data is user provided data for the function.
 */
typedef void (*WORKERFUNC) (HANDLE, void *);

/* Initialize global data structure for worker
 */
void worker_init (void);

/* Free global data structure for worker
 */
void worker_cleanup (void);

/* Submit a job to worker. Use returned data to synchronize with the procedure
 * submitted.
 */
LPWORKER worker_job_submit (WORKERFUNC f, void *data);

/* Get event to know when a job is done.
 */
HANDLE worker_job_event_done (LPWORKER);

/* Ask a job to stop processing.
 */
void worker_job_stop (LPWORKER);

/* End a job submitted to worker.
 */
void worker_job_finish (LPWORKER);

#endif /* _WINWORKER_H */
