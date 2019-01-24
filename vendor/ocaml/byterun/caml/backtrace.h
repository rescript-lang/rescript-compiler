/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_BACKTRACE_H
#define CAML_BACKTRACE_H

#ifdef CAML_INTERNALS

#include "mlvalues.h"
#include "exec.h"

/* Runtime support for backtrace generation.
 *
 * It has two kind of users:
 * - high-level API to capture and decode backtraces;
 * - low-level runtime routines, to introspect machine state and determine
 *   whether a backtrace should be generated when using "raise".
 *
 * Backtrace generation is split in multiple steps.
 * The lowest-level one, done by [backtrace_prim.c] just fills the
 * [caml_backtrace_buffer] variable each time a frame is unwinded.
 * At that point, we don't know whether the backtrace will be useful or not so
 * this code should be as fast as possible.
 *
 * If the backtrace happens to be useful, later passes will read
 * [caml_backtrace_buffer] and turn it into a [raw_backtrace] and then a
 * [backtrace].
 * This is done in [backtrace.c] and [stdlib/printexc.ml].
 *
 * Content of buffers
 * ------------------
 *
 * [caml_backtrace_buffer] (really cheap)
 *   Backend and process image dependent, abstracted by C-type backtrace_slot.
 * [raw_backtrace] (cheap)
 *   OCaml values of abstract type [Printexc.raw_backtrace_slot],
 *   still backend and process image dependent (unsafe to marshal).
 * [backtrace] (more expensive)
 *   OCaml values of algebraic data-type [Printexc.backtrace_slot]
 */

/* Non zero iff backtraces are recorded.
 * One should use to change this variable [caml_record_backtrace].
 */
CAMLextern int caml_backtrace_active;

/* The [backtrace_slot] type represents values stored in the
 * [caml_backtrace_buffer].  In bytecode, it is the same as a
 * [code_t], in native code it as a [frame_descr *].  The difference
 * doesn't matter for code outside [backtrace_prim.c], so it is just
 * exposed as a [backtrace_slot].
 */
typedef void * backtrace_slot;

/* The [caml_backtrace_buffer] and [caml_backtrace_last_exn]
 * variables are valid only if [caml_backtrace_active != 0].
 *
 * They are part of the state specific to each thread, and threading libraries
 * are responsible for copying them on context switch.
 * See [otherlibs/systhreads/st_stubs.c] and [otherlibs/threads/scheduler.c].
 */

/* [caml_backtrace_buffer] is filled by runtime when unwinding stack.
 * It is an array ranging from [0] to [caml_backtrace_pos - 1].
 * [caml_backtrace_pos] is always zero if [!caml_backtrace_active].
 *
 * Its maximum size is determined by [BACKTRACE_BUFFER_SIZE] from
 * [backtrace_prim.h], but this shouldn't affect users.
 */
CAMLextern backtrace_slot * caml_backtrace_buffer;
CAMLextern int caml_backtrace_pos;

/* [caml_backtrace_last_exn] stores the last exception value that was raised,
 * iff [caml_backtrace_active != 0].
 * It is tested for equality to determine whether a raise is a re-raise of the
 * same exception.
 *
 * FIXME: this shouldn't matter anymore. Since OCaml 4.02, non-parameterized
 * exceptions are constant, so physical equality is no longer appropriate.
 * raise and re-raise are distinguished by:
 * - passing reraise = 1 to [caml_stash_backtrace] (see below) in the bytecode
 *   interpreter;
 * - directly resetting [caml_backtrace_pos] to 0 in native runtimes for raise.
 */
CAMLextern value caml_backtrace_last_exn;

/* [caml_record_backtrace] toggle backtrace recording on and off.
 * This function can be called at runtime by user-code, or during
 * initialization if backtraces were requested.
 *
 * It might be called before GC initialization, so it shouldn't do OCaml
 * allocation.
 */
CAMLprim value caml_record_backtrace(value vflag);


#ifndef NATIVE_CODE

/* Path to the file containing debug information, if any, or NULL. */
CAMLextern char_os * caml_cds_file;

/* Primitive called _only_ by runtime to record unwinded frames to
 * backtrace.  A similar primitive exists for native code, but with a
 * different prototype. */
extern void caml_stash_backtrace(value exn, code_t pc, value * sp, int reraise);

#endif


/* Default (C-level) printer for backtraces.  It is called if an
 * exception causes a termination of the program or of a thread.
 *
 * [Printexc] provide a higher-level printer mimicking its output but making
 * use of registered exception printers, and is used when possible in place of
 * this function after [Printexc] initialization.
 */
CAMLextern void caml_print_exception_backtrace(void);

void caml_init_backtrace(void);
CAMLexport void caml_init_debug_info(void);

#endif /* CAML_INTERNALS */

#endif /* CAML_BACKTRACE_H */
