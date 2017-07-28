/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Machine-dependent interface with the asm code */

#ifndef CAML_STACK_H
#define CAML_STACK_H

/* Macros to access the stack frame */

#ifdef TARGET_sparc
#define Saved_return_address(sp) *((intnat *)((sp) + 92))
#define Callback_link(sp) ((struct caml_context *)((sp) + 104))
#endif

#ifdef TARGET_i386
#define Saved_return_address(sp) *((intnat *)((sp) - 4))
#ifndef SYS_win32
#define Callback_link(sp) ((struct caml_context *)((sp) + 16))
#else
#define Callback_link(sp) ((struct caml_context *)((sp) + 8))
#endif
#endif

#ifdef TARGET_power
#define Saved_return_address(sp) *((intnat *)((sp) - SIZEOF_PTR))
#define Already_scanned(sp, retaddr) ((retaddr) & 1)
#define Mark_scanned(sp, retaddr) \
          (*((intnat *)((sp) - SIZEOF_PTR)) = (retaddr) | 1)
#define Mask_already_scanned(retaddr) ((retaddr) & ~1)
#ifdef SYS_aix
#define Trap_frame_size 32
#else
#define Trap_frame_size 16
#endif
#define Callback_link(sp) ((struct caml_context *)((sp) + Trap_frame_size))
#endif

#ifdef TARGET_arm
#define Saved_return_address(sp) *((intnat *)((sp) - 4))
#define Callback_link(sp) ((struct caml_context *)((sp) + 8))
#endif

#ifdef TARGET_amd64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define Callback_link(sp) ((struct caml_context *)((sp) + 16))
#endif

#ifdef TARGET_arm64
#define Saved_return_address(sp) *((intnat *)((sp) - 8))
#define Callback_link(sp) ((struct caml_context *)((sp) + 16))
#endif

/* Structure of OCaml callback contexts */

struct caml_context {
  char * bottom_of_stack;       /* beginning of OCaml stack chunk */
  uintnat last_retaddr;         /* last return address in OCaml code */
  value * gc_regs;              /* pointer to register block */
};

/* Structure of frame descriptors */

typedef struct {
  uintnat retaddr;
  unsigned short frame_size;
  unsigned short num_live;
  unsigned short live_ofs[1];
} frame_descr;

struct caml_loc_info {
  int loc_valid;
  int loc_is_raise;
  char * loc_filename;
  int loc_lnum;
  int loc_startchr;
  int loc_endchr;
};

/* Hash table of frame descriptors */

extern frame_descr ** caml_frame_descriptors;
extern int caml_frame_descriptors_mask;

#define Hash_retaddr(addr) \
  (((uintnat)(addr) >> 3) & caml_frame_descriptors_mask)

extern void caml_init_frame_descriptors(void);
extern void caml_register_frametable(intnat *);
extern void caml_register_dyn_global(void *);

CAMLextern void extract_location_info(frame_descr * d,
                                      /*out*/ struct caml_loc_info * li);


extern uintnat caml_stack_usage (void);
extern uintnat (*caml_stack_usage_hook)(void);

/* Declaration of variables used in the asm code */
extern char * caml_top_of_stack;
extern char * caml_bottom_of_stack;
extern uintnat caml_last_return_address;
extern value * caml_gc_regs;
extern char * caml_exception_pointer;
extern value caml_globals[];
extern intnat caml_globals_inited;
extern intnat * caml_frametable[];

#endif /* CAML_STACK_H */
