/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1999 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* MD5 message digest */

#ifndef CAML_MD5_H
#define CAML_MD5_H

#ifdef CAML_INTERNALS

#include "mlvalues.h"
#include "io.h"

CAMLextern value caml_md5_string (value str, value ofs, value len);
CAMLextern value caml_md5_chan (value vchan, value len);
CAMLextern void caml_md5_block(unsigned char digest[16],
                               void * data, uintnat len);

CAMLextern value caml_md5_channel(struct channel *chan, intnat toread);

struct MD5Context {
        uint32_t buf[4];
        uint32_t bits[2];
        unsigned char in[64];
};

CAMLextern void caml_MD5Init (struct MD5Context *context);
CAMLextern void caml_MD5Update (struct MD5Context *context, unsigned char *buf,
                                uintnat len);
CAMLextern void caml_MD5Final (unsigned char *digest, struct MD5Context *ctx);
CAMLextern void caml_MD5Transform (uint32_t *buf, uint32_t *in);

#endif /* CAML_INTERNALS */

#endif /* CAML_MD5_H */
