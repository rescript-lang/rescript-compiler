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

void transitive_closure(unsigned int *R, int n)
{
    register int rowsize;
    register unsigned mask;
    register unsigned *rowj;
    register unsigned *rp;
    register unsigned *rend;
    register unsigned *ccol;
    register unsigned *relend;
    register unsigned *cword;
    register unsigned *rowi;

    rowsize = WORDSIZE(n);
    relend = R + n*rowsize;

    cword = R;
    mask = 1;
    rowi = R;
    while (rowi < relend)
    {
        ccol = cword;
        rowj = R;

        while (rowj < relend)
        {
            if (*ccol & mask)
            {
                rp = rowi;
                rend = rowj + rowsize;
                while (rowj < rend)
                    *rowj++ |= *rp++;
            }
            else
            {
                rowj += rowsize;
            }

            ccol += rowsize;
        }

        mask <<= 1;
        if (mask == 0)
        {
            mask = 1;
            cword++;
        }

        rowi += rowsize;
    }
}

void reflexive_transitive_closure(unsigned int *R, int n)
{
    register int rowsize;
    register unsigned mask;
    register unsigned *rp;
    register unsigned *relend;

    transitive_closure(R, n);

    rowsize = WORDSIZE(n);
    relend = R + n*rowsize;

    mask = 1;
    rp = R;
    while (rp < relend)
    {
        *rp |= mask;
        mask <<= 1;
        if (mask == 0)
        {
            mask = 1;
            rp++;
        }

        rp += rowsize;
    }
}
