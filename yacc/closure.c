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

short *itemset;
short *itemsetend;
unsigned *ruleset;

static unsigned *first_derives;
static unsigned *EFF;



void print_EFF ();
void print_first_derives ();
void print_closure ();

void set_EFF(void)
{
    register unsigned *row;
    register int symbol;
    register short *sp;
    register int rowsize;
    register int i;
    register int rule;

    rowsize = WORDSIZE(nvars);
    EFF = NEW2(nvars * rowsize, unsigned);

    row = EFF;
    for (i = start_symbol; i < nsyms; i++)
    {
        sp = derives[i];
        for (rule = *sp; rule > 0; rule = *++sp)
        {
            symbol = ritem[rrhs[rule]];
            if (ISVAR(symbol))
            {
                symbol -= start_symbol;
                SETBIT(row, symbol);
            }
        }
        row += rowsize;
    }

    reflexive_transitive_closure(EFF, nvars);

#ifdef        DEBUG
    print_EFF();
#endif
}


void set_first_derives(void)
{
  register unsigned *rrow;
  register unsigned *vrow;
  register int j;
  register unsigned mask;
  register unsigned cword;
  register short *rp;

  int rule;
  int i;
  int rulesetsize;
  int varsetsize;

  rulesetsize = WORDSIZE(nrules);
  varsetsize = WORDSIZE(nvars);
  first_derives = NEW2(nvars * rulesetsize, unsigned) - ntokens * rulesetsize;

  set_EFF();

  rrow = first_derives + ntokens * rulesetsize;
  for (i = start_symbol; i < nsyms; i++)
    {
      vrow = EFF + ((i - ntokens) * varsetsize);
      cword = *vrow++;
      mask = 1;
      for (j = start_symbol; j < nsyms; j++)
        {
          if (cword & mask)
            {
              rp = derives[j];
              while ((rule = *rp++) >= 0)
                {
                  SETBIT(rrow, rule);
                }
            }

          mask <<= 1;
          if (mask == 0)
            {
              cword = *vrow++;
              mask = 1;
            }
        }

      vrow += varsetsize;
      rrow += rulesetsize;
    }

#ifdef        DEBUG
  print_first_derives();
#endif

  FREE(EFF);
}


void closure(short int *nucleus, int n)
{
    register int ruleno;
    register unsigned word;
    register unsigned mask;
    register short *csp;
    register unsigned *dsp;
    register unsigned *rsp;
    register int rulesetsize;

    short *csend;
    unsigned *rsend;
    int symbol;
    int itemno;

    rulesetsize = WORDSIZE(nrules);
    rsp = ruleset;
    rsend = ruleset + rulesetsize;
    for (rsp = ruleset; rsp < rsend; rsp++)
        *rsp = 0;

    csend = nucleus + n;
    for (csp = nucleus; csp < csend; ++csp)
    {
        symbol = ritem[*csp];
        if (ISVAR(symbol))
        {
            dsp = first_derives + symbol * rulesetsize;
            rsp = ruleset;
            while (rsp < rsend)
                *rsp++ |= *dsp++;
        }
    }

    ruleno = 0;
    itemsetend = itemset;
    csp = nucleus;
    for (rsp = ruleset; rsp < rsend; ++rsp)
    {
        word = *rsp;
        if (word == 0)
            ruleno += BITS_PER_WORD;
        else
        {
            mask = 1;
            while (mask)
            {
                if (word & mask)
                {
                    itemno = rrhs[ruleno];
                    while (csp < csend && *csp < itemno)
                        *itemsetend++ = *csp++;
                    *itemsetend++ = itemno;
                    while (csp < csend && *csp == itemno)
                        ++csp;
                }

                    mask <<= 1;
                    ++ruleno;
            }
        }
    }

    while (csp < csend)
        *itemsetend++ = *csp++;

#ifdef        DEBUG
  print_closure(n);
#endif
}



void finalize_closure(void)
{
  FREE(itemset);
  FREE(ruleset);
  FREE(first_derives + ntokens * WORDSIZE(nrules));
}


#ifdef        DEBUG

void print_closure(int n)
{
  register short *isp;

  printf("\n\nn = %d\n\n", n);
  for (isp = itemset; isp < itemsetend; isp++)
    printf("   %d\n", *isp);
}


void print_EFF(void)
{
    register int i, j;
    register unsigned *rowp;
    register unsigned word;
    register unsigned mask;

    printf("\n\nEpsilon Free Firsts\n");

    for (i = start_symbol; i < nsyms; i++)
    {
        printf("\n%s", symbol_name[i]);
        rowp = EFF + ((i - start_symbol) * WORDSIZE(nvars));
        word = *rowp++;

        mask = 1;
        for (j = 0; j < nvars; j++)
        {
            if (word & mask)
                printf("  %s", symbol_name[start_symbol + j]);

            mask <<= 1;
            if (mask == 0)
            {
                word = *rowp++;
                mask = 1;
            }
        }
    }
}


void print_first_derives(void)
{
  register int i;
  register int j;
  register unsigned *rp;
  register unsigned cword;
  register unsigned mask;

  printf("\n\n\nFirst Derives\n");

  for (i = start_symbol; i < nsyms; i++)
    {
      printf("\n%s derives\n", symbol_name[i]);
      rp = first_derives + i * WORDSIZE(nrules);
      cword = *rp++;
      mask = 1;
      for (j = 0; j <= nrules; j++)
        {
          if (cword & mask)
            printf("   %d\n", j);

          mask <<= 1;
          if (mask == 0)
            {
              cword = *rp++;
              mask = 1;
            }
        }
    }

  fflush(stdout);
}

#endif
