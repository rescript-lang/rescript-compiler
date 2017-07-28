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

#include <string.h>
#include "defs.h"


bucket **symbol_table;
bucket *first_symbol;
bucket *last_symbol;


int
hash(char *name)
{
    register char *s;
    register int c, k;

    assert(name && *name);
    s = name;
    k = *s;
    while ((c = *++s))
        k = (31*k + c) & (TABLE_SIZE - 1);

    return (k);
}


bucket *
make_bucket(char *name)
{
    register bucket *bp;

    assert(name);
    bp = (bucket *) MALLOC(sizeof(bucket));
    if (bp == 0) no_space();
    bp->link = 0;
    bp->next = 0;
    bp->name = MALLOC(strlen(name) + 1);
    if (bp->name == 0) no_space();
    bp->tag = 0;
    bp->value = UNDEFINED;
    bp->index = 0;
    bp->prec = 0;
    bp-> class = UNKNOWN;
    bp->assoc = TOKEN;
    bp->entry = 0;
    bp->true_token = 0;

    if (bp->name == 0) no_space();
    strcpy(bp->name, name);

    return (bp);
}


bucket *
lookup(char *name)
{
    register bucket *bp, **bpp;

    bpp = symbol_table + hash(name);
    bp = *bpp;

    while (bp)
    {
        if (strcmp(name, bp->name) == 0) return (bp);
        bpp = &bp->link;
        bp = *bpp;
    }

    *bpp = bp = make_bucket(name);
    last_symbol->next = bp;
    last_symbol = bp;

    return (bp);
}


void create_symbol_table(void)
{
    register int i;
    register bucket *bp;

    symbol_table = (bucket **) MALLOC(TABLE_SIZE*sizeof(bucket *));
    if (symbol_table == 0) no_space();
    for (i = 0; i < TABLE_SIZE; i++)
        symbol_table[i] = 0;

    bp = make_bucket("error");
    bp->index = 1;
    bp->class = TERM;

    first_symbol = bp;
    last_symbol = bp;
    symbol_table[hash("error")] = bp;
}


void free_symbol_table(void)
{
    FREE(symbol_table);
    symbol_table = 0;
}


void free_symbols(void)
{
    register bucket *p, *q;

    for (p = first_symbol; p; p = q)
    {
        q = p->next;
        FREE(p);
    }
}
