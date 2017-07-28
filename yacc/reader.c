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

/*  The line size must be a positive integer.  One hundred was chosen      */
/*  because few lines in Yacc input grammars exceed 100 characters.        */
/*  Note that if a line exceeds LINESIZE characters, the line buffer       */
/*  will be expanded to accomodate it.                                     */

#define LINESIZE 100

char *cache;
int cinc, cache_size;

int ntags, tagmax;
char **tag_table;

char saw_eof, unionized;
char *cptr, *line;
int linesize;

bucket *goal;
int prec;
int gensym;
char last_was_action;

int maxitems;
bucket **pitem;

int maxrules;
bucket **plhs;

int name_pool_size;
char *name_pool;

char line_format[] = "# %d \"%s\"\n";



void start_rule (register bucket *bp, int s_lineno);

void cachec(int c)
{
    assert(cinc >= 0);
    if (cinc >= cache_size)
    {
        cache_size += 256;
        cache = REALLOC(cache, cache_size);
        if (cache == 0) no_space();
    }
    cache[cinc] = c;
    ++cinc;
}


void get_line(void)
{
    register FILE *f = input_file;
    register int c;
    register int i;

    if (saw_eof || (c = getc(f)) == EOF)
    {
        if (line) { FREE(line); line = 0; }
        cptr = 0;
        saw_eof = 1;
        return;
    }

    if (line == 0 || linesize != (LINESIZE + 1))
    {
        if (line) FREE(line);
        linesize = LINESIZE + 1;
        line = MALLOC(linesize);
        if (line == 0) no_space();
    }

    i = 0;
    ++lineno;
    for (;;)
    {
        line[i]  =  c;
        if (++i >= linesize)
        {
            linesize += LINESIZE;
            line = REALLOC(line, linesize);
            if (line ==  0) no_space();
        }
        if (c == '\n') { line[i] = '\0'; cptr = line; return; }
        c = getc(f);
        if (c ==  EOF) { saw_eof = 1; c = '\n'; }
    }
}


char *
dup_line(void)
{
    register char *p, *s, *t;

    if (line == 0) return (0);
    s = line;
    while (*s != '\n') ++s;
    p = MALLOC(s - line + 1);
    if (p == 0) no_space();

    s = line;
    t = p;
    while ((*t++ = *s++) != '\n') continue;
    return (p);
}


void skip_comment(void)
{
    register char *s;

    int st_lineno = lineno;
    char *st_line = dup_line();
    char *st_cptr = st_line + (cptr - line);

    s = cptr + 2;
    for (;;)
    {
        if (*s == '*' && s[1] == '/')
        {
            cptr = s + 2;
            FREE(st_line);
            return;
        }
        if (*s == '\n')
        {
            get_line();
            if (line == 0)
                unterminated_comment(st_lineno, st_line, st_cptr);
            s = cptr;
        }
        else
            ++s;
    }
}

char *substring (char *str, int start, int len)
{
  int i;
  char *buf = MALLOC (len+1);
  if (buf == NULL) return NULL;
  for (i = 0; i < len; i++){
    buf[i] = str[start+i];
  }
  buf[i] = '\0';      /* PR#4796 */
  return buf;
}

void parse_line_directive (void)
{
  int i = 0, j = 0;
  int line_number = 0;
  char *file_name = NULL;

 again:
  if (line == 0) return;
  if (line[i] != '#') return;
  ++ i;
  while (line[i] == ' ' || line[i] == '\t') ++ i;
  if (line[i] < '0' || line[i] > '9') return;
  while (line[i] >= '0' && line[i] <= '9'){
    line_number = line_number * 10 + line[i] - '0';
    ++ i;
  }
  while (line[i] == ' ' || line[i] == '\t') ++ i;
  if (line[i] == '"'){
    ++ i;
    j = i;
    while (line[j] != '"' && line[j] != '\0') ++j;
    if (line[j] == '"'){
      file_name = substring (line, i, j - i);
      if (file_name == NULL) no_space ();
    }
  }
  lineno = line_number - 1;
  if (file_name != NULL){
    if (virtual_input_file_name != NULL) FREE (virtual_input_file_name);
    virtual_input_file_name = file_name;
  }
  get_line ();
  goto again;
}

int
nextc(void)
{
    register char *s;

    if (line == 0)
    {
        get_line();
        parse_line_directive ();
        if (line == 0)
            return (EOF);
    }

    s = cptr;
    for (;;)
    {
        switch (*s)
        {
        case '\n':
            get_line();
            parse_line_directive ();
            if (line == 0) return (EOF);
            s = cptr;
            break;

        case ' ':
        case '\t':
        case '\f':
        case '\r':
        case '\v':
        case ',':
        case ';':
            ++s;
            break;

        case '\\':
            cptr = s;
            return ('%');

        case '/':
            if (s[1] == '*')
            {
                cptr = s;
                skip_comment();
                s = cptr;
                break;
            }
            else if (s[1] == '/')
            {
                get_line();
                parse_line_directive ();
                if (line == 0) return (EOF);
                s = cptr;
                break;
            }
            /* fall through */

        default:
            cptr = s;
            return (*s);
        }
    }
}


int
keyword(void)
{
    register int c;
    char *t_cptr = cptr;

    c = *++cptr;
    if (isalpha(c))
    {
        cinc = 0;
        for (;;)
        {
            if (isalpha(c))
            {
                if (isupper(c)) c = tolower(c);
                cachec(c);
            }
            else if (isdigit(c) || c == '_' || c == '.' || c == '$')
                cachec(c);
            else
                break;
            c = *++cptr;
        }
        cachec(NUL);

        if (strcmp(cache, "token") == 0 || strcmp(cache, "term") == 0)
            return (TOKEN);
        if (strcmp(cache, "type") == 0)
            return (TYPE);
        if (strcmp(cache, "left") == 0)
            return (LEFT);
        if (strcmp(cache, "right") == 0)
            return (RIGHT);
        if (strcmp(cache, "nonassoc") == 0 || strcmp(cache, "binary") == 0)
            return (NONASSOC);
        if (strcmp(cache, "start") == 0)
            return (START);
        if (strcmp(cache, "union") == 0)
            return (UNION);
        if (strcmp(cache, "ident") == 0)
            return (IDENT);
    }
    else
    {
        ++cptr;
        if (c == '{')
            return (TEXT);
        if (c == '%' || c == '\\')
            return (MARK);
        if (c == '<')
            return (LEFT);
        if (c == '>')
            return (RIGHT);
        if (c == '0')
            return (TOKEN);
        if (c == '2')
            return (NONASSOC);
    }
    syntax_error(lineno, line, t_cptr);
    /*NOTREACHED*/
    return 0;
}


void copy_ident(void)
{
    register int c;
    register FILE *f = output_file;

    c = nextc();
    if (c == EOF) unexpected_EOF();
    if (c != '"') syntax_error(lineno, line, cptr);
    ++outline;
    fprintf(f, "#ident \"");
    for (;;)
    {
        c = *++cptr;
        if (c == '\n')
        {
            fprintf(f, "\"\n");
            return;
        }
        putc(c, f);
        if (c == '"')
        {
            putc('\n', f);
            ++cptr;
            return;
        }
    }
}


void copy_text(void)
{
    register int c;
    int quote;
    register FILE *f = text_file;
    int need_newline = 0;
    int t_lineno = lineno;
    char *t_line = dup_line();
    char *t_cptr = t_line + (cptr - line - 2);

    if (*cptr == '\n')
    {
        get_line();
        if (line == 0)
            unterminated_text(t_lineno, t_line, t_cptr);
    }
    fprintf(f, line_format, lineno, input_file_name);

loop:
    c = *cptr++;
    switch (c)
    {
    case '\n':
        putc('\n', f);
        need_newline = 0;
        get_line();
        if (line) goto loop;
        unterminated_text(t_lineno, t_line, t_cptr);

    case '"':
        {
            int s_lineno = lineno;
            char *s_line = dup_line();
            char *s_cptr = s_line + (cptr - line - 1);

            quote = c;
            putc(c, f);
            for (;;)
            {
                c = *cptr++;
                putc(c, f);
                if (c == quote)
                {
                    need_newline = 1;
                    FREE(s_line);
                    goto loop;
                }
                if (c == '\n')
                    unterminated_string(s_lineno, s_line, s_cptr);
                if (c == '\\')
                {
                    c = *cptr++;
                    putc(c, f);
                    if (c == '\n')
                    {
                        get_line();
                        if (line == 0)
                            unterminated_string(s_lineno, s_line, s_cptr);
                    }
                }
            }
        }

    case '\'':
        putc(c, f);
        if (cptr[0] != 0 && cptr[0] != '\\' && cptr[1] == '\'') {
          fwrite(cptr, 1, 2, f);
          cptr += 2;
        } else
        if (cptr[0] == '\\'
            && isdigit((unsigned char) cptr[1])
            && isdigit((unsigned char) cptr[2])
            && isdigit((unsigned char) cptr[3])
            && cptr[4] == '\'') {
          fwrite(cptr, 1, 5, f);
          cptr += 5;
        } else
        if (cptr[0] == '\\' && cptr[2] == '\'') {
          fwrite(cptr, 1, 3, f);
          cptr += 3;
        }
        goto loop;

    case '(':
        putc(c, f);
        need_newline = 1;
        c = *cptr;
        if (c == '*')
        {
            int c_lineno = lineno;
            char *c_line = dup_line();
            char *c_cptr = c_line + (cptr - line - 1);

            putc('*', f);
            ++cptr;
            for (;;)
            {
                c = *cptr++;
                putc(c, f);
                if (c == '*' && *cptr == ')')
                {
                    putc(')', f);
                    ++cptr;
                    FREE(c_line);
                    goto loop;
                }
                if (c == '\n')
                {
                    get_line();
                    if (line == 0)
                        unterminated_comment(c_lineno, c_line, c_cptr);
                }
            }
        }
        need_newline = 1;
        goto loop;

    case '%':
    case '\\':
        if (*cptr == '}')
        {
            if (need_newline) putc('\n', f);
            ++cptr;
            FREE(t_line);
            return;
        }
        /* fall through */

    default:
        putc(c, f);
        need_newline = 1;
        goto loop;
    }
}


void copy_union(void)
{
    register int c;
    int quote;
    int depth;
    int u_lineno = lineno;
    char *u_line = dup_line();
    char *u_cptr = u_line + (cptr - line - 6);

    if (unionized) over_unionized(cptr - 6);
    unionized = 1;

    if (!lflag)
        fprintf(text_file, line_format, lineno, input_file_name);

    fprintf(text_file, "typedef union");
    if (dflag) fprintf(union_file, "typedef union");

    depth = 1;
    cptr++;

loop:
    c = *cptr++;
    putc(c, text_file);
    if (dflag) putc(c, union_file);
    switch (c)
    {
    case '\n':
        get_line();
        if (line == 0) unterminated_union(u_lineno, u_line, u_cptr);
        goto loop;

    case '{':
        ++depth;
        goto loop;

    case '}':
        --depth;
        if (c == '}' && depth == 0) {
          fprintf(text_file, " YYSTYPE;\n");
          FREE(u_line);
          return;
        }
        goto loop;

    case '\'':
    case '"':
        {
            int s_lineno = lineno;
            char *s_line = dup_line();
            char *s_cptr = s_line + (cptr - line - 1);

            quote = c;
            for (;;)
            {
                c = *cptr++;
                putc(c, text_file);
                if (dflag) putc(c, union_file);
                if (c == quote)
                {
                    FREE(s_line);
                    goto loop;
                }
                if (c == '\n')
                    unterminated_string(s_lineno, s_line, s_cptr);
                if (c == '\\')
                {
                    c = *cptr++;
                    putc(c, text_file);
                    if (dflag) putc(c, union_file);
                    if (c == '\n')
                    {
                        get_line();
                        if (line == 0)
                            unterminated_string(s_lineno, s_line, s_cptr);
                    }
                }
            }
        }

    case '(':
        c = *cptr;
        if (c == '*')
        {
            int c_lineno = lineno;
            char *c_line = dup_line();
            char *c_cptr = c_line + (cptr - line - 1);

            putc('*', text_file);
            if (dflag) putc('*', union_file);
            ++cptr;
            for (;;)
            {
                c = *cptr++;
                putc(c, text_file);
                if (dflag) putc(c, union_file);
                if (c == '*' && *cptr == ')')
                {
                    putc(')', text_file);
                    if (dflag) putc(')', union_file);
                    ++cptr;
                    FREE(c_line);
                    goto loop;
                }
                if (c == '\n')
                {
                    get_line();
                    if (line == 0)
                        unterminated_comment(c_lineno, c_line, c_cptr);
                }
            }
        }
        goto loop;

    default:
        goto loop;
    }
}


int
hexval(int c)
{
    if (c >= '0' && c <= '9')
        return (c - '0');
    if (c >= 'A' && c <= 'F')
        return (c - 'A' + 10);
    if (c >= 'a' && c <= 'f')
        return (c - 'a' + 10);
    return (-1);
}


bucket *
get_literal(void)
{
    register int c, quote;
    register int i;
    register int n;
    register char *s;
    register bucket *bp;
    int s_lineno = lineno;
    char *s_line = dup_line();
    char *s_cptr = s_line + (cptr - line);

    quote = *cptr++;
    cinc = 0;
    for (;;)
    {
        c = *cptr++;
        if (c == quote) break;
        if (c == '\n') unterminated_string(s_lineno, s_line, s_cptr);
        if (c == '\\')
        {
            char *c_cptr = cptr - 1;

            c = *cptr++;
            switch (c)
            {
            case '\n':
                get_line();
                if (line == 0) unterminated_string(s_lineno, s_line, s_cptr);
                continue;

            case '0': case '1': case '2': case '3':
            case '4': case '5': case '6': case '7':
                n = c - '0';
                c = *cptr;
                if (IS_OCTAL(c))
                {
                    n = (n << 3) + (c - '0');
                    c = *++cptr;
                    if (IS_OCTAL(c))
                    {
                        n = (n << 3) + (c - '0');
                        ++cptr;
                    }
                }
                if (n > MAXCHAR) illegal_character(c_cptr);
                c = n;
                    break;

            case 'x':
                c = *cptr++;
                n = hexval(c);
                if (n < 0 || n >= 16)
                    illegal_character(c_cptr);
                for (;;)
                {
                    c = *cptr;
                    i = hexval(c);
                    if (i < 0 || i >= 16) break;
                    ++cptr;
                    n = (n << 4) + i;
                    if (n > MAXCHAR) illegal_character(c_cptr);
                }
                c = n;
                break;

            case 'a': c = 7; break;
            case 'b': c = '\b'; break;
            case 'f': c = '\f'; break;
            case 'n': c = '\n'; break;
            case 'r': c = '\r'; break;
            case 't': c = '\t'; break;
            case 'v': c = '\v'; break;
            }
        }
        cachec(c);
    }
    FREE(s_line);

    n = cinc;
    s = MALLOC(n);
    if (s == 0) no_space();

    for (i = 0; i < n; ++i)
        s[i] = cache[i];

    cinc = 0;
    if (n == 1)
        cachec('\'');
    else
        cachec('"');

    for (i = 0; i < n; ++i)
    {
        c = ((unsigned char *)s)[i];
        if (c == '\\' || c == cache[0])
        {
            cachec('\\');
            cachec(c);
        }
        else if (isprint(c))
            cachec(c);
        else
        {
            cachec('\\');
            switch (c)
            {
            case 7: cachec('a'); break;
            case '\b': cachec('b'); break;
            case '\f': cachec('f'); break;
            case '\n': cachec('n'); break;
            case '\r': cachec('r'); break;
            case '\t': cachec('t'); break;
            case '\v': cachec('v'); break;
            default:
                cachec(((c >> 6) & 7) + '0');
                cachec(((c >> 3) & 7) + '0');
                cachec((c & 7) + '0');
                break;
            }
        }
    }

    if (n == 1)
        cachec('\'');
    else
        cachec('"');

    cachec(NUL);
    bp = lookup(cache);
    bp->class = TERM;
    if (n == 1 && bp->value == UNDEFINED)
        bp->value = *(unsigned char *)s;
    FREE(s);

    return (bp);
}


int
is_reserved(char *name)
{
    char *s;

    if (strcmp(name, ".") == 0 ||
            strcmp(name, "$accept") == 0 ||
            strcmp(name, "$end") == 0)
        return (1);

    if (name[0] == '$' && name[1] == '$' && isdigit((unsigned char) name[2]))
    {
        s = name + 3;
        while (isdigit((unsigned char) *s)) ++s;
        if (*s == NUL) return (1);
    }

    return (0);
}


bucket *
get_name(void)
{
    register int c;

    cinc = 0;
    for (c = *cptr; IS_IDENT(c); c = *++cptr)
        cachec(c);
    cachec(NUL);

    if (is_reserved(cache)) used_reserved(cache);

    return (lookup(cache));
}


int
get_number(void)
{
    register int c;
    register int n;

    n = 0;
    for (c = *cptr; isdigit(c); c = *++cptr)
        n = 10*n + (c - '0');

    return (n);
}


char *
get_tag(void)
{
    register int c;
    register int i;
    register char *s;
    char *t_line = dup_line();
    long bracket_depth;

    cinc = 0;
    bracket_depth = 0;
    while (1) {
      c = *++cptr;
      if (c == EOF) unexpected_EOF();
      if (c == '\n') syntax_error(lineno, line, cptr);
      if (c == '>' && 0 == bracket_depth && cptr[-1] != '-') break;
      if (c == '[') ++ bracket_depth;
      if (c == ']') -- bracket_depth;
      cachec(c);
    }
    ++cptr;
    cachec(NUL);

    for (i = 0; i < ntags; ++i)
    {
        if (strcmp(cache, tag_table[i]) == 0)
            return (tag_table[i]);
    }

    if (ntags >= tagmax)
    {
        tagmax += 16;
        tag_table = (char **)
                        (tag_table ? REALLOC(tag_table, tagmax*sizeof(char *))
                                   : MALLOC(tagmax*sizeof(char *)));
        if (tag_table == 0) no_space();
    }

    s = MALLOC(cinc);
    if  (s == 0) no_space();
    strcpy(s, cache);
    tag_table[ntags] = s;
    ++ntags;
    FREE(t_line);
    return (s);
}


void declare_tokens(int assoc)
{
    register int c;
    register bucket *bp;
    int value;
    char *tag = 0;

    if (assoc != TOKEN) ++prec;

    c = nextc();
    if (c == EOF) unexpected_EOF();
    if (c == '<')
    {
        tag = get_tag();
        c = nextc();
        if (c == EOF) unexpected_EOF();
    }

    for (;;)
    {
        if (isalpha(c) || c == '_' || c == '.' || c == '$')
            bp = get_name();
        else if (c == '\'' || c == '"')
            bp = get_literal();
        else
            return;

        if (bp == goal) tokenized_start(bp->name);
        bp->class = TERM;

        if (tag)
        {
            if (bp->tag && tag != bp->tag)
                retyped_warning(bp->name);
            bp->tag = tag;
        }

        if (assoc == TOKEN)
        {
            bp->true_token = 1;
        }
        else
        {
            if (bp->prec && prec != bp->prec)
                reprec_warning(bp->name);
            bp->assoc = assoc;
            bp->prec = prec;
        }

        if (strcmp(bp->name, "EOF") == 0)
            bp->value = 0;

        c = nextc();
        if (c == EOF) unexpected_EOF();
        value = UNDEFINED;
        if (isdigit(c))
        {
            value = get_number();
            if (bp->value != UNDEFINED && value != bp->value)
                revalued_warning(bp->name);
            bp->value = value;
            c = nextc();
            if (c == EOF) unexpected_EOF();
        }
    }
}


void declare_types(void)
{
    register int c;
    register bucket *bp;
    char *tag;

    c = nextc();
    if (c == EOF) unexpected_EOF();
    if (c != '<') syntax_error(lineno, line, cptr);
    tag = get_tag();

    for (;;)
    {
        c = nextc();
        if (isalpha(c) || c == '_' || c == '.' || c == '$')
            bp = get_name();
        else if (c == '\'' || c == '"')
            bp = get_literal();
        else
            return;

        if (bp->tag && tag != bp->tag)
            retyped_warning(bp->name);
        bp->tag = tag;
    }
}


void declare_start(void)
{
    register int c;
    register bucket *bp;
    static int entry_counter = 0;

    for (;;) {
      c = nextc();
      if (!isalpha(c) && c != '_' && c != '.' && c != '$') return;
      bp = get_name();

      if (bp->class == TERM)
        terminal_start(bp->name);
      bp->entry = ++entry_counter;
      if (entry_counter == 256)
        too_many_entries();
    }
}


void read_declarations(void)
{
    register int c, k;

    cache_size = 256;
    cache = MALLOC(cache_size);
    if (cache == 0) no_space();

    for (;;)
    {
        c = nextc();
        if (c == EOF) unexpected_EOF();
        if (c != '%') syntax_error(lineno, line, cptr);
        switch (k = keyword())
        {
        case MARK:
            return;

        case IDENT:
            copy_ident();
            break;

        case TEXT:
            copy_text();
            break;

        case UNION:
            copy_union();
            break;

        case TOKEN:
        case LEFT:
        case RIGHT:
        case NONASSOC:
            declare_tokens(k);
            break;

        case TYPE:
            declare_types();
            break;

        case START:
            declare_start();
            break;
        }
    }
}

void output_token_type(void)
{
  bucket * bp;
  int n;

  fprintf(interface_file, "type token =\n");
  if (!rflag) ++outline;
  fprintf(output_file, "type token =\n");
  n = 0;
  for (bp = first_symbol; bp; bp = bp->next) {
    if (bp->class == TERM && bp->true_token) {
      fprintf(interface_file, "  | %s", bp->name);
      fprintf(output_file, "  | %s", bp->name);
      if (bp->tag) {
        /* Print the type expression in parentheses to make sure
           that the constructor is unary */
        fprintf(interface_file, " of (%s)", bp->tag);
        fprintf(output_file, " of (%s)", bp->tag);
      }
      fprintf(interface_file, "\n");
      if (!rflag) ++outline;
      fprintf(output_file, "\n");
      n++;
    }
  }
  fprintf(interface_file, "\n");
  if (!rflag) ++outline;
  fprintf(output_file, "\n");
}

void initialize_grammar(void)
{
    nitems = 4;
    maxitems = 300;
    pitem = (bucket **) MALLOC(maxitems*sizeof(bucket *));
    if (pitem == 0) no_space();
    pitem[0] = 0;
    pitem[1] = 0;
    pitem[2] = 0;
    pitem[3] = 0;

    nrules = 3;
    maxrules = 100;
    plhs = (bucket **) MALLOC(maxrules*sizeof(bucket *));
    if (plhs == 0) no_space();
    plhs[0] = 0;
    plhs[1] = 0;
    plhs[2] = 0;
    rprec = (short *) MALLOC(maxrules*sizeof(short));
    if (rprec == 0) no_space();
    rprec[0] = 0;
    rprec[1] = 0;
    rprec[2] = 0;
    rassoc = (char *) MALLOC(maxrules*sizeof(char));
    if (rassoc == 0) no_space();
    rassoc[0] = TOKEN;
    rassoc[1] = TOKEN;
    rassoc[2] = TOKEN;
}


void expand_items(void)
{
    maxitems += 300;
    pitem = (bucket **) REALLOC(pitem, maxitems*sizeof(bucket *));
    if (pitem == 0) no_space();
}


void expand_rules(void)
{
    maxrules += 100;
    plhs = (bucket **) REALLOC(plhs, maxrules*sizeof(bucket *));
    if (plhs == 0) no_space();
    rprec = (short *) REALLOC(rprec, maxrules*sizeof(short));
    if (rprec == 0) no_space();
    rassoc = (char *) REALLOC(rassoc, maxrules*sizeof(char));
    if (rassoc == 0) no_space();
}


void advance_to_start(void)
{
    register int c;
    register bucket *bp;
    char *s_cptr;
    int s_lineno;

    for (;;)
    {
        c = nextc();
        if (c != '%') break;
        s_cptr = cptr;
        switch (keyword())
        {
        case MARK:
            no_grammar();

        case TEXT:
            copy_text();
            break;

        case START:
            declare_start();
            break;

        default:
            syntax_error(lineno, line, s_cptr);
        }
    }

    c = nextc();
    if (!isalpha(c) && c != '_' && c != '.' && c != '_')
        syntax_error(lineno, line, cptr);
    bp = get_name();
    if (goal == 0)
    {
        if (bp->class == TERM)
            terminal_start(bp->name);
        goal = bp;
    }

    s_lineno = lineno;
    c = nextc();
    if (c == EOF) unexpected_EOF();
    if (c != ':') syntax_error(lineno, line, cptr);
    start_rule(bp, s_lineno);
    ++cptr;
}


int at_first;

void start_rule(register bucket *bp, int s_lineno)
{
    if (bp->class == TERM)
        terminal_lhs(s_lineno);
    bp->class = NONTERM;
    if (nrules >= maxrules)
        expand_rules();
    plhs[nrules] = bp;
    rprec[nrules] = UNDEFINED;
    rassoc[nrules] = TOKEN;
    at_first = 1;
}


void end_rule(void)
{
    if (!last_was_action) default_action_error();

    last_was_action = 0;
    if (nitems >= maxitems) expand_items();
    pitem[nitems] = 0;
    ++nitems;
    ++nrules;
}


void insert_empty_rule(void)
{
    register bucket *bp, **bpp;

    assert(cache);
    sprintf(cache, "$$%d", ++gensym);
    bp = make_bucket(cache);
    last_symbol->next = bp;
    last_symbol = bp;
    bp->tag = plhs[nrules]->tag;
    bp->class = NONTERM;

    if ((nitems += 2) > maxitems)
        expand_items();
    bpp = pitem + nitems - 1;
    *bpp-- = bp;
    while ((bpp[0] = bpp[-1])) --bpp;

    if (++nrules >= maxrules)
        expand_rules();
    plhs[nrules] = plhs[nrules-1];
    plhs[nrules-1] = bp;
    rprec[nrules] = rprec[nrules-1];
    rprec[nrules-1] = 0;
    rassoc[nrules] = rassoc[nrules-1];
    rassoc[nrules-1] = TOKEN;
}


void add_symbol(void)
{
    register int c;
    register bucket *bp;
    int s_lineno = lineno;
    char *ecptr = cptr;

    c = *cptr;
    if (c == '\'' || c == '"')
        bp = get_literal();
    else
        bp = get_name();

    c = nextc();
    if (c == ':')
    {
        end_rule();
        start_rule(bp, s_lineno);
        ++cptr;
        return;
    }

    if (last_was_action) syntax_error (lineno, line, ecptr);
    last_was_action = 0;

    if (++nitems > maxitems)
        expand_items();
    pitem[nitems-1] = bp;
}


void copy_action(void)
{
    register int c;
    register int i, n;
    int depth;
    int quote;
    bucket *item;
    char *tagres;
    register FILE *f = action_file;
    int a_lineno = lineno;
    char *a_line = dup_line();
    char *a_cptr = a_line + (cptr - line);

    if (last_was_action) syntax_error (lineno, line, cptr);
    last_was_action = 1;

    /*
      fprintf(f, "(* Rule %d, file %s, line %d *)\n",
            nrules-2, input_file_name, lineno);
            */
    if (sflag)
      fprintf(f, "yyact.(%d) <- (fun __caml_parser_env ->\n", nrules-2);
    else
      fprintf(f, "; (fun __caml_parser_env ->\n");

    n = 0;
    for (i = nitems - 1; pitem[i]; --i) ++n;

    for (i = 1; i <= n; i++) {
      item = pitem[nitems + i - n - 1];
      if (item->class == TERM && !item->tag) continue;
      fprintf(f, "    let _%d = ", i);
      if (item->tag)
        fprintf(f, "(Parsing.peek_val __caml_parser_env %d : %s) in\n", n - i,
                item->tag);
      else if (sflag)
        fprintf(f, "Parsing.peek_val __caml_parser_env %d in\n", n - i);
      else
        fprintf(f, "(Parsing.peek_val __caml_parser_env %d : '%s) in\n", n - i,
                item->name);
    }
    fprintf(f, "    Obj.repr(\n");
    fprintf(f, line_format, lineno, input_file_name);
    for (i = 0; i < cptr - line; i++) fputc(' ', f);
    fputc ('(', f);

    depth = 1;
    cptr++;

loop:
    c = *cptr;
    if (c == '$')
    {
        if (isdigit((unsigned char) cptr[1]))
        {
            ++cptr;
            i = get_number();

            if (i <= 0 || i > n)
              unknown_rhs(i);
            item = pitem[nitems + i - n - 1];
            if (item->class == TERM && !item->tag)
              illegal_token_ref(i, item->name);
            fprintf(f, "_%d", i);
            goto loop;
        }
    }
    if (isalpha(c) || c == '_' || c == '$')
    {
        do
        {
            putc(c, f);
            c = *++cptr;
        } while (isalnum(c) || c == '_' || c == '$');
        goto loop;
    }
    if (c == '}' && depth == 1) {
      fprintf(f, ")\n# 0\n              ");
      cptr++;
      tagres = plhs[nrules]->tag;
      if (tagres)
        fprintf(f, " : %s))\n", tagres);
      else if (sflag)
        fprintf(f, "))\n");
      else
        fprintf(f, " : '%s))\n", plhs[nrules]->name);
      if (sflag)
        fprintf(f, "\n");
      return;
    }
    putc(c, f);
    ++cptr;
    switch (c)
    {
    case '\n':
        get_line();
        if (line) goto loop;
        unterminated_action(a_lineno, a_line, a_cptr);

    case '{':
        ++depth;
        goto loop;

    case '}':
        --depth;
        goto loop;

    case '"':
        {
            int s_lineno = lineno;
            char *s_line = dup_line();
            char *s_cptr = s_line + (cptr - line - 1);

            quote = c;
            for (;;)
            {
                c = *cptr++;
                putc(c, f);
                if (c == quote)
                {
                    FREE(s_line);
                    goto loop;
                }
                if (c == '\n')
                    unterminated_string(s_lineno, s_line, s_cptr);
                if (c == '\\')
                {
                    c = *cptr++;
                    putc(c, f);
                    if (c == '\n')
                    {
                        get_line();
                        if (line == 0)
                            unterminated_string(s_lineno, s_line, s_cptr);
                    }
                }
            }
        }

    case '\'':
        if (cptr[0] != 0 && cptr[0] != '\\' && cptr[1] == '\'') {
          fwrite(cptr, 1, 2, f);
          cptr += 2;
        } else
        if (cptr[0] == '\\'
            && isdigit((unsigned char) cptr[1])
            && isdigit((unsigned char) cptr[2])
            && isdigit((unsigned char) cptr[3])
            && cptr[4] == '\'') {
          fwrite(cptr, 1, 5, f);
          cptr += 5;
        } else
        if (cptr[0] == '\\' && cptr[2] == '\'') {
          fwrite(cptr, 1, 3, f);
          cptr += 3;
        }
        goto loop;

    case '(':
        c = *cptr;
        if (c == '*')
        {
            int c_lineno = lineno;
            char *c_line = dup_line();
            char *c_cptr = c_line + (cptr - line - 1);

            putc('*', f);
            ++cptr;
            for (;;)
            {
                c = *cptr++;
                putc(c, f);
                if (c == '*' && *cptr == ')')
                {
                    putc(')', f);
                    ++cptr;
                    FREE(c_line);
                    goto loop;
                }
                if (c == '\n')
                {
                    get_line();
                    if (line == 0)
                        unterminated_comment(c_lineno, c_line, c_cptr);
                }
            }
        }
        goto loop;

    default:
        goto loop;
    }
}


int
mark_symbol(void)
{
    register int c;
    register bucket *bp;

    c = cptr[1];
    if (c == '%' || c == '\\')
    {
        cptr += 2;
        return (1);
    }

    if (c == '=')
        cptr += 2;
    else if ((c == 'p' || c == 'P') &&
             ((c = cptr[2]) == 'r' || c == 'R') &&
             ((c = cptr[3]) == 'e' || c == 'E') &&
             ((c = cptr[4]) == 'c' || c == 'C') &&
             ((c = cptr[5], !IS_IDENT(c))))
        cptr += 5;
    else
        syntax_error(lineno, line, cptr);

    c = nextc();
    if (isalpha(c) || c == '_' || c == '.' || c == '$')
        bp = get_name();
    else if (c == '\'' || c == '"')
        bp = get_literal();
    else
    {
        syntax_error(lineno, line, cptr);
        /*NOTREACHED*/
    }

    if (rprec[nrules] != UNDEFINED && bp->prec != rprec[nrules])
        prec_redeclared();

    rprec[nrules] = bp->prec;
    rassoc[nrules] = bp->assoc;
    return (0);
}


void read_grammar(void)
{
    register int c;

    initialize_grammar();
    advance_to_start();

    for (;;)
    {
        c = nextc();
        if (c == '|' && at_first){
          ++cptr;
          c = nextc();
        }
        at_first = 0;
        if (c == EOF) break;
        if (isalpha(c) || c == '_' || c == '.' || c == '$' || c == '\'' ||
                c == '"')
            add_symbol();
        else if (c == '{' || c == '=')
            copy_action();
        else if (c == '|')
        {
            end_rule();
            start_rule(plhs[nrules-1], 0);
            ++cptr;
        }
        else if (c == '%')
        {
            if (mark_symbol()) break;
        }
        else
            syntax_error(lineno, line, cptr);
    }
    end_rule();
}


void free_tags(void)
{
    register int i;

    if (tag_table == 0) return;

    for (i = 0; i < ntags; ++i)
    {
        assert(tag_table[i]);
        FREE(tag_table[i]);
    }
    FREE(tag_table);
}


void pack_names(void)
{
    register bucket *bp;
    register char *p, *s, *t;

    name_pool_size = 13;  /* 13 == sizeof("$end") + sizeof("$accept") */
    for (bp = first_symbol; bp; bp = bp->next)
        name_pool_size += strlen(bp->name) + 1;
    name_pool = MALLOC(name_pool_size);
    if (name_pool == 0) no_space();

    strcpy(name_pool, "$accept");
    strcpy(name_pool+8, "$end");
    t = name_pool + 13;
    for (bp = first_symbol; bp; bp = bp->next)
    {
        p = t;
        s = bp->name;
        while ((*t++ = *s++)) continue;
        FREE(bp->name);
        bp->name = p;
    }
}


void check_symbols(void)
{
    register bucket *bp;

    if (goal->class == UNKNOWN)
        undefined_goal(goal->name);

    for (bp = first_symbol; bp; bp = bp->next)
    {
        if (bp->class == UNKNOWN)
        {
            undefined_symbol(bp->name);
            bp->class = TERM;
        }
    }
}


void pack_symbols(void)
{
    register bucket *bp;
    register bucket **v;
    register int i, j, k, n;

    nsyms = 2;
    ntokens = 1;
    for (bp = first_symbol; bp; bp = bp->next)
    {
        ++nsyms;
        if (bp->class == TERM) ++ntokens;
    }
    start_symbol = ntokens;
    nvars = nsyms - ntokens;

    symbol_name = (char **) MALLOC(nsyms*sizeof(char *));
    if (symbol_name == 0) no_space();
    symbol_value = (short *) MALLOC(nsyms*sizeof(short));
    if (symbol_value == 0) no_space();
    symbol_prec = (short *) MALLOC(nsyms*sizeof(short));
    if (symbol_prec == 0) no_space();
    symbol_assoc = MALLOC(nsyms);
    if (symbol_assoc == 0) no_space();
    symbol_tag = (char **) MALLOC(nsyms*sizeof(char *));
    if (symbol_tag == 0) no_space();
    symbol_true_token = (char *) MALLOC(nsyms*sizeof(char));
    if (symbol_true_token == 0) no_space();

    v = (bucket **) MALLOC(nsyms*sizeof(bucket *));
    if (v == 0) no_space();

    v[0] = 0;
    v[start_symbol] = 0;

    i = 1;
    j = start_symbol + 1;
    for (bp = first_symbol; bp; bp = bp->next)
    {
        if (bp->class == TERM)
            v[i++] = bp;
        else
            v[j++] = bp;
    }
    assert(i == ntokens && j == nsyms);

    for (i = 1; i < ntokens; ++i)
        v[i]->index = i;

    goal->index = start_symbol + 1;
    k = start_symbol + 2;
    while (++i < nsyms)
        if (v[i] != goal)
        {
            v[i]->index = k;
            ++k;
        }

    goal->value = 0;
    k = 1;
    for (i = start_symbol + 1; i < nsyms; ++i)
    {
        if (v[i] != goal)
        {
            v[i]->value = k;
            ++k;
        }
    }

    k = 0;
    for (i = 1; i < ntokens; ++i)
    {
        n = v[i]->value;
        if (n > 256)
        {
            for (j = k++; j > 0 && symbol_value[j-1] > n; --j)
                symbol_value[j] = symbol_value[j-1];
            symbol_value[j] = n;
        }
    }

    if (v[1]->value == UNDEFINED)
        v[1]->value = 256;

    j = 0;
    n = 257;
    for (i = 2; i < ntokens; ++i)
    {
        if (v[i]->value == UNDEFINED)
        {
            while (j < k && n == symbol_value[j])
            {
                while (++j < k && n == symbol_value[j]) continue;
                ++n;
            }
            v[i]->value = n;
            ++n;
        }
    }

    symbol_name[0] = name_pool + 8;
    symbol_value[0] = 0;
    symbol_prec[0] = 0;
    symbol_assoc[0] = TOKEN;
    symbol_tag[0] = "";
    symbol_true_token[0] = 0;
    for (i = 1; i < ntokens; ++i)
    {
        symbol_name[i] = v[i]->name;
        symbol_value[i] = v[i]->value;
        symbol_prec[i] = v[i]->prec;
        symbol_assoc[i] = v[i]->assoc;
        symbol_tag[i] = v[i]->tag;
        symbol_true_token[i] = v[i]->true_token;
    }
    symbol_name[start_symbol] = name_pool;
    symbol_value[start_symbol] = -1;
    symbol_prec[start_symbol] = 0;
    symbol_assoc[start_symbol] = TOKEN;
    symbol_tag[start_symbol] = "";
    symbol_true_token[start_symbol] = 0;
    for (++i; i < nsyms; ++i)
    {
        k = v[i]->index;
        symbol_name[k] = v[i]->name;
        symbol_value[k] = v[i]->value;
        symbol_prec[k] = v[i]->prec;
        symbol_assoc[k] = v[i]->assoc;
        symbol_tag[i] = v[i]->tag;
        symbol_true_token[i] = v[i]->true_token;
    }

    FREE(v);
}

static unsigned char caml_ident_start[32] =
"\000\000\000\000\000\000\000\000\376\377\377\207\376\377\377\007\000\000\000\000\000\000\000\000\377\377\177\377\377\377\177\377";
static unsigned char caml_ident_body[32] =
"\000\000\000\000\200\000\377\003\376\377\377\207\376\377\377\007\000\000\000\000\000\000\000\000\377\377\177\377\377\377\177\377";

#define In_bitmap(bm,c) (bm[(unsigned char)(c) >> 3] & (1 << ((c) & 7)))

static int is_polymorphic(char * s)
{
  while (*s != 0) {
    char c = *s++;
    if (c == '\'' || c == '#') return 1;
    if (c == '[') {
      c = *s;
      while (c == ' ' || c == '\t' || c == '\r' || c == '\n') c = *++s;
      if (c == '<' || c == '>') return 1;
    }
    if (In_bitmap(caml_ident_start, c)) {
      while (In_bitmap(caml_ident_body, *s)) s++;
    }
  }
  return 0;
}

void make_goal(void)
{
  static char name[7] = "'\\xxx'";
  bucket * bp;
  bucket * bc;

  goal = lookup("%entry%");
  ntotalrules = nrules - 2;
  for(bp = first_symbol; bp != 0; bp = bp->next) {
    if (bp->entry) {
      start_rule(goal, 0);
      if (nitems + 2> maxitems)
        expand_items();
      name[2] = '0' + ((bp->entry >> 6) & 7);
      name[3] = '0' + ((bp->entry >> 3) & 7);
      name[4] = '0' + (bp->entry & 7);
      bc = lookup(name);
      bc->class = TERM;
      bc->value = (unsigned char) bp->entry;
      pitem[nitems++] = bc;
      pitem[nitems++] = bp;
      if (bp->tag == NULL)
        entry_without_type(bp->name);
      if (is_polymorphic(bp->tag))
        polymorphic_entry_point(bp->name);
      fprintf(entry_file,
              "let %s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =\n   (Parsing.yyparse yytables %d lexfun lexbuf : %s)\n",
              bp->name, bp->entry, bp->tag);
      fprintf(interface_file,
              "val %s :\n  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> %s\n",
              bp->name,
              bp->tag);
      fprintf(action_file,
              "(* Entry %s *)\n", bp->name);
      if (sflag)
        fprintf(action_file,
                "yyact.(%d) <- (fun __caml_parser_env -> raise "
                "(Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))\n",
                ntotalrules);
      else
        fprintf(action_file,
                "; (fun __caml_parser_env -> raise "
                "(Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))\n");
      ntotalrules++;
      last_was_action = 1;
      end_rule();
    }
  }
}

void pack_grammar(void)
{
    register int i, j;
    int assoc, prec;

    ritem = (short *) MALLOC(nitems*sizeof(short));
    if (ritem == 0) no_space();
    rlhs = (short *) MALLOC(nrules*sizeof(short));
    if (rlhs == 0) no_space();
    rrhs = (short *) MALLOC((nrules+1)*sizeof(short));
    if (rrhs == 0) no_space();
    rprec = (short *) REALLOC(rprec, nrules*sizeof(short));
    if (rprec == 0) no_space();
    rassoc = REALLOC(rassoc, nrules);
    if (rassoc == 0) no_space();

    ritem[0] = -1;
    ritem[1] = goal->index;
    ritem[2] = 0;
    ritem[3] = -2;
    rlhs[0] = 0;
    rlhs[1] = 0;
    rlhs[2] = start_symbol;
    rrhs[0] = 0;
    rrhs[1] = 0;
    rrhs[2] = 1;

    j = 4;
    for (i = 3; i < nrules; ++i)
    {
        rlhs[i] = plhs[i]->index;
        rrhs[i] = j;
        assoc = TOKEN;
        prec = 0;
        while (pitem[j])
        {
            ritem[j] = pitem[j]->index;
            if (pitem[j]->class == TERM)
            {
                prec = pitem[j]->prec;
                assoc = pitem[j]->assoc;
            }
            ++j;
        }
        ritem[j] = -i;
        ++j;
        if (rprec[i] == UNDEFINED)
        {
            rprec[i] = prec;
            rassoc[i] = assoc;
        }
    }
    rrhs[i] = j;

    FREE(plhs);
    FREE(pitem);
}


void print_grammar(void)
{
    register int i, j, k;
    int spacing = 0;
    register FILE *f = verbose_file;

    if (!vflag) return;

    k = 1;
    for (i = 2; i < nrules; ++i)
    {
        if (rlhs[i] != rlhs[i-1])
        {
            if (i != 2) fprintf(f, "\n");
            fprintf(f, "%4d  %s :", i - 2, symbol_name[rlhs[i]]);
            spacing = strlen(symbol_name[rlhs[i]]) + 1;
        }
        else
        {
            fprintf(f, "%4d  ", i - 2);
            j = spacing;
            while (--j >= 0) putc(' ', f);
            putc('|', f);
        }

        while (ritem[k] >= 0)
        {
            fprintf(f, " %s", symbol_name[ritem[k]]);
            ++k;
        }
        ++k;
        putc('\n', f);
    }
}


void reader(void)
{
    virtual_input_file_name = substring (input_file_name, 0,
                                         strlen (input_file_name));
    create_symbol_table();
    read_declarations();
    output_token_type();
    read_grammar();
    make_goal();
    free_symbol_table();
    free_tags();
    pack_names();
    check_symbols();
    pack_symbols();
    pack_grammar();
    free_symbols();
    print_grammar();
}
