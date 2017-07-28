/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"
#ifndef _WIN32
#include <sys/types.h>
#include <netdb.h>
#endif

#define NETDB_BUFFER_SIZE 10000

#ifdef _WIN32
#define GETHOSTBYADDR_IS_REENTRANT 1
#define GETHOSTBYNAME_IS_REENTRANT 1
#endif

static int entry_h_length;

extern int socket_domain_table[];

static value alloc_one_addr(char const *a)
{
  struct in_addr addr;
#ifdef HAS_IPV6
  struct in6_addr addr6;
  if (entry_h_length == 16) {
    memmove(&addr6, a, 16);
    return alloc_inet6_addr(&addr6);
  }
#endif
  memmove (&addr, a, 4);
  return alloc_inet_addr(&addr);
}

static value alloc_host_entry(struct hostent *entry)
{
  value res;
  value name = Val_unit, aliases = Val_unit;
  value addr_list = Val_unit, adr = Val_unit;

  Begin_roots4 (name, aliases, addr_list, adr);
    name = copy_string((char *)(entry->h_name));
    /* PR#4043: protect against buggy implementations of gethostbyname()
       that return a NULL pointer in h_aliases */
    if (entry->h_aliases)
      aliases = copy_string_array((const char**)entry->h_aliases);
    else
      aliases = Atom(0);
    entry_h_length = entry->h_length;
#ifdef h_addr
    addr_list = alloc_array(alloc_one_addr, (const char**)entry->h_addr_list);
#else
    adr = alloc_one_addr(entry->h_addr);
    addr_list = alloc_small(1, 0);
    Field(addr_list, 0) = adr;
#endif
    res = alloc_small(4, 0);
    Field(res, 0) = name;
    Field(res, 1) = aliases;
    switch (entry->h_addrtype) {
    case PF_UNIX:          Field(res, 2) = Val_int(0); break;
    case PF_INET:          Field(res, 2) = Val_int(1); break;
    default: /*PF_INET6 */ Field(res, 2) = Val_int(2); break;
    }
    Field(res, 3) = addr_list;
  End_roots();
  return res;
}

CAMLprim value unix_gethostbyaddr(value a)
{
  struct in_addr adr = GET_INET_ADDR(a);
  struct hostent * hp;
#if HAS_GETHOSTBYADDR_R == 7
  struct hostent h;
  char buffer[NETDB_BUFFER_SIZE];
  int h_errnop;
  enter_blocking_section();
  hp = gethostbyaddr_r((char *) &adr, 4, AF_INET,
                       &h, buffer, sizeof(buffer), &h_errnop);
  leave_blocking_section();
#elif HAS_GETHOSTBYADDR_R == 8
  struct hostent h;
  char buffer[NETDB_BUFFER_SIZE];
  int h_errnop, rc;
  enter_blocking_section();
  rc = gethostbyaddr_r((char *) &adr, 4, AF_INET,
                       &h, buffer, sizeof(buffer), &hp, &h_errnop);
  leave_blocking_section();
  if (rc != 0) hp = NULL;
#else
#ifdef GETHOSTBYADDR_IS_REENTRANT
  enter_blocking_section();
#endif
  hp = gethostbyaddr((char *) &adr, 4, AF_INET);
#ifdef GETHOSTBYADDR_IS_REENTRANT
  leave_blocking_section();
#endif
#endif
  if (hp == (struct hostent *) NULL) raise_not_found();
  return alloc_host_entry(hp);
}

CAMLprim value unix_gethostbyname(value name)
{
  struct hostent * hp;
  char * hostname;

#if HAS_GETHOSTBYNAME_R || GETHOSTBYNAME_IS_REENTRANT
  hostname = caml_strdup(String_val(name));
#else
  hostname = String_val(name);
#endif

#if HAS_GETHOSTBYNAME_R == 5
  {
    struct hostent h;
    char buffer[NETDB_BUFFER_SIZE];
    int h_errno;
    enter_blocking_section();
    hp = gethostbyname_r(hostname, &h, buffer, sizeof(buffer), &h_errno);
    leave_blocking_section();
  }
#elif HAS_GETHOSTBYNAME_R == 6
  {
    struct hostent h;
    char buffer[NETDB_BUFFER_SIZE];
    int h_errno, rc;
    enter_blocking_section();
    rc = gethostbyname_r(hostname, &h, buffer, sizeof(buffer), &hp, &h_errno);
    leave_blocking_section();
    if (rc != 0) hp = NULL;
  }
#else
#ifdef GETHOSTBYNAME_IS_REENTRANT
  enter_blocking_section();
#endif
  hp = gethostbyname(hostname);
#ifdef GETHOSTBYNAME_IS_REENTRANT
  leave_blocking_section();
#endif
#endif

#if HAS_GETHOSTBYNAME_R || GETHOSTBYNAME_IS_REENTRANT
  stat_free(hostname);
#endif

  if (hp == (struct hostent *) NULL) raise_not_found();
  return alloc_host_entry(hp);
}

#else

CAMLprim value unix_gethostbyaddr(value name)
{ invalid_argument("gethostbyaddr not implemented"); }

CAMLprim value unix_gethostbyname(value name)
{ invalid_argument("gethostbyname not implemented"); }

#endif
