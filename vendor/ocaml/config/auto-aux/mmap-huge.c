/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                  Damien Doligez, Jane Street Group, LLC                */
/*                                                                        */
/*   Copyright 2015 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>

#define huge_page_size (4 * 1024 * 1024)

/* Test for the possible availability of huge pages. Answer yes
   if the OS knows about huge pages, even if they are not available
   on the build machine at configure time, because (on Linux) huge
   pages can be activated and deactivated easily while the system
   is running.
*/

int main (int argc, char *argv[]){
  void *block;
  char *p;
  int i, res;
  block = mmap (NULL, huge_page_size, PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS | MAP_HUGETLB,
                -1, 0);
  if (block == MAP_FAILED){
    block = mmap (NULL, huge_page_size, PROT_READ | PROT_WRITE,
                  MAP_PRIVATE | MAP_ANONYMOUS,
                  -1, 0);
  }
  if (block == MAP_FAILED){
    perror ("mmap");
    return 3;
  }
  /*printf ("block = %p\n", block);*/
  p = (char *) block;
  for (i = 0; i < huge_page_size; i += 4096){
    p[i] = (char) i;
  }
  return 0;
}
