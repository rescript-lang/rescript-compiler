/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy and Damien Doligez, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

let generic_quote = (quotequote, s) => {
  let l = String.length(s)
  let b = Buffer.create(l + 20)
  Buffer.add_char(b, '\'')
  for i in 0 to l - 1 {
    if String.get(s, i) == '\'' {
      Buffer.add_string(b, quotequote)
    } else {
      Buffer.add_char(b, String.get(s, i))
    }
  }
  Buffer.add_char(b, '\'')
  Buffer.contents(b)
}

/* This function implements the Open Group specification found here:
  [[1]] http://pubs.opengroup.org/onlinepubs/9699919799/utilities/basename.html
  In step 1 of [[1]], we choose to return "." for empty input.
    (for compatibility with previous versions of OCaml)
  In step 2, we choose to process "//" normally.
  Step 6 is not implemented: we consider that the [suffix] operand is
    always absent.  Suffixes are handled by [chop_suffix] and [chop_extension].
*/
let generic_basename = (is_dir_sep, current_dir_name, name) => {
  let rec find_end = n =>
    if n < 0 {
      String.sub(name, 0, 1)
    } else if is_dir_sep(name, n) {
      find_end(n - 1)
    } else {
      find_beg(n, n + 1)
    }
  and find_beg = (n, p) =>
    if n < 0 {
      String.sub(name, 0, p)
    } else if is_dir_sep(name, n) {
      String.sub(name, n + 1, p - n - 1)
    } else {
      find_beg(n - 1, p)
    }

  if name == "" {
    current_dir_name
  } else {
    find_end(String.length(name) - 1)
  }
}

/* This function implements the Open Group specification found here:
  [[2]] http://pubs.opengroup.org/onlinepubs/9699919799/utilities/dirname.html
  In step 6 of [[2]], we choose to process "//" normally.
*/
let generic_dirname = (is_dir_sep, current_dir_name, name) => {
  let rec trailing_sep = n =>
    if n < 0 {
      String.sub(name, 0, 1)
    } else if is_dir_sep(name, n) {
      trailing_sep(n - 1)
    } else {
      base(n)
    }
  and base = n =>
    if n < 0 {
      current_dir_name
    } else if is_dir_sep(name, n) {
      intermediate_sep(n)
    } else {
      base(n - 1)
    }
  and intermediate_sep = n =>
    if n < 0 {
      String.sub(name, 0, 1)
    } else if is_dir_sep(name, n) {
      intermediate_sep(n - 1)
    } else {
      String.sub(name, 0, n + 1)
    }

  if name == "" {
    current_dir_name
  } else {
    trailing_sep(String.length(name) - 1)
  }
}

module Unix = {
  let current_dir_name = "."
  let parent_dir_name = ".."
  let dir_sep = "/"
  let is_dir_sep = (s, i) => String.get(s, i) == '/'
  let is_relative = n => String.length(n) < 1 || String.get(n, 0) != '/'
  let is_implicit = n =>
    is_relative(n) &&
    ((String.length(n) < 2 || String.sub(n, 0, 2) != "./") &&
    (String.length(n) < 3 || String.sub(n, 0, 3) != "../"))
  let check_suffix = (name, suff) =>
    String.length(name) >= String.length(suff) &&
      String.sub(name, String.length(name) - String.length(suff), String.length(suff)) == suff
  let temp_dir_name = try Sys.getenv("TMPDIR") catch {
  | Not_found => "/tmp"
  }
  let quote = generic_quote("'\\''")
  let basename = generic_basename(is_dir_sep, current_dir_name)
  let dirname = generic_dirname(is_dir_sep, current_dir_name)
}

module Win32 = {
  let current_dir_name = "."
  let parent_dir_name = ".."
  let dir_sep = "\\"
  let is_dir_sep = (s, i) => {
    let c = String.get(s, i)
    c == '/' || (c == '\\' || c == ':')
  }
  let is_relative = n =>
    (String.length(n) < 1 || String.get(n, 0) != '/') &&
      ((String.length(n) < 1 || String.get(n, 0) != '\\') &&
      (String.length(n) < 2 || String.get(n, 1) != ':'))
  let is_implicit = n =>
    is_relative(n) &&
    ((String.length(n) < 2 || String.sub(n, 0, 2) != "./") &&
    ((String.length(n) < 2 || String.sub(n, 0, 2) != ".\\") &&
      ((String.length(n) < 3 || String.sub(n, 0, 3) != "../") &&
      (String.length(n) < 3 || String.sub(n, 0, 3) != "..\\"))))
  let check_suffix = (name, suff) =>
    String.length(name) >= String.length(suff) && {
        let s = String.sub(name, String.length(name) - String.length(suff), String.length(suff))
        String.lowercase_ascii(s) == String.lowercase_ascii(suff)
      }
  let temp_dir_name = try Sys.getenv("TEMP") catch {
  | Not_found => "."
  }
  let quote = s => {
    let l = String.length(s)
    let b = Buffer.create(l + 20)
    Buffer.add_char(b, '"')
    let rec loop = i =>
      if i == l {
        Buffer.add_char(b, '"')
      } else {
        switch String.get(s, i) {
        | '"' => loop_bs(0, i)
        | '\\' => loop_bs(0, i)
        | c =>
          Buffer.add_char(b, c)
          loop(i + 1)
        }
      }
    and loop_bs = (n, i) =>
      if i == l {
        Buffer.add_char(b, '"')
        add_bs(n)
      } else {
        switch String.get(s, i) {
        | '"' =>
          add_bs(2 * n + 1)
          Buffer.add_char(b, '"')
          loop(i + 1)
        | '\\' => loop_bs(n + 1, i + 1)
        | _ =>
          add_bs(n)
          loop(i)
        }
      }
    and add_bs = n =>
      for _j in 1 to n {
        Buffer.add_char(b, '\\')
      }

    loop(0)
    Buffer.contents(b)
  }
  let has_drive = s => {
    let is_letter = param =>
      switch param {
      | 'A' .. 'Z' | 'a' .. 'z' => true
      | _ => false
      }

    String.length(s) >= 2 && (is_letter(String.get(s, 0)) && String.get(s, 1) == ':')
  }
  let drive_and_path = s =>
    if has_drive(s) {
      (String.sub(s, 0, 2), String.sub(s, 2, String.length(s) - 2))
    } else {
      ("", s)
    }
  let dirname = s => {
    let (drive, path) = drive_and_path(s)
    let dir = generic_dirname(is_dir_sep, current_dir_name, path)
    drive ++ dir
  }
  let basename = s => {
    let (_drive, path) = drive_and_path(s)
    generic_basename(is_dir_sep, current_dir_name, path)
  }
}

module Cygwin = {
  let current_dir_name = "."
  let parent_dir_name = ".."
  let dir_sep = "/"
  let is_dir_sep = Win32.is_dir_sep
  let is_relative = Win32.is_relative
  let is_implicit = Win32.is_implicit
  let check_suffix = Win32.check_suffix
  let temp_dir_name = Unix.temp_dir_name
  let quote = Unix.quote
  let basename = generic_basename(is_dir_sep, current_dir_name)
  let dirname = generic_dirname(is_dir_sep, current_dir_name)
}

let (
  current_dir_name,
  parent_dir_name,
  dir_sep,
  is_dir_sep,
  is_relative,
  is_implicit,
  check_suffix,
  temp_dir_name,
  quote,
  basename,
  dirname,
) = switch Sys.os_type {
| "Win32" => (
    Win32.current_dir_name,
    Win32.parent_dir_name,
    Win32.dir_sep,
    Win32.is_dir_sep,
    Win32.is_relative,
    Win32.is_implicit,
    Win32.check_suffix,
    Win32.temp_dir_name,
    Win32.quote,
    Win32.basename,
    Win32.dirname,
  )
| "Cygwin" => (
    Cygwin.current_dir_name,
    Cygwin.parent_dir_name,
    Cygwin.dir_sep,
    Cygwin.is_dir_sep,
    Cygwin.is_relative,
    Cygwin.is_implicit,
    Cygwin.check_suffix,
    Cygwin.temp_dir_name,
    Cygwin.quote,
    Cygwin.basename,
    Cygwin.dirname,
  )
| _ => /* normally "Unix" */
  (
    Unix.current_dir_name,
    Unix.parent_dir_name,
    Unix.dir_sep,
    Unix.is_dir_sep,
    Unix.is_relative,
    Unix.is_implicit,
    Unix.check_suffix,
    Unix.temp_dir_name,
    Unix.quote,
    Unix.basename,
    Unix.dirname,
  )
}

let concat = (dirname, filename) => {
  let l = String.length(dirname)
  if l == 0 || is_dir_sep(dirname, l - 1) {
    dirname ++ filename
  } else {
    dirname ++ (dir_sep ++ filename)
  }
}

let chop_suffix = (name, suff) => {
  let n = String.length(name) - String.length(suff)
  if n < 0 {
    invalid_arg("Filename.chop_suffix")
  } else {
    String.sub(name, 0, n)
  }
}

let extension_len = name => {
  let rec check = (i0, i) =>
    if i < 0 || is_dir_sep(name, i) {
      0
    } else if String.get(name, i) == '.' {
      check(i0, i - 1)
    } else {
      String.length(name) - i0
    }

  let rec search_dot = i =>
    if i < 0 || is_dir_sep(name, i) {
      0
    } else if String.get(name, i) == '.' {
      check(i, i - 1)
    } else {
      search_dot(i - 1)
    }

  search_dot(String.length(name) - 1)
}

let extension = name => {
  let l = extension_len(name)
  if l == 0 {
    ""
  } else {
    String.sub(name, String.length(name) - l, l)
  }
}

let chop_extension = name => {
  let l = extension_len(name)
  if l == 0 {
    invalid_arg("Filename.chop_extension")
  } else {
    String.sub(name, 0, String.length(name) - l)
  }
}

let remove_extension = name => {
  let l = extension_len(name)
  if l == 0 {
    name
  } else {
    String.sub(name, 0, String.length(name) - l)
  }
}

let current_temp_dir_name = ref(temp_dir_name)

let set_temp_dir_name = s => current_temp_dir_name := s
let get_temp_dir_name = () => current_temp_dir_name.contents
