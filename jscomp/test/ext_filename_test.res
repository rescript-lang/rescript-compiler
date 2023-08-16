/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/** Used when produce node compatible paths */
let node_sep = "/"
let node_parent = ".."
let node_current = "."

type t = [
  | #File(string)
  | #Dir(string)
]

let cwd = lazy Sys.getcwd()

let \"//" = Filename.concat

let combine = (path1, path2) =>
  if path1 == "" {
    path2
  } else if path2 == "" {
    path1
  } else if Filename.is_relative(path2) {
    \"//"(path1, path2)
  } else {
    path2
  }

/* Note that [.//] is the same as [./] */
let path_as_directory = x =>
  if x == "" {
    x
  } else if Ext_string_test.ends_with(x, Filename.dir_sep) {
    x
  } else {
    x ++ Filename.dir_sep
  }

let absolute_path = s => {
  let process = s => {
    let s = if Filename.is_relative(s) {
      \"//"(Lazy.force(cwd), s)
    } else {
      s
    }
    /* Now simplify . and .. components */
    let rec aux = s => {
      let (base, dir) = (Filename.basename(s), Filename.dirname(s))
      if dir == s {
        dir
      } else if base == Filename.current_dir_name {
        aux(dir)
      } else if base == Filename.parent_dir_name {
        Filename.dirname(aux(dir))
      } else {
        \"//"(aux(dir), base)
      }
    }
    aux(s)
  }
  process(s)
}

let chop_extension = (~loc="", name) =>
  try Filename.chop_extension(name) catch {
  | Invalid_argument(_) => invalid_arg("Filename.chop_extension ( " ++ loc ++ " : " ++ name ++ " )")
  }

let chop_extension_if_any = fname =>
  try Filename.chop_extension(fname) catch {
  | Invalid_argument(_) => fname
  }

let os_path_separator_char = String.unsafe_get(Filename.dir_sep, 0)

/** example
    {[
      \"/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj\"
        \"/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml\"
    ]}

    The other way
    {[

      \"/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml\"
        \"/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj\"
    ]}
    {[
      \"/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib//ocaml_array.ml\"
    ]}
    {[
      /a/b
      /c/d
    ]}
*/
let relative_path = (file_or_dir_1, file_or_dir_2) => {
  let sep_char = os_path_separator_char
  let relevant_dir1 = switch file_or_dir_1 {
  | #Dir(x) => x
  | #File(file1) => Filename.dirname(file1)
  }
  let relevant_dir2 = switch file_or_dir_2 {
  | #Dir(x) => x
  | #File(file2) => Filename.dirname(file2)
  }
  let dir1 = Ext_string_test.split(relevant_dir1, sep_char)
  let dir2 = Ext_string_test.split(relevant_dir2, sep_char)
  let rec go = (dir1: list<string>, dir2: list<string>) =>
    switch (dir1, dir2) {
    | (list{x, ...xs}, list{y, ...ys}) if x == y => go(xs, ys)
    | (_, _) => \"@"(List.map(_ => node_parent, dir2), dir1)
    }

  switch go(dir1, dir2) {
  | list{x, ..._} as ys if x == node_parent => String.concat(node_sep, ys)
  | ys => \"@@"(String.concat(node_sep), list{node_current, ...ys})
  }
}

/** path2: a/b 
    path1: a 
    result:  ./b 
    TODO: [Filename.concat] with care

    [file1] is currently compilation file 
    [file2] is the dependency
    
    TODO: this is a hackish function: FIXME
*/
let node_relative_path = (
  node_modules_shorten,
  file1: t,
  #File(file2) as dep_file: [#File(string)],
) => {
  let v = Ext_string_test.find(file2, ~sub=Test_literals.node_modules)
  let len = String.length(file2)
  if node_modules_shorten && v >= 0 {
    let rec skip = i =>
      if i >= len {
        failwith("invalid path: " ++ file2)
      } else {
        /* https://en.wikipedia.org/wiki/Path_(computing))
           most path separator are a single char 
 */
        let curr_char = String.unsafe_get(file2, i)
        if curr_char == os_path_separator_char || curr_char == '.' {
          skip(i + 1)
        } else {
          i
        }
      }
    /*
          TODO: we need do more than this suppose user 
          input can be
           {[
             "xxxghsoghos/ghsoghso/node_modules/../buckle-stdlib/list.js"
           ]}
           This seems weird though
 */

    Ext_string_test.tail_from(file2, skip(v + Test_literals.node_modules_length))
  } else {
    relative_path(
      switch dep_file {
      | #File(x) => #File(absolute_path(x))
      | #Dir(x) => #Dir(absolute_path(x))
      },
      switch file1 {
      | #File(x) => #File(absolute_path(x))
      | #Dir(x) => #Dir(absolute_path(x))
      },
    ) ++
    (node_sep ++
    /* chop_extension_if_any */ Filename.basename(file2))
  }
}

/* Input must be absolute directory */
let rec find_root_filename = (~cwd, filename) =>
  if Sys.file_exists(\"//"(cwd, filename)) {
    cwd
  } else {
    let cwd' = Filename.dirname(cwd)
    if String.length(cwd') < String.length(cwd) {
      find_root_filename(~cwd=cwd', filename)
    } else {
      failwith("" ++ filename ++ " not found from " ++ cwd)
    }
  }

let find_package_json_dir = cwd => find_root_filename(~cwd, Test_literals.bsconfig_json)

let package_dir = lazy find_package_json_dir(Lazy.force(cwd))

let module_name_of_file = file =>
  String.capitalize_ascii(\"@@"(Filename.chop_extension, Filename.basename(file)))

let module_name_of_file_if_any = file =>
  String.capitalize_ascii(\"@@"(chop_extension_if_any, Filename.basename(file)))

/* For win32 or case insensitve OS 
    [\".cmj\"] is the same as [\".CMJ\"]
*/
/* let has_exact_suffix_then_chop fname suf = */

let combine = (p1, p2) =>
  if p1 == "" || p1 == Filename.current_dir_name {
    p2
  } else if p2 == "" || p2 == Filename.current_dir_name {
    p1
  } else if Filename.is_relative(p2) {
    Filename.concat(p1, p2)
  } else {
    p2
  }

/**
   {[
     split_aux \"//ghosg//ghsogh/\";;
     - : string * string list = (\"/\", [\"ghosg\"; \"ghsogh\"])
   ]}
   Note that 
   {[
     Filename.dirname \"/a/\" = \"/\"
       Filename.dirname \"/a/b/\" = Filename.dirname \"/a/b\" = \"/a\"
   ]}
   Special case:
   {[
     basename \"//\" = \"/\"
       basename \"///\"  = \"/\"
   ]}
   {[
     basename \"\" =  \".\"
       basename \"\" = \".\"
       dirname \"\" = \".\"
       dirname \"\" =  \".\"
   ]}  
*/
let split_aux = p => {
  let rec go = (p, acc) => {
    let dir = Filename.dirname(p)
    if dir == p {
      (dir, acc)
    } else {
      let new_path = Filename.basename(p)
      if Ext_string_test.equal(new_path, Filename.dir_sep) {
        go(dir, acc)
      } else {
        /* We could do more path simplification here
           leave to [rel_normalized_absolute_path]
 */

        go(dir, list{new_path, ...acc})
      }
    }
  }

  go(p, list{})
}

/** 
   TODO: optimization
   if [from] and [to] resolve to the same path, a zero-length string is returned 
*/
let rel_normalized_absolute_path = (from, to_) => {
  let (root1, paths1) = split_aux(from)
  let (root2, paths2) = split_aux(to_)
  if root1 != root2 {
    root2
  } else {
    let rec go = (xss, yss) =>
      switch (xss, yss) {
      | (list{x, ...xs}, list{y, ...ys}) =>
        if Ext_string_test.equal(x, y) {
          go(xs, ys)
        } else {
          let start = List.fold_left(
            (acc, _) => \"//"(acc, Ext_string_test.parent_dir_lit),
            Ext_string_test.parent_dir_lit,
            xs,
          )
          List.fold_left((acc, v) => \"//"(acc, v), start, yss)
        }
      | (list{}, list{}) => Ext_string_test.empty
      | (list{}, list{y, ...ys}) => List.fold_left((acc, x) => \"//"(acc, x), y, ys)
      | (list{x, ...xs}, list{}) =>
        List.fold_left(
          (acc, _) => \"//"(acc, Ext_string_test.parent_dir_lit),
          Ext_string_test.parent_dir_lit,
          xs,
        )
      }
    go(paths1, paths2)
  }
}

/* TODO: could be hgighly optimized later 
  {[
    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/a/b/../c../d/e/f";;

    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/gsho/./../..";;

    normalize_absolute_path "/a/b/c/d";;

    normalize_absolute_path "/a/b/c/d/";;

    normalize_absolute_path "/a/";;

    normalize_absolute_path "/a";;
  ]}
*/

/** See tests in {!Ounit_path_tests} */
let normalize_absolute_path = x => {
  let drop_if_exist = xs =>
    switch xs {
    | list{} => list{}
    | list{_, ...xs} => xs
    }
  let rec normalize_list = (acc, paths) =>
    switch paths {
    | list{} => acc
    | list{x, ...xs} =>
      if Ext_string_test.equal(x, Ext_string_test.current_dir_lit) {
        normalize_list(acc, xs)
      } else if Ext_string_test.equal(x, Ext_string_test.parent_dir_lit) {
        normalize_list(drop_if_exist(acc), xs)
      } else {
        normalize_list(list{x, ...acc}, xs)
      }
    }

  let (root, paths) = split_aux(x)
  let rev_paths = normalize_list(list{}, paths)
  let rec go = (acc, rev_paths) =>
    switch rev_paths {
    | list{} => Filename.concat(root, acc)
    | list{last, ...rest} => go(Filename.concat(last, acc), rest)
    }
  switch rev_paths {
  | list{} => root
  | list{last, ...rest} => go(last, rest)
  }
}

let get_extension = x => {
  let pos = Ext_string_test.rindex_neg(x, '.')
  if pos < 0 {
    ""
  } else {
    Ext_string_test.tail_from(x, pos)
  }
}

let simple_convert_node_path_to_os_path = if Sys.unix {
  x => x
} else if Sys.win32 || Sys.cygwin {
  Ext_string_test.replace_slash_backward
} else {
  failwith("Unknown OS : " ++ Sys.os_type)
}
