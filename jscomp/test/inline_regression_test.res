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

let basename = generic_basename((s, i) => String.get(s, i) == '/', Filename.current_dir_name)

let suites = {
  open Mt
  list{("basename", _ => Eq(basename("b/c/a.b"), "a.b"))}
}

Mt.from_pair_suites(__MODULE__, suites)
