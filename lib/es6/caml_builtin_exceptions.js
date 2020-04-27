


function make(name, id) {
  return {
          CamlId: id,
          name: name
        };
}

var out_of_memory = {
  CamlId: 0,
  name: "Out_of_memory"
};

var sys_error = {
  CamlId: -1,
  name: "Sys_error"
};

var failure = {
  CamlId: -2,
  name: "Failure"
};

var invalid_argument = {
  CamlId: -3,
  name: "Invalid_argument"
};

var end_of_file = {
  CamlId: -4,
  name: "End_of_file"
};

var division_by_zero = {
  CamlId: -5,
  name: "Division_by_zero"
};

var not_found = {
  CamlId: -6,
  name: "Not_found"
};

var match_failure = {
  CamlId: -7,
  name: "Match_failure"
};

var stack_overflow = {
  CamlId: -8,
  name: "Stack_overflow"
};

var sys_blocked_io = {
  CamlId: -9,
  name: "Sys_blocked_io"
};

var assert_failure = {
  CamlId: -10,
  name: "Assert_failure"
};

var undefined_recursive_module = {
  CamlId: -11,
  name: "Undefined_recursive_module"
};

export {
  make ,
  out_of_memory ,
  sys_error ,
  failure ,
  invalid_argument ,
  end_of_file ,
  division_by_zero ,
  not_found ,
  match_failure ,
  stack_overflow ,
  sys_blocked_io ,
  assert_failure ,
  undefined_recursive_module ,
  
}
/* No side effect */
