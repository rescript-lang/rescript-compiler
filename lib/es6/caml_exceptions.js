


function make(name, id) {
  return {
          ExceptionID: id,
          Debug: name
        };
}

var id = {
  contents: 0
};

function caml_set_oo_id(b) {
  b[1] = id.contents;
  id.contents = id.contents + 1;
  return b;
}

function create(str) {
  id.contents = id.contents + 1;
  return {
          ExceptionID: id.contents,
          Debug: str
        };
}

var caml_is_extension = (function (e){
  if(e == null ) {
    return false 
  }
  return typeof e.ExceptionID === "number" 
});

function caml_exn_slot_id(x) {
  return x.ExceptionID;
}

function caml_exn_slot_name(x) {
  return x.Debug;
}

export {
  make ,
  caml_set_oo_id ,
  create ,
  caml_is_extension ,
  caml_exn_slot_id ,
  caml_exn_slot_name ,
  
}
/* No side effect */
