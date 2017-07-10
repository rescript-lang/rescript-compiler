'use strict';
define(["exports", "./char.js", "./array.js", "./buffer.js", "./caml_array.js", "./pervasives.js", "./caml_format.js"],
  function(exports, Char, $$Array, Buffer, Caml_array, Pervasives, Caml_format){
    'use strict';
    function to_string(v) {
      var construct_string = function (b, _v, _tab_level) {
        while(true) {
          var tab_level = _tab_level;
          var v = _v;
          var add = function (tabs, str) {
            for(var i = 0; i <= tabs; ++i){
              Buffer.add_string(b, " ");
            }
            return Buffer.add_string(b, str);
          };
          if (typeof v === "number") {
            return add(tab_level, "None\n");
          } else {
            switch (v.tag | 0) {
              case 0 : 
                  add(tab_level, Caml_format.caml_int32_format("%d", v[0]));
                  return add(0, "l");
              case 1 : 
                  add(tab_level, Caml_format.caml_int64_format("%d", v[0]));
                  return add(0, "L");
              case 2 : 
                  return add(tab_level, "" + v[0]);
              case 3 : 
                  add(tab_level, Caml_format.caml_nativeint_format("%d", v[0]));
                  return add(0, "n");
              case 4 : 
                  return add(tab_level, v[0] ? "true" : "false");
              case 5 : 
                  return add(tab_level, Pervasives.string_of_float(v[0]));
              case 6 : 
                  add(tab_level, "'");
                  add(0, Char.escaped(v[0]));
                  return add(0, "'");
              case 7 : 
                  add(tab_level, "\"");
                  add(0, v[0]);
                  return add(0, "\"");
              case 8 : 
                  add(tab_level, "Some");
                  _tab_level = 0;
                  _v = v[0];
                  continue ;
                  case 9 : 
                  var x = v[0];
                  add(tab_level, "(\n");
                  $$Array.iteri((function(tab_level,x){
                      return function (i, item) {
                        construct_string(b, item, tab_level + 2 | 0);
                        if (i !== (x.length - 1 | 0)) {
                          add(0, ", ");
                        }
                        return add(0, "\n");
                      }
                      }(tab_level,x)), x);
                  return add(tab_level, ")");
              case 10 : 
                  add(tab_level, "[|\n");
                  $$Array.iter((function(tab_level){
                      return function (item) {
                        construct_string(b, item, tab_level + 2 | 0);
                        return add(0, ";\n");
                      }
                      }(tab_level)), v[0]);
                  return add(tab_level, "|]");
              case 11 : 
                  add(tab_level, "[");
                  $$Array.iter((function(tab_level){
                      return function (item) {
                        return construct_string(b, item, tab_level + 2 | 0);
                      }
                      }(tab_level)), v[0]);
                  return add(tab_level, "]\n");
              case 12 : 
                  var values = v[1];
                  add(tab_level, "{\n");
                  $$Array.iteri((function(tab_level,values){
                      return function (i, item) {
                        add(tab_level + 2 | 0, item);
                        add(0, " =\n");
                        construct_string(b, Caml_array.caml_array_get(values, i), tab_level + 2 | 0);
                        return add(0, ";\n");
                      }
                      }(tab_level,values)), v[0]);
                  return add(tab_level, "}");
              case 13 : 
                  var values$1 = v[2];
                  add(tab_level, Caml_array.caml_array_get(v[0][/* constructors */0], v[1]));
                  if (values$1.length !== 0) {
                    add(tab_level, "(\n");
                    $$Array.iteri((function(tab_level,values$1){
                        return function (i, _) {
                          return construct_string(b, Caml_array.caml_array_get(values$1, i), tab_level + 2 | 0);
                        }
                        }(tab_level,values$1)), values$1);
                    return add(tab_level, ")");
                  } else {
                    return 0;
                  }
              
            }
          }
        };
      };
      var buffer = Buffer.create(1024);
      construct_string(buffer, v, 0);
      return Buffer.contents(buffer);
    }
    
    exports.to_string = to_string;
    
  })
/* No side effect */
