// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
define(["../runtime/caml_obj_runtime","./camlinternalLazy","../runtime/caml_sys","../runtime/caml_exceptions","./pervasives","./printf","../runtime/caml_primitive","./buffer","./string","./random"],
  function(Caml_obj_runtime,CamlinternalLazy,Caml_sys,Caml_exceptions,Pervasives,Printf,Caml_primitive,Buffer,$$String,Random){
    'use strict';
    function generic_basename(is_dir_sep, current_dir_name, name) {
      if (name === "") {
        return current_dir_name;
      }
      else {
        var _n = name.length - 1;
        while(true) {
          var n = _n;
          if (n < 0) {
            return $$String.sub(name, 0, 1);
          }
          else if (is_dir_sep(name, n)) {
            _n = n - 1;
          }
          else {
            var _n$1 = n;
            var p = n + 1;
            while(true) {
              var n$1 = _n$1;
              if (n$1 < 0) {
                return $$String.sub(name, 0, p);
              }
              else if (is_dir_sep(name, n$1)) {
                return $$String.sub(name, n$1 + 1, p - n$1 - 1);
              }
              else {
                _n$1 = n$1 - 1;
              }
            };
          }
        };
      }
    }
    
    function generic_dirname(is_dir_sep, current_dir_name, name) {
      if (name === "") {
        return current_dir_name;
      }
      else {
        var _n = name.length - 1;
        while(true) {
          var n = _n;
          if (n < 0) {
            return $$String.sub(name, 0, 1);
          }
          else if (is_dir_sep(name, n)) {
            _n = n - 1;
          }
          else {
            var _n$1 = n;
            while(true) {
              var n$1 = _n$1;
              if (n$1 < 0) {
                return current_dir_name;
              }
              else if (is_dir_sep(name, n$1)) {
                var _n$2 = n$1;
                while(true) {
                  var n$2 = _n$2;
                  if (n$2 < 0) {
                    return $$String.sub(name, 0, 1);
                  }
                  else if (is_dir_sep(name, n$2)) {
                    _n$2 = n$2 - 1;
                  }
                  else {
                    return $$String.sub(name, 0, n$2 + 1);
                  }
                };
              }
              else {
                _n$1 = n$1 - 1;
              }
            };
          }
        };
      }
    }
    
    var current_dir_name = ".";
    
    function is_dir_sep(s, i) {
      return +(s[i] === "/");
    }
    
    function is_relative(n) {
      return +(n.length < 1 || n.charCodeAt(0) !== /* "/" */47);
    }
    
    function is_implicit(n) {
      return +(is_relative(n) && (n.length < 2 || $$String.sub(n, 0, 2) !== "./") && (n.length < 3 || $$String.sub(n, 0, 3) !== "../"));
    }
    
    function check_suffix(name, suff) {
      return +(name.length >= suff.length && $$String.sub(name, name.length - suff.length, suff.length) === suff);
    }
    
    var temp_dir_name;
    
    try {
      temp_dir_name = Caml_sys.caml_sys_getenv("TMPDIR");
    }
    catch (exn){
      if (exn === Caml_exceptions.Not_found) {
        temp_dir_name = "/tmp";
      }
      else {
        throw exn;
      }
    }
    
    function quote(param) {
      var quotequote = "'\\''";
      var s = param;
      var l = s.length;
      var b = Buffer.create(l + 20);
      Buffer.add_char(b, /* "'" */39);
      for(var i = 0 ,i_finish = l - 1; i<= i_finish; ++i){
        if (s[i] === "'") {
          Buffer.add_string(b, quotequote);
        }
        else {
          Buffer.add_char(b, s.charCodeAt(i));
        }
      }
      Buffer.add_char(b, /* "'" */39);
      return Buffer.contents(b);
    }
    
    function basename(param) {
      return generic_basename(is_dir_sep, current_dir_name, param);
    }
    
    function dirname(param) {
      return generic_dirname(is_dir_sep, current_dir_name, param);
    }
    
    var temp_dir_name$1;
    
    try {
      temp_dir_name$1 = Caml_sys.caml_sys_getenv("TEMP");
    }
    catch (exn$1){
      if (exn$1 === Caml_exceptions.Not_found) {
        temp_dir_name$1 = ".";
      }
      else {
        throw exn$1;
      }
    }
    
    var temp_dir_name$2 = temp_dir_name;
    
    var is_dir_sep$1 = is_dir_sep;
    
    var dir_sep = "/";
    
    function concat(dirname, filename) {
      var l = dirname.length;
      if (l === 0 || is_dir_sep$1(dirname, l - 1)) {
        return dirname + filename;
      }
      else {
        return dirname + (dir_sep + filename);
      }
    }
    
    function chop_suffix(name, suff) {
      var n = name.length - suff.length;
      if (n < 0) {
        return Pervasives.invalid_arg("Filename.chop_suffix");
      }
      else {
        return $$String.sub(name, 0, n);
      }
    }
    
    function chop_extension(name) {
      var _i = name.length - 1;
      while(true) {
        var i = _i;
        if (i < 0 || is_dir_sep$1(name, i)) {
          return Pervasives.invalid_arg("Filename.chop_extension");
        }
        else if (name[i] === ".") {
          return $$String.sub(name, 0, i);
        }
        else {
          _i = i - 1;
        }
      };
    }
    
    var prng = [
      246,
      function () {
        return Random.State[2](/* () */0);
      }
    ];
    
    function temp_file_name(temp_dir, prefix, suffix) {
      var tag = Caml_obj_runtime.caml_obj_tag(prng);
      var rnd = Random.State[4](tag === 250 ? prng[1] : (
              tag === 246 ? CamlinternalLazy.force_lazy_block(prng) : prng
            )) & 16777215;
      return concat(temp_dir, Printf.sprintf([
                        /* Format */0,
                        [
                          /* String */2,
                          /* No_padding */0,
                          [
                            /* Int */4,
                            /* Int_x */6,
                            [
                              /* Lit_padding */0,
                              /* Zeros */2,
                              6
                            ],
                            /* No_precision */0,
                            [
                              /* String */2,
                              /* No_padding */0,
                              /* End_of_format */0
                            ]
                          ]
                        ],
                        "%s%06x%s"
                      ])(prefix, rnd, suffix));
    }
    
    var current_temp_dir_name = [
      0,
      temp_dir_name$2
    ];
    
    function set_temp_dir_name(s) {
      current_temp_dir_name[1] = s;
      return /* () */0;
    }
    
    function get_temp_dir_name() {
      return current_temp_dir_name[1];
    }
    
    function temp_file($staropt$star, prefix, suffix) {
      var temp_dir = $staropt$star ? $staropt$star[1] : current_temp_dir_name[1];
      var _counter = 0;
      while(true) {
        var counter = _counter;
        var name = temp_file_name(temp_dir, prefix, suffix);
        try {
          Caml_primitive.caml_sys_close(Caml_primitive.caml_sys_open(name, [
                    /* :: */0,
                    /* Open_wronly */1,
                    [
                      /* :: */0,
                      /* Open_creat */3,
                      [
                        /* :: */0,
                        /* Open_excl */5,
                        /* [] */0
                      ]
                    ]
                  ], 384));
          return name;
        }
        catch (e){
          if (e[1] === Caml_exceptions.Sys_error) {
            if (counter >= 1000) {
              throw e;
            }
            else {
              _counter = counter + 1;
            }
          }
          else {
            throw e;
          }
        }
      };
    }
    
    function open_temp_file($staropt$star, $staropt$star$1, prefix, suffix) {
      var mode = $staropt$star ? $staropt$star[1] : [
          /* :: */0,
          /* Open_text */7,
          /* [] */0
        ];
      var temp_dir = $staropt$star$1 ? $staropt$star$1[1] : current_temp_dir_name[1];
      var _counter = 0;
      while(true) {
        var counter = _counter;
        var name = temp_file_name(temp_dir, prefix, suffix);
        try {
          return [
                  /* tuple */0,
                  name,
                  Pervasives.open_out_gen([
                        /* :: */0,
                        /* Open_wronly */1,
                        [
                          /* :: */0,
                          /* Open_creat */3,
                          [
                            /* :: */0,
                            /* Open_excl */5,
                            mode
                          ]
                        ]
                      ], 384, name)
                ];
        }
        catch (e){
          if (e[1] === Caml_exceptions.Sys_error) {
            if (counter >= 1000) {
              throw e;
            }
            else {
              _counter = counter + 1;
            }
          }
          else {
            throw e;
          }
        }
      };
    }
    
    var current_dir_name$1 = current_dir_name;
    
    var parent_dir_name = "..";
    
    var is_relative$1 = is_relative;
    
    var is_implicit$1 = is_implicit;
    
    var check_suffix$1 = check_suffix;
    
    var basename$1 = basename;
    
    var dirname$1 = dirname;
    
    var quote$1 = quote;
    return {
      current_dir_name : current_dir_name$1, 
      parent_dir_name : parent_dir_name, 
      dir_sep : dir_sep, 
      concat : concat, 
      is_relative : is_relative$1, 
      is_implicit : is_implicit$1, 
      check_suffix : check_suffix$1, 
      chop_suffix : chop_suffix, 
      chop_extension : chop_extension, 
      basename : basename$1, 
      dirname : dirname$1, 
      temp_file : temp_file, 
      open_temp_file : open_temp_file, 
      get_temp_dir_name : get_temp_dir_name, 
      set_temp_dir_name : set_temp_dir_name, 
      temp_dir_name : temp_dir_name$2, 
      quote : quote$1
    }
  })
/* match Not a pure module */
