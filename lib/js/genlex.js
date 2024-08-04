'use strict';

let Char = require("./char.js");
let List = require("./list.js");
let Bytes = require("./bytes.js");
let Stream = require("./stream.js");
let Hashtbl = require("./hashtbl.js");
let Caml_bytes = require("./caml_bytes.js");
let Caml_format = require("./caml_format.js");
let Caml_string = require("./caml_string.js");
let Caml_js_exceptions = require("./caml_js_exceptions.js");

let initial_buffer = Caml_bytes.create(32);

let buffer = {
  contents: initial_buffer
};

let bufpos = {
  contents: 0
};

function reset_buffer() {
  buffer.contents = initial_buffer;
  bufpos.contents = 0;
}

function store(c) {
  if (bufpos.contents >= buffer.contents.length) {
    let newbuffer = Caml_bytes.create((bufpos.contents << 1));
    Bytes.blit(buffer.contents, 0, newbuffer, 0, bufpos.contents);
    buffer.contents = newbuffer;
  }
  Caml_bytes.set(buffer.contents, bufpos.contents, c);
  bufpos.contents = bufpos.contents + 1 | 0;
}

function get_string() {
  let s = Bytes.sub_string(buffer.contents, 0, bufpos.contents);
  buffer.contents = initial_buffer;
  return s;
}

function make_lexer(keywords) {
  let kwd_table = Hashtbl.create(undefined, 17);
  List.iter((function (s) {
    Hashtbl.add(kwd_table, s, {
      TAG: "Kwd",
      _0: s
    });
  }), keywords);
  let ident_or_keyword = function (id) {
    try {
      return Hashtbl.find(kwd_table, id);
    } catch (raw_exn) {
      let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        return {
          TAG: "Ident",
          _0: id
        };
      }
      throw new Error(exn.RE_EXN_ID, {
        cause: exn
      });
    }
  };
  let keyword_or_error = function (c) {
    let s = Caml_string.make(1, c);
    try {
      return Hashtbl.find(kwd_table, s);
    } catch (raw_exn) {
      let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        throw new Error(Stream.$$Error, {
          cause: {
            RE_EXN_ID: Stream.$$Error,
            _1: "Illegal character " + s
          }
        });
      }
      throw new Error(exn.RE_EXN_ID, {
        cause: exn
      });
    }
  };
  let next_token = function (strm__) {
    while (true) {
      let c = Stream.peek(strm__);
      if (c === undefined) {
        return;
      }
      let exit = 0;
      if (c < 124) {
        if (c > 122 || c < 65) {
          if (c >= 123) {
            exit = 1;
          } else {
            switch (c) {
              case 9 :
              case 10 :
              case 12 :
              case 13 :
              case 26 :
              case 32 :
                Stream.junk(strm__);
                continue;
              case 34 :
                Stream.junk(strm__);
                reset_buffer();
                return {
                  TAG: "String",
                  _0: string(strm__)
                };
              case 39 :
                Stream.junk(strm__);
                let c$1;
                try {
                  c$1 = char(strm__);
                } catch (raw_exn) {
                  let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                  if (exn.RE_EXN_ID === Stream.Failure) {
                    throw new Error(Stream.$$Error, {
                      cause: {
                        RE_EXN_ID: Stream.$$Error,
                        _1: ""
                      }
                    });
                  }
                  throw new Error(exn.RE_EXN_ID, {
                    cause: exn
                  });
                }
                let match = Stream.peek(strm__);
                if (match !== undefined) {
                  if (match !== 39) {
                    throw new Error(Stream.$$Error, {
                      cause: {
                        RE_EXN_ID: Stream.$$Error,
                        _1: ""
                      }
                    });
                  }
                  Stream.junk(strm__);
                  return {
                    TAG: "Char",
                    _0: c$1
                  };
                }
                throw new Error(Stream.$$Error, {
                  cause: {
                    RE_EXN_ID: Stream.$$Error,
                    _1: ""
                  }
                });
              case 40 :
                Stream.junk(strm__);
                let match$1 = Stream.peek(strm__);
                if (match$1 === 42) {
                  Stream.junk(strm__);
                  comment(strm__);
                  return next_token(strm__);
                } else {
                  return keyword_or_error(/* '(' */40);
                }
              case 45 :
                Stream.junk(strm__);
                let c$2 = Stream.peek(strm__);
                if (c$2 !== undefined && !(c$2 > 57 || c$2 < 48)) {
                  Stream.junk(strm__);
                  reset_buffer();
                  store(/* '-' */45);
                  store(c$2);
                  return number(strm__);
                } else {
                  reset_buffer();
                  store(/* '-' */45);
                  return ident2(strm__);
                }
              case 48 :
              case 49 :
              case 50 :
              case 51 :
              case 52 :
              case 53 :
              case 54 :
              case 55 :
              case 56 :
              case 57 :
                exit = 4;
                break;
              case 0 :
              case 1 :
              case 2 :
              case 3 :
              case 4 :
              case 5 :
              case 6 :
              case 7 :
              case 8 :
              case 11 :
              case 14 :
              case 15 :
              case 16 :
              case 17 :
              case 18 :
              case 19 :
              case 20 :
              case 21 :
              case 22 :
              case 23 :
              case 24 :
              case 25 :
              case 27 :
              case 28 :
              case 29 :
              case 30 :
              case 31 :
              case 41 :
              case 44 :
              case 46 :
              case 59 :
                exit = 1;
                break;
              case 33 :
              case 35 :
              case 36 :
              case 37 :
              case 38 :
              case 42 :
              case 43 :
              case 47 :
              case 58 :
              case 60 :
              case 61 :
              case 62 :
              case 63 :
              case 64 :
                exit = 3;
                break;
            }
          }
        } else {
          switch (c) {
            case 92 :
            case 94 :
              exit = 3;
              break;
            case 91 :
            case 93 :
            case 96 :
              exit = 1;
              break;
            default:
              exit = 2;
          }
        }
      } else {
        exit = c >= 127 ? (
            c > 255 || c < 192 ? 1 : 2
          ) : (
            c !== 125 ? 3 : 1
          );
      }
      switch (exit) {
        case 1 :
          Stream.junk(strm__);
          return keyword_or_error(c);
        case 2 :
          Stream.junk(strm__);
          reset_buffer();
          store(c);
          while (true) {
            let c$3 = Stream.peek(strm__);
            if (c$3 === undefined) {
              return ident_or_keyword(get_string());
            }
            if (c$3 >= 91) {
              if (c$3 > 122 || c$3 < 95) {
                if (c$3 > 255 || c$3 < 192) {
                  return ident_or_keyword(get_string());
                }
                
              } else if (c$3 === 96) {
                return ident_or_keyword(get_string());
              }
              
            } else if (c$3 >= 48) {
              if (!(c$3 > 64 || c$3 < 58)) {
                return ident_or_keyword(get_string());
              }
              
            } else if (c$3 !== 39) {
              return ident_or_keyword(get_string());
            }
            Stream.junk(strm__);
            store(c$3);
            continue;
          };
        case 3 :
          Stream.junk(strm__);
          reset_buffer();
          store(c);
          return ident2(strm__);
        case 4 :
          Stream.junk(strm__);
          reset_buffer();
          store(c);
          return number(strm__);
      }
    };
  };
  let ident2 = function (strm__) {
    while (true) {
      let c = Stream.peek(strm__);
      if (c === undefined) {
        return ident_or_keyword(get_string());
      }
      if (c >= 94) {
        if (c > 125 || c < 95) {
          if (c >= 127) {
            return ident_or_keyword(get_string());
          }
          
        } else if (c !== 124) {
          return ident_or_keyword(get_string());
        }
        
      } else if (c >= 65) {
        if (c !== 92) {
          return ident_or_keyword(get_string());
        }
        
      } else {
        if (c < 33) {
          return ident_or_keyword(get_string());
        }
        switch (c) {
          case 34 :
          case 39 :
          case 40 :
          case 41 :
          case 44 :
          case 46 :
          case 48 :
          case 49 :
          case 50 :
          case 51 :
          case 52 :
          case 53 :
          case 54 :
          case 55 :
          case 56 :
          case 57 :
          case 59 :
            return ident_or_keyword(get_string());
          case 33 :
          case 35 :
          case 36 :
          case 37 :
          case 38 :
          case 42 :
          case 43 :
          case 45 :
          case 47 :
          case 58 :
          case 60 :
          case 61 :
          case 62 :
          case 63 :
          case 64 :
            break;
        }
      }
      Stream.junk(strm__);
      store(c);
      continue;
    };
  };
  let number = function (strm__) {
    while (true) {
      let c = Stream.peek(strm__);
      if (c !== undefined) {
        if (c >= 58) {
          if (!(c !== 69 && c !== 101)) {
            Stream.junk(strm__);
            store(/* 'E' */69);
            return exponent_part(strm__);
          }
          
        } else if (c !== 46) {
          if (c >= 48) {
            Stream.junk(strm__);
            store(c);
            continue;
          }
          
        } else {
          Stream.junk(strm__);
          store(/* '.' */46);
          while (true) {
            let c$1 = Stream.peek(strm__);
            if (c$1 !== undefined) {
              if (c$1 > 101 || c$1 < 69) {
                if (!(c$1 > 57 || c$1 < 48)) {
                  Stream.junk(strm__);
                  store(c$1);
                  continue;
                }
                
              } else if (c$1 > 100 || c$1 < 70) {
                Stream.junk(strm__);
                store(/* 'E' */69);
                return exponent_part(strm__);
              }
              
            }
            return {
              TAG: "Float",
              _0: Caml_format.float_of_string(get_string())
            };
          };
        }
      }
      return {
        TAG: "Int",
        _0: Caml_format.int_of_string(get_string())
      };
    };
  };
  let exponent_part = function (strm__) {
    let c = Stream.peek(strm__);
    if (c !== undefined && !(c !== 43 && c !== 45)) {
      Stream.junk(strm__);
      store(c);
      return end_exponent_part(strm__);
    } else {
      return end_exponent_part(strm__);
    }
  };
  let end_exponent_part = function (strm__) {
    while (true) {
      let c = Stream.peek(strm__);
      if (c === undefined) {
        return {
          TAG: "Float",
          _0: Caml_format.float_of_string(get_string())
        };
      }
      if (c > 57 || c < 48) {
        return {
          TAG: "Float",
          _0: Caml_format.float_of_string(get_string())
        };
      }
      Stream.junk(strm__);
      store(c);
      continue;
    };
  };
  let string = function (strm__) {
    while (true) {
      let c = Stream.peek(strm__);
      if (c !== undefined) {
        if (c !== 34) {
          if (c !== 92) {
            Stream.junk(strm__);
            store(c);
            continue;
          }
          Stream.junk(strm__);
          let c$1;
          try {
            c$1 = escape(strm__);
          } catch (raw_exn) {
            let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
            if (exn.RE_EXN_ID === Stream.Failure) {
              throw new Error(Stream.$$Error, {
                cause: {
                  RE_EXN_ID: Stream.$$Error,
                  _1: ""
                }
              });
            }
            throw new Error(exn.RE_EXN_ID, {
              cause: exn
            });
          }
          store(c$1);
          continue;
        }
        Stream.junk(strm__);
        return get_string();
      }
      throw new Error(Stream.Failure, {
        cause: {
          RE_EXN_ID: Stream.Failure
        }
      });
    };
  };
  let char = function (strm__) {
    let c = Stream.peek(strm__);
    if (c !== undefined) {
      if (c !== 92) {
        Stream.junk(strm__);
        return c;
      }
      Stream.junk(strm__);
      try {
        return escape(strm__);
      } catch (raw_exn) {
        let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === Stream.Failure) {
          throw new Error(Stream.$$Error, {
            cause: {
              RE_EXN_ID: Stream.$$Error,
              _1: ""
            }
          });
        }
        throw new Error(exn.RE_EXN_ID, {
          cause: exn
        });
      }
    } else {
      throw new Error(Stream.Failure, {
        cause: {
          RE_EXN_ID: Stream.Failure
        }
      });
    }
  };
  let escape = function (strm__) {
    let c1 = Stream.peek(strm__);
    if (c1 !== undefined) {
      if (c1 >= 58) {
        switch (c1) {
          case 110 :
            Stream.junk(strm__);
            return /* '\n' */10;
          case 114 :
            Stream.junk(strm__);
            return /* '\r' */13;
          case 116 :
            Stream.junk(strm__);
            return /* '\t' */9;
          default:
            Stream.junk(strm__);
            return c1;
        }
      } else {
        if (c1 >= 48) {
          Stream.junk(strm__);
          let c2 = Stream.peek(strm__);
          if (c2 !== undefined) {
            if (c2 > 57 || c2 < 48) {
              throw new Error(Stream.$$Error, {
                cause: {
                  RE_EXN_ID: Stream.$$Error,
                  _1: ""
                }
              });
            }
            Stream.junk(strm__);
            let c3 = Stream.peek(strm__);
            if (c3 !== undefined) {
              if (c3 > 57 || c3 < 48) {
                throw new Error(Stream.$$Error, {
                  cause: {
                    RE_EXN_ID: Stream.$$Error,
                    _1: ""
                  }
                });
              }
              Stream.junk(strm__);
              return Char.chr((Math.imul(c1 - 48 | 0, 100) + Math.imul(c2 - 48 | 0, 10) | 0) + (c3 - 48 | 0) | 0);
            }
            throw new Error(Stream.$$Error, {
              cause: {
                RE_EXN_ID: Stream.$$Error,
                _1: ""
              }
            });
          }
          throw new Error(Stream.$$Error, {
            cause: {
              RE_EXN_ID: Stream.$$Error,
              _1: ""
            }
          });
        }
        Stream.junk(strm__);
        return c1;
      }
    } else {
      throw new Error(Stream.Failure, {
        cause: {
          RE_EXN_ID: Stream.Failure
        }
      });
    }
  };
  let comment = function (strm__) {
    while (true) {
      let match = Stream.peek(strm__);
      if (match !== undefined) {
        switch (match) {
          case 40 :
            Stream.junk(strm__);
            let match$1 = Stream.peek(strm__);
            if (match$1 !== undefined) {
              if (match$1 !== 42) {
                Stream.junk(strm__);
                return comment(strm__);
              } else {
                Stream.junk(strm__);
                comment(strm__);
                return comment(strm__);
              }
            }
            throw new Error(Stream.Failure, {
              cause: {
                RE_EXN_ID: Stream.Failure
              }
            });
          case 42 :
            Stream.junk(strm__);
            while (true) {
              let match$2 = Stream.peek(strm__);
              if (match$2 !== undefined) {
                if (match$2 !== 41) {
                  if (match$2 !== 42) {
                    Stream.junk(strm__);
                    return comment(strm__);
                  }
                  Stream.junk(strm__);
                  continue;
                }
                Stream.junk(strm__);
                return;
              }
              throw new Error(Stream.Failure, {
                cause: {
                  RE_EXN_ID: Stream.Failure
                }
              });
            };
          default:
            Stream.junk(strm__);
            continue;
        }
      } else {
        throw new Error(Stream.Failure, {
          cause: {
            RE_EXN_ID: Stream.Failure
          }
        });
      }
    };
  };
  return function (input) {
    return Stream.from(function (_count) {
      return next_token(input);
    });
  };
}

exports.make_lexer = make_lexer;
/* No side effect */
