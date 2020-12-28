'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Scanf = require("../../lib/js/scanf.js");
var $$Buffer = require("../../lib/js/buffer.js");
var Printf = require("../../lib/js/printf.js");
var $$String = require("../../lib/js/string.js");
var Testing = require("./testing.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Mt_global = require("./mt_global.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(f, param) {
  return Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

function test(loc, b) {
  return eq(loc, [
              b,
              true
            ]);
}

function id(x) {
  return x;
}

function test0(param) {
  return ((((Curry._2(Scanf.sscanf("", /* Format */{
                          _0: /* End_of_format */0,
                          _1: ""
                        }), id, 1) + Curry._2(Scanf.sscanf("", /* Format */{
                          _0: {
                            TAG: /* Char_literal */12,
                            _0: /* " " */32,
                            _1: /* End_of_format */0
                          },
                          _1: " "
                        }), id, 2) | 0) + Curry._2(Scanf.sscanf(" ", /* Format */{
                        _0: {
                          TAG: /* Char_literal */12,
                          _0: /* " " */32,
                          _1: /* End_of_format */0
                        },
                        _1: " "
                      }), id, 3) | 0) + Curry._2(Scanf.sscanf("\t", /* Format */{
                      _0: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: /* End_of_format */0
                      },
                      _1: " "
                    }), id, 4) | 0) + Curry._2(Scanf.sscanf("\n", /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: /* End_of_format */0
                    },
                    _1: " "
                  }), id, 5) | 0) + Curry._1(Scanf.sscanf("\n\t 6", /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: /* End_of_format */0
                    }
                  },
                  _1: " %d"
                }), id) | 0;
}

test("File \"tscanf_test.ml\", line 42, characters 5-12", test0(undefined) === 21);

function test1(param) {
  return (((Curry._1(Scanf.sscanf("1", /* Format */{
                        _0: {
                          TAG: /* Int */4,
                          _0: /* Int_d */0,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: /* End_of_format */0
                        },
                        _1: "%d"
                      }), id) + Curry._1(Scanf.sscanf(" 2", /* Format */{
                        _0: {
                          TAG: /* Char_literal */12,
                          _0: /* " " */32,
                          _1: {
                            TAG: /* Int */4,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: /* End_of_format */0
                          }
                        },
                        _1: " %d"
                      }), id) | 0) + Curry._1(Scanf.sscanf(" -2", /* Format */{
                      _0: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: {
                          TAG: /* Int */4,
                          _0: /* Int_d */0,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: /* End_of_format */0
                        }
                      },
                      _1: " %d"
                    }), id) | 0) + Curry._1(Scanf.sscanf(" +2", /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* Int */4,
                        _0: /* Int_d */0,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: /* End_of_format */0
                      }
                    },
                    _1: " %d"
                  }), id) | 0) + Curry._1(Scanf.sscanf(" 2a ", /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Char_literal */12,
                        _0: /* "a" */97,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: " %da"
                }), id) | 0;
}

test("File \"tscanf_test.ml\", line 54, characters 5-12", test1(undefined) === 5);

function test2(param) {
  return (Curry._1(Scanf.sscanf("123", /* Format */{
                    _0: {
                      TAG: /* Int */4,
                      _0: /* Int_i */3,
                      _1: {
                        TAG: /* Lit_padding */0,
                        _0: /* Right */1,
                        _1: 2
                      },
                      _2: /* No_precision */0,
                      _3: /* End_of_format */0
                    },
                    _1: "%2i"
                  }), id) + Curry._1(Scanf.sscanf("245", /* Format */{
                    _0: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: /* End_of_format */0
                    },
                    _1: "%d"
                  }), id) | 0) + Curry._1(Scanf.sscanf(" 2a ", /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: {
                        TAG: /* Lit_padding */0,
                        _0: /* Right */1,
                        _1: 1
                      },
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Char_literal */12,
                        _0: /* "a" */97,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: " %1da"
                }), id) | 0;
}

test("File \"tscanf_test.ml\", line 63, characters 5-12", test2(undefined) === 259);

function test3(param) {
  return ((Curry._1(Scanf.sscanf("0xff", /* Format */{
                      _0: {
                        TAG: /* Int */4,
                        _0: /* Int_i */3,
                        _1: {
                          TAG: /* Lit_padding */0,
                          _0: /* Right */1,
                          _1: 3
                        },
                        _2: /* No_precision */0,
                        _3: /* End_of_format */0
                      },
                      _1: "%3i"
                    }), id) + Curry._1(Scanf.sscanf("0XEF", /* Format */{
                      _0: {
                        TAG: /* Int */4,
                        _0: /* Int_i */3,
                        _1: {
                          TAG: /* Lit_padding */0,
                          _0: /* Right */1,
                          _1: 3
                        },
                        _2: /* No_precision */0,
                        _3: /* End_of_format */0
                      },
                      _1: "%3i"
                    }), id) | 0) + Curry._1(Scanf.sscanf("x=-245", /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: " x = ",
                      _1: {
                        TAG: /* Int */4,
                        _0: /* Int_d */0,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: /* End_of_format */0
                      }
                    },
                    _1: " x = %d"
                  }), id) | 0) + Curry._1(Scanf.sscanf(" 2a ", /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: {
                        TAG: /* Lit_padding */0,
                        _0: /* Right */1,
                        _1: 1
                      },
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Char_literal */12,
                        _0: /* "a" */97,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: " %1da"
                }), id) | 0;
}

test("File \"tscanf_test.ml\", line 73, characters 5-12", test3(undefined) === -214);

function test4(param) {
  if (Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("1"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%f"
            }), (function (b0) {
            return b0 === 1.0;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("-1"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%f"
            }), (function (b0) {
            return b0 === -1.0;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("+1"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%f"
            }), (function (b0) {
            return b0 === 1.0;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("1."), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%f"
            }), (function (b0) {
            return b0 === 1.0;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string(".1"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%f"
            }), (function (b0) {
            return b0 === 0.1;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("-.1"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%f"
            }), (function (b0) {
            return b0 === -0.1;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("+.1"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%f"
            }), (function (b0) {
            return b0 === 0.1;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("+1."), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%f"
            }), (function (b0) {
            return b0 === 1.0;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("-1."), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%f"
            }), (function (b0) {
            return b0 === -1.0;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("0 1. 1.3"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* " " */32,
                  _1: {
                    TAG: /* Float */8,
                    _0: /* Float_f */0,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* Float */8,
                        _0: /* Float_f */0,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: /* End_of_format */0
                      }
                    }
                  }
                }
              },
              _1: "%f %f %f"
            }), (function (b0, b1, b2) {
            return b0 === 0.0 && b1 === 1.0 ? b2 === 1.3 : false;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("0.113"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: {
                  TAG: /* Lit_padding */0,
                  _0: /* Right */1,
                  _1: 4
                },
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%4f"
            }), (function (b0) {
            return b0 === 0.11;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("0.113"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: {
                  TAG: /* Lit_padding */0,
                  _0: /* Right */1,
                  _1: 5
                },
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%5f"
            }), (function (b0) {
            return b0 === 0.113;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("000.113"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: {
                  TAG: /* Lit_padding */0,
                  _0: /* Right */1,
                  _1: 15
                },
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%15f"
            }), (function (b0) {
            return b0 === 0.113;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("+000.113"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: {
                  TAG: /* Lit_padding */0,
                  _0: /* Right */1,
                  _1: 15
                },
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%15f"
            }), (function (b0) {
            return b0 === 0.113;
          }))) {
    return Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("-000.113"), /* Format */{
                    _0: {
                      TAG: /* Float */8,
                      _0: /* Float_f */0,
                      _1: {
                        TAG: /* Lit_padding */0,
                        _0: /* Right */1,
                        _1: 15
                      },
                      _2: /* No_precision */0,
                      _3: /* End_of_format */0
                    },
                    _1: "%15f"
                  }), (function (b0) {
                  return b0 === -0.113;
                }));
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 110, characters 5-12", test4(undefined));

function test5(param) {
  if (Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("1e1"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_e */3,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%e"
            }), (function (b) {
            return b === 10.0;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("1e+1"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_e */3,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%e"
            }), (function (b) {
            return b === 10.0;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("10e-1"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_e */3,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%e"
            }), (function (b) {
            return b === 1.0;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("10.e-1"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_e */3,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%e"
            }), (function (b) {
            return b === 1.0;
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("1e1 1.e+1 1.3e-1"), /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_e */3,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* " " */32,
                  _1: {
                    TAG: /* Float */8,
                    _0: /* Float_e */3,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* Float */8,
                        _0: /* Float_e */3,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: /* End_of_format */0
                      }
                    }
                  }
                }
              },
              _1: "%e %e %e"
            }), (function (b1, b2, b3) {
            return b1 === 10.0 && b2 === b1 ? b3 === 0.13 : false;
          }))) {
    return Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("1 1.1 0e+1 1.3e-1"), /* Format */{
                    _0: {
                      TAG: /* Float */8,
                      _0: /* Float_g */9,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: {
                          TAG: /* Float */8,
                          _0: /* Float_g */9,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: {
                            TAG: /* Char_literal */12,
                            _0: /* " " */32,
                            _1: {
                              TAG: /* Float */8,
                              _0: /* Float_g */9,
                              _1: /* No_padding */0,
                              _2: /* No_precision */0,
                              _3: {
                                TAG: /* Char_literal */12,
                                _0: /* " " */32,
                                _1: {
                                  TAG: /* Float */8,
                                  _0: /* Float_g */9,
                                  _1: /* No_padding */0,
                                  _2: /* No_precision */0,
                                  _3: /* End_of_format */0
                                }
                              }
                            }
                          }
                        }
                      }
                    },
                    _1: "%g %g %g %g"
                  }), (function (b1, b2, b3, b4) {
                  if (b1 === 1.0 && b2 === 1.1 && b3 === 0.0) {
                    return b4 === 0.13;
                  } else {
                    return false;
                  }
                }));
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 133, characters 5-12", test5(undefined));

function test6(param) {
  if (Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("truetrue"), /* Format */{
              _0: {
                TAG: /* Bool */9,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Bool */9,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "%B%B"
            }), (function (b1, b2) {
            return Caml_obj.caml_equal([
                        b1,
                        b2
                      ], [
                        true,
                        true
                      ]);
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("truefalse"), /* Format */{
              _0: {
                TAG: /* Bool */9,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Bool */9,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "%B%B"
            }), (function (b1, b2) {
            return Caml_obj.caml_equal([
                        b1,
                        b2
                      ], [
                        true,
                        false
                      ]);
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("falsetrue"), /* Format */{
              _0: {
                TAG: /* Bool */9,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Bool */9,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "%B%B"
            }), (function (b1, b2) {
            return Caml_obj.caml_equal([
                        b1,
                        b2
                      ], [
                        false,
                        true
                      ]);
          })) && Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("falsefalse"), /* Format */{
              _0: {
                TAG: /* Bool */9,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Bool */9,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "%B%B"
            }), (function (b1, b2) {
            return Caml_obj.caml_equal([
                        b1,
                        b2
                      ], [
                        false,
                        false
                      ]);
          }))) {
    return Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("true false"), /* Format */{
                    _0: {
                      TAG: /* Bool */9,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: {
                          TAG: /* Bool */9,
                          _0: /* No_padding */0,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "%B %B"
                  }), (function (b1, b2) {
                  return Caml_obj.caml_equal([
                              b1,
                              b2
                            ], [
                              true,
                              false
                            ]);
                }));
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 150, characters 5-12", test6(undefined));

function test7(param) {
  if (Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("'a' '\n' '\t' '\0' ' '"), /* Format */{
              _0: {
                TAG: /* Caml_char */1,
                _0: {
                  TAG: /* Char_literal */12,
                  _0: /* " " */32,
                  _1: {
                    TAG: /* Caml_char */1,
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* Caml_char */1,
                        _0: {
                          TAG: /* Char_literal */12,
                          _0: /* " " */32,
                          _1: {
                            TAG: /* Caml_char */1,
                            _0: {
                              TAG: /* Char_literal */12,
                              _0: /* " " */32,
                              _1: {
                                TAG: /* Caml_char */1,
                                _0: /* End_of_format */0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              },
              _1: "%C %C %C %C %C"
            }), (function (c1, c2, c3, c4, c5) {
            return c1 === /* "a" */97 && c2 === /* "\n" */10 && c3 === /* "\t" */9 && c4 === /* "\000" */0 ? c5 === /* " " */32 : false;
          }))) {
    return Curry._1(Scanf.bscanf(Scanf.Scanning.from_string("a \n \t \0  b"), /* Format */{
                    _0: {
                      TAG: /* Char */0,
                      _0: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: {
                          TAG: /* Char */0,
                          _0: {
                            TAG: /* Char_literal */12,
                            _0: /* " " */32,
                            _1: {
                              TAG: /* Char */0,
                              _0: {
                                TAG: /* Char_literal */12,
                                _0: /* " " */32,
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        }
                      }
                    },
                    _1: "%c %c %c "
                  }), (function (c1, c2, c3) {
                  if (c1 === /* "a" */97 && c2 === /* "\000" */0) {
                    return c3 === /* "b" */98;
                  } else {
                    return false;
                  }
                }));
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 168, characters 5-12", test7(undefined));

function verify_read(c) {
  var s = Curry._1(Printf.sprintf(/* Format */{
            _0: {
              TAG: /* Caml_char */1,
              _0: /* End_of_format */0
            },
            _1: "%C"
          }), c);
  var ib = Scanf.Scanning.from_string(s);
  if (Curry._1(Scanf.bscanf(ib, /* Format */{
              _0: {
                TAG: /* Caml_char */1,
                _0: /* End_of_format */0
              },
              _1: "%C"
            }), id) === c) {
    return ;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "tscanf_test.ml",
          174,
          2
        ],
        Error: new Error()
      };
}

function verify_scan_Chars(param) {
  for(var i = 0; i <= 255; ++i){
    verify_read(Pervasives.char_of_int(i));
  }
  
}

function test8(param) {
  return verify_scan_Chars(undefined) === undefined;
}

test("File \"tscanf_test.ml\", line 183, characters 5-12", verify_scan_Chars(undefined) === undefined);

function unit(fmt, s) {
  var ib = Scanf.Scanning.from_string(Curry._1(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* Caml_string */3,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                },
                _1: "%S"
              }), s));
  return Curry._1(Scanf.bscanf(ib, fmt), id);
}

function test_fmt(fmt, s) {
  return unit(fmt, s) === s;
}

var test9_string = "\xef\xbb\xbf";

var partial_arg = /* Format */{
  _0: {
    TAG: /* Caml_string */3,
    _0: /* No_padding */0,
    _1: /* End_of_format */0
  },
  _1: "%S"
};

function test_S(param) {
  return test_fmt(partial_arg, param);
}

function test9(param) {
  if (test_S("poi") && test_S("a\"b") && test_S("a\nb") && test_S("a\nb") && test_S("a\\\nb \\\nc\n\\\nb") && test_S("a\\\n\\\n\\\nb \\\nc\n\\\nb") && test_S("\xef") && test_S("\\xef") && Curry._1(Scanf.sscanf("\"\\xef\"", /* Format */{
              _0: {
                TAG: /* Caml_string */3,
                _0: /* No_padding */0,
                _1: /* End_of_format */0
              },
              _1: "%S"
            }), (function (s) {
            return s;
          })) === "\xef" && Curry._1(Scanf.sscanf("\"\\xef\\xbb\\xbf\"", /* Format */{
              _0: {
                TAG: /* Caml_string */3,
                _0: /* No_padding */0,
                _1: /* End_of_format */0
              },
              _1: "%S"
            }), (function (s) {
            return s;
          })) === test9_string && Curry._1(Scanf.sscanf("\"\\xef\\xbb\\xbf\"", /* Format */{
              _0: {
                TAG: /* Caml_string */3,
                _0: /* No_padding */0,
                _1: /* End_of_format */0
              },
              _1: "%S"
            }), (function (s) {
            return s;
          })) === "\xef\xbb\xbf" && Curry._1(Scanf.sscanf("\"\xef\xbb\xbf\"", /* Format */{
              _0: {
                TAG: /* Caml_string */3,
                _0: /* No_padding */0,
                _1: /* End_of_format */0
              },
              _1: "%S"
            }), (function (s) {
            return s;
          })) === test9_string && Curry._1(Scanf.sscanf("\"\\\\xef\\\\xbb\\\\xbf\"", /* Format */{
              _0: {
                TAG: /* Caml_string */3,
                _0: /* No_padding */0,
                _1: /* End_of_format */0
              },
              _1: "%S"
            }), (function (s) {
            return s;
          })) === "\\xef\\xbb\\xbf") {
    return Curry._1(Scanf.sscanf("\" \"", /* Format */{
                    _0: {
                      TAG: /* Caml_string */3,
                      _0: /* No_padding */0,
                      _1: /* End_of_format */0
                    },
                    _1: "%S"
                  }), (function (s) {
                  return s;
                })) === " ";
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 230, characters 5-12", test9(undefined));

function test10(param) {
  var unit = function (s) {
    var ib = Scanf.Scanning.from_string(s);
    return Curry._1(Scanf.bscanf(ib, /* Format */{
                    _0: {
                      TAG: /* Caml_string */3,
                      _0: /* No_padding */0,
                      _1: /* End_of_format */0
                    },
                    _1: "%S"
                  }), id);
  };
  var res = Curry._1(Scanf.sscanf("Une chaine: \"celle-ci\" et \"celle-la\"!", /* Format */{
            _0: {
              TAG: /* String */2,
              _0: /* No_padding */0,
              _1: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Caml_string */3,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* " " */32,
                            _1: {
                              TAG: /* Caml_string */3,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* Char_literal */12,
                                _0: /* " " */32,
                                _1: {
                                  TAG: /* String */2,
                                  _0: /* No_padding */0,
                                  _1: /* End_of_format */0
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            },
            _1: "%s %s %S %s %S %s"
          }), (function (s1, s2, s3, s4, s5, s6) {
          return s1 + (s2 + (s3 + (s4 + (s5 + s6))));
        }));
  if (res === "Unechaine:celle-cietcelle-la!" && unit("\"a\\\n  b\"") === "ab" && unit("\"\\\n  ab\"") === "ab" && unit("\"\n\\\n  ab\"") === "\nab" && unit("\"\n\\\n  a\nb\"") === "\na\nb" && unit("\"\n\\\n  \\\n  a\nb\"") === "\na\nb" && unit("\"\n\\\n  a\n\\\nb\\\n\"") === "\na\nb") {
    return unit("\"a\\\n  \"") === "a";
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 254, characters 5-12", test10(undefined));

function test11(param) {
  if (Curry._1(Scanf.sscanf("Pierre\tWeis\t70", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* " " */32,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: /* End_of_format */0
                      }
                    }
                  }
                }
              },
              _1: "%s %s %s"
            }), (function (prenom, nom, poids) {
            return prenom === "Pierre" && nom === "Weis" ? Caml_format.caml_int_of_string(poids) === 70 : false;
          })) && Curry._1(Scanf.sscanf("Jean-Luc\tde Leage\t68", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\xff\xfd\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                _2: {
                  TAG: /* Char_literal */12,
                  _0: /* " " */32,
                  _1: {
                    TAG: /* Scan_char_set */20,
                    _0: undefined,
                    _1: "\xff\xfd\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                    _2: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* Int */4,
                        _0: /* Int_d */0,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: /* End_of_format */0
                      }
                    }
                  }
                }
              },
              _1: "%[^\t] %[^\t] %d"
            }), (function (prenom, nom, poids) {
            return prenom === "Jean-Luc" && nom === "de Leage" ? poids === 68 : false;
          }))) {
    return Curry._1(Scanf.sscanf("Daniel\tde Rauglaudre\t66", /* Format */{
                    _0: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Formatting_lit */17,
                        _0: {
                          TAG: /* Scan_indic */2,
                          _0: /* "\t" */9
                        },
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* " " */32,
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* Formatting_lit */17,
                              _0: {
                                TAG: /* Scan_indic */2,
                                _0: /* "\t" */9
                              },
                              _1: {
                                TAG: /* Char_literal */12,
                                _0: /* " " */32,
                                _1: {
                                  TAG: /* Int */4,
                                  _0: /* Int_d */0,
                                  _1: /* No_padding */0,
                                  _2: /* No_precision */0,
                                  _3: /* End_of_format */0
                                }
                              }
                            }
                          }
                        }
                      }
                    },
                    _1: "%s@\t %s@\t %d"
                  }), (function (prenom, nom, poids) {
                  if (prenom === "Daniel" && nom === "de Rauglaudre") {
                    return poids === 66;
                  } else {
                    return false;
                  }
                }));
  } else {
    return false;
  }
}

function test110(param) {
  if (Curry._2(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: /* End_of_format */0
              },
              _1: " "
            }), (function (x) {
            return x;
          }), "") === "" && Curry._1(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: /* End_of_format */0
              },
              _1: "%s"
            }), (function (x) {
            return x === "";
          })) && Curry._1(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "%s%s"
            }), (function (x, y) {
            return x === "" ? y === "" : false;
          })) && Curry._1(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* " " */32,
                  _1: /* End_of_format */0
                }
              },
              _1: "%s "
            }), (function (x) {
            return x === "";
          })) && Curry._1(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: " %s"
            }), (function (x) {
            return x === "";
          })) && Curry._1(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: " %s "
            }), (function (x) {
            return x === "";
          })) && Curry._1(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\xff\xfb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                _2: /* End_of_format */0
              },
              _1: "%[^\n]"
            }), (function (x) {
            return x === "";
          })) && Curry._1(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\xff\xfb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                _2: {
                  TAG: /* Char_literal */12,
                  _0: /* " " */32,
                  _1: /* End_of_format */0
                }
              },
              _1: "%[^\n] "
            }), (function (x) {
            return x === "";
          })) && Curry._1(Scanf.sscanf(" ", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: /* End_of_format */0
              },
              _1: "%s"
            }), (function (x) {
            return x === "";
          })) && Curry._1(Scanf.sscanf(" ", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "%s%s"
            }), (function (x, y) {
            return x === "" ? y === "" : false;
          })) && Curry._1(Scanf.sscanf(" ", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: " %s "
            }), (function (x) {
            return x === "";
          })) && Curry._1(Scanf.sscanf(" ", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: /* End_of_format */0
                    }
                  }
                }
              },
              _1: " %s %s"
            }), (function (x, y) {
            return x === "" ? x === y : false;
          })) && Curry._1(Scanf.sscanf(" ", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Formatting_lit */17,
                    _0: {
                      TAG: /* Break */0,
                      _0: "@ ",
                      _1: 1,
                      _2: 0
                    },
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: /* End_of_format */0
                    }
                  }
                }
              },
              _1: " %s@ %s"
            }), (function (x, y) {
            return x === "" ? x === y : false;
          })) && Curry._1(Scanf.sscanf(" poi !", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Formatting_lit */17,
                    _0: {
                      TAG: /* Break */0,
                      _0: "@ ",
                      _1: 1,
                      _2: 0
                    },
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Formatting_lit */17,
                        _0: /* Flush_newline */4,
                        _1: /* End_of_format */0
                      }
                    }
                  }
                }
              },
              _1: " %s@ %s@."
            }), (function (x, y) {
            return x === "poi" ? y === "!" : false;
          }))) {
    return Curry._1(Scanf.sscanf(" poi !", /* Format */{
                    _0: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Formatting_lit */17,
                        _0: {
                          TAG: /* Break */0,
                          _0: "@ ",
                          _1: 1,
                          _2: 0
                        },
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Formatting_lit */17,
                            _0: /* Flush_newline */4,
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    },
                    _1: "%s@ %s@."
                  }), (function (x, y) {
                  if (x === "") {
                    return y === "poi !";
                  } else {
                    return false;
                  }
                }));
  } else {
    return false;
  }
}

function test111(param) {
  return Curry._1(Scanf.sscanf("", /* Format */{
                  _0: {
                    TAG: /* Scan_char_set */20,
                    _0: undefined,
                    _1: "\xff\xfb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                    _2: {
                      TAG: /* Formatting_lit */17,
                      _0: /* Force_newline */3,
                      _1: /* End_of_format */0
                    }
                  },
                  _1: "%[^\n]@\n"
                }), (function (x) {
                return x === "";
              }));
}

test("File \"tscanf_test.ml\", line 293, characters 5-12", test11(undefined) && test110(undefined) && test111(undefined));

function ib(param) {
  return Scanf.Scanning.from_string("[1;2;3;4; ]");
}

function f(ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: " [",
              _1: /* End_of_format */0
            },
            _1: " ["
          }), undefined);
  return Curry._1(Scanf.bscanf(ib, /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_i */3,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Char_literal */12,
                        _0: /* ";" */59,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: " %i;"
                }), (function (i) {
                return Curry._1(Scanf.bscanf(ib, /* Format */{
                                _0: {
                                  TAG: /* Char_literal */12,
                                  _0: /* " " */32,
                                  _1: {
                                    TAG: /* Int */4,
                                    _0: /* Int_i */3,
                                    _1: /* No_padding */0,
                                    _2: /* No_precision */0,
                                    _3: {
                                      TAG: /* Char_literal */12,
                                      _0: /* ";" */59,
                                      _1: /* End_of_format */0
                                    }
                                  }
                                },
                                _1: " %i;"
                              }), (function (j) {
                              return Curry._1(Scanf.bscanf(ib, /* Format */{
                                              _0: {
                                                TAG: /* Char_literal */12,
                                                _0: /* " " */32,
                                                _1: {
                                                  TAG: /* Int */4,
                                                  _0: /* Int_i */3,
                                                  _1: /* No_padding */0,
                                                  _2: /* No_precision */0,
                                                  _3: {
                                                    TAG: /* Char_literal */12,
                                                    _0: /* ";" */59,
                                                    _1: /* End_of_format */0
                                                  }
                                                }
                                              },
                                              _1: " %i;"
                                            }), (function (k) {
                                            return Curry._1(Scanf.bscanf(ib, /* Format */{
                                                            _0: {
                                                              TAG: /* Char_literal */12,
                                                              _0: /* " " */32,
                                                              _1: {
                                                                TAG: /* Int */4,
                                                                _0: /* Int_i */3,
                                                                _1: /* No_padding */0,
                                                                _2: /* No_precision */0,
                                                                _3: {
                                                                  TAG: /* Char_literal */12,
                                                                  _0: /* ";" */59,
                                                                  _1: /* End_of_format */0
                                                                }
                                                              }
                                                            },
                                                            _1: " %i;"
                                                          }), (function (l) {
                                                          Curry._1(Scanf.bscanf(ib, /* Format */{
                                                                    _0: {
                                                                      TAG: /* String_literal */11,
                                                                      _0: " ]",
                                                                      _1: /* End_of_format */0
                                                                    },
                                                                    _1: " ]"
                                                                  }), undefined);
                                                          return {
                                                                  hd: i,
                                                                  tl: {
                                                                    hd: j,
                                                                    tl: {
                                                                      hd: k,
                                                                      tl: {
                                                                        hd: l,
                                                                        tl: /* [] */0
                                                                      }
                                                                    }
                                                                  }
                                                                };
                                                        }));
                                          }));
                            }));
              }));
}

function test12(param) {
  return Caml_obj.caml_equal(f(Scanf.Scanning.from_string("[1;2;3;4; ]")), {
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: {
                    hd: 4,
                    tl: /* [] */0
                  }
                }
              }
            });
}

test("File \"tscanf_test.ml\", line 311, characters 5-12", test12(undefined));

function scan_elems(ib, accu) {
  try {
    return Curry._1(Scanf.bscanf(ib, /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* Int */4,
                        _0: /* Int_i */3,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: {
                          TAG: /* Char_literal */12,
                          _0: /* ";" */59,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: " %i;"
                  }), (function (i) {
                  return scan_elems(ib, {
                              hd: i,
                              tl: accu
                            });
                }));
  }
  catch (exn){
    return accu;
  }
}

function g(ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[ ",
              _1: /* End_of_format */0
            },
            _1: "[ "
          }), undefined);
  return List.rev(scan_elems(ib, /* [] */0));
}

function test13(param) {
  return Caml_obj.caml_equal(g(Scanf.Scanning.from_string("[1;2;3;4; ]")), {
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: {
                    hd: 4,
                    tl: /* [] */0
                  }
                }
              }
            });
}

test("File \"tscanf_test.ml\", line 324, characters 5-12", test13(undefined));

function scan_int_list(ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[ ",
              _1: /* End_of_format */0
            },
            _1: "[ "
          }), undefined);
  var accu = scan_elems(ib, /* [] */0);
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: " ]",
              _1: /* End_of_format */0
            },
            _1: " ]"
          }), undefined);
  return List.rev(accu);
}

function test14(param) {
  return Caml_obj.caml_equal(scan_int_list(Scanf.Scanning.from_string("[1;2;3;4; ]")), {
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: {
                    hd: 4,
                    tl: /* [] */0
                  }
                }
              }
            });
}

test("File \"tscanf_test.ml\", line 337, characters 5-12", test14(undefined));

function scan_elems$1(ib, accu) {
  return Curry._1(Scanf.bscanf(ib, /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_i */3,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: {
                          TAG: /* Char */0,
                          _0: /* End_of_format */0
                        }
                      }
                    }
                  },
                  _1: " %i %c"
                }), (function (i, c) {
                if (c === 59) {
                  return scan_elems$1(ib, {
                              hd: i,
                              tl: accu
                            });
                }
                if (c !== 93) {
                  throw {
                        RE_EXN_ID: "Failure",
                        _1: "scan_elems",
                        Error: new Error()
                      };
                }
                return List.rev({
                            hd: i,
                            tl: accu
                          });
              }));
}

function scan_int_list$1(ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[ ",
              _1: /* End_of_format */0
            },
            _1: "[ "
          }), undefined);
  return scan_elems$1(ib, /* [] */0);
}

function test15(param) {
  return Caml_obj.caml_equal(scan_int_list$1(Scanf.Scanning.from_string("[1;2;3;4]")), {
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: {
                    hd: 4,
                    tl: /* [] */0
                  }
                }
              }
            });
}

test("File \"tscanf_test.ml\", line 357, characters 5-12", test15(undefined));

function scan_elems$2(ib, accu) {
  try {
    return Curry._1(Scanf.bscanf(ib, /* Format */{
                    _0: {
                      TAG: /* Char */0,
                      _0: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: {
                          TAG: /* Int */4,
                          _0: /* Int_i */3,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: /* End_of_format */0
                        }
                      }
                    },
                    _1: "%c %i"
                  }), (function (c, i) {
                  if (c >= 91) {
                    if (c < 94) {
                      switch (c) {
                        case 91 :
                            if (accu === /* [] */0) {
                              return scan_elems$2(ib, {
                                          hd: i,
                                          tl: accu
                                        });
                            }
                            break;
                        case 92 :
                            break;
                        case 93 :
                            return List.rev({
                                        hd: i,
                                        tl: accu
                                      });
                        
                      }
                    }
                    
                  } else if (c === 59) {
                    return scan_elems$2(ib, {
                                hd: i,
                                tl: accu
                              });
                  }
                  console.log(Caml_string.make(1, c));
                  throw {
                        RE_EXN_ID: "Failure",
                        _1: "scan_elems",
                        Error: new Error()
                      };
                }));
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Scanf.Scan_failure) {
      Curry._1(Scanf.bscanf(ib, /* Format */{
                _0: {
                  TAG: /* Char_literal */12,
                  _0: /* "]" */93,
                  _1: /* End_of_format */0
                },
                _1: "]"
              }), undefined);
      return accu;
    }
    if (exn.RE_EXN_ID === "End_of_file") {
      return accu;
    }
    throw exn;
  }
}

function test16(param) {
  if (Caml_obj.caml_equal(scan_elems$2(Scanf.Scanning.from_string("[]"), /* [] */0), List.rev(/* [] */0)) && Caml_obj.caml_equal(scan_elems$2(Scanf.Scanning.from_string("[1;2;3;4]"), /* [] */0), List.rev({
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: {
                    hd: 4,
                    tl: /* [] */0
                  }
                }
              }
            })) && Caml_obj.caml_equal(scan_elems$2(Scanf.Scanning.from_string("[1;2;3;4; ]"), /* [] */0), List.rev({
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: {
                    hd: 4,
                    tl: /* [] */0
                  }
                }
              }
            }))) {
    return Caml_obj.caml_equal(scan_elems$2(Scanf.Scanning.from_string("[1;2;3;4"), /* [] */0), List.rev({
                    hd: 1,
                    tl: {
                      hd: 2,
                      tl: {
                        hd: 3,
                        tl: {
                          hd: 4,
                          tl: /* [] */0
                        }
                      }
                    }
                  }));
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 383, characters 5-12", test16(undefined));

function scan_elems$3(ib, accu) {
  return Curry._1(Scanf.bscanf(ib, /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_i */3,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Scan_char_set */20,
                        _0: undefined,
                        _1: "\0&\0\0\x01\0\0\b\0\0\0 \0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                        _2: /* End_of_format */0
                      }
                    }
                  },
                  _1: " %i%[]; \t\n\r]"
                }), (function (i, s) {
                switch (s) {
                  case ";" :
                      return scan_elems$3(ib, {
                                  hd: i,
                                  tl: accu
                                });
                  case "]" :
                      return List.rev({
                                  hd: i,
                                  tl: accu
                                });
                  default:
                    return List.rev({
                                hd: i,
                                tl: accu
                              });
                }
              }));
}

function scan_int_list$2(ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: " [",
              _1: /* End_of_format */0
            },
            _1: " ["
          }), undefined);
  return scan_elems$3(ib, /* [] */0);
}

function test17(param) {
  if (Caml_obj.caml_equal(scan_int_list$2(Scanf.Scanning.from_string("[1;2;3;4]")), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        }) && Caml_obj.caml_equal(scan_int_list$2(Scanf.Scanning.from_string("[1;2;3;4; ]")), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        })) {
    return Caml_obj.caml_equal(scan_int_list$2(Scanf.Scanning.from_string("[1;2;3;4 5]")), {
                hd: 1,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 3,
                    tl: {
                      hd: 4,
                      tl: /* [] */0
                    }
                  }
                }
              });
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 406, characters 5-12", test17(undefined));

function scan_rest(ib, accu) {
  return Curry._1(Scanf.bscanf(ib, /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Char */0,
                      _0: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: " %c "
                }), (function (c) {
                if (c === 59) {
                  return Curry._1(Scanf.bscanf(ib, /* Format */{
                                  _0: {
                                    TAG: /* Scan_char_set */20,
                                    _0: undefined,
                                    _1: "\0\0\0\0\0\0\0\0\0\0\0 \0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                                    _2: /* End_of_format */0
                                  },
                                  _1: "%[]]"
                                }), (function (param) {
                                if (param === "]") {
                                  return accu;
                                } else {
                                  return Curry._1(Scanf.bscanf(ib, /* Format */{
                                                  _0: {
                                                    TAG: /* Char_literal */12,
                                                    _0: /* " " */32,
                                                    _1: {
                                                      TAG: /* Int */4,
                                                      _0: /* Int_i */3,
                                                      _1: /* No_padding */0,
                                                      _2: /* No_precision */0,
                                                      _3: {
                                                        TAG: /* Char_literal */12,
                                                        _0: /* " " */32,
                                                        _1: /* End_of_format */0
                                                      }
                                                    }
                                                  },
                                                  _1: " %i "
                                                }), (function (i) {
                                                return scan_rest(ib, {
                                                            hd: i,
                                                            tl: accu
                                                          });
                                              }));
                                }
                              }));
                }
                if (c !== 93) {
                  throw {
                        RE_EXN_ID: "Failure",
                        _1: "scan_rest",
                        Error: new Error()
                      };
                }
                return accu;
              }));
}

function scan_elems$4(ib, accu) {
  return Curry._1(Scanf.bscanf(ib, /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Char */0,
                      _0: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: " %c "
                }), (function (c) {
                if (c !== 91) {
                  throw {
                        RE_EXN_ID: "Failure",
                        _1: "scan_elems",
                        Error: new Error()
                      };
                }
                if (accu === /* [] */0) {
                  return Curry._1(Scanf.bscanf(ib, /* Format */{
                                  _0: {
                                    TAG: /* Scan_char_set */20,
                                    _0: undefined,
                                    _1: "\0\0\0\0\0\0\0\0\0\0\0 \0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                                    _2: /* End_of_format */0
                                  },
                                  _1: "%[]]"
                                }), (function (param) {
                                if (param === "]") {
                                  return accu;
                                } else {
                                  return Curry._1(Scanf.bscanf(ib, /* Format */{
                                                  _0: {
                                                    TAG: /* Char_literal */12,
                                                    _0: /* " " */32,
                                                    _1: {
                                                      TAG: /* Int */4,
                                                      _0: /* Int_i */3,
                                                      _1: /* No_padding */0,
                                                      _2: /* No_precision */0,
                                                      _3: {
                                                        TAG: /* Char_literal */12,
                                                        _0: /* " " */32,
                                                        _1: /* End_of_format */0
                                                      }
                                                    }
                                                  },
                                                  _1: " %i "
                                                }), (function (i) {
                                                return scan_rest(ib, {
                                                            hd: i,
                                                            tl: accu
                                                          });
                                              }));
                                }
                              }));
                }
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "scan_elems",
                      Error: new Error()
                    };
              }));
}

function scan_int_list$3(ib) {
  return List.rev(scan_elems$4(ib, /* [] */0));
}

function test18(param) {
  var ib = Scanf.Scanning.from_string("[]");
  if (List.rev(scan_elems$4(ib, /* [] */0)) !== /* [] */0) {
    return false;
  }
  var ib$1 = Scanf.Scanning.from_string("[ ]");
  if (List.rev(scan_elems$4(ib$1, /* [] */0)) !== /* [] */0) {
    return false;
  }
  var ib$2 = Scanf.Scanning.from_string("[1;2;3;4]");
  if (!Caml_obj.caml_equal(List.rev(scan_elems$4(ib$2, /* [] */0)), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        })) {
    return false;
  }
  var ib$3 = Scanf.Scanning.from_string("[1;2;3;4; ]");
  return Caml_obj.caml_equal(List.rev(scan_elems$4(ib$3, /* [] */0)), {
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: {
                    hd: 4,
                    tl: /* [] */0
                  }
                }
              }
            });
}

test("File \"tscanf_test.ml\", line 446, characters 5-12", test18(undefined));

function test19(param) {
  return Testing.failure_test(scan_int_list$3, Scanf.Scanning.from_string("[1;2;3;4 5]"), "scan_rest");
}

test19(undefined);

function test20(param) {
  return Testing.scan_failure_test(scan_int_list$3, Scanf.Scanning.from_string("[1;2;3;4;; 5]"));
}

test20(undefined);

function test21(param) {
  return Testing.scan_failure_test(scan_int_list$3, Scanf.Scanning.from_string("[1;2;3;4;;"));
}

test21(undefined);

function scan_rest$1(ib, accu) {
  return Curry._1(Scanf.bscanf(ib, /* Format */{
                  _0: {
                    TAG: /* Scan_char_set */20,
                    _0: undefined,
                    _1: "\0\0\0\0\0\0\0\0\0\0\0 \0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                    _2: /* End_of_format */0
                  },
                  _1: "%[]]"
                }), (function (param) {
                if (param === "]") {
                  return accu;
                } else {
                  return Curry._1(Scanf.bscanf(ib, /* Format */{
                                  _0: {
                                    TAG: /* Char_literal */12,
                                    _0: /* " " */32,
                                    _1: {
                                      TAG: /* Int */4,
                                      _0: /* Int_i */3,
                                      _1: /* No_padding */0,
                                      _2: /* No_precision */0,
                                      _3: {
                                        TAG: /* Char_literal */12,
                                        _0: /* " " */32,
                                        _1: /* End_of_format */0
                                      }
                                    }
                                  },
                                  _1: " %i "
                                }), (function (i) {
                                var accu$1 = {
                                  hd: i,
                                  tl: accu
                                };
                                return Curry._1(Scanf.bscanf(ib, /* Format */{
                                                _0: {
                                                  TAG: /* Scan_char_set */20,
                                                  _0: 1,
                                                  _1: "\0\0\0\0\0\0\0\b\0\0\0 \0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                                                  _2: /* End_of_format */0
                                                },
                                                _1: "%1[];]"
                                              }), (function (param) {
                                              switch (param) {
                                                case ";" :
                                                    return scan_rest$1(ib, accu$1);
                                                case "]" :
                                                    return accu$1;
                                                default:
                                                  var s = Printf.sprintf(/* Format */{
                                                        _0: {
                                                          TAG: /* String_literal */11,
                                                          _0: "scan_int_list",
                                                          _1: /* End_of_format */0
                                                        },
                                                        _1: "scan_int_list"
                                                      });
                                                  throw {
                                                        RE_EXN_ID: "Failure",
                                                        _1: s,
                                                        Error: new Error()
                                                      };
                                              }
                                            }));
                              }));
                }
              }));
}

function scan_int_list$4(ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: " [ ",
              _1: /* End_of_format */0
            },
            _1: " [ "
          }), undefined);
  return List.rev(scan_rest$1(ib, /* [] */0));
}

function test22(param) {
  if (scan_int_list$4(Scanf.Scanning.from_string("[]")) === /* [] */0 && scan_int_list$4(Scanf.Scanning.from_string("[ ]")) === /* [] */0 && Caml_obj.caml_equal(scan_int_list$4(Scanf.Scanning.from_string("[1]")), {
          hd: 1,
          tl: /* [] */0
        }) && Caml_obj.caml_equal(scan_int_list$4(Scanf.Scanning.from_string("[1;2;3;4]")), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        })) {
    return Caml_obj.caml_equal(scan_int_list$4(Scanf.Scanning.from_string("[1;2;3;4;]")), {
                hd: 1,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 3,
                    tl: {
                      hd: 4,
                      tl: /* [] */0
                    }
                  }
                }
              });
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 506, characters 5-12", test22(undefined));

function scan_elems$5(ib, scan_elem, accu) {
  try {
    return Curry._2(scan_elem, ib, (function (i, s) {
                  var accu$1 = {
                    hd: i,
                    tl: accu
                  };
                  if (s === "") {
                    return accu$1;
                  } else {
                    return scan_elems$5(ib, scan_elem, accu$1);
                  }
                }));
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Scanf.Scan_failure) {
      return accu;
    }
    throw exn;
  }
}

function scan_list(scan_elem, ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[ ",
              _1: /* End_of_format */0
            },
            _1: "[ "
          }), undefined);
  var accu = scan_elems$5(ib, scan_elem, /* [] */0);
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: " ]",
              _1: /* End_of_format */0
            },
            _1: " ]"
          }), undefined);
  return List.rev(accu);
}

function scan_int_elem(ib) {
  return Scanf.bscanf(ib, /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: {
                  TAG: /* Int */4,
                  _0: /* Int_i */3,
                  _1: /* No_padding */0,
                  _2: /* No_precision */0,
                  _3: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Scan_char_set */20,
                      _0: 1,
                      _1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                      _2: /* End_of_format */0
                    }
                  }
                }
              },
              _1: " %i %1[;]"
            });
}

function scan_int_list$5(param) {
  return scan_list(scan_int_elem, param);
}

function test23(param) {
  if (scan_list(scan_int_elem, Scanf.Scanning.from_string("[]")) === /* [] */0 && scan_list(scan_int_elem, Scanf.Scanning.from_string("[ ]")) === /* [] */0 && Caml_obj.caml_equal(scan_list(scan_int_elem, Scanf.Scanning.from_string("[1]")), {
          hd: 1,
          tl: /* [] */0
        }) && Caml_obj.caml_equal(scan_list(scan_int_elem, Scanf.Scanning.from_string("[1;2;3;4]")), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        })) {
    return Caml_obj.caml_equal(scan_list(scan_int_elem, Scanf.Scanning.from_string("[1;2;3;4;]")), {
                hd: 1,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 3,
                    tl: {
                      hd: 4,
                      tl: /* [] */0
                    }
                  }
                }
              });
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 562, characters 5-12", test23(undefined));

function test24(param) {
  return Testing.scan_failure_test(scan_int_list$5, Scanf.Scanning.from_string("[1;2;3;4 5]"));
}

function test25(param) {
  return Testing.scan_failure_test(scan_int_list$5, Scanf.Scanning.from_string("[1;2;3;4;;"));
}

function test26(param) {
  return Testing.scan_failure_test(scan_int_list$5, Scanf.Scanning.from_string("[1;2;3;4;; 5]"));
}

function test27(param) {
  return Testing.scan_failure_test(scan_int_list$5, Scanf.Scanning.from_string("[1;2;3;4;; 23]"));
}

test24(undefined) && test25(undefined) && test26(undefined) && test27(undefined);

function scan_string_elem(ib) {
  return Scanf.bscanf(ib, /* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: " \"",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Formatting_lit */17,
                    _0: {
                      TAG: /* Scan_indic */2,
                      _0: /* "\"" */34
                    },
                    _1: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* Scan_char_set */20,
                        _0: 1,
                        _1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                        _2: /* End_of_format */0
                      }
                    }
                  }
                }
              },
              _1: " \"%s@\" %1[;]"
            });
}

function scan_String_elem(ib) {
  return Scanf.bscanf(ib, /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: {
                  TAG: /* Caml_string */3,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Scan_char_set */20,
                      _0: 1,
                      _1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                      _2: /* End_of_format */0
                    }
                  }
                }
              },
              _1: " %S %1[;]"
            });
}

function scan_String_list(param) {
  return scan_list(scan_String_elem, param);
}

function test28(param) {
  if (scan_list(scan_string_elem, Scanf.Scanning.from_string("[]")) === /* [] */0 && Caml_obj.caml_equal(scan_list(scan_string_elem, Scanf.Scanning.from_string("[\"Le\"]")), {
          hd: "Le",
          tl: /* [] */0
        }) && Caml_obj.caml_equal(scan_list(scan_string_elem, Scanf.Scanning.from_string("[\"Le\";\"langage\";\"Objective\";\"Caml\"]")), {
          hd: "Le",
          tl: {
            hd: "langage",
            tl: {
              hd: "Objective",
              tl: {
                hd: "Caml",
                tl: /* [] */0
              }
            }
          }
        }) && Caml_obj.caml_equal(scan_list(scan_string_elem, Scanf.Scanning.from_string("[\"Le\";\"langage\";\"Objective\";\"Caml\"; ]")), {
          hd: "Le",
          tl: {
            hd: "langage",
            tl: {
              hd: "Objective",
              tl: {
                hd: "Caml",
                tl: /* [] */0
              }
            }
          }
        }) && scan_String_list(Scanf.Scanning.from_string("[]")) === /* [] */0 && Caml_obj.caml_equal(scan_String_list(Scanf.Scanning.from_string("[\"Le\"]")), {
          hd: "Le",
          tl: /* [] */0
        }) && Caml_obj.caml_equal(scan_String_list(Scanf.Scanning.from_string("[\"Le\";\"langage\";\"Objective\";\"Caml\"]")), {
          hd: "Le",
          tl: {
            hd: "langage",
            tl: {
              hd: "Objective",
              tl: {
                hd: "Caml",
                tl: /* [] */0
              }
            }
          }
        })) {
    return Caml_obj.caml_equal(scan_String_list(Scanf.Scanning.from_string("[\"Le\";\"langage\";\"Objective\";\"Caml\"; ]")), {
                hd: "Le",
                tl: {
                  hd: "langage",
                  tl: {
                    hd: "Objective",
                    tl: {
                      hd: "Caml",
                      tl: /* [] */0
                    }
                  }
                }
              });
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 609, characters 5-12", test28(undefined));

function scan_elems$6(ib, scan_elem, accu) {
  return Curry._3(scan_elem, ib, (function (i, s) {
                var accu$1 = {
                  hd: i,
                  tl: accu
                };
                if (s === "") {
                  return accu$1;
                } else {
                  return scan_elems$6(ib, scan_elem, accu$1);
                }
              }), (function (ib, exc) {
                return accu;
              }));
}

function scan_list$1(scan_elem, ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[ ",
              _1: /* End_of_format */0
            },
            _1: "[ "
          }), undefined);
  var accu = scan_elems$6(ib, scan_elem, /* [] */0);
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: " ]",
              _1: /* End_of_format */0
            },
            _1: " ]"
          }), undefined);
  return List.rev(accu);
}

function scan_int_elem$1(ib, f, ek) {
  return Curry._1(Scanf.kscanf(ib, ek, /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_i */3,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: {
                          TAG: /* Scan_char_set */20,
                          _0: 1,
                          _1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                          _2: /* End_of_format */0
                        }
                      }
                    }
                  },
                  _1: " %i %1[;]"
                }), f);
}

function test29(param) {
  if (scan_list$1(scan_int_elem$1, Scanf.Scanning.from_string("[]")) === /* [] */0 && scan_list$1(scan_int_elem$1, Scanf.Scanning.from_string("[ ]")) === /* [] */0 && Caml_obj.caml_equal(scan_list$1(scan_int_elem$1, Scanf.Scanning.from_string("[1]")), {
          hd: 1,
          tl: /* [] */0
        }) && Caml_obj.caml_equal(scan_list$1(scan_int_elem$1, Scanf.Scanning.from_string("[1;2;3;4]")), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        })) {
    return Caml_obj.caml_equal(scan_list$1(scan_int_elem$1, Scanf.Scanning.from_string("[1;2;3;4;]")), {
                hd: 1,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 3,
                    tl: {
                      hd: 4,
                      tl: /* [] */0
                    }
                  }
                }
              });
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 639, characters 5-12", test29(undefined));

function scan_string_elem$1(ib, f, ek) {
  return Curry._1(Scanf.kscanf(ib, ek, /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* " " */32,
                    _1: {
                      TAG: /* Caml_string */3,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: {
                          TAG: /* Scan_char_set */20,
                          _0: 1,
                          _1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                          _2: /* End_of_format */0
                        }
                      }
                    }
                  },
                  _1: " %S %1[;]"
                }), f);
}

function test30(param) {
  if (scan_list$1(scan_string_elem$1, Scanf.Scanning.from_string("[]")) === /* [] */0 && scan_list$1(scan_string_elem$1, Scanf.Scanning.from_string("[ ]")) === /* [] */0 && Caml_obj.caml_equal(scan_list$1(scan_string_elem$1, Scanf.Scanning.from_string("[ \"1\" ]")), {
          hd: "1",
          tl: /* [] */0
        }) && Caml_obj.caml_equal(scan_list$1(scan_string_elem$1, Scanf.Scanning.from_string("[\"1\"; \"2\"; \"3\"; \"4\"]")), {
          hd: "1",
          tl: {
            hd: "2",
            tl: {
              hd: "3",
              tl: {
                hd: "4",
                tl: /* [] */0
              }
            }
          }
        })) {
    return Caml_obj.caml_equal(scan_list$1(scan_string_elem$1, Scanf.Scanning.from_string("[\"1\"; \"2\"; \"3\"; \"4\";]")), {
                hd: "1",
                tl: {
                  hd: "2",
                  tl: {
                    hd: "3",
                    tl: {
                      hd: "4",
                      tl: /* [] */0
                    }
                  }
                }
              });
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 656, characters 5-12", test30(undefined));

function scan_elem(fmt, ib, f, ek) {
  return Curry._1(Scanf.kscanf(ib, ek, fmt), f);
}

function scan_elems$7(ib, scan_elem, accu) {
  return Curry._3(scan_elem, ib, (function (i) {
                var accu$1 = {
                  hd: i,
                  tl: accu
                };
                return Curry._1(Scanf.kscanf(ib, (function (ib, exc) {
                                  return accu$1;
                                }), /* Format */{
                                _0: {
                                  TAG: /* Char_literal */12,
                                  _0: /* " " */32,
                                  _1: {
                                    TAG: /* Scan_char_set */20,
                                    _0: 1,
                                    _1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                                    _2: /* End_of_format */0
                                  }
                                },
                                _1: " %1[;]"
                              }), (function (s) {
                              if (s === "") {
                                return accu$1;
                              } else {
                                return scan_elems$7(ib, scan_elem, accu$1);
                              }
                            }));
              }), (function (ib, exc) {
                return accu;
              }));
}

function scan_list$2(scan_elem, ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[ ",
              _1: /* End_of_format */0
            },
            _1: "[ "
          }), undefined);
  var accu = scan_elems$7(ib, scan_elem, /* [] */0);
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: " ]",
              _1: /* End_of_format */0
            },
            _1: " ]"
          }), undefined);
  return List.rev(accu);
}

var partial_arg$1 = /* Format */{
  _0: {
    TAG: /* Char_literal */12,
    _0: /* " " */32,
    _1: {
      TAG: /* Int */4,
      _0: /* Int_i */3,
      _1: /* No_padding */0,
      _2: /* No_precision */0,
      _3: /* End_of_format */0
    }
  },
  _1: " %i"
};

function partial_arg$2(param, param$1, param$2) {
  return scan_elem(partial_arg$1, param, param$1, param$2);
}

function scan_int_list$6(param) {
  return scan_list$2(partial_arg$2, param);
}

var partial_arg$3 = /* Format */{
  _0: {
    TAG: /* Char_literal */12,
    _0: /* " " */32,
    _1: {
      TAG: /* Caml_string */3,
      _0: /* No_padding */0,
      _1: /* End_of_format */0
    }
  },
  _1: " %S"
};

function partial_arg$4(param, param$1, param$2) {
  return scan_elem(partial_arg$3, param, param$1, param$2);
}

function scan_string_list(param) {
  return scan_list$2(partial_arg$4, param);
}

function test31(param) {
  if (Curry._1(scan_int_list$6, Scanf.Scanning.from_string("[]")) === /* [] */0 && Curry._1(scan_int_list$6, Scanf.Scanning.from_string("[ ]")) === /* [] */0 && Caml_obj.caml_equal(Curry._1(scan_int_list$6, Scanf.Scanning.from_string("[1]")), {
          hd: 1,
          tl: /* [] */0
        }) && Caml_obj.caml_equal(Curry._1(scan_int_list$6, Scanf.Scanning.from_string("[1;2;3;4]")), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        })) {
    return Caml_obj.caml_equal(Curry._1(scan_int_list$6, Scanf.Scanning.from_string("[1;2;3;4;]")), {
                hd: 1,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 3,
                    tl: {
                      hd: 4,
                      tl: /* [] */0
                    }
                  }
                }
              });
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 714, characters 5-12", test31(undefined));

function test32(param) {
  if (Curry._1(scan_string_list, Scanf.Scanning.from_string("[]")) === /* [] */0 && Curry._1(scan_string_list, Scanf.Scanning.from_string("[ ]")) === /* [] */0 && Caml_obj.caml_equal(Curry._1(scan_string_list, Scanf.Scanning.from_string("[ \"1\" ]")), {
          hd: "1",
          tl: /* [] */0
        }) && Caml_obj.caml_equal(Curry._1(scan_string_list, Scanf.Scanning.from_string("[\"1\"; \"2\"; \"3\"; \"4\"]")), {
          hd: "1",
          tl: {
            hd: "2",
            tl: {
              hd: "3",
              tl: {
                hd: "4",
                tl: /* [] */0
              }
            }
          }
        })) {
    return Caml_obj.caml_equal(Curry._1(scan_string_list, Scanf.Scanning.from_string("[\"1\"; \"2\"; \"3\"; \"4\";]")), {
                hd: "1",
                tl: {
                  hd: "2",
                  tl: {
                    hd: "3",
                    tl: {
                      hd: "4",
                      tl: /* [] */0
                    }
                  }
                }
              });
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 728, characters 5-12", test32(undefined));

function scan_elems$8(ib, scan_elem_fmt, accu) {
  return Curry._1(Scanf.kscanf(ib, (function (ib, exc) {
                    return accu;
                  }), scan_elem_fmt), (function (i) {
                var accu$1 = {
                  hd: i,
                  tl: accu
                };
                return Curry._1(Scanf.bscanf(ib, /* Format */{
                                _0: {
                                  TAG: /* Char_literal */12,
                                  _0: /* " " */32,
                                  _1: {
                                    TAG: /* Scan_char_set */20,
                                    _0: 1,
                                    _1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                                    _2: {
                                      TAG: /* Char_literal */12,
                                      _0: /* " " */32,
                                      _1: /* End_of_format */0
                                    }
                                  }
                                },
                                _1: " %1[;] "
                              }), (function (param) {
                              if (param === "") {
                                return accu$1;
                              } else {
                                return scan_elems$8(ib, scan_elem_fmt, accu$1);
                              }
                            }));
              }));
}

function scan_list$3(scan_elem_fmt, ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[ ",
              _1: /* End_of_format */0
            },
            _1: "[ "
          }), undefined);
  var accu = scan_elems$8(ib, scan_elem_fmt, /* [] */0);
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: " ]",
              _1: /* End_of_format */0
            },
            _1: " ]"
          }), undefined);
  return List.rev(accu);
}

var partial_arg$5 = /* Format */{
  _0: {
    TAG: /* Int */4,
    _0: /* Int_i */3,
    _1: /* No_padding */0,
    _2: /* No_precision */0,
    _3: /* End_of_format */0
  },
  _1: "%i"
};

function scan_int_list$7(param) {
  return scan_list$3(partial_arg$5, param);
}

var partial_arg$6 = /* Format */{
  _0: {
    TAG: /* Caml_string */3,
    _0: /* No_padding */0,
    _1: /* End_of_format */0
  },
  _1: "%S"
};

function scan_string_list$1(param) {
  return scan_list$3(partial_arg$6, param);
}

function test33(param) {
  if (Curry._1(scan_int_list$7, Scanf.Scanning.from_string("[]")) === /* [] */0 && Curry._1(scan_int_list$7, Scanf.Scanning.from_string("[ ]")) === /* [] */0 && Caml_obj.caml_equal(Curry._1(scan_int_list$7, Scanf.Scanning.from_string("[ 1 ]")), {
          hd: 1,
          tl: /* [] */0
        }) && Caml_obj.caml_equal(Curry._1(scan_int_list$7, Scanf.Scanning.from_string("[ 1; 2; 3; 4 ]")), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        })) {
    return Caml_obj.caml_equal(Curry._1(scan_int_list$7, Scanf.Scanning.from_string("[1;2;3;4;]")), {
                hd: 1,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 3,
                    tl: {
                      hd: 4,
                      tl: /* [] */0
                    }
                  }
                }
              });
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 773, characters 5-12", test33(undefined));

function test34(param) {
  if (Curry._1(scan_string_list$1, Scanf.Scanning.from_string("[]")) === /* [] */0 && Curry._1(scan_string_list$1, Scanf.Scanning.from_string("[ ]")) === /* [] */0 && Caml_obj.caml_equal(Curry._1(scan_string_list$1, Scanf.Scanning.from_string("[ \"1\" ]")), {
          hd: "1",
          tl: /* [] */0
        }) && Caml_obj.caml_equal(Curry._1(scan_string_list$1, Scanf.Scanning.from_string("[\"1\"; \"2\"; \"3\"; \"4\"]")), {
          hd: "1",
          tl: {
            hd: "2",
            tl: {
              hd: "3",
              tl: {
                hd: "4",
                tl: /* [] */0
              }
            }
          }
        })) {
    return Caml_obj.caml_equal(Curry._1(scan_string_list$1, Scanf.Scanning.from_string("[\"1\"; \"2\"; \"3\"; \"4\";]")), {
                hd: "1",
                tl: {
                  hd: "2",
                  tl: {
                    hd: "3",
                    tl: {
                      hd: "4",
                      tl: /* [] */0
                    }
                  }
                }
              });
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 787, characters 5-12", test34(undefined));

function scan_elems$9(scan_elem, accu, ib) {
  return Curry._2(Scanf.kscanf(ib, (function (ib, exc) {
                    return accu;
                  }), /* Format */{
                  _0: {
                    TAG: /* Reader */19,
                    _0: /* End_of_format */0
                  },
                  _1: "%r"
                }), (function (ib) {
                return Curry._2(scan_elem, ib, (function (elem) {
                              var accu$1 = {
                                hd: elem,
                                tl: accu
                              };
                              return Curry._1(Scanf.bscanf(ib, /* Format */{
                                              _0: {
                                                TAG: /* Char_literal */12,
                                                _0: /* " " */32,
                                                _1: {
                                                  TAG: /* Scan_char_set */20,
                                                  _0: 1,
                                                  _1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                                                  _2: {
                                                    TAG: /* Char_literal */12,
                                                    _0: /* " " */32,
                                                    _1: /* End_of_format */0
                                                  }
                                                }
                                              },
                                              _1: " %1[;] "
                                            }), (function (param) {
                                            if (param === "") {
                                              return accu$1;
                                            } else {
                                              return scan_elems$9(scan_elem, accu$1, ib);
                                            }
                                          }));
                            }));
              }), (function (l) {
                return l;
              }));
}

function scan_list$4(scan_elem, ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[ ",
              _1: /* End_of_format */0
            },
            _1: "[ "
          }), undefined);
  var accu = scan_elems$9(scan_elem, /* [] */0, ib);
  Curry._1(Scanf.bscanf(ib, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: " ]",
              _1: /* End_of_format */0
            },
            _1: " ]"
          }), undefined);
  return List.rev(accu);
}

function scan_float(ib) {
  return Scanf.bscanf(ib, /* Format */{
              _0: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%f"
            });
}

function scan_int_list$8(param) {
  return scan_list$4((function (ib) {
                return Scanf.bscanf(ib, /* Format */{
                            _0: {
                              TAG: /* Int */4,
                              _0: /* Int_i */3,
                              _1: /* No_padding */0,
                              _2: /* No_precision */0,
                              _3: /* End_of_format */0
                            },
                            _1: "%i"
                          });
              }), param);
}

function scan_string_list$2(param) {
  return scan_list$4((function (ib) {
                return Scanf.bscanf(ib, /* Format */{
                            _0: {
                              TAG: /* Caml_string */3,
                              _0: /* No_padding */0,
                              _1: /* End_of_format */0
                            },
                            _1: "%S"
                          });
              }), param);
}

function scan_bool_list(param) {
  return scan_list$4((function (ib) {
                return Scanf.bscanf(ib, /* Format */{
                            _0: {
                              TAG: /* Bool */9,
                              _0: /* No_padding */0,
                              _1: /* End_of_format */0
                            },
                            _1: "%B"
                          });
              }), param);
}

function scan_char_list(param) {
  return scan_list$4((function (ib) {
                return Scanf.bscanf(ib, /* Format */{
                            _0: {
                              TAG: /* Caml_char */1,
                              _0: /* End_of_format */0
                            },
                            _1: "%C"
                          });
              }), param);
}

function scan_float_list_list(param) {
  return scan_list$4((function (ib, k) {
                return Curry._1(k, scan_list$4(scan_float, ib));
              }), param);
}

function test340(param) {
  return Caml_obj.caml_equal(scan_float_list_list(Scanf.Scanning.from_string("[[1.0] ; []; [2.0; 3; 5.0; 6.];]")), {
              hd: {
                hd: 1,
                tl: /* [] */0
              },
              tl: {
                hd: /* [] */0,
                tl: {
                  hd: {
                    hd: 2,
                    tl: {
                      hd: 3,
                      tl: {
                        hd: 5,
                        tl: {
                          hd: 6,
                          tl: /* [] */0
                        }
                      }
                    }
                  },
                  tl: /* [] */0
                }
              }
            });
}

function scan_list_list(scan_elems, ib) {
  return scan_list$4((function (ib, k) {
                return Curry._1(k, Curry._1(scan_elems, ib));
              }), ib);
}

function scan_float_item(ib, k) {
  return Curry._1(k, Curry._1(scan_float(ib), (function (x) {
                    return x;
                  })));
}

function scan_float_list(ib, k) {
  return Curry._1(k, scan_list$4(scan_float_item, ib));
}

function scan_float_list_list$1(ib, k) {
  return Curry._1(k, scan_list$4(scan_float_list, ib));
}

function test35(param) {
  if (Curry._1(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* Scan_get_counter */21,
                _0: /* Token_counter */2,
                _1: /* End_of_format */0
              },
              _1: "%N"
            }), (function (x) {
            return x;
          })) === 0 && Curry._1(Scanf.sscanf("456", /* Format */{
              _0: {
                TAG: /* Scan_get_counter */21,
                _0: /* Token_counter */2,
                _1: /* End_of_format */0
              },
              _1: "%N"
            }), (function (x) {
            return x;
          })) === 0 && Caml_obj.caml_equal(Curry._1(Scanf.sscanf("456", /* Format */{
                  _0: {
                    TAG: /* Int */4,
                    _0: /* Int_d */0,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* Scan_get_counter */21,
                      _0: /* Token_counter */2,
                      _1: /* End_of_format */0
                    }
                  },
                  _1: "%d%N"
                }), (function (x, y) {
                return [
                        x,
                        y
                      ];
              })), [
          456,
          1
        ])) {
    return Caml_obj.caml_equal(Curry._1(Scanf.sscanf(" ", /* Format */{
                        _0: {
                          TAG: /* Scan_get_counter */21,
                          _0: /* Token_counter */2,
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* Scan_get_counter */21,
                              _0: /* Token_counter */2,
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "%N%s%N"
                      }), (function (x, s, y) {
                      return [
                              x,
                              s,
                              y
                            ];
                    })), [
                0,
                "",
                1
              ]);
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 940, characters 5-12", test340(undefined) && test35(undefined));

function read_elems(read_elem, accu, ib) {
  return Curry._2(Scanf.kscanf(ib, (function (ib, exc) {
                    return accu;
                  }), /* Format */{
                  _0: {
                    TAG: /* Reader */19,
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* Scan_char_set */20,
                        _0: 1,
                        _1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                        _2: {
                          TAG: /* Char_literal */12,
                          _0: /* " " */32,
                          _1: /* End_of_format */0
                        }
                      }
                    }
                  },
                  _1: "%r %1[;] "
                }), Curry._1(read_elem, (function (elem) {
                    return {
                            hd: elem,
                            tl: accu
                          };
                  })), (function (accu, s) {
                if (s === "") {
                  return accu;
                } else {
                  return read_elems(read_elem, accu, ib);
                }
              }));
}

function read_list(read_elem, ib) {
  return Curry._2(Scanf.bscanf(ib, /* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "[ ",
                    _1: {
                      TAG: /* Reader */19,
                      _0: {
                        TAG: /* String_literal */11,
                        _0: " ]",
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "[ %r ]"
                }), (function (param) {
                return read_elems(read_elem, /* [] */0, param);
              }), List.rev);
}

function make_read_elem(fmt, f, ib) {
  return Curry._1(Scanf.bscanf(ib, fmt), f);
}

function scan_List(fmt) {
  return function (param) {
    return read_list((function (param, param$1) {
                  return Curry._1(Scanf.bscanf(param$1, fmt), param);
                }), param);
  };
}

function test36(param) {
  if (Curry._1(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* Scan_get_counter */21,
                _0: /* Char_counter */1,
                _1: /* End_of_format */0
              },
              _1: "%n"
            }), (function (x) {
            return x;
          })) === 0 && Curry._1(Scanf.sscanf("456", /* Format */{
              _0: {
                TAG: /* Scan_get_counter */21,
                _0: /* Char_counter */1,
                _1: /* End_of_format */0
              },
              _1: "%n"
            }), (function (x) {
            return x;
          })) === 0 && Caml_obj.caml_equal(Curry._1(Scanf.sscanf("456", /* Format */{
                  _0: {
                    TAG: /* Int */4,
                    _0: /* Int_d */0,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* Scan_get_counter */21,
                      _0: /* Char_counter */1,
                      _1: /* End_of_format */0
                    }
                  },
                  _1: "%d%n"
                }), (function (x, y) {
                return [
                        x,
                        y
                      ];
              })), [
          456,
          3
        ])) {
    return Caml_obj.caml_equal(Curry._1(Scanf.sscanf(" ", /* Format */{
                        _0: {
                          TAG: /* Scan_get_counter */21,
                          _0: /* Char_counter */1,
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* Scan_get_counter */21,
                              _0: /* Char_counter */1,
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "%n%s%n"
                      }), (function (x, s, y) {
                      return [
                              x,
                              s,
                              y
                            ];
                    })), [
                0,
                "",
                0
              ]);
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 995, characters 5-12", test36(undefined));

function test37(param) {
  if (Curry._1(Scanf.sscanf("", /* Format */{
              _0: /* End_of_format */0,
              _1: ""
            }), true) && Curry._2(Scanf.sscanf("", /* Format */{
              _0: /* End_of_format */0,
              _1: ""
            }), (function (x) {
            return x;
          }), 1) === 1) {
    return Curry._2(Scanf.sscanf("123", /* Format */{
                    _0: /* End_of_format */0,
                    _1: ""
                  }), (function (x) {
                  return x;
                }), 1) === 1;
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1005, characters 5-12", test37(undefined));

function test38(param) {
  if (Curry._1(Scanf.sscanf("a", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* "a" */97,
                _1: {
                  TAG: /* Flush */10,
                  _0: /* End_of_format */0
                }
              },
              _1: "a%!"
            }), true) && Curry._1(Scanf.sscanf("a", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* "a" */97,
                _1: {
                  TAG: /* Flush */10,
                  _0: {
                    TAG: /* Flush */10,
                    _0: /* End_of_format */0
                  }
                }
              },
              _1: "a%!%!"
            }), true) && Curry._1(Scanf.sscanf(" a", /* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: " a",
                _1: {
                  TAG: /* Flush */10,
                  _0: /* End_of_format */0
                }
              },
              _1: " a%!"
            }), true) && Curry._1(Scanf.sscanf("a ", /* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "a ",
                _1: {
                  TAG: /* Flush */10,
                  _0: /* End_of_format */0
                }
              },
              _1: "a %!"
            }), true) && Curry._1(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* Flush */10,
                _0: /* End_of_format */0
              },
              _1: "%!"
            }), true) && Curry._1(Scanf.sscanf(" ", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: {
                  TAG: /* Flush */10,
                  _0: /* End_of_format */0
                }
              },
              _1: " %!"
            }), true) && Curry._1(Scanf.sscanf("", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: {
                  TAG: /* Flush */10,
                  _0: /* End_of_format */0
                }
              },
              _1: " %!"
            }), true)) {
    return Curry._1(Scanf.sscanf("", /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* Flush */10,
                        _0: {
                          TAG: /* Flush */10,
                          _0: /* End_of_format */0
                        }
                      }
                    },
                    _1: " %!%!"
                  }), true);
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1020, characters 5-12", test38(undefined));

function test39(param) {
  var is_empty_buff = function (ib) {
    if (Scanf.Scanning.beginning_of_input(ib)) {
      return Scanf.Scanning.end_of_input(ib);
    } else {
      return false;
    }
  };
  var ib = Scanf.Scanning.from_string("");
  if (is_empty_buff(ib)) {
    return is_empty_buff(ib);
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1036, characters 5-12", test39(undefined));

function test40(param) {
  var ib = Scanf.Scanning.from_string("cba");
  return Curry._1(Scanf.bscanf(ib, /* Format */{
                  _0: {
                    TAG: /* Scan_char_set */20,
                    _0: undefined,
                    _1: "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf9\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                    _2: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Flush */10,
                        _0: /* End_of_format */0
                      }
                    }
                  },
                  _1: "%[^ab]%s%!"
                }), (function (s1, s2) {
                if (s1 === "c") {
                  return s2 === "ba";
                } else {
                  return false;
                }
              }));
}

test("File \"tscanf_test.ml\", line 1046, characters 5-12", test40(undefined));

function test41(param) {
  var ib = Scanf.Scanning.from_string("cba");
  return Curry._1(Scanf.bscanf(ib, /* Format */{
                  _0: {
                    TAG: /* Scan_char_set */20,
                    _0: undefined,
                    _1: "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf1\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                    _2: {
                      TAG: /* Scan_char_set */20,
                      _0: undefined,
                      _1: "\0\0\0\0\0\0\0\0\0\0\0\0\x0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                      _2: {
                        TAG: /* Flush */10,
                        _0: /* End_of_format */0
                      }
                    }
                  },
                  _1: "%[^abc]%[cba]%!"
                }), (function (s1, s2) {
                if (s1 === "") {
                  return s2 === "cba";
                } else {
                  return false;
                }
              }));
}

test("File \"tscanf_test.ml\", line 1055, characters 5-12", test41(undefined));

function test42(param) {
  var s = "defcbaaghi";
  var ib = Scanf.Scanning.from_string(s);
  if (!Curry._1(Scanf.bscanf(ib, /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf1\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                _2: {
                  TAG: /* Scan_char_set */20,
                  _0: undefined,
                  _1: "\0\0\0\0\0\0\0\0\0\0\0\0\x0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                  _2: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Flush */10,
                      _0: /* End_of_format */0
                    }
                  }
                }
              },
              _1: "%[^abc]%[abc]%s%!"
            }), (function (s1, s2, s3) {
            if (s1 === "def" && s2 === "cbaa") {
              return s3 === "ghi";
            } else {
              return false;
            }
          }))) {
    return false;
  }
  var ib$1 = Scanf.Scanning.from_string(s);
  return Curry._1(Scanf.bscanf(ib$1, /* Format */{
                  _0: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Formatting_lit */17,
                      _0: {
                        TAG: /* Scan_indic */2,
                        _0: /* "\t" */9
                      },
                      _1: /* End_of_format */0
                    }
                  },
                  _1: "%s@\t"
                }), (function (s) {
                return s === "defcbaaghi";
              }));
}

test("File \"tscanf_test.ml\", line 1067, characters 5-12", test42(undefined));

var ib$1 = Scanf.Scanning.from_string("");

function test43(param) {
  return Curry._1(Scanf.bscanf(ib$1, /* Format */{
                  _0: {
                    TAG: /* Int */4,
                    _0: /* Int_i */3,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* Flush */10,
                      _0: /* End_of_format */0
                    }
                  },
                  _1: "%i%!"
                }), (function (i) {
                return i;
              }));
}

function test44(param) {
  return Curry._1(Scanf.bscanf(ib$1, /* Format */{
                  _0: {
                    TAG: /* Flush */10,
                    _0: {
                      TAG: /* Int */4,
                      _0: /* Int_i */3,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: /* End_of_format */0
                    }
                  },
                  _1: "%!%i"
                }), (function (i) {
                return i;
              }));
}

Testing.test_raises_this_exc({
        RE_EXN_ID: "End_of_file"
      })(test43, undefined) && Testing.test_raises_this_exc({
        RE_EXN_ID: "End_of_file"
      })(test44, undefined);

function test45(param) {
  var ib = Scanf.Scanning.from_string("12.2");
  return Curry._1(Scanf.bscanf(ib, /* Format */{
                  _0: {
                    TAG: /* Scan_char_set */20,
                    _0: undefined,
                    _1: "\0\0\0\0\0\0\xff\x03\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                    _2: {
                      TAG: /* Char_literal */12,
                      _0: /* "." */46,
                      _1: {
                        TAG: /* Scan_char_set */20,
                        _0: undefined,
                        _1: "\0\0\0\0\0\0\xff\x03\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                        _2: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Flush */10,
                            _0: /* End_of_format */0
                          }
                        }
                      }
                    }
                  },
                  _1: "%[0-9].%[0-9]%s%!"
                }), (function (s1, s2, s3) {
                if (s1 === "12" && s2 === "2") {
                  return s3 === "";
                } else {
                  return false;
                }
              }));
}

test("File \"tscanf_test.ml\", line 1090, characters 5-12", test45(undefined));

function test46(param) {
  return Curry._3(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* Int */4,
                    _0: /* Int_i */3,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* Format_subst */14,
                        _0: undefined,
                        _1: {
                          TAG: /* String_ty */1,
                          _0: /* End_of_fmtty */0
                        },
                        _2: {
                          TAG: /* Char_literal */12,
                          _0: /* "." */46,
                          _1: /* End_of_format */0
                        }
                      }
                    }
                  },
                  _1: "%i %(%s%)."
                }), 1, /* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "spells one, ",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "spells one, %s"
            }, "in english");
}

function test47(param) {
  return Curry._3(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* Int */4,
                    _0: /* Int_i */3,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* Char_literal */12,
                      _0: /* " " */32,
                      _1: {
                        TAG: /* Format_arg */13,
                        _0: undefined,
                        _1: {
                          TAG: /* String_ty */1,
                          _0: /* End_of_fmtty */0
                        },
                        _2: {
                          TAG: /* String_literal */11,
                          _0: ", ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* Char_literal */12,
                              _0: /* "." */46,
                              _1: /* End_of_format */0
                            }
                          }
                        }
                      }
                    }
                  },
                  _1: "%i %{%s%}, %s."
                }), 1, /* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "spells one ",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "spells one %s"
            }, "in english");
}

test("File \"tscanf_test.ml\", line 1104, characters 5-12", test46(undefined) === "1 spells one, in english.");

test("File \"tscanf_test.ml\", line 1106, characters 5-12", test47(undefined) === "1 %s, in english.");

function test48(param) {
  var test_meta_read = function (s, fmt, efmt) {
    return Caml_obj.caml_equal(Scanf.format_from_string(s, fmt), efmt);
  };
  var fmt = /* Format */{
    _0: {
      TAG: /* Int */4,
      _0: /* Int_i */3,
      _1: /* No_padding */0,
      _2: /* No_precision */0,
      _3: /* End_of_format */0
    },
    _1: "%i"
  };
  if (!test_meta_read("%i", fmt, fmt)) {
    return false;
  }
  if (!test_meta_read("%i", /* Format */{
          _0: {
            TAG: /* Int */4,
            _0: /* Int_d */0,
            _1: /* No_padding */0,
            _2: /* No_precision */0,
            _3: /* End_of_format */0
          },
          _1: "%d"
        }, /* Format */{
          _0: {
            TAG: /* Int */4,
            _0: /* Int_i */3,
            _1: /* No_padding */0,
            _2: /* No_precision */0,
            _3: /* End_of_format */0
          },
          _1: "%i"
        })) {
    return false;
  }
  if (!Curry._1(Scanf.sscanf("12 \"%i\"89 ", /* Format */{
              _0: {
                TAG: /* Int */4,
                _0: /* Int_i */3,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* " " */32,
                  _1: {
                    TAG: /* Format_arg */13,
                    _0: undefined,
                    _1: {
                      TAG: /* Int_ty */2,
                      _0: /* End_of_fmtty */0
                    },
                    _2: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: {
                          TAG: /* Flush */10,
                          _0: /* End_of_format */0
                        }
                      }
                    }
                  }
                }
              },
              _1: "%i %{%d%}%s %!"
            }), (function (i, f, s) {
            if (i === 12 && Caml_obj.caml_equal(f, /* Format */{
                    _0: {
                      TAG: /* Int */4,
                      _0: /* Int_i */3,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: /* End_of_format */0
                    },
                    _1: "%i"
                  })) {
              return s === "89";
            } else {
              return false;
            }
          }))) {
    return false;
  }
  var k = function (s) {
    return Curry._1(Scanf.sscanf(s, /* Format */{
                    _0: {
                      TAG: /* Format_subst */14,
                      _0: undefined,
                      _1: {
                        TAG: /* Float_ty */6,
                        _0: /* End_of_fmtty */0
                      },
                      _2: /* End_of_format */0
                    },
                    _1: "%(%f%)"
                  }), (function (_fmt, i) {
                  return i;
                }));
  };
  if (k("\" : %1f\": 987654321") !== 9.0) {
    return false;
  }
  if (k("\" : %2f\": 987654321") !== 98.0) {
    return false;
  }
  if (k("\" : %3f\": 9.87654321") !== 9.8) {
    return false;
  }
  if (k("\" : %4f\": 9.87654321") !== 9.87) {
    return false;
  }
  var h = function (s) {
    return Curry._1(Scanf.sscanf(s, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "Read integers with ",
                      _1: {
                        TAG: /* Format_subst */14,
                        _0: undefined,
                        _1: {
                          TAG: /* Int_ty */2,
                          _0: /* End_of_fmtty */0
                        },
                        _2: /* End_of_format */0
                      }
                    },
                    _1: "Read integers with %(%i%)"
                  }), (function (_fmt, i) {
                  return i;
                }));
  };
  if (h("Read integers with \"%1d\"987654321") !== 9) {
    return false;
  }
  if (h("Read integers with \"%2d\"987654321") !== 98) {
    return false;
  }
  if (h("Read integers with \"%3u\"987654321") !== 987) {
    return false;
  }
  if (h("Read integers with \"%4x\"987654321") !== 39030) {
    return false;
  }
  var i = function (s) {
    return Curry._1(Scanf.sscanf(s, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "with ",
                      _1: {
                        TAG: /* Format_subst */14,
                        _0: undefined,
                        _1: {
                          TAG: /* Int_ty */2,
                          _0: {
                            TAG: /* String_ty */1,
                            _0: /* End_of_fmtty */0
                          }
                        },
                        _2: /* End_of_format */0
                      }
                    },
                    _1: "with %(%i %s%)"
                  }), (function (_fmt, amount, currency) {
                  return [
                          amount,
                          currency
                        ];
                }));
  };
  if (!Caml_obj.caml_equal(i("with \" : %d %s\" :        21 euros"), [
          21,
          "euros"
        ])) {
    return false;
  }
  if (!Caml_obj.caml_equal(i("with \" : %d %s\" : 987654321 dollars"), [
          987654321,
          "dollars"
        ])) {
    return false;
  }
  if (!Caml_obj.caml_equal(i("with \" : %u %s\" :     54321 pounds"), [
          54321,
          "pounds"
        ])) {
    return false;
  }
  if (!Caml_obj.caml_equal(i("with \" : %x %s\" :       321 yens"), [
          801,
          "yens"
        ])) {
    return false;
  }
  var j = function (s) {
    return Curry._1(Scanf.sscanf(s, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "with ",
                      _1: {
                        TAG: /* Format_subst */14,
                        _0: undefined,
                        _1: {
                          TAG: /* Int_ty */2,
                          _0: {
                            TAG: /* String_ty */1,
                            _0: /* End_of_fmtty */0
                          }
                        },
                        _2: /* End_of_format */0
                      }
                    },
                    _1: "with %(%i %_s %s%)"
                  }), (function (_fmt, amount, currency) {
                  return [
                          amount,
                          currency
                        ];
                }));
  };
  if (Caml_obj.caml_equal(j("with \" : %1d %_s %s\" : 987654321 euros"), [
          9,
          "euros"
        ]) && Caml_obj.caml_equal(j("with \" : %2d %_s %s\" : 987654321 dollars"), [
          98,
          "dollars"
        ]) && Caml_obj.caml_equal(j("with \" : %3u %_s %s\" : 987654321 pounds"), [
          987,
          "pounds"
        ])) {
    return Caml_obj.caml_equal(j("with \" : %4x %_s %s\" : 987654321 yens"), [
                39030,
                "yens"
              ]);
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1157, characters 5-12", test48(undefined));

function test49(param) {
  if (Curry._1(Scanf.sscanf("as", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\0\0\0\0\0\0\0\0\0\0\0\x10\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                _2: /* End_of_format */0
              },
              _1: "%[\\]"
            }), (function (s) {
            return s === "";
          })) && Curry._1(Scanf.sscanf("as", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\0\0\0\0\0\0\0\0\0\0\0\x10\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                _2: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "%[\\]%s"
            }), (function (s, t) {
            return s === "" ? t === "as" : false;
          })) && Curry._1(Scanf.sscanf("as", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\0\0\0\0\0\0\0\0\0\0\0\x10\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                _2: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Flush */10,
                    _0: /* End_of_format */0
                  }
                }
              },
              _1: "%[\\]%s%!"
            }), (function (s, t) {
            return s === "" ? t === "as" : false;
          })) && Curry._1(Scanf.sscanf("as", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\0\0\0\0\0@\0\0\0\0\0\0\x02\0\0\x04\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                _2: /* End_of_format */0
              },
              _1: "%[a..z]"
            }), (function (s) {
            return s === "a";
          })) && Curry._1(Scanf.sscanf("as", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\0\0\0\0\0\0\0\0\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                _2: /* End_of_format */0
              },
              _1: "%[a-z]"
            }), (function (s) {
            return s === "as";
          })) && Curry._1(Scanf.sscanf("as", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\0\0\0\0\0@\0\0\0\0\0\0\x02\0\0\x04\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                _2: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "%[a..z]%s"
            }), (function (s, t) {
            return s === "a" ? t === "s" : false;
          })) && Curry._1(Scanf.sscanf("as", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\0\0\0\0\0\0\0\0\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                _2: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "%[a-z]%s"
            }), (function (s, t) {
            return s === "as" ? t === "" : false;
          })) && Curry._1(Scanf.sscanf("-as", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\0\0\0\0\0 \0\0\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                _2: /* End_of_format */0
              },
              _1: "%[-a-z]"
            }), (function (s) {
            return s === "-as";
          })) && Curry._1(Scanf.sscanf("-as", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\0\0\0\0\0 \0\0\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                _2: {
                  TAG: /* Formatting_lit */17,
                  _0: {
                    TAG: /* Scan_indic */2,
                    _0: /* "s" */115
                  },
                  _1: /* End_of_format */0
                }
              },
              _1: "%[-a-z]@s"
            }), (function (s) {
            return s === "-a";
          })) && Curry._1(Scanf.sscanf("-as", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* "-" */45,
                _1: {
                  TAG: /* Scan_char_set */20,
                  _0: undefined,
                  _1: "\0\0\0\0\0\0\0\0\0\0\0\0\x02\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                  _2: {
                    TAG: /* Formatting_lit */17,
                    _0: {
                      TAG: /* Scan_indic */2,
                      _0: /* "s" */115
                    },
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "-%[a]@s"
            }), (function (s) {
            return s === "a";
          })) && Curry._1(Scanf.sscanf("-asb", /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* "-" */45,
                _1: {
                  TAG: /* Scan_char_set */20,
                  _0: undefined,
                  _1: "\0\0\0\0\0\0\0\0\0\0\0\0\x02\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                  _2: {
                    TAG: /* Formatting_lit */17,
                    _0: {
                      TAG: /* Scan_indic */2,
                      _0: /* "s" */115
                    },
                    _1: {
                      TAG: /* Char_literal */12,
                      _0: /* "b" */98,
                      _1: {
                        TAG: /* Flush */10,
                        _0: /* End_of_format */0
                      }
                    }
                  }
                }
              },
              _1: "-%[a]@sb%!"
            }), (function (s) {
            return s === "a";
          }))) {
    return Curry._1(Scanf.sscanf("-asb", /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* "-" */45,
                      _1: {
                        TAG: /* Scan_char_set */20,
                        _0: undefined,
                        _1: "\0\0\0\0\0\0\0\0\0\0\0\0\x02\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                        _2: {
                          TAG: /* Formatting_lit */17,
                          _0: {
                            TAG: /* Scan_indic */2,
                            _0: /* "s" */115
                          },
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    },
                    _1: "-%[a]@s%s"
                  }), (function (s, t) {
                  if (s === "a") {
                    return t === "b";
                  } else {
                    return false;
                  }
                }));
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1176, characters 5-12", test49(undefined));

function next_char(ob, param) {
  var s = $$Buffer.contents(ob);
  var len = s.length;
  if (len === 0) {
    throw {
          RE_EXN_ID: "End_of_file",
          Error: new Error()
        };
  }
  var c = Caml_string.get(s, 0);
  ob.position = 0;
  $$Buffer.add_string(ob, $$String.sub(s, 1, len - 1 | 0));
  return c;
}

function send_string(ob, s) {
  $$Buffer.add_string(ob, s);
  return $$Buffer.add_char(ob, /* "\n" */10);
}

function send_int(ob, i) {
  return send_string(ob, String(i));
}

function writer(ib, ob) {
  return Curry._1(Scanf.bscanf(ib, /* Format */{
                  _0: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Char_literal */12,
                      _0: /* "\n" */10,
                      _1: /* End_of_format */0
                    }
                  },
                  _1: "%s\n"
                }), (function (s) {
                switch (s) {
                  case "start" :
                      send_string(ob, "Hello World!");
                      return reader(ib, ob);
                  case "stop" :
                      return Curry._1(Scanf.bscanf(ib, /* Format */{
                                      _0: {
                                        TAG: /* Int */4,
                                        _0: /* Int_i */3,
                                        _1: /* No_padding */0,
                                        _2: /* No_precision */0,
                                        _3: /* End_of_format */0
                                      },
                                      _1: "%i"
                                    }), (function (i) {
                                    return i;
                                  }));
                  default:
                    var i = Caml_format.caml_int_of_string(s);
                    send_string(ob, String(i));
                    return reader(ib, ob);
                }
              }));
}

var count = {
  contents: 0
};

function reader(ib, ob) {
  if (Scanf.Scanning.beginning_of_input(ib)) {
    count.contents = 0;
    send_string(ob, "start");
    return writer(ib, ob);
  } else {
    return Curry._1(Scanf.bscanf(ib, /* Format */{
                    _0: {
                      TAG: /* Scan_char_set */20,
                      _0: undefined,
                      _1: "\xff\xfb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                      _2: {
                        TAG: /* Char_literal */12,
                        _0: /* "\n" */10,
                        _1: /* End_of_format */0
                      }
                    },
                    _1: "%[^\n]\n"
                  }), (function (s) {
                  if (s === "stop") {
                    send_string(ob, "stop");
                    return writer(ib, ob);
                  }
                  var l = s.length;
                  count.contents = l + count.contents | 0;
                  if (count.contents >= 100) {
                    send_string(ob, "stop");
                    send_string(ob, String(count.contents));
                  } else {
                    send_string(ob, String(l));
                  }
                  return writer(ib, ob);
                }));
  }
}

function go(param) {
  var ob = $$Buffer.create(17);
  var ib = Scanf.Scanning.from_function(function (param) {
        return next_char(ob, param);
      });
  return reader(ib, ob);
}

function test50(param) {
  return go(undefined) === 100;
}

test("File \"tscanf_test.ml\", line 1228, characters 5-12", go(undefined) === 100);

function test51(param) {
  if (Curry._1(Scanf.sscanf("Hello", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: /* End_of_format */0
              },
              _1: "%s"
            }), id) === "Hello" && Curry._1(Scanf.sscanf("Hello\n", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              },
              _1: "%s\n"
            }), id) === "Hello" && Curry._1(Scanf.sscanf("Hello\n", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* "\n" */10,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "%s%s\n"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "" : false;
          })) && Curry._1(Scanf.sscanf("Hello\nWorld", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Flush */10,
                      _0: /* End_of_format */0
                    }
                  }
                }
              },
              _1: "%s\n%s%!"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "World" : false;
          })) && Curry._1(Scanf.sscanf("Hello\nWorld!", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "%s\n%s"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "World!" : false;
          })) && Curry._1(Scanf.sscanf("Hello\n", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Formatting_lit */17,
                  _0: /* Force_newline */3,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "%s@\n%s"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "" : false;
          }))) {
    return Curry._1(Scanf.sscanf("Hello \n", /* Format */{
                    _0: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Formatting_lit */17,
                        _0: /* Force_newline */3,
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "%s@\n%s"
                  }), (function (s1, s2) {
                  if (s1 === "Hello ") {
                    return s2 === "";
                  } else {
                    return false;
                  }
                }));
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1248, characters 5-12", test51(undefined));

function test52(param) {
  if (Curry._1(Scanf.sscanf("Hello\n", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Formatting_lit */17,
                  _0: /* Force_newline */3,
                  _1: /* End_of_format */0
                }
              },
              _1: "%s@\n"
            }), id) === "Hello" && Curry._1(Scanf.sscanf("Hello", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Formatting_lit */17,
                  _0: /* Force_newline */3,
                  _1: /* End_of_format */0
                }
              },
              _1: "%s@\n"
            }), id) === "Hello" && Curry._1(Scanf.sscanf("Hello", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Formatting_lit */17,
                    _0: /* Force_newline */3,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "%s%s@\n"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "" : false;
          })) && Curry._1(Scanf.sscanf("Hello\nWorld", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Formatting_lit */17,
                  _0: /* Force_newline */3,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Flush */10,
                      _0: /* End_of_format */0
                    }
                  }
                }
              },
              _1: "%s@\n%s%!"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "World" : false;
          })) && Curry._1(Scanf.sscanf("Hello\nWorld!", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Formatting_lit */17,
                  _0: /* Force_newline */3,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Formatting_lit */17,
                      _0: /* Force_newline */3,
                      _1: /* End_of_format */0
                    }
                  }
                }
              },
              _1: "%s@\n%s@\n"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "World!" : false;
          })) && Curry._1(Scanf.sscanf("Hello\n", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Formatting_lit */17,
                  _0: /* Force_newline */3,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "%s@\n%s"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "" : false;
          })) && Curry._1(Scanf.sscanf("Hello \n", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Formatting_lit */17,
                    _0: /* Force_newline */3,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "%s%s@\n"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === " " : false;
          })) && Curry._1(Scanf.sscanf("Hello \n", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Ignored_param */23,
                    _0: {
                      TAG: /* Ignored_scan_char_set */10,
                      _0: 1,
                      _1: "\0\0\0\0\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
                    },
                    _1: {
                      TAG: /* Char_literal */12,
                      _0: /* "\n" */10,
                      _1: /* End_of_format */0
                    }
                  }
                }
              },
              _1: "%s%s%_1[ ]\n"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "" : false;
          })) && Curry._1(Scanf.sscanf("Hello \n", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Ignored_param */23,
                  _0: {
                    TAG: /* Ignored_scan_char_set */10,
                    _0: 1,
                    _1: "\0\0\0\0\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
                  },
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Char_literal */12,
                      _0: /* "\n" */10,
                      _1: /* End_of_format */0
                    }
                  }
                }
              },
              _1: "%s%_1[ ]%s\n"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "" : false;
          })) && Curry._1(Scanf.sscanf("Hello\nWorld", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Flush */10,
                      _0: /* End_of_format */0
                    }
                  }
                }
              },
              _1: "%s\n%s%!"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "World" : false;
          })) && Curry._1(Scanf.sscanf("Hello\nWorld!", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Flush */10,
                      _0: /* End_of_format */0
                    }
                  }
                }
              },
              _1: "%s\n%s%!"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "World!" : false;
          })) && Curry._1(Scanf.sscanf("Hello\nWorld!", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Formatting_lit */17,
                      _0: {
                        TAG: /* Scan_indic */2,
                        _0: /* "!" */33
                      },
                      _1: {
                        TAG: /* Flush */10,
                        _0: /* End_of_format */0
                      }
                    }
                  }
                }
              },
              _1: "%s\n%s@!%!"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "World" : false;
          })) && Curry._1(Scanf.sscanf("Hello{foo}", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Formatting_gen */18,
                  _0: {
                    TAG: /* Open_tag */0,
                    _0: /* Format */{
                      _0: /* End_of_format */0,
                      _1: ""
                    }
                  },
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "%s@{%s"
            }), (function (s1, s2) {
            return s1 === "Hello" ? s2 === "foo}" : false;
          }))) {
    return Curry._1(Scanf.sscanf("Hello[foo]", /* Format */{
                    _0: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Formatting_gen */18,
                        _0: {
                          TAG: /* Open_box */1,
                          _0: /* Format */{
                            _0: /* End_of_format */0,
                            _1: ""
                          }
                        },
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "%s@[%s"
                  }), (function (s1, s2) {
                  if (s1 === "Hello") {
                    return s2 === "foo]";
                  } else {
                    return false;
                  }
                }));
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1286, characters 5-12", test52(undefined));

function test53(param) {
  if (Curry._1(Scanf.sscanf("123", /* Format */{
              _0: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%d"
            }), id) === 123 && Curry._1(Scanf.sscanf("124", /* Format */{
              _0: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%d"
            }), (function (i) {
            return (i - 1 | 0) === 123;
          })) && Curry._1(Scanf.sscanf("123", /* Format */{
              _0: {
                TAG: /* Int32 */5,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%ld"
            }), id) === 123 && Curry._1(Scanf.sscanf("124", /* Format */{
              _0: {
                TAG: /* Int32 */5,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: /* End_of_format */0
              },
              _1: "%ld"
            }), (function (i) {
            return (i + 1 | 0) === 125;
          })) && Caml_int64.eq(Curry._1(Scanf.sscanf("123", /* Format */{
                  _0: {
                    TAG: /* Int64 */7,
                    _0: /* Int_d */0,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: /* End_of_format */0
                  },
                  _1: "%Ld"
                }), id), Caml_int64.mk(123, 0))) {
    return Curry._1(Scanf.sscanf("124", /* Format */{
                    _0: {
                      TAG: /* Int64 */7,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: /* End_of_format */0
                    },
                    _1: "%Ld"
                  }), (function (i) {
                  return Caml_int64.eq(Caml_int64.sub(i, Caml_int64.one), Caml_int64.mk(123, 0));
                }));
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1301, characters 5-12", test53(undefined));

function test56(param) {
  var g = function (s) {
    return Curry._1(Scanf.sscanf(s, /* Format */{
                    _0: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Scan_get_counter */21,
                        _0: /* Char_counter */1,
                        _1: /* End_of_format */0
                      }
                    },
                    _1: "%d%n"
                  }), (function (i, n) {
                  return [
                          i,
                          n
                        ];
                }));
  };
  if (Caml_obj.caml_equal(g("99"), [
          99,
          2
        ]) && Caml_obj.caml_equal(g("99 syntaxes all in a row"), [
          99,
          2
        ])) {
    return Caml_obj.caml_equal(g("-20 degrees Celsius"), [
                -20,
                3
              ]);
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1316, characters 5-12", test56(undefined));

function test57(param) {
  var test_format_scan = function (s, fmt, efmt) {
    return Caml_obj.caml_equal(Scanf.format_from_string(s, fmt), efmt);
  };
  if (test_format_scan(" %i ", /* Format */{
          _0: {
            TAG: /* Int */4,
            _0: /* Int_i */3,
            _1: /* No_padding */0,
            _2: /* No_precision */0,
            _3: /* End_of_format */0
          },
          _1: "%i"
        }, /* Format */{
          _0: {
            TAG: /* Char_literal */12,
            _0: /* " " */32,
            _1: {
              TAG: /* Int */4,
              _0: /* Int_i */3,
              _1: /* No_padding */0,
              _2: /* No_precision */0,
              _3: {
                TAG: /* Char_literal */12,
                _0: /* " " */32,
                _1: /* End_of_format */0
              }
            }
          },
          _1: " %i "
        }) && test_format_scan("%i", /* Format */{
          _0: {
            TAG: /* Int */4,
            _0: /* Int_d */0,
            _1: /* No_padding */0,
            _2: /* No_precision */0,
            _3: /* End_of_format */0
          },
          _1: "%d"
        }, /* Format */{
          _0: {
            TAG: /* Int */4,
            _0: /* Int_i */3,
            _1: /* No_padding */0,
            _2: /* No_precision */0,
            _3: /* End_of_format */0
          },
          _1: "%i"
        }) && test_format_scan("Read an int %i then a string %s.", /* Format */{
          _0: {
            TAG: /* String_literal */11,
            _0: "Spec",
            _1: {
              TAG: /* Int */4,
              _0: /* Int_d */0,
              _1: /* No_padding */0,
              _2: /* No_precision */0,
              _3: {
                TAG: /* String_literal */11,
                _0: "ifi",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: "cation",
                    _1: /* End_of_format */0
                  }
                }
              }
            }
          },
          _1: "Spec%difi%scation"
        }, /* Format */{
          _0: {
            TAG: /* String_literal */11,
            _0: "Read an int ",
            _1: {
              TAG: /* Int */4,
              _0: /* Int_i */3,
              _1: /* No_padding */0,
              _2: /* No_precision */0,
              _3: {
                TAG: /* String_literal */11,
                _0: " then a string ",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* "." */46,
                    _1: /* End_of_format */0
                  }
                }
              }
            }
          },
          _1: "Read an int %i then a string %s."
        }) && test_format_scan("Read an int %i then a string \"%s\".", /* Format */{
          _0: {
            TAG: /* String_literal */11,
            _0: "Spec",
            _1: {
              TAG: /* Int */4,
              _0: /* Int_d */0,
              _1: /* No_padding */0,
              _2: /* No_precision */0,
              _3: {
                TAG: /* String_literal */11,
                _0: "ifi",
                _1: {
                  TAG: /* Caml_string */3,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: "cation",
                    _1: /* End_of_format */0
                  }
                }
              }
            }
          },
          _1: "Spec%difi%Scation"
        }, /* Format */{
          _0: {
            TAG: /* String_literal */11,
            _0: "Read an int ",
            _1: {
              TAG: /* Int */4,
              _0: /* Int_i */3,
              _1: /* No_padding */0,
              _2: /* No_precision */0,
              _3: {
                TAG: /* String_literal */11,
                _0: " then a string \"",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: "\".",
                    _1: /* End_of_format */0
                  }
                }
              }
            }
          },
          _1: "Read an int %i then a string \"%s\"."
        }) && test_format_scan("Read an int %i then a string \"%s\".", /* Format */{
          _0: {
            TAG: /* String_literal */11,
            _0: "Spec",
            _1: {
              TAG: /* Int */4,
              _0: /* Int_d */0,
              _1: /* No_padding */0,
              _2: /* No_precision */0,
              _3: {
                TAG: /* String_literal */11,
                _0: "ifi",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: "cation",
                    _1: /* End_of_format */0
                  }
                }
              }
            }
          },
          _1: "Spec%difi%scation"
        }, /* Format */{
          _0: {
            TAG: /* String_literal */11,
            _0: "Read an int ",
            _1: {
              TAG: /* Int */4,
              _0: /* Int_i */3,
              _1: /* No_padding */0,
              _2: /* No_precision */0,
              _3: {
                TAG: /* String_literal */11,
                _0: " then a string \"",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: "\".",
                    _1: /* End_of_format */0
                  }
                }
              }
            }
          },
          _1: "Read an int %i then a string \"%s\"."
        })) {
    return Curry._1(Scanf.sscanf("12 \"%i\"89 ", /* Format */{
                    _0: {
                      TAG: /* Int */4,
                      _0: /* Int_i */3,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Char_literal */12,
                        _0: /* " " */32,
                        _1: {
                          TAG: /* Format_arg */13,
                          _0: undefined,
                          _1: {
                            TAG: /* Int_ty */2,
                            _0: /* End_of_fmtty */0
                          },
                          _2: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* Char_literal */12,
                              _0: /* " " */32,
                              _1: {
                                TAG: /* Flush */10,
                                _0: /* End_of_format */0
                              }
                            }
                          }
                        }
                      }
                    },
                    _1: "%i %{%d%}%s %!"
                  }), (function (i, f, s) {
                  if (i === 12 && Caml_obj.caml_equal(f, /* Format */{
                          _0: {
                            TAG: /* Int */4,
                            _0: /* Int_i */3,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: /* End_of_format */0
                          },
                          _1: "%i"
                        })) {
                    return s === "89";
                  } else {
                    return false;
                  }
                }));
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1357, characters 5-12", test57(undefined));

function test58(param) {
  if (Curry._1(Scanf.sscanf("string1%string2", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Formatting_lit */17,
                  _0: /* Escaped_percent */6,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* "s" */115,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "%s@%%s"
            }), id) === "string1" && Curry._1(Scanf.sscanf("string1%string2", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Formatting_lit */17,
                  _0: /* Escaped_percent */6,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "%s@%%%s"
            }), (function (prim, prim$1) {
            return prim + prim$1;
          })) === "string1string2" && Curry._1(Scanf.sscanf("string1@string2", /* Format */{
              _0: {
                TAG: /* Scan_char_set */20,
                _0: undefined,
                _1: "\0\0\0\0\0\0\xff\x03\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                _2: {
                  TAG: /* Char_literal */12,
                  _0: /* "@" */64,
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "%[a-z0-9]@%s"
            }), (function (prim, prim$1) {
            return prim + prim$1;
          })) === "string1string2") {
    return Curry._1(Scanf.sscanf("string1@%string2", /* Format */{
                    _0: {
                      TAG: /* Scan_char_set */20,
                      _0: undefined,
                      _1: "\0\0\0\0\0\0\xff\x03\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                      _2: {
                        TAG: /* Char_literal */12,
                        _0: /* "@" */64,
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* "%" */37,
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    },
                    _1: "%[a-z0-9]%@%%%s"
                  }), (function (prim, prim$1) {
                  return prim + prim$1;
                })) === "string1string2";
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1367, characters 5-12", test58(undefined));

test("File \"tscanf_test.ml\", line 1371, characters 14-21", true);

function test60(param) {
  if (Curry._1(Scanf.sscanf("abc", /* Format */{
              _0: {
                TAG: /* Scan_next_char */22,
                _0: {
                  TAG: /* Scan_next_char */22,
                  _0: {
                    TAG: /* Char */0,
                    _0: {
                      TAG: /* Scan_get_counter */21,
                      _0: /* Char_counter */1,
                      _1: /* End_of_format */0
                    }
                  }
                }
              },
              _1: "%0c%0c%c%n"
            }), (function (c1, c2, c3, n) {
            return c1 === /* "a" */97 && c2 === /* "a" */97 && c3 === /* "a" */97 ? n === 1 : false;
          })) && Curry._1(Scanf.sscanf("abc", /* Format */{
              _0: {
                TAG: /* String */2,
                _0: {
                  TAG: /* Lit_padding */0,
                  _0: /* Right */1,
                  _1: 0
                },
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "%0s%s"
            }), (function (s1, s2) {
            return s1 === "" ? s2 === "abc" : false;
          }))) {
    return Curry._1(Scanf.sscanf("abc", /* Format */{
                    _0: {
                      TAG: /* String */2,
                      _0: {
                        TAG: /* Lit_padding */0,
                        _0: /* Right */1,
                        _1: 1
                      },
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: /* End_of_format */0
                      }
                    },
                    _1: "%1s%s"
                  }), (function (s1, s2) {
                  if (s1 === "a") {
                    return s2 === "bc";
                  } else {
                    return false;
                  }
                }));
  } else {
    return false;
  }
}

test("File \"tscanf_test.ml\", line 1414, characters 5-12", test60(undefined));

Mt.from_pair_suites("Tscanf_test", suites.contents);

var tscanf_data_file_lines = {
  hd: [
    "Objective",
    "Caml"
  ],
  tl: /* [] */0
};

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.test = test;
exports.id = id;
exports.test0 = test0;
exports.test1 = test1;
exports.test2 = test2;
exports.test3 = test3;
exports.test4 = test4;
exports.test5 = test5;
exports.test6 = test6;
exports.test7 = test7;
exports.verify_read = verify_read;
exports.verify_scan_Chars = verify_scan_Chars;
exports.test8 = test8;
exports.unit = unit;
exports.test_fmt = test_fmt;
exports.test9_string = test9_string;
exports.test_S = test_S;
exports.test9 = test9;
exports.test10 = test10;
exports.test11 = test11;
exports.test110 = test110;
exports.test111 = test111;
exports.ib = ib;
exports.f = f;
exports.test12 = test12;
exports.g = g;
exports.test13 = test13;
exports.test14 = test14;
exports.test15 = test15;
exports.test16 = test16;
exports.test17 = test17;
exports.test18 = test18;
exports.test19 = test19;
exports.test20 = test20;
exports.test21 = test21;
exports.scan_rest = scan_rest$1;
exports.test22 = test22;
exports.test23 = test23;
exports.test24 = test24;
exports.test25 = test25;
exports.test26 = test26;
exports.test27 = test27;
exports.scan_String_elem = scan_String_elem;
exports.scan_String_list = scan_String_list;
exports.test28 = test28;
exports.scan_int_elem = scan_int_elem$1;
exports.test29 = test29;
exports.scan_string_elem = scan_string_elem$1;
exports.test30 = test30;
exports.scan_elem = scan_elem;
exports.test31 = test31;
exports.test32 = test32;
exports.test33 = test33;
exports.test34 = test34;
exports.scan_elems = scan_elems$9;
exports.scan_list = scan_list$4;
exports.scan_float = scan_float;
exports.scan_int_list = scan_int_list$8;
exports.scan_string_list = scan_string_list$2;
exports.scan_bool_list = scan_bool_list;
exports.scan_char_list = scan_char_list;
exports.test340 = test340;
exports.scan_list_list = scan_list_list;
exports.scan_float_item = scan_float_item;
exports.scan_float_list = scan_float_list;
exports.scan_float_list_list = scan_float_list_list$1;
exports.test35 = test35;
exports.read_elems = read_elems;
exports.read_list = read_list;
exports.make_read_elem = make_read_elem;
exports.scan_List = scan_List;
exports.test36 = test36;
exports.test37 = test37;
exports.test38 = test38;
exports.test39 = test39;
exports.test40 = test40;
exports.test41 = test41;
exports.test42 = test42;
exports.test43 = test43;
exports.test44 = test44;
exports.test45 = test45;
exports.test46 = test46;
exports.test47 = test47;
exports.test48 = test48;
exports.test49 = test49;
exports.next_char = next_char;
exports.send_string = send_string;
exports.send_int = send_int;
exports.reader = reader;
exports.writer = writer;
exports.go = go;
exports.test50 = test50;
exports.test51 = test51;
exports.test52 = test52;
exports.test53 = test53;
exports.test56 = test56;
exports.tscanf_data_file_lines = tscanf_data_file_lines;
exports.test57 = test57;
exports.test58 = test58;
exports.test60 = test60;
/*  Not a pure module */
