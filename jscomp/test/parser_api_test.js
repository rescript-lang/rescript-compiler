'use strict';

var Mt = require("./mt.js");
var Parser_api = require("./parser_api.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var match = Parser_api.implementation(Parser_api.from_string("let v str = \n  str  \n  |> Lexing.from_string \n  |> Parse.implementation\n"));

if (match) {
  var match$1 = match[0].pstr_desc;
  if (match$1.tag === /* Pstr_value */1 && !match$1[0]) {
    var match$2 = match$1[1];
    if (match$2) {
      var match$3 = match$2[0];
      var match$4 = match$3.pvb_pat;
      var match$5 = match$4.ppat_desc;
      if (typeof match$5 === "number" || match$5.tag) {
        eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
      } else {
        var match$6 = match$5[0];
        if (match$6.txt === "v") {
          var match$7 = match$6.loc;
          var match$8 = match$7.loc_start;
          if (match$8.pos_fname === "" && !(match$8.pos_lnum !== 1 || match$8.pos_bol !== 0 || match$8.pos_cnum !== 4)) {
            var match$9 = match$7.loc_end;
            if (match$9.pos_fname === "" && !(match$9.pos_lnum !== 1 || match$9.pos_bol !== 0 || match$9.pos_cnum !== 5 || match$7.loc_ghost)) {
              var match$10 = match$4.ppat_loc;
              var match$11 = match$10.loc_start;
              if (match$11.pos_fname === "" && !(match$11.pos_lnum !== 1 || match$11.pos_bol !== 0 || match$11.pos_cnum !== 4)) {
                var match$12 = match$10.loc_end;
                if (match$12.pos_fname === "" && !(match$12.pos_lnum !== 1 || match$12.pos_bol !== 0 || match$12.pos_cnum !== 5 || match$10.loc_ghost || match$4.ppat_attributes)) {
                  var match$13 = match$3.pvb_expr;
                  var match$14 = match$13.pexp_desc;
                  if (match$14.tag === /* Pexp_fun */4 && match$14[0] === "" && match$14[1] === undefined) {
                    var match$15 = match$14[2];
                    var match$16 = match$15.ppat_desc;
                    if (typeof match$16 === "number" || match$16.tag) {
                      eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                    } else {
                      var match$17 = match$16[0];
                      if (match$17.txt === "str") {
                        var match$18 = match$17.loc;
                        var match$19 = match$18.loc_start;
                        if (match$19.pos_fname === "" && !(match$19.pos_lnum !== 1 || match$19.pos_bol !== 0 || match$19.pos_cnum !== 6)) {
                          var match$20 = match$18.loc_end;
                          if (match$20.pos_fname === "" && !(match$20.pos_lnum !== 1 || match$20.pos_bol !== 0 || match$20.pos_cnum !== 9 || match$18.loc_ghost)) {
                            var match$21 = match$15.ppat_loc;
                            var match$22 = match$21.loc_start;
                            if (match$22.pos_fname === "" && !(match$22.pos_lnum !== 1 || match$22.pos_bol !== 0 || match$22.pos_cnum !== 6)) {
                              var match$23 = match$21.loc_end;
                              if (match$23.pos_fname === "" && !(match$23.pos_lnum !== 1 || match$23.pos_bol !== 0 || match$23.pos_cnum !== 9 || match$21.loc_ghost || match$15.ppat_attributes)) {
                                var match$24 = match$14[3];
                                var match$25 = match$24.pexp_desc;
                                if (match$25.tag === /* Pexp_apply */5) {
                                  var match$26 = match$25[0];
                                  var match$27 = match$26.pexp_desc;
                                  if (match$27.tag) {
                                    eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                  } else {
                                    var match$28 = match$27[0];
                                    var match$29 = match$28.txt;
                                    switch (match$29.tag | 0) {
                                      case /* Lident */0 :
                                          if (match$29[0] === "|>") {
                                            var match$30 = match$28.loc;
                                            var match$31 = match$30.loc_start;
                                            if (match$31.pos_fname === "" && !(match$31.pos_lnum !== 4 || match$31.pos_bol !== 46 || match$31.pos_cnum !== 48)) {
                                              var match$32 = match$30.loc_end;
                                              if (match$32.pos_fname === "" && !(match$32.pos_lnum !== 4 || match$32.pos_bol !== 46 || match$32.pos_cnum !== 50 || match$30.loc_ghost)) {
                                                var match$33 = match$26.pexp_loc;
                                                var match$34 = match$33.loc_start;
                                                if (match$34.pos_fname === "" && !(match$34.pos_lnum !== 4 || match$34.pos_bol !== 46 || match$34.pos_cnum !== 48)) {
                                                  var match$35 = match$33.loc_end;
                                                  if (match$35.pos_fname === "" && !(match$35.pos_lnum !== 4 || match$35.pos_bol !== 46 || match$35.pos_cnum !== 50 || match$33.loc_ghost || match$26.pexp_attributes)) {
                                                    var match$36 = match$25[1];
                                                    if (match$36) {
                                                      var match$37 = match$36[0];
                                                      if (match$37[0] === "") {
                                                        var match$38 = match$37[1];
                                                        var match$39 = match$38.pexp_desc;
                                                        if (match$39.tag === /* Pexp_apply */5) {
                                                          var match$40 = match$39[0];
                                                          var match$41 = match$40.pexp_desc;
                                                          if (match$41.tag) {
                                                            eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                          } else {
                                                            var match$42 = match$41[0];
                                                            var match$43 = match$42.txt;
                                                            switch (match$43.tag | 0) {
                                                              case /* Lident */0 :
                                                                  if (match$43[0] === "|>") {
                                                                    var match$44 = match$42.loc;
                                                                    var match$45 = match$44.loc_start;
                                                                    if (match$45.pos_fname === "" && !(match$45.pos_lnum !== 3 || match$45.pos_bol !== 21 || match$45.pos_cnum !== 23)) {
                                                                      var match$46 = match$44.loc_end;
                                                                      if (match$46.pos_fname === "" && !(match$46.pos_lnum !== 3 || match$46.pos_bol !== 21 || match$46.pos_cnum !== 25 || match$44.loc_ghost)) {
                                                                        var match$47 = match$40.pexp_loc;
                                                                        var match$48 = match$47.loc_start;
                                                                        if (match$48.pos_fname === "" && !(match$48.pos_lnum !== 3 || match$48.pos_bol !== 21 || match$48.pos_cnum !== 23)) {
                                                                          var match$49 = match$47.loc_end;
                                                                          if (match$49.pos_fname === "" && !(match$49.pos_lnum !== 3 || match$49.pos_bol !== 21 || match$49.pos_cnum !== 25 || match$47.loc_ghost || match$40.pexp_attributes)) {
                                                                            var match$50 = match$39[1];
                                                                            if (match$50) {
                                                                              var match$51 = match$50[0];
                                                                              if (match$51[0] === "") {
                                                                                var match$52 = match$51[1];
                                                                                var match$53 = match$52.pexp_desc;
                                                                                if (match$53.tag) {
                                                                                  eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                } else {
                                                                                  var match$54 = match$53[0];
                                                                                  var match$55 = match$54.txt;
                                                                                  switch (match$55.tag | 0) {
                                                                                    case /* Lident */0 :
                                                                                        if (match$55[0] === "str") {
                                                                                          var match$56 = match$54.loc;
                                                                                          var match$57 = match$56.loc_start;
                                                                                          if (match$57.pos_fname === "" && !(match$57.pos_lnum !== 2 || match$57.pos_bol !== 13 || match$57.pos_cnum !== 15)) {
                                                                                            var match$58 = match$56.loc_end;
                                                                                            if (match$58.pos_fname === "" && !(match$58.pos_lnum !== 2 || match$58.pos_bol !== 13 || match$58.pos_cnum !== 18 || match$56.loc_ghost)) {
                                                                                              var match$59 = match$52.pexp_loc;
                                                                                              var match$60 = match$59.loc_start;
                                                                                              if (match$60.pos_fname === "" && !(match$60.pos_lnum !== 2 || match$60.pos_bol !== 13 || match$60.pos_cnum !== 15)) {
                                                                                                var match$61 = match$59.loc_end;
                                                                                                if (match$61.pos_fname === "" && !(match$61.pos_lnum !== 2 || match$61.pos_bol !== 13 || match$61.pos_cnum !== 18 || match$59.loc_ghost || match$52.pexp_attributes)) {
                                                                                                  var match$62 = match$50[1];
                                                                                                  if (match$62) {
                                                                                                    var match$63 = match$62[0];
                                                                                                    if (match$63[0] === "") {
                                                                                                      var match$64 = match$63[1];
                                                                                                      var match$65 = match$64.pexp_desc;
                                                                                                      if (match$65.tag) {
                                                                                                        eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                      } else {
                                                                                                        var match$66 = match$65[0];
                                                                                                        var match$67 = match$66.txt;
                                                                                                        switch (match$67.tag | 0) {
                                                                                                          case /* Ldot */1 :
                                                                                                              var match$68 = match$67[0];
                                                                                                              switch (match$68.tag | 0) {
                                                                                                                case /* Lident */0 :
                                                                                                                    if (match$68[0] === "Lexing" && match$67[1] === "from_string") {
                                                                                                                      var match$69 = match$66.loc;
                                                                                                                      var match$70 = match$69.loc_start;
                                                                                                                      if (match$70.pos_fname === "" && !(match$70.pos_lnum !== 3 || match$70.pos_bol !== 21 || match$70.pos_cnum !== 26)) {
                                                                                                                        var match$71 = match$69.loc_end;
                                                                                                                        if (match$71.pos_fname === "" && !(match$71.pos_lnum !== 3 || match$71.pos_bol !== 21 || match$71.pos_cnum !== 44 || match$69.loc_ghost)) {
                                                                                                                          var match$72 = match$64.pexp_loc;
                                                                                                                          var match$73 = match$72.loc_start;
                                                                                                                          if (match$73.pos_fname === "" && !(match$73.pos_lnum !== 3 || match$73.pos_bol !== 21 || match$73.pos_cnum !== 26)) {
                                                                                                                            var match$74 = match$72.loc_end;
                                                                                                                            if (match$74.pos_fname === "" && !(match$74.pos_lnum !== 3 || match$74.pos_bol !== 21 || match$74.pos_cnum !== 44 || match$72.loc_ghost || match$64.pexp_attributes || match$62[1])) {
                                                                                                                              var match$75 = match$38.pexp_loc;
                                                                                                                              var match$76 = match$75.loc_start;
                                                                                                                              if (match$76.pos_fname === "" && !(match$76.pos_lnum !== 2 || match$76.pos_bol !== 13 || match$76.pos_cnum !== 15)) {
                                                                                                                                var match$77 = match$75.loc_end;
                                                                                                                                if (match$77.pos_fname === "" && !(match$77.pos_lnum !== 3 || match$77.pos_bol !== 21 || match$77.pos_cnum !== 44 || match$75.loc_ghost || match$38.pexp_attributes)) {
                                                                                                                                  var match$78 = match$36[1];
                                                                                                                                  if (match$78) {
                                                                                                                                    var match$79 = match$78[0];
                                                                                                                                    if (match$79[0] === "") {
                                                                                                                                      var match$80 = match$79[1];
                                                                                                                                      var match$81 = match$80.pexp_desc;
                                                                                                                                      if (match$81.tag) {
                                                                                                                                        eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                      } else {
                                                                                                                                        var match$82 = match$81[0];
                                                                                                                                        var match$83 = match$82.txt;
                                                                                                                                        switch (match$83.tag | 0) {
                                                                                                                                          case /* Ldot */1 :
                                                                                                                                              var match$84 = match$83[0];
                                                                                                                                              switch (match$84.tag | 0) {
                                                                                                                                                case /* Lident */0 :
                                                                                                                                                    if (match$84[0] === "Parse" && match$83[1] === "implementation") {
                                                                                                                                                      var match$85 = match$82.loc;
                                                                                                                                                      var match$86 = match$85.loc_start;
                                                                                                                                                      if (match$86.pos_fname === "" && !(match$86.pos_lnum !== 4 || match$86.pos_bol !== 46 || match$86.pos_cnum !== 51)) {
                                                                                                                                                        var match$87 = match$85.loc_end;
                                                                                                                                                        if (match$87.pos_fname === "" && !(match$87.pos_lnum !== 4 || match$87.pos_bol !== 46 || match$87.pos_cnum !== 71 || match$85.loc_ghost)) {
                                                                                                                                                          var match$88 = match$80.pexp_loc;
                                                                                                                                                          var match$89 = match$88.loc_start;
                                                                                                                                                          if (match$89.pos_fname === "" && !(match$89.pos_lnum !== 4 || match$89.pos_bol !== 46 || match$89.pos_cnum !== 51)) {
                                                                                                                                                            var match$90 = match$88.loc_end;
                                                                                                                                                            if (match$90.pos_fname === "" && !(match$90.pos_lnum !== 4 || match$90.pos_bol !== 46 || match$90.pos_cnum !== 71 || match$88.loc_ghost || match$80.pexp_attributes || match$78[1])) {
                                                                                                                                                              var match$91 = match$24.pexp_loc;
                                                                                                                                                              var match$92 = match$91.loc_start;
                                                                                                                                                              if (match$92.pos_fname === "" && !(match$92.pos_lnum !== 2 || match$92.pos_bol !== 13 || match$92.pos_cnum !== 15)) {
                                                                                                                                                                var match$93 = match$91.loc_end;
                                                                                                                                                                if (match$93.pos_fname === "" && !(match$93.pos_lnum !== 4 || match$93.pos_bol !== 46 || match$93.pos_cnum !== 71 || match$91.loc_ghost || match$24.pexp_attributes)) {
                                                                                                                                                                  var match$94 = match$13.pexp_loc;
                                                                                                                                                                  var match$95 = match$94.loc_start;
                                                                                                                                                                  if (match$95.pos_fname === "" && !(match$95.pos_lnum !== 1 || match$95.pos_bol !== 0 || match$95.pos_cnum !== 6)) {
                                                                                                                                                                    var match$96 = match$94.loc_end;
                                                                                                                                                                    if (match$96.pos_fname === "" && !(match$96.pos_lnum !== 4 || match$96.pos_bol !== 46 || match$96.pos_cnum !== 71 || !(match$94.loc_ghost && !(match$13.pexp_attributes || match$3.pvb_attributes)))) {
                                                                                                                                                                      var match$97 = match$3.pvb_loc;
                                                                                                                                                                      var match$98 = match$97.loc_start;
                                                                                                                                                                      if (match$98.pos_fname === "" && !(match$98.pos_lnum !== 1 || match$98.pos_bol !== 0 || match$98.pos_cnum !== 0)) {
                                                                                                                                                                        var match$99 = match$97.loc_end;
                                                                                                                                                                        if (match$99.pos_fname === "" && !(match$99.pos_lnum !== 4 || match$99.pos_bol !== 46 || match$99.pos_cnum !== 71 || match$97.loc_ghost || match$2[1])) {
                                                                                                                                                                          eq("File \"parser_api_test.ml\", line 210, characters 10-17", true, true);
                                                                                                                                                                        } else {
                                                                                                                                                                          eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                                        }
                                                                                                                                                                      } else {
                                                                                                                                                                        eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                                      }
                                                                                                                                                                    } else {
                                                                                                                                                                      eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                                    }
                                                                                                                                                                  } else {
                                                                                                                                                                    eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                                  }
                                                                                                                                                                } else {
                                                                                                                                                                  eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                                }
                                                                                                                                                              } else {
                                                                                                                                                                eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                              }
                                                                                                                                                            } else {
                                                                                                                                                              eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                            }
                                                                                                                                                          } else {
                                                                                                                                                            eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                          }
                                                                                                                                                        } else {
                                                                                                                                                          eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                        }
                                                                                                                                                      } else {
                                                                                                                                                        eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                      }
                                                                                                                                                    } else {
                                                                                                                                                      eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                    }
                                                                                                                                                    break;
                                                                                                                                                case /* Ldot */1 :
                                                                                                                                                case /* Lapply */2 :
                                                                                                                                                    eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                    break;
                                                                                                                                                
                                                                                                                                              }
                                                                                                                                              break;
                                                                                                                                          case /* Lident */0 :
                                                                                                                                          case /* Lapply */2 :
                                                                                                                                              eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                              break;
                                                                                                                                          
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                    } else {
                                                                                                                                      eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                    }
                                                                                                                                  } else {
                                                                                                                                    eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                  }
                                                                                                                                } else {
                                                                                                                                  eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                }
                                                                                                                              } else {
                                                                                                                                eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                              }
                                                                                                                            } else {
                                                                                                                              eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                            }
                                                                                                                          } else {
                                                                                                                            eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                          }
                                                                                                                        } else {
                                                                                                                          eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                        }
                                                                                                                      } else {
                                                                                                                        eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                      }
                                                                                                                    } else {
                                                                                                                      eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                    }
                                                                                                                    break;
                                                                                                                case /* Ldot */1 :
                                                                                                                case /* Lapply */2 :
                                                                                                                    eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                    break;
                                                                                                                
                                                                                                              }
                                                                                                              break;
                                                                                                          case /* Lident */0 :
                                                                                                          case /* Lapply */2 :
                                                                                                              eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                              break;
                                                                                                          
                                                                                                        }
                                                                                                      }
                                                                                                    } else {
                                                                                                      eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                    }
                                                                                                  } else {
                                                                                                    eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                  }
                                                                                                } else {
                                                                                                  eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                }
                                                                                              } else {
                                                                                                eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                              }
                                                                                            } else {
                                                                                              eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                            }
                                                                                          } else {
                                                                                            eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                          }
                                                                                        } else {
                                                                                          eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                        }
                                                                                        break;
                                                                                    case /* Ldot */1 :
                                                                                    case /* Lapply */2 :
                                                                                        eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                        break;
                                                                                    
                                                                                  }
                                                                                }
                                                                              } else {
                                                                                eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                              }
                                                                            } else {
                                                                              eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                            }
                                                                          } else {
                                                                            eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                          }
                                                                        } else {
                                                                          eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                        }
                                                                      } else {
                                                                        eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                      }
                                                                    } else {
                                                                      eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                    }
                                                                  } else {
                                                                    eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                  }
                                                                  break;
                                                              case /* Ldot */1 :
                                                              case /* Lapply */2 :
                                                                  eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                  break;
                                                              
                                                            }
                                                          }
                                                        } else {
                                                          eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                        }
                                                      } else {
                                                        eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                      }
                                                    } else {
                                                      eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                    }
                                                  } else {
                                                    eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                  }
                                                } else {
                                                  eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                }
                                              } else {
                                                eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                              }
                                            } else {
                                              eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                            }
                                          } else {
                                            eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                          }
                                          break;
                                      case /* Ldot */1 :
                                      case /* Lapply */2 :
                                          eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                          break;
                                      
                                    }
                                  }
                                } else {
                                  eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                                }
                              } else {
                                eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                              }
                            } else {
                              eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                            }
                          } else {
                            eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                          }
                        } else {
                          eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                        }
                      } else {
                        eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                      }
                    }
                  } else {
                    eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                  }
                } else {
                  eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
                }
              } else {
                eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
              }
            } else {
              eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
            }
          } else {
            eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
          }
        } else {
          eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
        }
      }
    } else {
      eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
    }
  } else {
    eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
  }
} else {
  eq("File \"parser_api_test.ml\", line 211, characters 12-19", true, false);
}

Mt.from_pair_suites("Parser_api_test", suites.contents);

var lex = Parser_api.from_string;

var parse = Parser_api.implementation;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.lex = lex;
exports.parse = parse;
/* match Not a pure module */
