'use strict';

var Mt = require("./mt.js");
var Parser_api = require("./parser_api.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var match = Parser_api.implementation(Parser_api.from_string("let v str = \n  str  \n  |> Lexing.from_string \n  |> Parse.implementation\n"));

if (match) {
  var match$1 = match[0][/* pstr_desc */0];
  if (match$1.tag === 1 && !match$1[0]) {
    var match$2 = match$1[1];
    if (match$2) {
      var match$3 = match$2[0];
      var match$4 = match$3[/* pvb_pat */0];
      var match$5 = match$4[/* ppat_desc */0];
      if (typeof match$5 === "number" || match$5.tag) {
        eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
      } else {
        var match$6 = match$5[0];
        if (match$6[/* txt */0] === "v") {
          var match$7 = match$6[/* loc */1];
          var match$8 = match$7[/* loc_start */0];
          if (match$8[/* pos_fname */0] === "" && !(match$8[/* pos_lnum */1] !== 1 || match$8[/* pos_bol */2] !== 0 || match$8[/* pos_cnum */3] !== 4)) {
            var match$9 = match$7[/* loc_end */1];
            if (match$9[/* pos_fname */0] === "" && !(match$9[/* pos_lnum */1] !== 1 || match$9[/* pos_bol */2] !== 0 || match$9[/* pos_cnum */3] !== 5 || match$7[/* loc_ghost */2])) {
              var match$10 = match$4[/* ppat_loc */1];
              var match$11 = match$10[/* loc_start */0];
              if (match$11[/* pos_fname */0] === "" && !(match$11[/* pos_lnum */1] !== 1 || match$11[/* pos_bol */2] !== 0 || match$11[/* pos_cnum */3] !== 4)) {
                var match$12 = match$10[/* loc_end */1];
                if (match$12[/* pos_fname */0] === "" && !(match$12[/* pos_lnum */1] !== 1 || match$12[/* pos_bol */2] !== 0 || match$12[/* pos_cnum */3] !== 5 || match$10[/* loc_ghost */2] || match$4[/* ppat_attributes */2])) {
                  var match$13 = match$3[/* pvb_expr */1];
                  var match$14 = match$13[/* pexp_desc */0];
                  if (match$14.tag === 4 && match$14[0] === "" && match$14[1] === undefined) {
                    var match$15 = match$14[2];
                    var match$16 = match$15[/* ppat_desc */0];
                    if (typeof match$16 === "number" || match$16.tag) {
                      eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                    } else {
                      var match$17 = match$16[0];
                      if (match$17[/* txt */0] === "str") {
                        var match$18 = match$17[/* loc */1];
                        var match$19 = match$18[/* loc_start */0];
                        if (match$19[/* pos_fname */0] === "" && !(match$19[/* pos_lnum */1] !== 1 || match$19[/* pos_bol */2] !== 0 || match$19[/* pos_cnum */3] !== 6)) {
                          var match$20 = match$18[/* loc_end */1];
                          if (match$20[/* pos_fname */0] === "" && !(match$20[/* pos_lnum */1] !== 1 || match$20[/* pos_bol */2] !== 0 || match$20[/* pos_cnum */3] !== 9 || match$18[/* loc_ghost */2])) {
                            var match$21 = match$15[/* ppat_loc */1];
                            var match$22 = match$21[/* loc_start */0];
                            if (match$22[/* pos_fname */0] === "" && !(match$22[/* pos_lnum */1] !== 1 || match$22[/* pos_bol */2] !== 0 || match$22[/* pos_cnum */3] !== 6)) {
                              var match$23 = match$21[/* loc_end */1];
                              if (match$23[/* pos_fname */0] === "" && !(match$23[/* pos_lnum */1] !== 1 || match$23[/* pos_bol */2] !== 0 || match$23[/* pos_cnum */3] !== 9 || match$21[/* loc_ghost */2] || match$15[/* ppat_attributes */2])) {
                                var match$24 = match$14[3];
                                var match$25 = match$24[/* pexp_desc */0];
                                if (match$25.tag === 5) {
                                  var match$26 = match$25[0];
                                  var match$27 = match$26[/* pexp_desc */0];
                                  if (match$27.tag) {
                                    eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                  } else {
                                    var match$28 = match$27[0];
                                    var match$29 = match$28[/* txt */0];
                                    switch (match$29.tag | 0) {
                                      case 0 : 
                                          if (match$29[0] === "|>") {
                                            var match$30 = match$28[/* loc */1];
                                            var match$31 = match$30[/* loc_start */0];
                                            if (match$31[/* pos_fname */0] === "" && !(match$31[/* pos_lnum */1] !== 4 || match$31[/* pos_bol */2] !== 46 || match$31[/* pos_cnum */3] !== 48)) {
                                              var match$32 = match$30[/* loc_end */1];
                                              if (match$32[/* pos_fname */0] === "" && !(match$32[/* pos_lnum */1] !== 4 || match$32[/* pos_bol */2] !== 46 || match$32[/* pos_cnum */3] !== 50 || match$30[/* loc_ghost */2])) {
                                                var match$33 = match$26[/* pexp_loc */1];
                                                var match$34 = match$33[/* loc_start */0];
                                                if (match$34[/* pos_fname */0] === "" && !(match$34[/* pos_lnum */1] !== 4 || match$34[/* pos_bol */2] !== 46 || match$34[/* pos_cnum */3] !== 48)) {
                                                  var match$35 = match$33[/* loc_end */1];
                                                  if (match$35[/* pos_fname */0] === "" && !(match$35[/* pos_lnum */1] !== 4 || match$35[/* pos_bol */2] !== 46 || match$35[/* pos_cnum */3] !== 50 || match$33[/* loc_ghost */2] || match$26[/* pexp_attributes */2])) {
                                                    var match$36 = match$25[1];
                                                    if (match$36) {
                                                      var match$37 = match$36[0];
                                                      if (match$37[0] === "") {
                                                        var match$38 = match$37[1];
                                                        var match$39 = match$38[/* pexp_desc */0];
                                                        if (match$39.tag === 5) {
                                                          var match$40 = match$39[0];
                                                          var match$41 = match$40[/* pexp_desc */0];
                                                          if (match$41.tag) {
                                                            eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                          } else {
                                                            var match$42 = match$41[0];
                                                            var match$43 = match$42[/* txt */0];
                                                            switch (match$43.tag | 0) {
                                                              case 0 : 
                                                                  if (match$43[0] === "|>") {
                                                                    var match$44 = match$42[/* loc */1];
                                                                    var match$45 = match$44[/* loc_start */0];
                                                                    if (match$45[/* pos_fname */0] === "" && !(match$45[/* pos_lnum */1] !== 3 || match$45[/* pos_bol */2] !== 21 || match$45[/* pos_cnum */3] !== 23)) {
                                                                      var match$46 = match$44[/* loc_end */1];
                                                                      if (match$46[/* pos_fname */0] === "" && !(match$46[/* pos_lnum */1] !== 3 || match$46[/* pos_bol */2] !== 21 || match$46[/* pos_cnum */3] !== 25 || match$44[/* loc_ghost */2])) {
                                                                        var match$47 = match$40[/* pexp_loc */1];
                                                                        var match$48 = match$47[/* loc_start */0];
                                                                        if (match$48[/* pos_fname */0] === "" && !(match$48[/* pos_lnum */1] !== 3 || match$48[/* pos_bol */2] !== 21 || match$48[/* pos_cnum */3] !== 23)) {
                                                                          var match$49 = match$47[/* loc_end */1];
                                                                          if (match$49[/* pos_fname */0] === "" && !(match$49[/* pos_lnum */1] !== 3 || match$49[/* pos_bol */2] !== 21 || match$49[/* pos_cnum */3] !== 25 || match$47[/* loc_ghost */2] || match$40[/* pexp_attributes */2])) {
                                                                            var match$50 = match$39[1];
                                                                            if (match$50) {
                                                                              var match$51 = match$50[0];
                                                                              if (match$51[0] === "") {
                                                                                var match$52 = match$51[1];
                                                                                var match$53 = match$52[/* pexp_desc */0];
                                                                                if (match$53.tag) {
                                                                                  eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                } else {
                                                                                  var match$54 = match$53[0];
                                                                                  var match$55 = match$54[/* txt */0];
                                                                                  switch (match$55.tag | 0) {
                                                                                    case 0 : 
                                                                                        if (match$55[0] === "str") {
                                                                                          var match$56 = match$54[/* loc */1];
                                                                                          var match$57 = match$56[/* loc_start */0];
                                                                                          if (match$57[/* pos_fname */0] === "" && !(match$57[/* pos_lnum */1] !== 2 || match$57[/* pos_bol */2] !== 13 || match$57[/* pos_cnum */3] !== 15)) {
                                                                                            var match$58 = match$56[/* loc_end */1];
                                                                                            if (match$58[/* pos_fname */0] === "" && !(match$58[/* pos_lnum */1] !== 2 || match$58[/* pos_bol */2] !== 13 || match$58[/* pos_cnum */3] !== 18 || match$56[/* loc_ghost */2])) {
                                                                                              var match$59 = match$52[/* pexp_loc */1];
                                                                                              var match$60 = match$59[/* loc_start */0];
                                                                                              if (match$60[/* pos_fname */0] === "" && !(match$60[/* pos_lnum */1] !== 2 || match$60[/* pos_bol */2] !== 13 || match$60[/* pos_cnum */3] !== 15)) {
                                                                                                var match$61 = match$59[/* loc_end */1];
                                                                                                if (match$61[/* pos_fname */0] === "" && !(match$61[/* pos_lnum */1] !== 2 || match$61[/* pos_bol */2] !== 13 || match$61[/* pos_cnum */3] !== 18 || match$59[/* loc_ghost */2] || match$52[/* pexp_attributes */2])) {
                                                                                                  var match$62 = match$50[1];
                                                                                                  if (match$62) {
                                                                                                    var match$63 = match$62[0];
                                                                                                    if (match$63[0] === "") {
                                                                                                      var match$64 = match$63[1];
                                                                                                      var match$65 = match$64[/* pexp_desc */0];
                                                                                                      if (match$65.tag) {
                                                                                                        eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                      } else {
                                                                                                        var match$66 = match$65[0];
                                                                                                        var match$67 = match$66[/* txt */0];
                                                                                                        switch (match$67.tag | 0) {
                                                                                                          case 1 : 
                                                                                                              var match$68 = match$67[0];
                                                                                                              switch (match$68.tag | 0) {
                                                                                                                case 0 : 
                                                                                                                    if (match$68[0] === "Lexing" && match$67[1] === "from_string") {
                                                                                                                      var match$69 = match$66[/* loc */1];
                                                                                                                      var match$70 = match$69[/* loc_start */0];
                                                                                                                      if (match$70[/* pos_fname */0] === "" && !(match$70[/* pos_lnum */1] !== 3 || match$70[/* pos_bol */2] !== 21 || match$70[/* pos_cnum */3] !== 26)) {
                                                                                                                        var match$71 = match$69[/* loc_end */1];
                                                                                                                        if (match$71[/* pos_fname */0] === "" && !(match$71[/* pos_lnum */1] !== 3 || match$71[/* pos_bol */2] !== 21 || match$71[/* pos_cnum */3] !== 44 || match$69[/* loc_ghost */2])) {
                                                                                                                          var match$72 = match$64[/* pexp_loc */1];
                                                                                                                          var match$73 = match$72[/* loc_start */0];
                                                                                                                          if (match$73[/* pos_fname */0] === "" && !(match$73[/* pos_lnum */1] !== 3 || match$73[/* pos_bol */2] !== 21 || match$73[/* pos_cnum */3] !== 26)) {
                                                                                                                            var match$74 = match$72[/* loc_end */1];
                                                                                                                            if (match$74[/* pos_fname */0] === "" && !(match$74[/* pos_lnum */1] !== 3 || match$74[/* pos_bol */2] !== 21 || match$74[/* pos_cnum */3] !== 44 || match$72[/* loc_ghost */2] || match$64[/* pexp_attributes */2] || match$62[1])) {
                                                                                                                              var match$75 = match$38[/* pexp_loc */1];
                                                                                                                              var match$76 = match$75[/* loc_start */0];
                                                                                                                              if (match$76[/* pos_fname */0] === "" && !(match$76[/* pos_lnum */1] !== 2 || match$76[/* pos_bol */2] !== 13 || match$76[/* pos_cnum */3] !== 15)) {
                                                                                                                                var match$77 = match$75[/* loc_end */1];
                                                                                                                                if (match$77[/* pos_fname */0] === "" && !(match$77[/* pos_lnum */1] !== 3 || match$77[/* pos_bol */2] !== 21 || match$77[/* pos_cnum */3] !== 44 || match$75[/* loc_ghost */2] || match$38[/* pexp_attributes */2])) {
                                                                                                                                  var match$78 = match$36[1];
                                                                                                                                  if (match$78) {
                                                                                                                                    var match$79 = match$78[0];
                                                                                                                                    if (match$79[0] === "") {
                                                                                                                                      var match$80 = match$79[1];
                                                                                                                                      var match$81 = match$80[/* pexp_desc */0];
                                                                                                                                      if (match$81.tag) {
                                                                                                                                        eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                      } else {
                                                                                                                                        var match$82 = match$81[0];
                                                                                                                                        var match$83 = match$82[/* txt */0];
                                                                                                                                        switch (match$83.tag | 0) {
                                                                                                                                          case 1 : 
                                                                                                                                              var match$84 = match$83[0];
                                                                                                                                              switch (match$84.tag | 0) {
                                                                                                                                                case 0 : 
                                                                                                                                                    if (match$84[0] === "Parse" && match$83[1] === "implementation") {
                                                                                                                                                      var match$85 = match$82[/* loc */1];
                                                                                                                                                      var match$86 = match$85[/* loc_start */0];
                                                                                                                                                      if (match$86[/* pos_fname */0] === "" && !(match$86[/* pos_lnum */1] !== 4 || match$86[/* pos_bol */2] !== 46 || match$86[/* pos_cnum */3] !== 51)) {
                                                                                                                                                        var match$87 = match$85[/* loc_end */1];
                                                                                                                                                        if (match$87[/* pos_fname */0] === "" && !(match$87[/* pos_lnum */1] !== 4 || match$87[/* pos_bol */2] !== 46 || match$87[/* pos_cnum */3] !== 71 || match$85[/* loc_ghost */2])) {
                                                                                                                                                          var match$88 = match$80[/* pexp_loc */1];
                                                                                                                                                          var match$89 = match$88[/* loc_start */0];
                                                                                                                                                          if (match$89[/* pos_fname */0] === "" && !(match$89[/* pos_lnum */1] !== 4 || match$89[/* pos_bol */2] !== 46 || match$89[/* pos_cnum */3] !== 51)) {
                                                                                                                                                            var match$90 = match$88[/* loc_end */1];
                                                                                                                                                            if (match$90[/* pos_fname */0] === "" && !(match$90[/* pos_lnum */1] !== 4 || match$90[/* pos_bol */2] !== 46 || match$90[/* pos_cnum */3] !== 71 || match$88[/* loc_ghost */2] || match$80[/* pexp_attributes */2] || match$78[1])) {
                                                                                                                                                              var match$91 = match$24[/* pexp_loc */1];
                                                                                                                                                              var match$92 = match$91[/* loc_start */0];
                                                                                                                                                              if (match$92[/* pos_fname */0] === "" && !(match$92[/* pos_lnum */1] !== 2 || match$92[/* pos_bol */2] !== 13 || match$92[/* pos_cnum */3] !== 15)) {
                                                                                                                                                                var match$93 = match$91[/* loc_end */1];
                                                                                                                                                                if (match$93[/* pos_fname */0] === "" && !(match$93[/* pos_lnum */1] !== 4 || match$93[/* pos_bol */2] !== 46 || match$93[/* pos_cnum */3] !== 71 || match$91[/* loc_ghost */2] || match$24[/* pexp_attributes */2])) {
                                                                                                                                                                  var match$94 = match$13[/* pexp_loc */1];
                                                                                                                                                                  var match$95 = match$94[/* loc_start */0];
                                                                                                                                                                  if (match$95[/* pos_fname */0] === "" && !(match$95[/* pos_lnum */1] !== 1 || match$95[/* pos_bol */2] !== 0 || match$95[/* pos_cnum */3] !== 6)) {
                                                                                                                                                                    var match$96 = match$94[/* loc_end */1];
                                                                                                                                                                    if (match$96[/* pos_fname */0] === "" && !(match$96[/* pos_lnum */1] !== 4 || match$96[/* pos_bol */2] !== 46 || match$96[/* pos_cnum */3] !== 71 || !(match$94[/* loc_ghost */2] && !(match$13[/* pexp_attributes */2] || match$3[/* pvb_attributes */2])))) {
                                                                                                                                                                      var match$97 = match$3[/* pvb_loc */3];
                                                                                                                                                                      var match$98 = match$97[/* loc_start */0];
                                                                                                                                                                      if (match$98[/* pos_fname */0] === "" && !(match$98[/* pos_lnum */1] !== 1 || match$98[/* pos_bol */2] !== 0 || match$98[/* pos_cnum */3] !== 0)) {
                                                                                                                                                                        var match$99 = match$97[/* loc_end */1];
                                                                                                                                                                        if (match$99[/* pos_fname */0] === "" && !(match$99[/* pos_lnum */1] !== 4 || match$99[/* pos_bol */2] !== 46 || match$99[/* pos_cnum */3] !== 71 || match$97[/* loc_ghost */2] || match$2[1])) {
                                                                                                                                                                          eq("File \"test/parser_api_test.ml\", line 210, characters 10-17", true, true);
                                                                                                                                                                        } else {
                                                                                                                                                                          eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                                        }
                                                                                                                                                                      } else {
                                                                                                                                                                        eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                                      }
                                                                                                                                                                    } else {
                                                                                                                                                                      eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                                    }
                                                                                                                                                                  } else {
                                                                                                                                                                    eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                                  }
                                                                                                                                                                } else {
                                                                                                                                                                  eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                                }
                                                                                                                                                              } else {
                                                                                                                                                                eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                              }
                                                                                                                                                            } else {
                                                                                                                                                              eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                            }
                                                                                                                                                          } else {
                                                                                                                                                            eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                          }
                                                                                                                                                        } else {
                                                                                                                                                          eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                        }
                                                                                                                                                      } else {
                                                                                                                                                        eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                      }
                                                                                                                                                    } else {
                                                                                                                                                      eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                    }
                                                                                                                                                    break;
                                                                                                                                                case 1 : 
                                                                                                                                                case 2 : 
                                                                                                                                                    eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                                    break;
                                                                                                                                                
                                                                                                                                              }
                                                                                                                                              break;
                                                                                                                                          case 0 : 
                                                                                                                                          case 2 : 
                                                                                                                                              eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                              break;
                                                                                                                                          
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                    } else {
                                                                                                                                      eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                    }
                                                                                                                                  } else {
                                                                                                                                    eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                  }
                                                                                                                                } else {
                                                                                                                                  eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                                }
                                                                                                                              } else {
                                                                                                                                eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                              }
                                                                                                                            } else {
                                                                                                                              eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                            }
                                                                                                                          } else {
                                                                                                                            eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                          }
                                                                                                                        } else {
                                                                                                                          eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                        }
                                                                                                                      } else {
                                                                                                                        eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                      }
                                                                                                                    } else {
                                                                                                                      eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                    }
                                                                                                                    break;
                                                                                                                case 1 : 
                                                                                                                case 2 : 
                                                                                                                    eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                                    break;
                                                                                                                
                                                                                                              }
                                                                                                              break;
                                                                                                          case 0 : 
                                                                                                          case 2 : 
                                                                                                              eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                              break;
                                                                                                          
                                                                                                        }
                                                                                                      }
                                                                                                    } else {
                                                                                                      eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                    }
                                                                                                  } else {
                                                                                                    eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                  }
                                                                                                } else {
                                                                                                  eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                                }
                                                                                              } else {
                                                                                                eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                              }
                                                                                            } else {
                                                                                              eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                            }
                                                                                          } else {
                                                                                            eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                          }
                                                                                        } else {
                                                                                          eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                        }
                                                                                        break;
                                                                                    case 1 : 
                                                                                    case 2 : 
                                                                                        eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                                        break;
                                                                                    
                                                                                  }
                                                                                }
                                                                              } else {
                                                                                eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                              }
                                                                            } else {
                                                                              eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                            }
                                                                          } else {
                                                                            eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                          }
                                                                        } else {
                                                                          eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                        }
                                                                      } else {
                                                                        eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                      }
                                                                    } else {
                                                                      eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                    }
                                                                  } else {
                                                                    eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                  }
                                                                  break;
                                                              case 1 : 
                                                              case 2 : 
                                                                  eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                                  break;
                                                              
                                                            }
                                                          }
                                                        } else {
                                                          eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                        }
                                                      } else {
                                                        eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                      }
                                                    } else {
                                                      eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                    }
                                                  } else {
                                                    eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                  }
                                                } else {
                                                  eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                                }
                                              } else {
                                                eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                              }
                                            } else {
                                              eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                            }
                                          } else {
                                            eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                          }
                                          break;
                                      case 1 : 
                                      case 2 : 
                                          eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                          break;
                                      
                                    }
                                  }
                                } else {
                                  eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                                }
                              } else {
                                eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                              }
                            } else {
                              eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                            }
                          } else {
                            eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                          }
                        } else {
                          eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                        }
                      } else {
                        eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                      }
                    }
                  } else {
                    eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                  }
                } else {
                  eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
                }
              } else {
                eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
              }
            } else {
              eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
            }
          } else {
            eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
          }
        } else {
          eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
        }
      }
    } else {
      eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
    }
  } else {
    eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
  }
} else {
  eq("File \"test/parser_api_test.ml\", line 211, characters 12-19", true, false);
}

Mt.from_pair_suites("Parser_api_test", suites[0]);

var lex = Parser_api.from_string;

var parse = Parser_api.implementation;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.lex = lex;
exports.parse = parse;
/* match Not a pure module */
