// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt             = require("./mt");
var Printf         = require("../stdlib/printf");
var Caml_primitive = require("../runtime/caml_primitive");
var Caml_curry     = require("../runtime/caml_curry");
var List           = require("../stdlib/list");

var tests_16 = /* :: */[
  /* tuple */[
    1,
    256
  ],
  /* :: */[
    /* tuple */[
      2,
      512
    ],
    /* :: */[
      /* tuple */[
        4,
        1024
      ],
      /* :: */[
        /* tuple */[
          8,
          2048
        ],
        /* :: */[
          /* tuple */[
            16,
            4096
          ],
          /* :: */[
            /* tuple */[
              32,
              8192
            ],
            /* :: */[
              /* tuple */[
                64,
                16384
              ],
              /* :: */[
                /* tuple */[
                  128,
                  32768
                ],
                /* :: */[
                  /* tuple */[
                    256,
                    1
                  ],
                  /* :: */[
                    /* tuple */[
                      512,
                      2
                    ],
                    /* :: */[
                      /* tuple */[
                        1024,
                        4
                      ],
                      /* :: */[
                        /* tuple */[
                          2048,
                          8
                        ],
                        /* :: */[
                          /* tuple */[
                            4096,
                            16
                          ],
                          /* :: */[
                            /* tuple */[
                              8192,
                              32
                            ],
                            /* :: */[
                              /* tuple */[
                                16384,
                                64
                              ],
                              /* :: */[
                                /* tuple */[
                                  32768,
                                  128
                                ],
                                /* [] */0
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

var suites_16 = List.map(function (param) {
      var b = param[1];
      var a = param[0];
      return /* tuple */[
              Caml_curry.app1(Printf.sprintf(/* Format */{
                        0: /* String_literal */{
                          0: "swap16 ",
                          1: /* Int */{
                            0: /* Int_d */0,
                            1: /* No_padding */0,
                            2: /* No_precision */0,
                            3: /* End_of_format */0,
                            length: 4,
                            tag: 4
                          },
                          length: 2,
                          tag: 11
                        },
                        1: "swap16 %d",
                        length: 2,
                        tag: 0
                      }), a),
              function () {
                return /* Eq */{
                        0: Caml_primitive.caml_bswap16(a),
                        1: b,
                        length: 2,
                        tag: 0
                      };
              }
            ];
    }, tests_16);

Mt.from_pair_suites("swap_test.ml", suites_16);

exports.tests_16  = tests_16;
exports.suites_16 = suites_16;
/* suites_16 Not a pure module */
