// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Caml_oo        = require("../runtime/caml_oo");
var Mt             = require("./mt");
var CamlinternalOO = require("../stdlib/camlinternalOO");

var shared = [
  0,
  "hi",
  "add"
];

var shared$1 = [
  0,
  "hi",
  "hello"
];

var $$class = CamlinternalOO.create_table(shared$1);

var ids = CamlinternalOO.get_method_labels($$class, shared$1);

var hi = ids[1];

var hello = ids[2];

CamlinternalOO.set_methods($$class, /* array */[
      hi,
      function (_, x, y) {
        return x + y;
      },
      hello,
      function (self$neg1, z) {
        return self$neg1[1][hi](self$neg1, 10, z);
      }
    ]);

function obj_init() {
  return CamlinternalOO.create_object_opt(0, $$class);
}

CamlinternalOO.init_class($$class);

var vv = obj_init(0);

var $$class$1 = CamlinternalOO.create_table([
      0,
      "x",
      "y"
    ]);

var ids$1 = CamlinternalOO.get_method_labels($$class$1, [
      0,
      "y",
      "x"
    ]);

var y = ids$1[1];

var x = ids$1[2];

CamlinternalOO.set_methods($$class$1, /* array */[
      x,
      function () {
        return 3;
      },
      y,
      function () {
        return 32;
      }
    ]);

function obj_init$1() {
  return CamlinternalOO.create_object_opt(0, $$class$1);
}

CamlinternalOO.init_class($$class$1);

var v = obj_init$1(0);

var $$class$2 = CamlinternalOO.create_table([
      0,
      "hi",
      "id1",
      "id2",
      "hello"
    ]);

var ids$2 = CamlinternalOO.get_method_labels($$class$2, [
      0,
      "id2",
      "id1",
      "hi",
      "hello"
    ]);

var id2 = ids$2[1];

var id1 = ids$2[2];

var hi$1 = ids$2[3];

var hello$1 = ids$2[4];

CamlinternalOO.set_methods($$class$2, /* array */[
      hi$1,
      function (_, v, z) {
        return v + z;
      },
      id1,
      function () {
        return 3;
      },
      id2,
      function () {
        return 4;
      },
      hello$1,
      function (_, v) {
        return v;
      }
    ]);

function obj_init$2() {
  return CamlinternalOO.create_object_opt(0, $$class$2);
}

CamlinternalOO.init_class($$class$2);

var u = obj_init$2(0);

var $$class$3 = CamlinternalOO.create_table([
      0,
      "id"
    ]);

var id = CamlinternalOO.get_method_label($$class$3, "id");

CamlinternalOO.set_method($$class$3, id, function () {
      return "uu";
    });

function obj_init$3() {
  return CamlinternalOO.create_object_opt(0, $$class$3);
}

CamlinternalOO.init_class($$class$3);

var uu = obj_init$3(0);

var $$class$4 = CamlinternalOO.create_table([
      0,
      "add"
    ]);

var add = CamlinternalOO.get_method_label($$class$4, "add");

CamlinternalOO.set_method($$class$4, add, function (_, x, y) {
      return x + y;
    });

function obj_init$4() {
  return CamlinternalOO.create_object_opt(0, $$class$4);
}

CamlinternalOO.init_class($$class$4);

var uuu = obj_init$4(0);

var $$class$5 = CamlinternalOO.create_table(shared);

var ids$3 = CamlinternalOO.get_method_labels($$class$5, shared);

var hi$2 = ids$3[1];

var add$1 = ids$3[2];

CamlinternalOO.set_methods($$class$5, /* array */[
      add$1,
      function (_, x, y) {
        return x + y;
      },
      hi$2,
      function (self$neg6, x) {
        return self$neg6[1][add$1](self$neg6, x, 32);
      }
    ]);

function obj_init$5() {
  return CamlinternalOO.create_object_opt(0, $$class$5);
}

CamlinternalOO.init_class($$class$5);

var vvvv = obj_init$5(0);

var suites_001 = [
  /* tuple */0,
  "single_obj",
  function () {
    return [
            /* Eq */0,
            /* array */[
              3,
              32
            ],
            /* array */[
              Caml_oo.caml_get_public_method(v, 120, 1)(v),
              Caml_oo.caml_get_public_method(v, 121, 2)(v)
            ]
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "single_obj_cache",
    function () {
      return [
              /* Eq */0,
              /* array */[
                3,
                32
              ],
              /* array */[
                Caml_oo.caml_get_public_method(v, 120, 3)(v),
                Caml_oo.caml_get_public_method(v, 121, 4)(v)
              ]
            ];
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "self_obj",
      function () {
        return [
                /* Eq */0,
                13,
                Caml_oo.caml_get_public_method(vv, 616641298, 5)(vv, 3)
              ];
      }
    ],
    [
      /* :: */0,
      [
        /* tuple */0,
        "uu_id",
        function () {
          return [
                  /* Eq */0,
                  "uu",
                  Caml_oo.caml_get_public_method(uu, 23515, 6)(uu)
                ];
        }
      ],
      [
        /* :: */0,
        [
          /* tuple */0,
          "uu_add",
          function () {
            return [
                    /* Eq */0,
                    Caml_oo.caml_get_public_method(uuu, 4846113, 7)(uuu, 1, 20),
                    21
                  ];
          }
        ],
        [
          /* :: */0,
          [
            /* tuple */0,
            "v_add",
            function () {
              return [
                      /* Eq */0,
                      Caml_oo.caml_get_public_method(vvvv, 4846113, 8)(vvvv, 3, 7),
                      10
                    ];
            }
          ],
          [
            /* :: */0,
            [
              /* tuple */0,
              "u_id1",
              function () {
                return [
                        /* Eq */0,
                        Caml_oo.caml_get_public_method(u, 5243894, 9)(u),
                        3
                      ];
              }
            ],
            [
              /* :: */0,
              [
                /* tuple */0,
                "u_id2",
                function () {
                  return [
                          /* Eq */0,
                          Caml_oo.caml_get_public_method(u, 5243895, 10)(u),
                          4
                        ];
                }
              ],
              [
                /* :: */0,
                [
                  /* tuple */0,
                  "u hi",
                  function () {
                    return [
                            /* Eq */0,
                            Caml_oo.caml_get_public_method(u, 23297, 11)(u, 1, 2),
                            3
                          ];
                  }
                ],
                [
                  /* :: */0,
                  [
                    /* tuple */0,
                    "u hello",
                    function () {
                      return [
                              /* Eq */0,
                              Caml_oo.caml_get_public_method(u, 616641298, 12)(u, 32),
                              32
                            ];
                    }
                  ],
                  [
                    /* :: */0,
                    [
                      /* tuple */0,
                      "v hi",
                      function () {
                        return [
                                /* Eq */0,
                                Caml_oo.caml_get_public_method(vvvv, 23297, 13)(vvvv, 31),
                                63
                              ];
                      }
                    ],
                    [
                      /* :: */0,
                      [
                        /* tuple */0,
                        "uuu add",
                        function () {
                          return [
                                  /* Eq */0,
                                  Caml_oo.caml_get_public_method(uuu, 4846113, 14)(uuu, 3, 4),
                                  7
                                ];
                        }
                      ],
                      [
                        /* :: */0,
                        [
                          /* tuple */0,
                          "v x",
                          function () {
                            return [
                                    /* Eq */0,
                                    Caml_oo.caml_get_public_method(v, 120, 15)(v),
                                    3
                                  ];
                          }
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
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_pair_suites("obj_test.ml", suites);

exports.vv     = vv;
exports.v      = v;
exports.u      = u;
exports.uu     = uu;
exports.uuu    = uuu;
exports.vvvv   = vvvv;
exports.suites = suites;
/* class Not a pure module */
