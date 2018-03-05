'use strict';

var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var Random = require("../../lib/js/random.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var Actors = /* module */[];

var Dom_html = /* module */[];

function setup_sprite($staropt$star, $staropt$star$1, $staropt$star$2, img_src, max_frames, max_ticks, frame_size, src_offset) {
  var loop = $staropt$star ? $staropt$star[0] : /* true */1;
  var bbox_offset = $staropt$star$1 ? $staropt$star$1[0] : /* tuple */[
      0,
      0
    ];
  var bbox_size = $staropt$star$2 ? $staropt$star$2[0] : /* tuple */[
      0,
      0
    ];
  var bbox_size$1 = Caml_obj.caml_equal(bbox_size, /* tuple */[
        0,
        0
      ]) ? frame_size : bbox_size;
  var img_src$1 = "./sprites/" + img_src;
  return /* record */[
          /* max_frames */max_frames,
          /* max_ticks */max_ticks,
          /* img_src */img_src$1,
          /* frame_size */frame_size,
          /* src_offset */src_offset,
          /* bbox_offset */bbox_offset,
          /* bbox_size */bbox_size$1,
          /* loop */loop
        ];
}

function make_enemy(param) {
  var dir = param[1];
  switch (param[0]) {
    case 0 : 
        return setup_sprite(/* None */0, /* Some */[/* tuple */[
                      1,
                      1
                    ]], /* Some */[/* tuple */[
                      14,
                      14
                    ]], "enemies.png", 2, 10, /* tuple */[
                    16,
                    16
                  ], /* tuple */[
                    0,
                    128
                  ]);
    case 1 : 
        if (dir !== 0) {
          return setup_sprite(/* None */0, /* Some */[/* tuple */[
                        1,
                        10
                      ]], /* Some */[/* tuple */[
                        11,
                        16
                      ]], "enemies.png", 2, 10, /* tuple */[
                      16,
                      27
                    ], /* tuple */[
                      32,
                      69
                    ]);
        } else {
          return setup_sprite(/* None */0, /* Some */[/* tuple */[
                        4,
                        10
                      ]], /* Some */[/* tuple */[
                        11,
                        16
                      ]], "enemies.png", 2, 10, /* tuple */[
                      16,
                      27
                    ], /* tuple */[
                      0,
                      69
                    ]);
        }
    case 2 : 
        if (dir !== 0) {
          return setup_sprite(/* None */0, /* Some */[/* tuple */[
                        1,
                        10
                      ]], /* Some */[/* tuple */[
                        11,
                        16
                      ]], "enemies.png", 2, 10, /* tuple */[
                      16,
                      27
                    ], /* tuple */[
                      32,
                      5
                    ]);
        } else {
          return setup_sprite(/* None */0, /* Some */[/* tuple */[
                        4,
                        10
                      ]], /* Some */[/* tuple */[
                        11,
                        16
                      ]], "enemies.png", 2, 10, /* tuple */[
                      16,
                      27
                    ], /* tuple */[
                      0,
                      5
                    ]);
        }
    case 3 : 
        return setup_sprite(/* None */0, /* Some */[/* tuple */[
                      2,
                      2
                    ]], /* Some */[/* tuple */[
                      12,
                      13
                    ]], "enemies.png", 4, 10, /* tuple */[
                    16,
                    16
                  ], /* tuple */[
                    0,
                    96
                  ]);
    case 4 : 
        return setup_sprite(/* None */0, /* Some */[/* tuple */[
                      2,
                      2
                    ]], /* Some */[/* tuple */[
                      12,
                      13
                    ]], "enemies.png", 4, 10, /* tuple */[
                    16,
                    16
                  ], /* tuple */[
                    0,
                    32
                  ]);
    
  }
}

function make_particle(param) {
  switch (param) {
    case 0 : 
        return setup_sprite(/* None */0, /* None */0, /* None */0, "enemies.png", 1, 0, /* tuple */[
                    16,
                    16
                  ], /* tuple */[
                    0,
                    144
                  ]);
    case 1 : 
        return setup_sprite(/* None */0, /* None */0, /* None */0, "chunks.png", 1, 0, /* tuple */[
                    8,
                    8
                  ], /* tuple */[
                    0,
                    0
                  ]);
    case 2 : 
        return setup_sprite(/* None */0, /* None */0, /* None */0, "chunks.png", 1, 0, /* tuple */[
                    8,
                    8
                  ], /* tuple */[
                    8,
                    0
                  ]);
    case 3 : 
        return setup_sprite(/* None */0, /* None */0, /* None */0, "score.png", 1, 0, /* tuple */[
                    12,
                    8
                  ], /* tuple */[
                    0,
                    0
                  ]);
    case 4 : 
        return setup_sprite(/* None */0, /* None */0, /* None */0, "score.png", 1, 0, /* tuple */[
                    12,
                    9
                  ], /* tuple */[
                    0,
                    9
                  ]);
    case 5 : 
        return setup_sprite(/* None */0, /* None */0, /* None */0, "score.png", 1, 0, /* tuple */[
                    12,
                    9
                  ], /* tuple */[
                    0,
                    18
                  ]);
    case 6 : 
        return setup_sprite(/* None */0, /* None */0, /* None */0, "score.png", 1, 0, /* tuple */[
                    12,
                    9
                  ], /* tuple */[
                    0,
                    27
                  ]);
    case 7 : 
        return setup_sprite(/* None */0, /* None */0, /* None */0, "score.png", 1, 0, /* tuple */[
                    14,
                    9
                  ], /* tuple */[
                    13,
                    0
                  ]);
    case 8 : 
        return setup_sprite(/* None */0, /* None */0, /* None */0, "score.png", 1, 0, /* tuple */[
                    14,
                    9
                  ], /* tuple */[
                    13,
                    9
                  ]);
    case 9 : 
        return setup_sprite(/* None */0, /* None */0, /* None */0, "score.png", 1, 0, /* tuple */[
                    14,
                    9
                  ], /* tuple */[
                    13,
                    18
                  ]);
    case 10 : 
        return setup_sprite(/* None */0, /* None */0, /* None */0, "score.png", 1, 0, /* tuple */[
                    14,
                    9
                  ], /* tuple */[
                    13,
                    27
                  ]);
    
  }
}

function make_type(typ, dir) {
  switch (typ.tag | 0) {
    case 0 : 
        var pt = typ[0];
        var spr_type = /* tuple */[
          typ[1],
          dir
        ];
        if (pt !== 0) {
          var param = spr_type;
          var typ$1 = param[0];
          if (param[1] !== 0) {
            switch (typ$1) {
              case 0 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                1,
                                1
                              ]], /* Some */[/* tuple */[
                                11,
                                15
                              ]], "mario-small.png", 1, 0, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              0,
                              32
                            ]);
              case 1 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                2,
                                1
                              ]], /* Some */[/* tuple */[
                                13,
                                15
                              ]], "mario-small.png", 2, 10, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              16,
                              48
                            ]);
              case 2 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                2,
                                1
                              ]], /* Some */[/* tuple */[
                                12,
                                15
                              ]], "mario-small.png", 3, 5, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              16,
                              32
                            ]);
              case 3 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                1,
                                5
                              ]], /* Some */[/* tuple */[
                                14,
                                10
                              ]], "mario-small.png", 1, 0, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              0,
                              64
                            ]);
              
            }
          } else {
            switch (typ$1) {
              case 0 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                3,
                                1
                              ]], /* Some */[/* tuple */[
                                11,
                                15
                              ]], "mario-small.png", 1, 0, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              0,
                              0
                            ]);
              case 1 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                2,
                                1
                              ]], /* Some */[/* tuple */[
                                13,
                                15
                              ]], "mario-small.png", 2, 10, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              16,
                              16
                            ]);
              case 2 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                2,
                                1
                              ]], /* Some */[/* tuple */[
                                12,
                                15
                              ]], "mario-small.png", 3, 5, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              16,
                              0
                            ]);
              case 3 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                1,
                                5
                              ]], /* Some */[/* tuple */[
                                14,
                                10
                              ]], "mario-small.png", 1, 0, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              0,
                              64
                            ]);
              
            }
          }
        } else {
          var param$1 = spr_type;
          var typ$2 = param$1[0];
          if (param$1[1] !== 0) {
            switch (typ$2) {
              case 0 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                1,
                                1
                              ]], /* Some */[/* tuple */[
                                13,
                                25
                              ]], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              26
                            ], /* tuple */[
                              16,
                              69
                            ]);
              case 1 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                2,
                                1
                              ]], /* Some */[/* tuple */[
                                12,
                                25
                              ]], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              26
                            ], /* tuple */[
                              48,
                              70
                            ]);
              case 2 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                2,
                                1
                              ]], /* Some */[/* tuple */[
                                13,
                                25
                              ]], "mario-big.png", 4, 10, /* tuple */[
                              16,
                              27
                            ], /* tuple */[
                              0,
                              101
                            ]);
              case 3 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                2,
                                10
                              ]], /* Some */[/* tuple */[
                                13,
                                17
                              ]], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              27
                            ], /* tuple */[
                              32,
                              69
                            ]);
              
            }
          } else {
            switch (typ$2) {
              case 0 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                2,
                                1
                              ]], /* Some */[/* tuple */[
                                13,
                                25
                              ]], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              27
                            ], /* tuple */[
                              16,
                              5
                            ]);
              case 1 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                2,
                                1
                              ]], /* Some */[/* tuple */[
                                12,
                                25
                              ]], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              26
                            ], /* tuple */[
                              48,
                              6
                            ]);
              case 2 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                2,
                                1
                              ]], /* Some */[/* tuple */[
                                13,
                                25
                              ]], "mario-big.png", 4, 10, /* tuple */[
                              16,
                              27
                            ], /* tuple */[
                              0,
                              37
                            ]);
              case 3 : 
                  return setup_sprite(/* None */0, /* Some */[/* tuple */[
                                2,
                                10
                              ]], /* Some */[/* tuple */[
                                13,
                                17
                              ]], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              27
                            ], /* tuple */[
                              32,
                              5
                            ]);
              
            }
          }
        }
    case 1 : 
        return make_enemy(/* tuple */[
                    typ[0],
                    dir
                  ]);
    case 2 : 
        var param$2 = typ[0];
        switch (param$2) {
          case 0 : 
              return setup_sprite(/* None */0, /* Some */[/* tuple */[
                            2,
                            0
                          ]], /* Some */[/* tuple */[
                            12,
                            16
                          ]], "items.png", 1, 0, /* tuple */[
                          16,
                          16
                        ], /* tuple */[
                          0,
                          0
                        ]);
          case 1 : 
              return setup_sprite(/* None */0, /* None */0, /* None */0, "items.png", 1, 0, /* tuple */[
                          16,
                          16
                        ], /* tuple */[
                          0,
                          188
                        ]);
          case 2 : 
              return setup_sprite(/* None */0, /* None */0, /* None */0, "items.png", 1, 0, /* tuple */[
                          16,
                          16
                        ], /* tuple */[
                          16,
                          48
                        ]);
          case 3 : 
              return setup_sprite(/* None */0, /* Some */[/* tuple */[
                            3,
                            0
                          ]], /* Some */[/* tuple */[
                            12,
                            16
                          ]], "items.png", 3, 15, /* tuple */[
                          16,
                          16
                        ], /* tuple */[
                          0,
                          80
                        ]);
          
        }
    case 3 : 
        var param$3 = typ[0];
        if (typeof param$3 === "number") {
          switch (param$3) {
            case 0 : 
                return setup_sprite(/* None */0, /* None */0, /* None */0, "blocks.png", 1, 0, /* tuple */[
                            16,
                            16
                          ], /* tuple */[
                            0,
                            32
                          ]);
            case 1 : 
                return setup_sprite(/* None */0, /* None */0, /* None */0, "blocks.png", 5, 10, /* tuple */[
                            16,
                            16
                          ], /* tuple */[
                            0,
                            0
                          ]);
            case 2 : 
                return setup_sprite(/* None */0, /* None */0, /* None */0, "blocks.png", 1, 0, /* tuple */[
                            16,
                            16
                          ], /* tuple */[
                            0,
                            48
                          ]);
            case 3 : 
                return setup_sprite(/* None */0, /* None */0, /* None */0, "blocks.png", 1, 0, /* tuple */[
                            16,
                            16
                          ], /* tuple */[
                            0,
                            64
                          ]);
            case 4 : 
                return setup_sprite(/* None */0, /* None */0, /* None */0, "panel.png", 3, 15, /* tuple */[
                            26,
                            26
                          ], /* tuple */[
                            0,
                            0
                          ]);
            case 5 : 
                return setup_sprite(/* None */0, /* None */0, /* None */0, "ground.png", 1, 0, /* tuple */[
                            16,
                            16
                          ], /* tuple */[
                            0,
                            32
                          ]);
            
          }
        } else {
          return setup_sprite(/* None */0, /* None */0, /* None */0, "blocks.png", 4, 15, /* tuple */[
                      16,
                      16
                    ], /* tuple */[
                      0,
                      16
                    ]);
        }
    
  }
}

function make_from_params(params, context) {
  var img = document.createElement("img");
  img.src = params[/* img_src */2];
  return /* record */[
          /* params */params,
          /* context */context,
          /* frame */[0],
          /* ticks */[0],
          /* img */img
        ];
}

function make(spawn, dir, context) {
  var params = make_type(spawn, dir);
  return make_from_params(params, context);
}

function make_bgd(context) {
  var params = setup_sprite(/* None */0, /* None */0, /* None */0, "bgd-1.png", 1, 0, /* tuple */[
        512,
        256
      ], /* tuple */[
        0,
        0
      ]);
  return make_from_params(params, context);
}

function make_particle$1(ptyp, context) {
  var params = make_particle(ptyp);
  return make_from_params(params, context);
}

function transform_enemy(enemy_typ, spr, dir) {
  var params = make_enemy(/* tuple */[
        enemy_typ,
        dir
      ]);
  var img = document.createElement("img");
  img.src = params[/* img_src */2];
  spr[/* params */0] = params;
  spr[/* img */4] = img;
  return /* () */0;
}

function update_animation(spr) {
  var curr_ticks = spr[/* ticks */3][0];
  if (curr_ticks >= spr[/* params */0][/* max_ticks */1]) {
    spr[/* ticks */3][0] = 0;
    if (spr[/* params */0][/* loop */7]) {
      spr[/* frame */2][0] = Caml_int32.mod_(spr[/* frame */2][0] + 1 | 0, spr[/* params */0][/* max_frames */0]);
      return /* () */0;
    } else {
      return 0;
    }
  } else {
    spr[/* ticks */3][0] = curr_ticks + 1 | 0;
    return /* () */0;
  }
}

var Sprite = /* module */[
  /* setup_sprite */setup_sprite,
  /* make */make,
  /* make_bgd */make_bgd,
  /* make_particle */make_particle$1,
  /* transform_enemy */transform_enemy,
  /* update_animation */update_animation
];

function pair_to_xy(pair) {
  return /* float array */[
          pair[0],
          pair[1]
        ];
}

function make_type$1(typ, ctx) {
  if (typ === 2 || typ === 1) {
    return /* record */[
            /* sprite */make_particle$1(typ, ctx),
            /* rot */0,
            /* lifetime */300
          ];
  } else {
    return /* record */[
            /* sprite */make_particle$1(typ, ctx),
            /* rot */0,
            /* lifetime */30
          ];
  }
}

function make$1($staropt$star, $staropt$star$1, part_type, pos, ctx) {
  var vel = $staropt$star ? $staropt$star[0] : /* tuple */[
      0,
      0
    ];
  var acc = $staropt$star$1 ? $staropt$star$1[0] : /* tuple */[
      0,
      0
    ];
  var params = make_type$1(part_type, ctx);
  var pos$1 = pair_to_xy(pos);
  var vel$1 = pair_to_xy(vel);
  var acc$1 = pair_to_xy(acc);
  return /* record */[
          /* params */params,
          /* part_type */part_type,
          /* pos */pos$1,
          /* vel */vel$1,
          /* acc */acc$1,
          /* kill : false */0,
          /* life */params[/* lifetime */2]
        ];
}

function make_score(score, pos, ctx) {
  var t = score >= 801 ? (
      score >= 2001 ? (
          score !== 4000 ? (
              score !== 8000 ? /* Score100 */3 : /* Score8000 */10
            ) : /* Score4000 */9
        ) : (
          score !== 1000 ? (
              score >= 2000 ? /* Score2000 */8 : /* Score100 */3
            ) : /* Score1000 */7
        )
    ) : (
      score >= 201 ? (
          score !== 400 ? (
              score >= 800 ? /* Score800 */6 : /* Score100 */3
            ) : /* Score400 */5
        ) : (
          score !== 100 && score >= 200 ? /* Score200 */4 : /* Score100 */3
        )
    );
  return make$1(/* Some */[/* tuple */[
                0.5,
                -0.7
              ]], /* None */0, t, pos, ctx);
}

function update_vel(part) {
  part[/* vel */3][/* x */0] = part[/* vel */3][/* x */0] + part[/* acc */4][/* x */0];
  part[/* vel */3][/* y */1] = part[/* vel */3][/* y */1] + part[/* acc */4][/* y */1];
  return /* () */0;
}

function $$process(part) {
  part[/* life */6] = part[/* life */6] - 1 | 0;
  if (part[/* life */6] === 0) {
    part[/* kill */5] = /* true */1;
  }
  update_vel(part);
  var part$1 = part;
  part$1[/* pos */2][/* x */0] = part$1[/* vel */3][/* x */0] + part$1[/* pos */2][/* x */0];
  part$1[/* pos */2][/* y */1] = part$1[/* vel */3][/* y */1] + part$1[/* pos */2][/* y */1];
  return /* () */0;
}

var Particle = /* module */[
  /* make */make$1,
  /* make_score */make_score,
  /* process */$$process
];

var id_counter = [Pervasives.min_int];

function setup_obj($staropt$star, $staropt$star$1, _) {
  var has_gravity = $staropt$star ? $staropt$star[0] : /* true */1;
  var speed = $staropt$star$1 ? $staropt$star$1[0] : 1;
  return /* record */[
          /* has_gravity */has_gravity,
          /* speed */speed
        ];
}

function set_vel_to_speed(obj) {
  var speed = obj[/* params */0][/* speed */1];
  var match = obj[/* dir */6];
  if (match !== 0) {
    obj[/* vel */2][/* x */0] = speed;
    return /* () */0;
  } else {
    obj[/* vel */2][/* x */0] = -speed;
    return /* () */0;
  }
}

function make_type$2(param) {
  switch (param.tag | 0) {
    case 0 : 
        return setup_obj(/* None */0, /* Some */[2.8], /* () */0);
    case 1 : 
        var param$1 = param[0];
        if (param$1 >= 3) {
          return setup_obj(/* None */0, /* Some */[3], /* () */0);
        } else {
          return setup_obj(/* None */0, /* None */0, /* () */0);
        }
    case 2 : 
        var param$2 = param[0];
        if (param$2 >= 3) {
          return setup_obj(/* Some */[/* false */0], /* None */0, /* () */0);
        } else {
          return setup_obj(/* None */0, /* None */0, /* () */0);
        }
    case 3 : 
        return setup_obj(/* Some */[/* false */0], /* None */0, /* () */0);
    
  }
}

function new_id() {
  id_counter[0] = id_counter[0] + 1 | 0;
  return id_counter[0];
}

function make$2($staropt$star, $staropt$star$1, spawnable, context, param) {
  var id = $staropt$star ? $staropt$star[0] : /* None */0;
  var dir = $staropt$star$1 ? $staropt$star$1[0] : /* Left */0;
  var spr = make(spawnable, dir, context);
  var params = make_type$2(spawnable);
  var id$1 = id ? id[0] : new_id(/* () */0);
  var obj = /* record */[
    /* params */params,
    /* pos : float array */[
      param[0],
      param[1]
    ],
    /* vel : float array */[
      0.0,
      0.0
    ],
    /* id */id$1,
    /* jumping : false */0,
    /* grounded : false */0,
    /* dir */dir,
    /* invuln */0,
    /* kill : false */0,
    /* health */1,
    /* crouch : false */0,
    /* score */0
  ];
  return /* tuple */[
          spr,
          obj
        ];
}

function spawn(spawnable, context, param) {
  var match = make$2(/* None */0, /* None */0, spawnable, context, /* tuple */[
        param[0],
        param[1]
      ]);
  var obj = match[1];
  var spr = match[0];
  switch (spawnable.tag | 0) {
    case 0 : 
        return /* Player */Block.__(0, [
                  spawnable[0],
                  spr,
                  obj
                ]);
    case 1 : 
        set_vel_to_speed(obj);
        return /* Enemy */Block.__(1, [
                  spawnable[0],
                  spr,
                  obj
                ]);
    case 2 : 
        return /* Item */Block.__(2, [
                  spawnable[0],
                  spr,
                  obj
                ]);
    case 3 : 
        return /* Block */Block.__(3, [
                  spawnable[0],
                  spr,
                  obj
                ]);
    
  }
}

function get_sprite(param) {
  return param[1];
}

function get_obj(param) {
  return param[2];
}

function is_player(param) {
  if (param.tag) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function is_enemy(param) {
  if (param.tag === 1) {
    return /* true */1;
  } else {
    return /* false */0;
  }
}

function equals(col1, col2) {
  return +(col1[2][/* id */3] === col2[2][/* id */3]);
}

function normalize_pos(pos, p1, p2) {
  var match = p1[/* bbox_offset */5];
  var match$1 = p2[/* bbox_offset */5];
  var match$2 = p1[/* bbox_size */6];
  var match$3 = p2[/* bbox_size */6];
  pos[/* x */0] = pos[/* x */0] - (match$3[0] + match$1[0]) + (match$2[0] + match[0]);
  pos[/* y */1] = pos[/* y */1] - (match$3[1] + match$1[1]) + (match$2[1] + match[1]);
  return /* () */0;
}

function update_player(player, keys, context) {
  var prev_jumping = player[/* jumping */4];
  var prev_dir = player[/* dir */6];
  var prev_vx = Math.abs(player[/* vel */2][/* x */0]);
  List.iter((function (param) {
          var player$1 = player;
          var controls = param;
          var lr_acc = player$1[/* vel */2][/* x */0] * 0.2;
          switch (controls) {
            case 0 : 
                if (player$1[/* crouch */10]) {
                  return 0;
                } else {
                  if (player$1[/* vel */2][/* x */0] > -player$1[/* params */0][/* speed */1]) {
                    player$1[/* vel */2][/* x */0] = player$1[/* vel */2][/* x */0] - (0.4 - lr_acc);
                  }
                  player$1[/* dir */6] = /* Left */0;
                  return /* () */0;
                }
                break;
            case 1 : 
                if (player$1[/* crouch */10]) {
                  return 0;
                } else {
                  if (player$1[/* vel */2][/* x */0] < player$1[/* params */0][/* speed */1]) {
                    player$1[/* vel */2][/* x */0] = player$1[/* vel */2][/* x */0] + (0.4 + lr_acc);
                  }
                  player$1[/* dir */6] = /* Right */1;
                  return /* () */0;
                }
                break;
            case 2 : 
                if (!player$1[/* jumping */4] && player$1[/* grounded */5]) {
                  player$1[/* jumping */4] = /* true */1;
                  player$1[/* grounded */5] = /* false */0;
                  player$1[/* vel */2][/* y */1] = Caml_primitive.caml_float_max(player$1[/* vel */2][/* y */1] - (5.7 + Math.abs(player$1[/* vel */2][/* x */0]) * 0.25), -6);
                  return /* () */0;
                } else {
                  return 0;
                }
            case 3 : 
                if (!player$1[/* jumping */4] && player$1[/* grounded */5]) {
                  player$1[/* crouch */10] = /* true */1;
                  return /* () */0;
                } else {
                  return 0;
                }
            
          }
        }), keys);
  var v = player[/* vel */2][/* x */0] * 0.9;
  var vel_damped = Math.abs(v) < 0.1 ? 0 : v;
  player[/* vel */2][/* x */0] = vel_damped;
  var pl_typ = player[/* health */9] <= 1 ? /* SmallM */1 : /* BigM */0;
  if (!prev_jumping && player[/* jumping */4]) {
    return /* Some */[/* tuple */[
              pl_typ,
              make(/* SPlayer */Block.__(0, [
                      pl_typ,
                      /* Jumping */1
                    ]), player[/* dir */6], context)
            ]];
  } else if (prev_dir !== player[/* dir */6] || prev_vx === 0 && Math.abs(player[/* vel */2][/* x */0]) > 0 && !player[/* jumping */4]) {
    return /* Some */[/* tuple */[
              pl_typ,
              make(/* SPlayer */Block.__(0, [
                      pl_typ,
                      /* Running */2
                    ]), player[/* dir */6], context)
            ]];
  } else if (prev_dir !== player[/* dir */6] && player[/* jumping */4] && prev_jumping) {
    return /* Some */[/* tuple */[
              pl_typ,
              make(/* SPlayer */Block.__(0, [
                      pl_typ,
                      /* Jumping */1
                    ]), player[/* dir */6], context)
            ]];
  } else if (player[/* vel */2][/* y */1] === 0 && player[/* crouch */10]) {
    return /* Some */[/* tuple */[
              pl_typ,
              make(/* SPlayer */Block.__(0, [
                      pl_typ,
                      /* Crouching */3
                    ]), player[/* dir */6], context)
            ]];
  } else if (player[/* vel */2][/* y */1] === 0 && player[/* vel */2][/* x */0] === 0) {
    return /* Some */[/* tuple */[
              pl_typ,
              make(/* SPlayer */Block.__(0, [
                      pl_typ,
                      /* Standing */0
                    ]), player[/* dir */6], context)
            ]];
  } else {
    return /* None */0;
  }
}

function update_vel$1(obj) {
  if (obj[/* grounded */5]) {
    obj[/* vel */2][/* y */1] = 0;
    return /* () */0;
  } else if (obj[/* params */0][/* has_gravity */0]) {
    obj[/* vel */2][/* y */1] = Caml_primitive.caml_float_min(obj[/* vel */2][/* y */1] + 0.2 + Math.abs(obj[/* vel */2][/* y */1]) * 0.01, 4.5);
    return /* () */0;
  } else {
    return 0;
  }
}

function update_pos(obj) {
  obj[/* pos */1][/* x */0] = obj[/* vel */2][/* x */0] + obj[/* pos */1][/* x */0];
  if (obj[/* params */0][/* has_gravity */0]) {
    obj[/* pos */1][/* y */1] = obj[/* vel */2][/* y */1] + obj[/* pos */1][/* y */1];
    return /* () */0;
  } else {
    return 0;
  }
}

function process_obj(obj, mapy) {
  update_vel$1(obj);
  update_pos(obj);
  if (obj[/* pos */1][/* y */1] > mapy) {
    obj[/* kill */8] = /* true */1;
    return /* () */0;
  } else {
    return 0;
  }
}

function normalize_origin(pos, spr) {
  var p = spr[/* params */0];
  var match = p[/* bbox_offset */5];
  var match$1 = p[/* bbox_size */6];
  pos[0] -= match[0];
  pos[1] -= (match[1] + match$1[1]);
  return /* () */0;
}

function collide_block($staropt$star, dir, obj) {
  var check_x = $staropt$star ? $staropt$star[0] : /* true */1;
  if (dir !== 1) {
    if (dir !== 0) {
      if (check_x) {
        obj[/* vel */2][/* x */0] = 0;
        return /* () */0;
      } else {
        return 0;
      }
    } else {
      obj[/* vel */2][/* y */1] = -0.001;
      return /* () */0;
    }
  } else {
    obj[/* vel */2][/* y */1] = 0;
    obj[/* grounded */5] = /* true */1;
    obj[/* jumping */4] = /* false */0;
    return /* () */0;
  }
}

function opposite_dir(dir) {
  if (dir !== 0) {
    return /* Left */0;
  } else {
    return /* Right */1;
  }
}

function reverse_left_right(obj) {
  obj[/* vel */2][/* x */0] = -obj[/* vel */2][/* x */0];
  obj[/* dir */6] = opposite_dir(obj[/* dir */6]);
  return /* () */0;
}

function evolve_enemy(player_dir, typ, spr, obj, context) {
  var exit = 0;
  switch (typ) {
    case 0 : 
        obj[/* kill */8] = /* true */1;
        return /* None */0;
    case 1 : 
        var match = make$2(/* None */0, /* Some */[obj[/* dir */6]], /* SEnemy */Block.__(1, [/* GKoopaShell */3]), context, /* tuple */[
              obj[/* pos */1][/* x */0],
              obj[/* pos */1][/* y */1]
            ]);
        var new_obj = match[1];
        var new_spr = match[0];
        normalize_pos(new_obj[/* pos */1], spr[/* params */0], new_spr[/* params */0]);
        return /* Some */[/* Enemy */Block.__(1, [
                    /* GKoopaShell */3,
                    new_spr,
                    new_obj
                  ])];
    case 2 : 
        var match$1 = make$2(/* None */0, /* Some */[obj[/* dir */6]], /* SEnemy */Block.__(1, [/* RKoopaShell */4]), context, /* tuple */[
              obj[/* pos */1][/* x */0],
              obj[/* pos */1][/* y */1]
            ]);
        var new_obj$1 = match$1[1];
        var new_spr$1 = match$1[0];
        normalize_pos(new_obj$1[/* pos */1], spr[/* params */0], new_spr$1[/* params */0]);
        return /* Some */[/* Enemy */Block.__(1, [
                    /* RKoopaShell */4,
                    new_spr$1,
                    new_obj$1
                  ])];
    case 3 : 
    case 4 : 
        exit = 1;
        break;
    
  }
  if (exit === 1) {
    obj[/* dir */6] = player_dir;
    if (obj[/* vel */2][/* x */0] !== 0) {
      obj[/* vel */2][/* x */0] = 0;
    } else {
      set_vel_to_speed(obj);
    }
    return /* None */0;
  }
  
}

function rev_dir(o, t, s) {
  reverse_left_right(o);
  var old_params = s[/* params */0];
  transform_enemy(t, s, o[/* dir */6]);
  return normalize_pos(o[/* pos */1], old_params, s[/* params */0]);
}

function dec_health(obj) {
  var health = obj[/* health */9] - 1 | 0;
  if (health === 0) {
    obj[/* kill */8] = /* true */1;
    return /* () */0;
  } else if (obj[/* invuln */7] === 0) {
    obj[/* health */9] = health;
    return /* () */0;
  } else {
    return 0;
  }
}

function evolve_block(obj, context) {
  dec_health(obj);
  var match = make$2(/* None */0, /* None */0, /* SBlock */Block.__(3, [/* QBlockUsed */0]), context, /* tuple */[
        obj[/* pos */1][/* x */0],
        obj[/* pos */1][/* y */1]
      ]);
  return /* Block */Block.__(3, [
            /* QBlockUsed */0,
            match[0],
            match[1]
          ]);
}

function spawn_above(player_dir, obj, typ, context) {
  var item = spawn(/* SItem */Block.__(2, [typ]), context, /* tuple */[
        obj[/* pos */1][/* x */0],
        obj[/* pos */1][/* y */1]
      ]);
  var item_obj = item[2];
  item_obj[/* pos */1][/* y */1] = item_obj[/* pos */1][/* y */1] - item[1][/* params */0][/* frame_size */3][1];
  item_obj[/* dir */6] = opposite_dir(player_dir);
  set_vel_to_speed(item_obj);
  return item;
}

function get_aabb(obj) {
  var spr = obj[1][/* params */0];
  var obj$1 = obj[2];
  var match = spr[/* bbox_offset */5];
  var box = obj$1[/* pos */1][/* x */0] + match[0];
  var boy = obj$1[/* pos */1][/* y */1] + match[1];
  var match$1 = spr[/* bbox_size */6];
  var sy = match$1[1];
  var sx = match$1[0];
  return /* record */[
          /* center : float array */[
            box + sx / 2,
            boy + sy / 2
          ],
          /* half : float array */[
            sx / 2,
            sy / 2
          ]
        ];
}

function col_bypass(c1, c2) {
  var o1 = c1[2];
  var o2 = c2[2];
  var ctypes;
  switch (c1.tag | 0) {
    case 0 : 
        ctypes = c2.tag === 1 && c1[2][/* invuln */7] > 0 ? /* true */1 : /* false */0;
        break;
    case 1 : 
        ctypes = c2.tag === 2 ? /* true */1 : /* false */0;
        break;
    case 2 : 
        switch (c2.tag | 0) {
          case 1 : 
          case 2 : 
              ctypes = /* true */1;
              break;
          case 0 : 
          case 3 : 
              ctypes = /* false */0;
              break;
          
        }
        break;
    case 3 : 
        ctypes = /* false */0;
        break;
    
  }
  if (o1[/* kill */8] || o2[/* kill */8]) {
    return /* true */1;
  } else {
    return ctypes;
  }
}

function check_collision(c1, c2) {
  var b1 = get_aabb(c1);
  var b2 = get_aabb(c2);
  var o1 = c1[2];
  if (col_bypass(c1, c2)) {
    return /* None */0;
  } else {
    var vx = b1[/* center */0][/* x */0] - b2[/* center */0][/* x */0];
    var vy = b1[/* center */0][/* y */1] - b2[/* center */0][/* y */1];
    var hwidths = b1[/* half */1][/* x */0] + b2[/* half */1][/* x */0];
    var hheights = b1[/* half */1][/* y */1] + b2[/* half */1][/* y */1];
    if (Math.abs(vx) < hwidths && Math.abs(vy) < hheights) {
      var ox = hwidths - Math.abs(vx);
      var oy = hheights - Math.abs(vy);
      if (ox >= oy) {
        if (vy > 0) {
          o1[/* pos */1][/* y */1] = o1[/* pos */1][/* y */1] + oy;
          return /* Some */[/* North */0];
        } else {
          o1[/* pos */1][/* y */1] = o1[/* pos */1][/* y */1] - oy;
          return /* Some */[/* South */1];
        }
      } else if (vx > 0) {
        o1[/* pos */1][/* x */0] = o1[/* pos */1][/* x */0] + ox;
        return /* Some */[/* West */3];
      } else {
        o1[/* pos */1][/* x */0] = o1[/* pos */1][/* x */0] - ox;
        return /* Some */[/* East */2];
      }
    } else {
      return /* None */0;
    }
  }
}

function kill(collid, ctx) {
  switch (collid.tag | 0) {
    case 0 : 
        return /* [] */0;
    case 1 : 
        var o = collid[2];
        var pos_000 = o[/* pos */1][/* x */0];
        var pos_001 = o[/* pos */1][/* y */1];
        var pos = /* tuple */[
          pos_000,
          pos_001
        ];
        var score = o[/* score */11] > 0 ? /* :: */[
            make_score(o[/* score */11], pos, ctx),
            /* [] */0
          ] : /* [] */0;
        var remains = collid[0] !== 0 ? /* [] */0 : /* :: */[
            make$1(/* None */0, /* None */0, /* GoombaSquish */0, pos, ctx),
            /* [] */0
          ];
        return Pervasives.$at(score, remains);
    case 2 : 
        var o$1 = collid[2];
        if (collid[0] !== 0) {
          return /* [] */0;
        } else {
          return /* :: */[
                  make_score(o$1[/* score */11], /* tuple */[
                        o$1[/* pos */1][/* x */0],
                        o$1[/* pos */1][/* y */1]
                      ], ctx),
                  /* [] */0
                ];
        }
    case 3 : 
        var o$2 = collid[2];
        var t = collid[0];
        if (typeof t === "number") {
          if (t !== 1) {
            return /* [] */0;
          } else {
            var pos_000$1 = o$2[/* pos */1][/* x */0];
            var pos_001$1 = o$2[/* pos */1][/* y */1];
            var pos$1 = /* tuple */[
              pos_000$1,
              pos_001$1
            ];
            var p1 = make$1(/* Some */[/* tuple */[
                    -5,
                    -5
                  ]], /* Some */[/* tuple */[
                    0,
                    0.2
                  ]], /* BrickChunkL */1, pos$1, ctx);
            var p2 = make$1(/* Some */[/* tuple */[
                    -3,
                    -4
                  ]], /* Some */[/* tuple */[
                    0,
                    0.2
                  ]], /* BrickChunkL */1, pos$1, ctx);
            var p3 = make$1(/* Some */[/* tuple */[
                    3,
                    -4
                  ]], /* Some */[/* tuple */[
                    0,
                    0.2
                  ]], /* BrickChunkR */2, pos$1, ctx);
            var p4 = make$1(/* Some */[/* tuple */[
                    5,
                    -5
                  ]], /* Some */[/* tuple */[
                    0,
                    0.2
                  ]], /* BrickChunkR */2, pos$1, ctx);
            return /* :: */[
                    p1,
                    /* :: */[
                      p2,
                      /* :: */[
                        p3,
                        /* :: */[
                          p4,
                          /* [] */0
                        ]
                      ]
                    ]
                  ];
          }
        } else {
          return /* [] */0;
        }
        break;
    
  }
}

var $$Object = /* module */[
  /* invuln */60,
  /* dampen_jump */4,
  /* get_sprite */get_sprite,
  /* get_obj */get_obj,
  /* spawn */spawn,
  /* equals */equals,
  /* is_player */is_player,
  /* is_enemy */is_enemy,
  /* normalize_origin */normalize_origin,
  /* normalize_pos */normalize_pos,
  /* kill */kill,
  /* process_obj */process_obj,
  /* update_player */update_player,
  /* check_collision */check_collision,
  /* evolve_enemy */evolve_enemy,
  /* evolve_block */evolve_block,
  /* dec_health */dec_health,
  /* rev_dir */rev_dir,
  /* reverse_left_right */reverse_left_right,
  /* collide_block */collide_block,
  /* spawn_above */spawn_above
];

function render_bbox(sprite, param) {
  var context = sprite[/* context */1];
  var match = sprite[/* params */0][/* bbox_offset */5];
  var match$1 = sprite[/* params */0][/* bbox_size */6];
  context.strokeStyle = "#FF0000";
  return context.strokeRect(param[0] + match[0], param[1] + match[1], match$1[0], match$1[1]);
}

function render(sprite, param) {
  var context = sprite[/* context */1];
  var match = sprite[/* params */0][/* src_offset */4];
  var match$1 = sprite[/* params */0][/* frame_size */3];
  var sw = match$1[0];
  var match$2 = sprite[/* params */0][/* frame_size */3];
  var sx = match[0] + sprite[/* frame */2][0] * sw;
  return context.drawImage(sprite[/* img */4], sx, match[1], sw, match$1[1], param[0], param[1], match$2[0], match$2[1]);
}

function draw_bgd(bgd, off_x) {
  render(bgd, /* tuple */[
        -off_x,
        0
      ]);
  return render(bgd, /* tuple */[
              bgd[/* params */0][/* frame_size */3][0] - off_x,
              0
            ]);
}

function clear_canvas(canvas) {
  var context = canvas.getContext("2d");
  var cwidth = canvas.width;
  var cheight = canvas.height;
  context.clearRect(0, 0, cwidth, cheight);
  return /* () */0;
}

function hud(canvas, score, coins) {
  var score_string = "" + score;
  var coin_string = "" + coins;
  var context = canvas.getContext("2d");
  context.font = "10px 'Press Start 2P'";
  context.fillText("Score: " + score_string, canvas.width - 140, 18);
  context.fillText("Coins: " + coin_string, 120, 18);
  return /* () */0;
}

function fps(canvas, fps_val) {
  var fps_str = "" + (fps_val | 0);
  var context = canvas.getContext("2d");
  context.fillText(fps_str, 10, 18);
  return /* () */0;
}

function game_win(ctx) {
  ctx.rect(0, 0, 512, 512);
  ctx.fillStyle = "black";
  ctx.fill();
  ctx.fillStyle = "white";
  ctx.font = "20px 'Press Start 2P'";
  ctx.fillText("You win!", 180, 128);
  throw [
        Caml_builtin_exceptions.failure,
        "Game over."
      ];
}

function game_loss(ctx) {
  ctx.rect(0, 0, 512, 512);
  ctx.fillStyle = "black";
  ctx.fill();
  ctx.fillStyle = "white";
  ctx.font = "20px 'Press Start 2P'";
  ctx.fillText("GAME OVER. You lose!", 60, 128);
  throw [
        Caml_builtin_exceptions.failure,
        "Game over."
      ];
}

var Draw = /* module */[
  /* render */render,
  /* clear_canvas */clear_canvas,
  /* draw_bgd */draw_bgd,
  /* render_bbox */render_bbox,
  /* fps */fps,
  /* hud */hud,
  /* game_win */game_win,
  /* game_loss */game_loss
];

function make$3(param, param$1) {
  return /* record */[
          /* pos : float array */[
            0,
            0
          ],
          /* v_dim : float array */[
            param[0],
            param[1]
          ],
          /* m_dim : float array */[
            param$1[0],
            param$1[1]
          ]
        ];
}

function calc_viewport_point(cc, vc, mc) {
  var vc_half = vc / 2;
  return Caml_primitive.caml_float_min(Caml_primitive.caml_float_max(cc - vc_half, 0), Caml_primitive.caml_float_min(mc - vc, Math.abs(cc - vc_half)));
}

function in_viewport(v, pos) {
  var v_min_x = v[/* pos */0][/* x */0] - 32;
  var v_max_x = v[/* pos */0][/* x */0] + v[/* v_dim */1][/* x */0];
  var v_min_y = v[/* pos */0][/* y */1] - 32;
  var v_max_y = v[/* pos */0][/* y */1] + v[/* v_dim */1][/* y */1];
  var x = pos[/* x */0];
  var y = pos[/* y */1];
  if (x >= v_min_x && x <= v_max_x && y >= v_min_y) {
    return +(y <= v_max_y);
  } else {
    return /* false */0;
  }
}

function out_of_viewport_below(v, y) {
  var v_max_y = v[/* pos */0][/* y */1] + v[/* v_dim */1][/* y */1];
  return +(y >= v_max_y);
}

function coord_to_viewport(viewport, coord) {
  return /* float array */[
          coord[/* x */0] - viewport[/* pos */0][/* x */0],
          coord[/* y */1] - viewport[/* pos */0][/* y */1]
        ];
}

function update(vpt, ctr) {
  var new_x = calc_viewport_point(ctr[/* x */0], vpt[/* v_dim */1][/* x */0], vpt[/* m_dim */2][/* x */0]);
  var new_y = calc_viewport_point(ctr[/* y */1], vpt[/* v_dim */1][/* y */1], vpt[/* m_dim */2][/* y */1]);
  var pos = /* float array */[
    new_x,
    new_y
  ];
  return /* record */[
          /* pos */pos,
          /* v_dim */vpt[/* v_dim */1],
          /* m_dim */vpt[/* m_dim */2]
        ];
}

var Viewport = /* module */[
  /* make */make$3,
  /* calc_viewport_point */calc_viewport_point,
  /* in_viewport */in_viewport,
  /* out_of_viewport_below */out_of_viewport_below,
  /* coord_to_viewport */coord_to_viewport,
  /* update */update
];

var pressed_keys = /* record */[
  /* left : false */0,
  /* right : false */0,
  /* up : false */0,
  /* down : false */0,
  /* bbox */0
];

var collid_objs = [/* [] */0];

var particles = [/* [] */0];

var last_time = [0];

function calc_fps(t0, t1) {
  var delta = (t1 - t0) / 1000;
  return 1 / delta;
}

function update_score(state, i) {
  state[/* score */4] = state[/* score */4] + i | 0;
  return /* () */0;
}

function process_collision(dir, c1, c2, state) {
  var context = state[/* ctx */1];
  var exit = 0;
  var s1;
  var o1;
  var typ;
  var s2;
  var o2;
  var s1$1;
  var o1$1;
  var t2;
  var s2$1;
  var o2$1;
  var o1$2;
  var t2$1;
  var o2$2;
  switch (c1.tag | 0) {
    case 0 : 
        var o1$3 = c1[2];
        var s1$2 = c1[1];
        switch (c2.tag | 0) {
          case 0 : 
              return /* tuple */[
                      /* None */0,
                      /* None */0
                    ];
          case 1 : 
              var o2$3 = c2[2];
              var s2$2 = c2[1];
              var typ$1 = c2[0];
              if (dir !== 1) {
                s1$1 = s1$2;
                o1$1 = o1$3;
                t2 = typ$1;
                s2$1 = s2$2;
                o2$1 = o2$3;
                exit = 2;
              } else {
                s1 = s1$2;
                o1 = o1$3;
                typ = typ$1;
                s2 = s2$2;
                o2 = o2$3;
                exit = 1;
              }
              break;
          case 2 : 
              o1$2 = o1$3;
              t2$1 = c2[0];
              o2$2 = c2[2];
              exit = 3;
              break;
          case 3 : 
              var o2$4 = c2[2];
              var t = c2[0];
              if (dir !== 0) {
                var exit$1 = 0;
                if (typeof t === "number") {
                  if (t !== 4) {
                    exit$1 = 4;
                  } else {
                    game_win(state[/* ctx */1]);
                    return /* tuple */[
                            /* None */0,
                            /* None */0
                          ];
                  }
                } else {
                  exit$1 = 4;
                }
                if (exit$1 === 4) {
                  if (dir !== 1) {
                    collide_block(/* None */0, dir, o1$3);
                    return /* tuple */[
                            /* None */0,
                            /* None */0
                          ];
                  } else {
                    state[/* multiplier */6] = 1;
                    collide_block(/* None */0, dir, o1$3);
                    return /* tuple */[
                            /* None */0,
                            /* None */0
                          ];
                  }
                }
                
              } else if (typeof t === "number") {
                if (t !== 1) {
                  if (t !== 4) {
                    collide_block(/* None */0, dir, o1$3);
                    return /* tuple */[
                            /* None */0,
                            /* None */0
                          ];
                  } else {
                    game_win(state[/* ctx */1]);
                    return /* tuple */[
                            /* None */0,
                            /* None */0
                          ];
                  }
                } else if (c1[0] === /* BigM */0) {
                  collide_block(/* None */0, dir, o1$3);
                  dec_health(o2$4);
                  return /* tuple */[
                          /* None */0,
                          /* None */0
                        ];
                } else {
                  collide_block(/* None */0, dir, o1$3);
                  return /* tuple */[
                          /* None */0,
                          /* None */0
                        ];
                }
              } else {
                var updated_block = evolve_block(o2$4, context);
                var spawned_item = spawn_above(o1$3[/* dir */6], o2$4, t[0], context);
                collide_block(/* None */0, dir, o1$3);
                return /* tuple */[
                        /* Some */[spawned_item],
                        /* Some */[updated_block]
                      ];
              }
              break;
          
        }
        break;
    case 1 : 
        var o1$4 = c1[2];
        var s1$3 = c1[1];
        var t1 = c1[0];
        switch (c2.tag | 0) {
          case 0 : 
              var o1$5 = c2[2];
              var s1$4 = c2[1];
              if (dir !== 0) {
                s1$1 = s1$4;
                o1$1 = o1$5;
                t2 = t1;
                s2$1 = s1$3;
                o2$1 = o1$4;
                exit = 2;
              } else {
                s1 = s1$4;
                o1 = o1$5;
                typ = t1;
                s2 = s1$3;
                o2 = o1$4;
                exit = 1;
              }
              break;
          case 1 : 
              var t1$1 = t1;
              var s1$5 = s1$3;
              var o1$6 = o1$4;
              var t2$2 = c2[0];
              var s2$3 = c2[1];
              var o2$5 = c2[2];
              var dir$1 = dir;
              var exit$2 = 0;
              if (t1$1 !== 3) {
                if (t1$1 >= 4) {
                  if (t2$2 >= 3) {
                    dec_health(o1$6);
                    dec_health(o2$5);
                    return /* tuple */[
                            /* None */0,
                            /* None */0
                          ];
                  } else {
                    exit$2 = 1;
                  }
                } else if (t2$2 >= 3) {
                  if (o2$5[/* vel */2][/* x */0] === 0) {
                    rev_dir(o1$6, t1$1, s1$5);
                    return /* tuple */[
                            /* None */0,
                            /* None */0
                          ];
                  } else {
                    dec_health(o1$6);
                    return /* tuple */[
                            /* None */0,
                            /* None */0
                          ];
                  }
                } else if (dir$1 >= 2) {
                  rev_dir(o1$6, t1$1, s1$5);
                  rev_dir(o2$5, t2$2, s2$3);
                  return /* tuple */[
                          /* None */0,
                          /* None */0
                        ];
                } else {
                  return /* tuple */[
                          /* None */0,
                          /* None */0
                        ];
                }
              } else if (t2$2 >= 3) {
                dec_health(o1$6);
                dec_health(o2$5);
                return /* tuple */[
                        /* None */0,
                        /* None */0
                      ];
              } else {
                exit$2 = 1;
              }
              if (exit$2 === 1) {
                if (o1$6[/* vel */2][/* x */0] === 0) {
                  rev_dir(o2$5, t2$2, s2$3);
                  return /* tuple */[
                          /* None */0,
                          /* None */0
                        ];
                } else {
                  dec_health(o2$5);
                  return /* tuple */[
                          /* None */0,
                          /* None */0
                        ];
                }
              }
              case 2 : 
              return /* tuple */[
                      /* None */0,
                      /* None */0
                    ];
          case 3 : 
              var o2$6 = c2[2];
              var t2$3 = c2[0];
              if (dir >= 2) {
                if (t1 >= 3) {
                  if (typeof t2$3 === "number") {
                    if (t2$3 !== 1) {
                      rev_dir(o1$4, t1, s1$3);
                      return /* tuple */[
                              /* None */0,
                              /* None */0
                            ];
                    } else {
                      dec_health(o2$6);
                      reverse_left_right(o1$4);
                      return /* tuple */[
                              /* None */0,
                              /* None */0
                            ];
                    }
                  } else {
                    var updated_block$1 = evolve_block(o2$6, context);
                    var spawned_item$1 = spawn_above(o1$4[/* dir */6], o2$6, t2$3[0], context);
                    rev_dir(o1$4, t1, s1$3);
                    return /* tuple */[
                            /* Some */[updated_block$1],
                            /* Some */[spawned_item$1]
                          ];
                  }
                } else {
                  rev_dir(o1$4, t1, s1$3);
                  return /* tuple */[
                          /* None */0,
                          /* None */0
                        ];
                }
              } else {
                collide_block(/* None */0, dir, o1$4);
                return /* tuple */[
                        /* None */0,
                        /* None */0
                      ];
              }
              break;
          
        }
        break;
    case 2 : 
        var o2$7 = c1[2];
        switch (c2.tag | 0) {
          case 0 : 
              o1$2 = c2[2];
              t2$1 = c1[0];
              o2$2 = o2$7;
              exit = 3;
              break;
          case 1 : 
          case 2 : 
              return /* tuple */[
                      /* None */0,
                      /* None */0
                    ];
          case 3 : 
              if (dir >= 2) {
                reverse_left_right(o2$7);
                return /* tuple */[
                        /* None */0,
                        /* None */0
                      ];
              } else {
                collide_block(/* None */0, dir, o2$7);
                return /* tuple */[
                        /* None */0,
                        /* None */0
                      ];
              }
          
        }
        break;
    case 3 : 
        return /* tuple */[
                /* None */0,
                /* None */0
              ];
    
  }
  switch (exit) {
    case 1 : 
        var o1$7 = o1;
        var typ$2 = typ;
        var s2$4 = s2;
        var o2$8 = o2;
        var state$1 = state;
        var context$1 = context;
        o1$7[/* invuln */7] = 10;
        o1$7[/* jumping */4] = /* false */0;
        o1$7[/* grounded */5] = /* true */1;
        if (typ$2 >= 3) {
          var r2 = evolve_enemy(o1$7[/* dir */6], typ$2, s2$4, o2$8, context$1);
          o1$7[/* vel */2][/* y */1] = -4;
          o1$7[/* pos */1][/* y */1] = o1$7[/* pos */1][/* y */1] - 5;
          return /* tuple */[
                  /* None */0,
                  r2
                ];
        } else {
          dec_health(o2$8);
          o1$7[/* vel */2][/* y */1] = -4;
          if (state$1[/* multiplier */6] === 8) {
            update_score(state$1, 800);
            o2$8[/* score */11] = 800;
            return /* tuple */[
                    /* None */0,
                    evolve_enemy(o1$7[/* dir */6], typ$2, s2$4, o2$8, context$1)
                  ];
          } else {
            var score = Caml_int32.imul(100, state$1[/* multiplier */6]);
            update_score(state$1, score);
            o2$8[/* score */11] = score;
            state$1[/* multiplier */6] = (state$1[/* multiplier */6] << 1);
            return /* tuple */[
                    /* None */0,
                    evolve_enemy(o1$7[/* dir */6], typ$2, s2$4, o2$8, context$1)
                  ];
          }
        }
    case 2 : 
        var o1$8 = o1$1;
        var t2$4 = t2;
        var s2$5 = s2$1;
        var o2$9 = o2$1;
        var context$2 = context;
        if (t2$4 >= 3) {
          var r2$1 = o2$9[/* vel */2][/* x */0] === 0 ? evolve_enemy(o1$8[/* dir */6], t2$4, s2$5, o2$9, context$2) : (dec_health(o1$8), o1$8[/* invuln */7] = 60, /* None */0);
          return /* tuple */[
                  /* None */0,
                  r2$1
                ];
        } else {
          dec_health(o1$8);
          o1$8[/* invuln */7] = 60;
          return /* tuple */[
                  /* None */0,
                  /* None */0
                ];
        }
    case 3 : 
        if (t2$1 !== 0) {
          if (t2$1 >= 3) {
            state[/* coins */5] = state[/* coins */5] + 1 | 0;
            dec_health(o2$2);
            update_score(state, 100);
            return /* tuple */[
                    /* None */0,
                    /* None */0
                  ];
          } else {
            dec_health(o2$2);
            update_score(state, 1000);
            return /* tuple */[
                    /* None */0,
                    /* None */0
                  ];
          }
        } else {
          dec_health(o2$2);
          if (o1$2[/* health */9] !== 2) {
            o1$2[/* health */9] = o1$2[/* health */9] + 1 | 0;
          }
          o1$2[/* vel */2][/* x */0] = 0;
          o1$2[/* vel */2][/* y */1] = 0;
          update_score(state, 1000);
          o2$2[/* score */11] = 1000;
          return /* tuple */[
                  /* None */0,
                  /* None */0
                ];
        }
        break;
    
  }
}

function broad_phase(collid, all_collids, state) {
  var obj = collid[2];
  return List.filter((function () {
                  if (in_viewport(state[/* vpt */2], obj[/* pos */1]) || is_player(collid)) {
                    return /* true */1;
                  } else {
                    return out_of_viewport_below(state[/* vpt */2], obj[/* pos */1][/* y */1]);
                  }
                }))(all_collids);
}

function check_collisions(collid, all_collids, state) {
  if (collid.tag === 3) {
    return /* [] */0;
  } else {
    var broad = broad_phase(collid, all_collids, state);
    var c = collid;
    var cs = broad;
    var state$1 = state;
    var c$1 = c;
    var _cs = cs;
    var state$2 = state$1;
    var _acc = /* [] */0;
    while(true) {
      var acc = _acc;
      var cs$1 = _cs;
      if (cs$1) {
        var h = cs$1[0];
        var c_obj = c$1[2];
        var new_objs;
        if (equals(c$1, h)) {
          new_objs = /* tuple */[
            /* None */0,
            /* None */0
          ];
        } else {
          var match = check_collision(c$1, h);
          new_objs = match ? (
              h[2][/* id */3] !== c_obj[/* id */3] ? process_collision(match[0], c$1, h, state$2) : /* tuple */[
                  /* None */0,
                  /* None */0
                ]
            ) : /* tuple */[
              /* None */0,
              /* None */0
            ];
        }
        var match$1 = new_objs[0];
        var acc$1;
        if (match$1) {
          var match$2 = new_objs[1];
          var o = match$1[0];
          acc$1 = match$2 ? /* :: */[
              o,
              /* :: */[
                match$2[0],
                acc
              ]
            ] : /* :: */[
              o,
              acc
            ];
        } else {
          var match$3 = new_objs[1];
          acc$1 = match$3 ? /* :: */[
              match$3[0],
              acc
            ] : acc;
        }
        _acc = acc$1;
        _cs = cs$1[1];
        continue ;
        
      } else {
        return acc;
      }
    };
  }
}

function update_collidable(state, collid, all_collids) {
  var obj = collid[2];
  var spr = collid[1];
  obj[/* invuln */7] = obj[/* invuln */7] > 0 ? obj[/* invuln */7] - 1 | 0 : 0;
  var viewport_filter = in_viewport(state[/* vpt */2], obj[/* pos */1]) || is_player(collid) || out_of_viewport_below(state[/* vpt */2], obj[/* pos */1][/* y */1]);
  if (!obj[/* kill */8] && viewport_filter) {
    obj[/* grounded */5] = /* false */0;
    process_obj(obj, state[/* map */3]);
    var evolved = check_collisions(collid, all_collids, state);
    var vpt_adj_xy = coord_to_viewport(state[/* vpt */2], obj[/* pos */1]);
    render(spr, /* tuple */[
          vpt_adj_xy[/* x */0],
          vpt_adj_xy[/* y */1]
        ]);
    if (pressed_keys[/* bbox */4] === 1) {
      render_bbox(spr, /* tuple */[
            vpt_adj_xy[/* x */0],
            vpt_adj_xy[/* y */1]
          ]);
    }
    if (obj[/* vel */2][/* x */0] !== 0 || !is_enemy(collid)) {
      update_animation(spr);
    }
    return evolved;
  } else {
    return /* [] */0;
  }
}

function translate_keys() {
  var ctrls_000 = /* tuple */[
    pressed_keys[/* left */0],
    /* CLeft */0
  ];
  var ctrls_001 = /* :: */[
    /* tuple */[
      pressed_keys[/* right */1],
      /* CRight */1
    ],
    /* :: */[
      /* tuple */[
        pressed_keys[/* up */2],
        /* CUp */2
      ],
      /* :: */[
        /* tuple */[
          pressed_keys[/* down */3],
          /* CDown */3
        ],
        /* [] */0
      ]
    ]
  ];
  var ctrls = /* :: */[
    ctrls_000,
    ctrls_001
  ];
  return List.fold_left((function (a, x) {
                if (x[0]) {
                  return /* :: */[
                          x[1],
                          a
                        ];
                } else {
                  return a;
                }
              }), /* [] */0, ctrls);
}

function run_update_collid(state, collid, all_collids) {
  if (collid.tag) {
    var obj = collid[2];
    var evolved = update_collidable(state, collid, all_collids);
    if (!obj[/* kill */8]) {
      collid_objs[0] = /* :: */[
        collid,
        Pervasives.$at(collid_objs[0], evolved)
      ];
    }
    var new_parts = obj[/* kill */8] ? kill(collid, state[/* ctx */1]) : /* [] */0;
    particles[0] = Pervasives.$at(particles[0], new_parts);
    return collid;
  } else {
    var o = collid[2];
    var keys = translate_keys(/* () */0);
    o[/* crouch */10] = /* false */0;
    var match = update_player(o, keys, state[/* ctx */1]);
    var player;
    if (match) {
      var match$1 = match[0];
      var new_spr = match$1[1];
      normalize_pos(o[/* pos */1], collid[1][/* params */0], new_spr[/* params */0]);
      player = /* Player */Block.__(0, [
          match$1[0],
          new_spr,
          o
        ]);
    } else {
      player = collid;
    }
    var evolved$1 = update_collidable(state, player, all_collids);
    collid_objs[0] = Pervasives.$at(collid_objs[0], evolved$1);
    return player;
  }
}

function update_loop(canvas, param, map_dim) {
  var player = param[0];
  var ctx = canvas.getContext("2d");
  var cwidth = canvas.width / 1;
  var cheight = canvas.height / 1;
  var viewport = make$3(/* tuple */[
        cwidth,
        cheight
      ], map_dim);
  var state = /* record */[
    /* bgd */make_bgd(ctx),
    /* ctx */ctx,
    /* vpt */update(viewport, player[2][/* pos */1]),
    /* map */map_dim[1],
    /* score */0,
    /* coins */0,
    /* multiplier */1,
    /* game_over : false */0
  ];
  state[/* ctx */1].scale(1, 1);
  var update_helper = function (time, state, player, objs, parts) {
    if (state[/* game_over */7] === /* true */1) {
      return game_win(state[/* ctx */1]);
    } else {
      collid_objs[0] = /* [] */0;
      particles[0] = /* [] */0;
      var fps$1 = calc_fps(last_time[0], time);
      last_time[0] = time;
      clear_canvas(canvas);
      var vpos_x_int = state[/* vpt */2][/* pos */0][/* x */0] / 5 | 0;
      var bgd_width = state[/* bgd */0][/* params */0][/* frame_size */3][0] | 0;
      draw_bgd(state[/* bgd */0], Caml_int32.mod_(vpos_x_int, bgd_width));
      var player$1 = run_update_collid(state, player, objs);
      if (player$1[2][/* kill */8] === /* true */1) {
        return game_loss(state[/* ctx */1]);
      } else {
        var newrecord = state.slice();
        newrecord[/* vpt */2] = update(state[/* vpt */2], player$1[2][/* pos */1]);
        List.iter((function (obj) {
                run_update_collid(newrecord, obj, objs);
                return /* () */0;
              }), objs);
        List.iter((function (part) {
                var state = newrecord;
                var part$1 = part;
                $$process(part$1);
                var x = part$1[/* pos */2][/* x */0] - state[/* vpt */2][/* pos */0][/* x */0];
                var y = part$1[/* pos */2][/* y */1] - state[/* vpt */2][/* pos */0][/* y */1];
                render(part$1[/* params */0][/* sprite */0], /* tuple */[
                      x,
                      y
                    ]);
                if (part$1[/* kill */5]) {
                  return 0;
                } else {
                  particles[0] = /* :: */[
                    part$1,
                    particles[0]
                  ];
                  return /* () */0;
                }
              }), parts);
        fps(canvas, fps$1);
        hud(canvas, newrecord[/* score */4], newrecord[/* coins */5]);
        requestAnimationFrame((function (t) {
                return update_helper(t, newrecord, player$1, collid_objs[0], particles[0]);
              }));
        return /* () */0;
      }
    }
  };
  return update_helper(0, state, player, param[1], /* [] */0);
}

function keydown(evt) {
  var match = evt.keyCode;
  if (match >= 41) {
    var switcher = match - 65 | 0;
    if (!(switcher > 22 || switcher < 0)) {
      switch (switcher) {
        case 0 : 
            pressed_keys[/* left */0] = /* true */1;
            break;
        case 1 : 
            pressed_keys[/* bbox */4] = (pressed_keys[/* bbox */4] + 1 | 0) % 2;
            break;
        case 3 : 
            pressed_keys[/* right */1] = /* true */1;
            break;
        case 18 : 
            pressed_keys[/* down */3] = /* true */1;
            break;
        case 2 : 
        case 4 : 
        case 5 : 
        case 6 : 
        case 7 : 
        case 8 : 
        case 9 : 
        case 10 : 
        case 11 : 
        case 12 : 
        case 13 : 
        case 14 : 
        case 15 : 
        case 16 : 
        case 17 : 
        case 19 : 
        case 20 : 
        case 21 : 
            break;
        case 22 : 
            pressed_keys[/* up */2] = /* true */1;
            break;
        
      }
    }
    
  } else if (match >= 32) {
    switch (match - 32 | 0) {
      case 1 : 
      case 2 : 
      case 3 : 
      case 4 : 
          break;
      case 5 : 
          pressed_keys[/* left */0] = /* true */1;
          break;
      case 0 : 
      case 6 : 
          pressed_keys[/* up */2] = /* true */1;
          break;
      case 7 : 
          pressed_keys[/* right */1] = /* true */1;
          break;
      case 8 : 
          pressed_keys[/* down */3] = /* true */1;
          break;
      
    }
  }
  return true;
}

function keyup(evt) {
  var match = evt.keyCode;
  if (match >= 68) {
    if (match !== 83) {
      if (match !== 87) {
        if (match >= 69) {
          
        } else {
          pressed_keys[/* right */1] = /* false */0;
        }
      } else {
        pressed_keys[/* up */2] = /* false */0;
      }
    } else {
      pressed_keys[/* down */3] = /* false */0;
    }
  } else if (match >= 41) {
    if (match === 65) {
      pressed_keys[/* left */0] = /* false */0;
    }
    
  } else if (match >= 32) {
    switch (match - 32 | 0) {
      case 1 : 
      case 2 : 
      case 3 : 
      case 4 : 
          break;
      case 5 : 
          pressed_keys[/* left */0] = /* false */0;
          break;
      case 0 : 
      case 6 : 
          pressed_keys[/* up */2] = /* false */0;
          break;
      case 7 : 
          pressed_keys[/* right */1] = /* false */0;
          break;
      case 8 : 
          pressed_keys[/* down */3] = /* false */0;
          break;
      
    }
  }
  return true;
}

var Director = /* module */[
  /* update_loop */update_loop,
  /* keydown */keydown,
  /* keyup */keyup
];

function mem_loc(checkloc, _loclist) {
  while(true) {
    var loclist = _loclist;
    if (loclist) {
      if (Caml_obj.caml_equal(checkloc, loclist[0][1])) {
        return /* true */1;
      } else {
        _loclist = loclist[1];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function convert_list(lst) {
  if (lst) {
    var h = lst[0];
    return Pervasives.$at(/* :: */[
                /* tuple */[
                  h[0],
                  /* tuple */[
                    h[1][0] * 16,
                    h[1][1] * 16
                  ]
                ],
                /* [] */0
              ], convert_list(lst[1]));
  } else {
    return /* [] */0;
  }
}

function choose_enemy_typ(typ) {
  if (typ > 2 || typ < 0) {
    throw [
          Caml_builtin_exceptions.failure,
          "Shouldn't reach here"
        ];
  } else {
    switch (typ) {
      case 0 : 
          return /* RKoopa */2;
      case 1 : 
          return /* GKoopa */1;
      case 2 : 
          return /* Goomba */0;
      
    }
  }
}

function choose_sblock_typ(typ) {
  if (typ > 4 || typ < 0) {
    throw [
          Caml_builtin_exceptions.failure,
          "Shouldn't reach here"
        ];
  } else {
    switch (typ) {
      case 0 : 
          return /* Brick */1;
      case 1 : 
          return /* UnBBlock */2;
      case 2 : 
          return /* Cloud */3;
      case 3 : 
          return /* QBlock */[/* Mushroom */0];
      case 4 : 
          return /* Ground */5;
      
    }
  }
}

function avoid_overlap(_lst, currentLst) {
  while(true) {
    var lst = _lst;
    if (lst) {
      var t = lst[1];
      var h = lst[0];
      if (mem_loc(h[1], currentLst)) {
        _lst = t;
        continue ;
        
      } else {
        return Pervasives.$at(/* :: */[
                    h,
                    /* [] */0
                  ], avoid_overlap(t, currentLst));
      }
    } else {
      return /* [] */0;
    }
  };
}

function trim_edges(_lst, blockw, blockh) {
  while(true) {
    var lst = _lst;
    if (lst) {
      var t = lst[1];
      var h = lst[0];
      var cx = h[1][0];
      var cy = h[1][1];
      var pixx = blockw * 16;
      var pixy = blockh * 16;
      if (cx < 128 || pixx - cx < 528 || cy === 0 || pixy - cy < 48) {
        _lst = t;
        continue ;
        
      } else {
        return Pervasives.$at(/* :: */[
                    h,
                    /* [] */0
                  ], trim_edges(t, blockw, blockh));
      }
    } else {
      return /* [] */0;
    }
  };
}

function generate_clouds(cbx, cby, typ, num) {
  if (num === 0) {
    return /* [] */0;
  } else {
    return Pervasives.$at(/* :: */[
                /* tuple */[
                  typ,
                  /* tuple */[
                    cbx,
                    cby
                  ]
                ],
                /* [] */0
              ], generate_clouds(cbx + 1, cby, typ, num - 1 | 0));
  }
}

function generate_coins(_block_coord) {
  while(true) {
    var block_coord = _block_coord;
    var place_coin = Random.$$int(2);
    if (block_coord) {
      var t = block_coord[1];
      var h = block_coord[0];
      if (place_coin === 0) {
        var xc = h[1][0];
        var yc = h[1][1];
        return Pervasives.$at(/* :: */[
                    /* tuple */[
                      0,
                      /* tuple */[
                        xc,
                        yc - 16
                      ]
                    ],
                    /* [] */0
                  ], generate_coins(t));
      } else {
        _block_coord = t;
        continue ;
        
      }
    } else {
      return /* [] */0;
    }
  };
}

function choose_block_pattern(blockw, blockh, cbx, cby, prob) {
  if (cbx > blockw || cby > blockh) {
    return /* [] */0;
  } else {
    var block_typ = Random.$$int(4);
    var stair_typ = Random.$$int(2);
    var life_block_chance = Random.$$int(5);
    var middle_block = life_block_chance === 0 ? 3 : stair_typ;
    if (prob > 5 || prob < 0) {
      throw [
            Caml_builtin_exceptions.failure,
            "Shouldn't reach here"
          ];
    } else {
      switch (prob) {
        case 0 : 
            if (blockw - cbx > 2) {
              return /* :: */[
                      /* tuple */[
                        stair_typ,
                        /* tuple */[
                          cbx,
                          cby
                        ]
                      ],
                      /* :: */[
                        /* tuple */[
                          middle_block,
                          /* tuple */[
                            cbx + 1,
                            cby
                          ]
                        ],
                        /* :: */[
                          /* tuple */[
                            stair_typ,
                            /* tuple */[
                              cbx + 2,
                              cby
                            ]
                          ],
                          /* [] */0
                        ]
                      ]
                    ];
            } else if (blockw - cbx > 1) {
              return /* :: */[
                      /* tuple */[
                        block_typ,
                        /* tuple */[
                          cbx,
                          cby
                        ]
                      ],
                      /* :: */[
                        /* tuple */[
                          block_typ,
                          /* tuple */[
                            cbx + 1,
                            cby
                          ]
                        ],
                        /* [] */0
                      ]
                    ];
            } else {
              return /* :: */[
                      /* tuple */[
                        block_typ,
                        /* tuple */[
                          cbx,
                          cby
                        ]
                      ],
                      /* [] */0
                    ];
            }
        case 1 : 
            var num_clouds = Random.$$int(5) + 5 | 0;
            if (cby < 5) {
              return generate_clouds(cbx, cby, 2, num_clouds);
            } else {
              return /* [] */0;
            }
        case 2 : 
            if (blockh - cby === 1) {
              var cbx$1 = cbx;
              var cby$1 = cby;
              var typ = stair_typ;
              var four_000 = /* tuple */[
                typ,
                /* tuple */[
                  cbx$1,
                  cby$1
                ]
              ];
              var four_001 = /* :: */[
                /* tuple */[
                  typ,
                  /* tuple */[
                    cbx$1 + 1,
                    cby$1
                  ]
                ],
                /* :: */[
                  /* tuple */[
                    typ,
                    /* tuple */[
                      cbx$1 + 2,
                      cby$1
                    ]
                  ],
                  /* :: */[
                    /* tuple */[
                      typ,
                      /* tuple */[
                        cbx$1 + 3,
                        cby$1
                      ]
                    ],
                    /* [] */0
                  ]
                ]
              ];
              var four = /* :: */[
                four_000,
                four_001
              ];
              var three_000 = /* tuple */[
                typ,
                /* tuple */[
                  cbx$1 + 1,
                  cby$1 - 1
                ]
              ];
              var three_001 = /* :: */[
                /* tuple */[
                  typ,
                  /* tuple */[
                    cbx$1 + 2,
                    cby$1 - 1
                  ]
                ],
                /* :: */[
                  /* tuple */[
                    typ,
                    /* tuple */[
                      cbx$1 + 3,
                      cby$1 - 1
                    ]
                  ],
                  /* [] */0
                ]
              ];
              var three = /* :: */[
                three_000,
                three_001
              ];
              var two_000 = /* tuple */[
                typ,
                /* tuple */[
                  cbx$1 + 2,
                  cby$1 - 2
                ]
              ];
              var two_001 = /* :: */[
                /* tuple */[
                  typ,
                  /* tuple */[
                    cbx$1 + 3,
                    cby$1 - 2
                  ]
                ],
                /* [] */0
              ];
              var two = /* :: */[
                two_000,
                two_001
              ];
              var one_000 = /* tuple */[
                typ,
                /* tuple */[
                  cbx$1 + 3,
                  cby$1 - 3
                ]
              ];
              var one = /* :: */[
                one_000,
                /* [] */0
              ];
              return Pervasives.$at(four, Pervasives.$at(three, Pervasives.$at(two, one)));
            } else {
              return /* [] */0;
            }
        case 3 : 
            if (stair_typ === 0 && blockh - cby > 3) {
              var cbx$2 = cbx;
              var cby$2 = cby;
              var typ$1 = stair_typ;
              var three_000$1 = /* tuple */[
                typ$1,
                /* tuple */[
                  cbx$2,
                  cby$2
                ]
              ];
              var three_001$1 = /* :: */[
                /* tuple */[
                  typ$1,
                  /* tuple */[
                    cbx$2 + 1,
                    cby$2
                  ]
                ],
                /* :: */[
                  /* tuple */[
                    typ$1,
                    /* tuple */[
                      cbx$2 + 2,
                      cby$2
                    ]
                  ],
                  /* [] */0
                ]
              ];
              var three$1 = /* :: */[
                three_000$1,
                three_001$1
              ];
              var two_000$1 = /* tuple */[
                typ$1,
                /* tuple */[
                  cbx$2 + 2,
                  cby$2 + 1
                ]
              ];
              var two_001$1 = /* :: */[
                /* tuple */[
                  typ$1,
                  /* tuple */[
                    cbx$2 + 3,
                    cby$2 + 1
                  ]
                ],
                /* [] */0
              ];
              var two$1 = /* :: */[
                two_000$1,
                two_001$1
              ];
              var one_000$1 = /* tuple */[
                typ$1,
                /* tuple */[
                  cbx$2 + 5,
                  cby$2 + 2
                ]
              ];
              var one_001 = /* :: */[
                /* tuple */[
                  typ$1,
                  /* tuple */[
                    cbx$2 + 6,
                    cby$2 + 2
                  ]
                ],
                /* [] */0
              ];
              var one$1 = /* :: */[
                one_000$1,
                one_001
              ];
              return Pervasives.$at(three$1, Pervasives.$at(two$1, one$1));
            } else if (blockh - cby > 2) {
              var cbx$3 = cbx;
              var cby$3 = cby;
              var typ$2 = stair_typ;
              var one_000$2 = /* tuple */[
                typ$2,
                /* tuple */[
                  cbx$3,
                  cby$3
                ]
              ];
              var one_001$1 = /* :: */[
                /* tuple */[
                  typ$2,
                  /* tuple */[
                    cbx$3 + 1,
                    cby$3
                  ]
                ],
                /* [] */0
              ];
              var one$2 = /* :: */[
                one_000$2,
                one_001$1
              ];
              var two_000$2 = /* tuple */[
                typ$2,
                /* tuple */[
                  cbx$3 + 3,
                  cby$3 - 1
                ]
              ];
              var two_001$2 = /* :: */[
                /* tuple */[
                  typ$2,
                  /* tuple */[
                    cbx$3 + 4,
                    cby$3 - 1
                  ]
                ],
                /* [] */0
              ];
              var two$2 = /* :: */[
                two_000$2,
                two_001$2
              ];
              var three_000$2 = /* tuple */[
                typ$2,
                /* tuple */[
                  cbx$3 + 4,
                  cby$3 - 2
                ]
              ];
              var three_001$2 = /* :: */[
                /* tuple */[
                  typ$2,
                  /* tuple */[
                    cbx$3 + 5,
                    cby$3 - 2
                  ]
                ],
                /* :: */[
                  /* tuple */[
                    typ$2,
                    /* tuple */[
                      cbx$3 + 6,
                      cby$3 - 2
                    ]
                  ],
                  /* [] */0
                ]
              ];
              var three$2 = /* :: */[
                three_000$2,
                three_001$2
              ];
              return Pervasives.$at(one$2, Pervasives.$at(two$2, three$2));
            } else {
              return /* :: */[
                      /* tuple */[
                        stair_typ,
                        /* tuple */[
                          cbx,
                          cby
                        ]
                      ],
                      /* [] */0
                    ];
            }
        case 4 : 
            if (cby + 3 - blockh === 2) {
              return /* :: */[
                      /* tuple */[
                        stair_typ,
                        /* tuple */[
                          cbx,
                          cby
                        ]
                      ],
                      /* [] */0
                    ];
            } else if (cby + 3 - blockh === 1) {
              return /* :: */[
                      /* tuple */[
                        stair_typ,
                        /* tuple */[
                          cbx,
                          cby
                        ]
                      ],
                      /* :: */[
                        /* tuple */[
                          stair_typ,
                          /* tuple */[
                            cbx,
                            cby + 1
                          ]
                        ],
                        /* [] */0
                      ]
                    ];
            } else {
              return /* :: */[
                      /* tuple */[
                        stair_typ,
                        /* tuple */[
                          cbx,
                          cby
                        ]
                      ],
                      /* :: */[
                        /* tuple */[
                          stair_typ,
                          /* tuple */[
                            cbx,
                            cby + 1
                          ]
                        ],
                        /* :: */[
                          /* tuple */[
                            stair_typ,
                            /* tuple */[
                              cbx,
                              cby + 2
                            ]
                          ],
                          /* [] */0
                        ]
                      ]
                    ];
            }
        case 5 : 
            return /* :: */[
                    /* tuple */[
                      3,
                      /* tuple */[
                        cbx,
                        cby
                      ]
                    ],
                    /* [] */0
                  ];
        
      }
    }
  }
}

function generate_enemies(blockw, blockh, _cbx, _cby, acc) {
  while(true) {
    var cby = _cby;
    var cbx = _cbx;
    if (cbx > blockw - 32) {
      return /* [] */0;
    } else if (cby > blockh - 1 || cbx < 15) {
      _cby = 0;
      _cbx = cbx + 1;
      continue ;
      
    } else if (mem_loc(/* tuple */[
            cbx,
            cby
          ], acc) || cby === 0) {
      _cby = cby + 1;
      continue ;
      
    } else {
      var prob = Random.$$int(30);
      if (prob < 3 && blockh - 1 === cby) {
        var enemy_000 = /* tuple */[
          prob,
          /* tuple */[
            cbx * 16,
            cby * 16
          ]
        ];
        var enemy = /* :: */[
          enemy_000,
          /* [] */0
        ];
        return Pervasives.$at(enemy, generate_enemies(blockw, blockh, cbx, cby + 1, acc));
      } else {
        _cby = cby + 1;
        continue ;
        
      }
    }
  };
}

function generate_block_enemies(_block_coord) {
  while(true) {
    var block_coord = _block_coord;
    var place_enemy = Random.$$int(20);
    var enemy_typ = Random.$$int(3);
    if (block_coord) {
      var t = block_coord[1];
      var h = block_coord[0];
      if (place_enemy === 0) {
        var xc = h[1][0];
        var yc = h[1][1];
        return Pervasives.$at(/* :: */[
                    /* tuple */[
                      enemy_typ,
                      /* tuple */[
                        xc,
                        yc - 16
                      ]
                    ],
                    /* [] */0
                  ], generate_block_enemies(t));
      } else {
        _block_coord = t;
        continue ;
        
      }
    } else {
      return /* [] */0;
    }
  };
}

function generate_block_locs(blockw, blockh, _cbx, _cby, _acc) {
  while(true) {
    var acc = _acc;
    var cby = _cby;
    var cbx = _cbx;
    if (blockw - cbx < 33) {
      return acc;
    } else if (cby > blockh - 1) {
      _cby = 0;
      _cbx = cbx + 1;
      continue ;
      
    } else if (mem_loc(/* tuple */[
            cbx,
            cby
          ], acc) || cby === 0) {
      _cby = cby + 1;
      continue ;
      
    } else {
      var prob = Random.$$int(100);
      if (prob < 5) {
        var newacc = choose_block_pattern(blockw, blockh, cbx, cby, prob);
        var undup_lst = avoid_overlap(newacc, acc);
        var called_acc = Pervasives.$at(acc, undup_lst);
        _acc = called_acc;
        _cby = cby + 1;
        continue ;
        
      } else {
        _cby = cby + 1;
        continue ;
        
      }
    }
  };
}

function generate_panel(context, blockw, blockh) {
  return spawn(/* SBlock */Block.__(3, [/* Panel */4]), context, /* tuple */[
              blockw * 16 - 256,
              blockh * 16 * 2 / 3
            ]);
}

function generate_ground(blockw, blockh, _inc, _acc) {
  while(true) {
    var acc = _acc;
    var inc = _inc;
    if (inc > blockw) {
      return acc;
    } else if (inc > 10) {
      var skip = Random.$$int(10);
      var newacc = Pervasives.$at(acc, /* :: */[
            /* tuple */[
              4,
              /* tuple */[
                inc * 16,
                blockh * 16
              ]
            ],
            /* [] */0
          ]);
      if (skip === 7 && blockw - inc > 32) {
        _inc = inc + 1;
        continue ;
        
      } else {
        _acc = newacc;
        _inc = inc + 1;
        continue ;
        
      }
    } else {
      var newacc$1 = Pervasives.$at(acc, /* :: */[
            /* tuple */[
              4,
              /* tuple */[
                inc * 16,
                blockh * 16
              ]
            ],
            /* [] */0
          ]);
      _acc = newacc$1;
      _inc = inc + 1;
      continue ;
      
    }
  };
}

function convert_to_block_obj(lst, context) {
  if (lst) {
    var h = lst[0];
    var sblock_typ = choose_sblock_typ(h[0]);
    var ob = spawn(/* SBlock */Block.__(3, [sblock_typ]), context, h[1]);
    return Pervasives.$at(/* :: */[
                ob,
                /* [] */0
              ], convert_to_block_obj(lst[1], context));
  } else {
    return /* [] */0;
  }
}

function convert_to_enemy_obj(lst, context) {
  if (lst) {
    var h = lst[0];
    var senemy_typ = choose_enemy_typ(h[0]);
    var ob = spawn(/* SEnemy */Block.__(1, [senemy_typ]), context, h[1]);
    return Pervasives.$at(/* :: */[
                ob,
                /* [] */0
              ], convert_to_enemy_obj(lst[1], context));
  } else {
    return /* [] */0;
  }
}

function convert_to_coin_obj(lst, context) {
  if (lst) {
    var ob = spawn(/* SItem */Block.__(2, [/* Coin */3]), context, lst[0][1]);
    return Pervasives.$at(/* :: */[
                ob,
                /* [] */0
              ], convert_to_coin_obj(lst[1], context));
  } else {
    return /* [] */0;
  }
}

function generate_helper(blockw, blockh, _, _$1, context) {
  var block_locs = generate_block_locs(blockw, blockh, 0, 0, /* [] */0);
  var converted_block_locs = trim_edges(convert_list(block_locs), blockw, blockh);
  var obj_converted_block_locs = convert_to_block_obj(converted_block_locs, context);
  var ground_blocks = generate_ground(blockw, blockh, 0, /* [] */0);
  var obj_converted_ground_blocks = convert_to_block_obj(ground_blocks, context);
  var block_locations = Pervasives.$at(block_locs, ground_blocks);
  var all_blocks = Pervasives.$at(obj_converted_block_locs, obj_converted_ground_blocks);
  var enemy_locs = generate_enemies(blockw, blockh, 0, 0, block_locations);
  var obj_converted_enemies = convert_to_enemy_obj(enemy_locs, context);
  var coin_locs = generate_coins(converted_block_locs);
  var undup_coin_locs = trim_edges(avoid_overlap(coin_locs, converted_block_locs), blockw, blockh);
  var converted_block_coin_locs = Pervasives.$at(converted_block_locs, coin_locs);
  var enemy_block_locs = generate_block_enemies(converted_block_locs);
  var undup_enemy_block_locs = avoid_overlap(enemy_block_locs, converted_block_coin_locs);
  var obj_enemy_blocks = convert_to_enemy_obj(undup_enemy_block_locs, context);
  var coin_objects = convert_to_coin_obj(undup_coin_locs, context);
  var obj_panel = generate_panel(context, blockw, blockh);
  return Pervasives.$at(all_blocks, Pervasives.$at(obj_converted_enemies, Pervasives.$at(coin_objects, Pervasives.$at(obj_enemy_blocks, /* :: */[
                          obj_panel,
                          /* [] */0
                        ]))));
}

function generate(w, h, context) {
  var blockw = w / 16;
  var blockh = h / 16 - 1;
  var collide_list = generate_helper(blockw, blockh, 0, 0, context);
  var player = spawn(/* SPlayer */Block.__(0, [
          /* SmallM */1,
          /* Standing */0
        ]), context, /* tuple */[
        100,
        224
      ]);
  return /* tuple */[
          player,
          collide_list
        ];
}

function init() {
  return Random.self_init(/* () */0);
}

var Procedural_generator = /* module */[
  /* init */init,
  /* generate */generate
];

var loadCount = [0];

function load() {
  Random.self_init(/* () */0);
  var canvas_id = "canvas";
  var match = document.getElementById(canvas_id);
  var canvas;
  if (match !== null) {
    canvas = match;
  } else {
    Curry._1(Printf.printf(/* Format */[
              /* String_literal */Block.__(11, [
                  "cant find canvas ",
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* String_literal */Block.__(11, [
                          " \n",
                          /* End_of_format */0
                        ])
                    ])
                ]),
              "cant find canvas %s \n"
            ]), canvas_id);
    throw [
          Caml_builtin_exceptions.failure,
          "fail"
        ];
  }
  var context = canvas.getContext("2d");
  document.addEventListener("keydown", keydown, true);
  document.addEventListener("keyup", keyup, true);
  Random.self_init(/* () */0);
  update_loop(canvas, generate(2400, 256, context), /* tuple */[
        2400,
        256
      ]);
  console.log("asd");
  return /* () */0;
}

function inc_counter() {
  loadCount[0] = loadCount[0] + 1 | 0;
  if (loadCount[0] === 4) {
    return load(/* () */0);
  } else {
    return /* () */0;
  }
}

function preload() {
  return List.map((function (img_src) {
                var img_src$1 = "sprites/" + img_src;
                var img = document.createElement("img");
                img.src = img_src$1;
                img.addEventListener("load", (function () {
                        inc_counter(/* () */0);
                        return true;
                      }), true);
                return /* () */0;
              }), /* :: */[
              "blocks.png",
              /* :: */[
                "items.png",
                /* :: */[
                  "enemies.png",
                  /* :: */[
                    "mario-small.png",
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

window.onload = (function () {
    preload(/* () */0);
    return true;
  });

var Main = /* module */[
  /* Html */0,
  /* Pg */0,
  /* loadCount */loadCount,
  /* imgsToLoad */4,
  /* level_width */2400,
  /* level_height */256,
  /* load */load,
  /* inc_counter */inc_counter,
  /* preload */preload
];

exports.Actors = Actors;
exports.Dom_html = Dom_html;
exports.Sprite = Sprite;
exports.Particle = Particle;
exports.$$Object = $$Object;
exports.Draw = Draw;
exports.Viewport = Viewport;
exports.Director = Director;
exports.Procedural_generator = Procedural_generator;
exports.Main = Main;
/*  Not a pure module */
