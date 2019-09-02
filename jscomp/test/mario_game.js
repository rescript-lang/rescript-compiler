'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var Random = require("../../lib/js/random.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var Actors = { };

var Dom_html = { };

function setup_sprite($staropt$star, $staropt$star$1, $staropt$star$2, img_src, max_frames, max_ticks, frame_size, src_offset) {
  var loop = $staropt$star !== undefined ? $staropt$star : true;
  var bbox_offset = $staropt$star$1 !== undefined ? $staropt$star$1 : /* tuple */[
      0,
      0
    ];
  var bbox_size = $staropt$star$2 !== undefined ? $staropt$star$2 : /* tuple */[
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
    case "Goomba" :
        return setup_sprite(undefined, /* tuple */[
                    1,
                    1
                  ], /* tuple */[
                    14,
                    14
                  ], "enemies.png", 2, 10, /* tuple */[
                    16,
                    16
                  ], /* tuple */[
                    0,
                    128
                  ]);
    case "GKoopa" :
        if (dir !== "Left") {
          return setup_sprite(undefined, /* tuple */[
                      1,
                      10
                    ], /* tuple */[
                      11,
                      16
                    ], "enemies.png", 2, 10, /* tuple */[
                      16,
                      27
                    ], /* tuple */[
                      32,
                      69
                    ]);
        } else {
          return setup_sprite(undefined, /* tuple */[
                      4,
                      10
                    ], /* tuple */[
                      11,
                      16
                    ], "enemies.png", 2, 10, /* tuple */[
                      16,
                      27
                    ], /* tuple */[
                      0,
                      69
                    ]);
        }
    case "RKoopa" :
        if (dir !== "Left") {
          return setup_sprite(undefined, /* tuple */[
                      1,
                      10
                    ], /* tuple */[
                      11,
                      16
                    ], "enemies.png", 2, 10, /* tuple */[
                      16,
                      27
                    ], /* tuple */[
                      32,
                      5
                    ]);
        } else {
          return setup_sprite(undefined, /* tuple */[
                      4,
                      10
                    ], /* tuple */[
                      11,
                      16
                    ], "enemies.png", 2, 10, /* tuple */[
                      16,
                      27
                    ], /* tuple */[
                      0,
                      5
                    ]);
        }
    case "GKoopaShell" :
        return setup_sprite(undefined, /* tuple */[
                    2,
                    2
                  ], /* tuple */[
                    12,
                    13
                  ], "enemies.png", 4, 10, /* tuple */[
                    16,
                    16
                  ], /* tuple */[
                    0,
                    96
                  ]);
    case "RKoopaShell" :
        return setup_sprite(undefined, /* tuple */[
                    2,
                    2
                  ], /* tuple */[
                    12,
                    13
                  ], "enemies.png", 4, 10, /* tuple */[
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
    case "GoombaSquish" :
        return setup_sprite(undefined, undefined, undefined, "enemies.png", 1, 0, /* tuple */[
                    16,
                    16
                  ], /* tuple */[
                    0,
                    144
                  ]);
    case "BrickChunkL" :
        return setup_sprite(undefined, undefined, undefined, "chunks.png", 1, 0, /* tuple */[
                    8,
                    8
                  ], /* tuple */[
                    0,
                    0
                  ]);
    case "BrickChunkR" :
        return setup_sprite(undefined, undefined, undefined, "chunks.png", 1, 0, /* tuple */[
                    8,
                    8
                  ], /* tuple */[
                    8,
                    0
                  ]);
    case "Score100" :
        return setup_sprite(undefined, undefined, undefined, "score.png", 1, 0, /* tuple */[
                    12,
                    8
                  ], /* tuple */[
                    0,
                    0
                  ]);
    case "Score200" :
        return setup_sprite(undefined, undefined, undefined, "score.png", 1, 0, /* tuple */[
                    12,
                    9
                  ], /* tuple */[
                    0,
                    9
                  ]);
    case "Score400" :
        return setup_sprite(undefined, undefined, undefined, "score.png", 1, 0, /* tuple */[
                    12,
                    9
                  ], /* tuple */[
                    0,
                    18
                  ]);
    case "Score800" :
        return setup_sprite(undefined, undefined, undefined, "score.png", 1, 0, /* tuple */[
                    12,
                    9
                  ], /* tuple */[
                    0,
                    27
                  ]);
    case "Score1000" :
        return setup_sprite(undefined, undefined, undefined, "score.png", 1, 0, /* tuple */[
                    14,
                    9
                  ], /* tuple */[
                    13,
                    0
                  ]);
    case "Score2000" :
        return setup_sprite(undefined, undefined, undefined, "score.png", 1, 0, /* tuple */[
                    14,
                    9
                  ], /* tuple */[
                    13,
                    9
                  ]);
    case "Score4000" :
        return setup_sprite(undefined, undefined, undefined, "score.png", 1, 0, /* tuple */[
                    14,
                    9
                  ], /* tuple */[
                    13,
                    18
                  ]);
    case "Score8000" :
        return setup_sprite(undefined, undefined, undefined, "score.png", 1, 0, /* tuple */[
                    14,
                    9
                  ], /* tuple */[
                    13,
                    27
                  ]);
    
  }
}

function make_type(typ, dir) {
  switch (/* XXX */typ.tag) {
    case "SPlayer" :
        var pt = typ.Arg0;
        var spr_type = /* tuple */[
          typ.Arg1,
          dir
        ];
        if (pt !== "BigM") {
          var param = spr_type;
          var typ$1 = param[0];
          if (param[1] !== "Left") {
            switch (typ$1) {
              case "Standing" :
                  return setup_sprite(undefined, /* tuple */[
                              1,
                              1
                            ], /* tuple */[
                              11,
                              15
                            ], "mario-small.png", 1, 0, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              0,
                              32
                            ]);
              case "Jumping" :
                  return setup_sprite(undefined, /* tuple */[
                              2,
                              1
                            ], /* tuple */[
                              13,
                              15
                            ], "mario-small.png", 2, 10, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              16,
                              48
                            ]);
              case "Running" :
                  return setup_sprite(undefined, /* tuple */[
                              2,
                              1
                            ], /* tuple */[
                              12,
                              15
                            ], "mario-small.png", 3, 5, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              16,
                              32
                            ]);
              case "Crouching" :
                  return setup_sprite(undefined, /* tuple */[
                              1,
                              5
                            ], /* tuple */[
                              14,
                              10
                            ], "mario-small.png", 1, 0, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              0,
                              64
                            ]);
              
            }
          } else {
            switch (typ$1) {
              case "Standing" :
                  return setup_sprite(undefined, /* tuple */[
                              3,
                              1
                            ], /* tuple */[
                              11,
                              15
                            ], "mario-small.png", 1, 0, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              0,
                              0
                            ]);
              case "Jumping" :
                  return setup_sprite(undefined, /* tuple */[
                              2,
                              1
                            ], /* tuple */[
                              13,
                              15
                            ], "mario-small.png", 2, 10, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              16,
                              16
                            ]);
              case "Running" :
                  return setup_sprite(undefined, /* tuple */[
                              2,
                              1
                            ], /* tuple */[
                              12,
                              15
                            ], "mario-small.png", 3, 5, /* tuple */[
                              16,
                              16
                            ], /* tuple */[
                              16,
                              0
                            ]);
              case "Crouching" :
                  return setup_sprite(undefined, /* tuple */[
                              1,
                              5
                            ], /* tuple */[
                              14,
                              10
                            ], "mario-small.png", 1, 0, /* tuple */[
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
          if (param$1[1] !== "Left") {
            switch (typ$2) {
              case "Standing" :
                  return setup_sprite(undefined, /* tuple */[
                              1,
                              1
                            ], /* tuple */[
                              13,
                              25
                            ], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              26
                            ], /* tuple */[
                              16,
                              69
                            ]);
              case "Jumping" :
                  return setup_sprite(undefined, /* tuple */[
                              2,
                              1
                            ], /* tuple */[
                              12,
                              25
                            ], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              26
                            ], /* tuple */[
                              48,
                              70
                            ]);
              case "Running" :
                  return setup_sprite(undefined, /* tuple */[
                              2,
                              1
                            ], /* tuple */[
                              13,
                              25
                            ], "mario-big.png", 4, 10, /* tuple */[
                              16,
                              27
                            ], /* tuple */[
                              0,
                              101
                            ]);
              case "Crouching" :
                  return setup_sprite(undefined, /* tuple */[
                              2,
                              10
                            ], /* tuple */[
                              13,
                              17
                            ], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              27
                            ], /* tuple */[
                              32,
                              69
                            ]);
              
            }
          } else {
            switch (typ$2) {
              case "Standing" :
                  return setup_sprite(undefined, /* tuple */[
                              2,
                              1
                            ], /* tuple */[
                              13,
                              25
                            ], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              27
                            ], /* tuple */[
                              16,
                              5
                            ]);
              case "Jumping" :
                  return setup_sprite(undefined, /* tuple */[
                              2,
                              1
                            ], /* tuple */[
                              12,
                              25
                            ], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              26
                            ], /* tuple */[
                              48,
                              6
                            ]);
              case "Running" :
                  return setup_sprite(undefined, /* tuple */[
                              2,
                              1
                            ], /* tuple */[
                              13,
                              25
                            ], "mario-big.png", 4, 10, /* tuple */[
                              16,
                              27
                            ], /* tuple */[
                              0,
                              37
                            ]);
              case "Crouching" :
                  return setup_sprite(undefined, /* tuple */[
                              2,
                              10
                            ], /* tuple */[
                              13,
                              17
                            ], "mario-big.png", 1, 0, /* tuple */[
                              16,
                              27
                            ], /* tuple */[
                              32,
                              5
                            ]);
              
            }
          }
        }
    case "SEnemy" :
        return make_enemy(/* tuple */[
                    typ.Arg0,
                    dir
                  ]);
    case "SItem" :
        var param$2 = typ.Arg0;
        switch (param$2) {
          case "Mushroom" :
              return setup_sprite(undefined, /* tuple */[
                          2,
                          0
                        ], /* tuple */[
                          12,
                          16
                        ], "items.png", 1, 0, /* tuple */[
                          16,
                          16
                        ], /* tuple */[
                          0,
                          0
                        ]);
          case "FireFlower" :
              return setup_sprite(undefined, undefined, undefined, "items.png", 1, 0, /* tuple */[
                          16,
                          16
                        ], /* tuple */[
                          0,
                          188
                        ]);
          case "Star" :
              return setup_sprite(undefined, undefined, undefined, "items.png", 1, 0, /* tuple */[
                          16,
                          16
                        ], /* tuple */[
                          16,
                          48
                        ]);
          case "Coin" :
              return setup_sprite(undefined, /* tuple */[
                          3,
                          0
                        ], /* tuple */[
                          12,
                          16
                        ], "items.png", 3, 15, /* tuple */[
                          16,
                          16
                        ], /* tuple */[
                          0,
                          80
                        ]);
          
        }
    case "SBlock" :
        var param$3 = typ.Arg0;
        if (typeof param$3 === "string") {
          switch (param$3) {
            case "QBlockUsed" :
                return setup_sprite(undefined, undefined, undefined, "blocks.png", 1, 0, /* tuple */[
                            16,
                            16
                          ], /* tuple */[
                            0,
                            32
                          ]);
            case "Brick" :
                return setup_sprite(undefined, undefined, undefined, "blocks.png", 5, 10, /* tuple */[
                            16,
                            16
                          ], /* tuple */[
                            0,
                            0
                          ]);
            case "UnBBlock" :
                return setup_sprite(undefined, undefined, undefined, "blocks.png", 1, 0, /* tuple */[
                            16,
                            16
                          ], /* tuple */[
                            0,
                            48
                          ]);
            case "Cloud" :
                return setup_sprite(undefined, undefined, undefined, "blocks.png", 1, 0, /* tuple */[
                            16,
                            16
                          ], /* tuple */[
                            0,
                            64
                          ]);
            case "Panel" :
                return setup_sprite(undefined, undefined, undefined, "panel.png", 3, 15, /* tuple */[
                            26,
                            26
                          ], /* tuple */[
                            0,
                            0
                          ]);
            case "Ground" :
                return setup_sprite(undefined, undefined, undefined, "ground.png", 1, 0, /* tuple */[
                            16,
                            16
                          ], /* tuple */[
                            0,
                            32
                          ]);
            
          }
        } else {
          return setup_sprite(undefined, undefined, undefined, "blocks.png", 4, 15, /* tuple */[
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
          /* frame : record */[/* contents */0],
          /* ticks : record */[/* contents */0],
          /* img */img
        ];
}

function make(spawn, dir, context) {
  var params = make_type(spawn, dir);
  return make_from_params(params, context);
}

function make_bgd(context) {
  var params = setup_sprite(undefined, undefined, undefined, "bgd-1.png", 1, 0, /* tuple */[
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

var Sprite = {
  setup_sprite: setup_sprite,
  make: make,
  make_bgd: make_bgd,
  make_particle: make_particle$1,
  transform_enemy: transform_enemy,
  update_animation: update_animation
};

function pair_to_xy(pair) {
  return /* record */[
          /* x */pair[0],
          /* y */pair[1]
        ];
}

function make_type$1(typ, ctx) {
  switch (typ) {
    case "BrickChunkL" :
    case "BrickChunkR" :
        return /* record */[
                /* sprite */make_particle$1(typ, ctx),
                /* rot */0,
                /* lifetime */300
              ];
    default:
      return /* record */[
              /* sprite */make_particle$1(typ, ctx),
              /* rot */0,
              /* lifetime */30
            ];
  }
}

function make$1($staropt$star, $staropt$star$1, part_type, pos, ctx) {
  var vel = $staropt$star !== undefined ? $staropt$star : /* tuple */[
      0,
      0
    ];
  var acc = $staropt$star$1 !== undefined ? $staropt$star$1 : /* tuple */[
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
          /* kill */false,
          /* life */params[/* lifetime */2]
        ];
}

function make_score(score, pos, ctx) {
  var t = score >= 801 ? (
      score >= 2001 ? (
          score !== 4000 ? (
              score !== 8000 ? "Score100" : "Score8000"
            ) : "Score4000"
        ) : (
          score !== 1000 ? (
              score >= 2000 ? "Score2000" : "Score100"
            ) : "Score1000"
        )
    ) : (
      score >= 201 ? (
          score !== 400 ? (
              score >= 800 ? "Score800" : "Score100"
            ) : "Score400"
        ) : (
          score !== 100 && score >= 200 ? "Score200" : "Score100"
        )
    );
  return make$1(/* tuple */[
              0.5,
              -0.7
            ], undefined, t, pos, ctx);
}

function update_vel(part) {
  part[/* vel */3][/* x */0] = part[/* vel */3][/* x */0] + part[/* acc */4][/* x */0];
  part[/* vel */3][/* y */1] = part[/* vel */3][/* y */1] + part[/* acc */4][/* y */1];
  return /* () */0;
}

function $$process(part) {
  part[/* life */6] = part[/* life */6] - 1 | 0;
  if (part[/* life */6] === 0) {
    part[/* kill */5] = true;
  }
  update_vel(part);
  var part$1 = part;
  part$1[/* pos */2][/* x */0] = part$1[/* vel */3][/* x */0] + part$1[/* pos */2][/* x */0];
  part$1[/* pos */2][/* y */1] = part$1[/* vel */3][/* y */1] + part$1[/* pos */2][/* y */1];
  return /* () */0;
}

var Particle = {
  make: make$1,
  make_score: make_score,
  process: $$process
};

var id_counter = /* record */[/* contents */Pervasives.min_int];

function setup_obj($staropt$star, $staropt$star$1, param) {
  var has_gravity = $staropt$star !== undefined ? $staropt$star : true;
  var speed = $staropt$star$1 !== undefined ? $staropt$star$1 : 1;
  return /* record */[
          /* has_gravity */has_gravity,
          /* speed */speed
        ];
}

function set_vel_to_speed(obj) {
  var speed = obj[/* params */0][/* speed */1];
  var match = obj[/* dir */6];
  if (match !== "Left") {
    obj[/* vel */2][/* x */0] = speed;
    return /* () */0;
  } else {
    obj[/* vel */2][/* x */0] = -speed;
    return /* () */0;
  }
}

function make_type$2(param) {
  switch (/* XXX */param.tag) {
    case "SPlayer" :
        return setup_obj(undefined, 2.8, /* () */0);
    case "SEnemy" :
        var param$1 = param.Arg0;
        switch (param$1) {
          case "GKoopaShell" :
          case "RKoopaShell" :
              return setup_obj(undefined, 3, /* () */0);
          default:
            return setup_obj(undefined, undefined, /* () */0);
        }
    case "SItem" :
        var param$2 = param.Arg0;
        if (param$2 === "Coin") {
          return setup_obj(false, undefined, /* () */0);
        } else {
          return setup_obj(undefined, undefined, /* () */0);
        }
    case "SBlock" :
        param.Arg0;
        return setup_obj(false, undefined, /* () */0);
    
  }
}

function new_id(param) {
  id_counter[0] = id_counter[0] + 1 | 0;
  return id_counter[0];
}

function make$2($staropt$star, $staropt$star$1, spawnable, context, param) {
  var id = $staropt$star !== undefined ? Caml_option.valFromOption($staropt$star) : undefined;
  var dir = $staropt$star$1 !== undefined ? $staropt$star$1 : "Left";
  var spr = make(spawnable, dir, context);
  var params = make_type$2(spawnable);
  var id$1 = id !== undefined ? id : new_id(/* () */0);
  var obj = /* record */[
    /* params */params,
    /* pos : record */[
      /* x */param[0],
      /* y */param[1]
    ],
    /* vel : record */[
      /* x */0.0,
      /* y */0.0
    ],
    /* id */id$1,
    /* jumping */false,
    /* grounded */false,
    /* dir */dir,
    /* invuln */0,
    /* kill */false,
    /* health */1,
    /* crouch */false,
    /* score */0
  ];
  return /* tuple */[
          spr,
          obj
        ];
}

function spawn(spawnable, context, param) {
  var match = make$2(undefined, undefined, spawnable, context, /* tuple */[
        param[0],
        param[1]
      ]);
  var obj = match[1];
  var spr = match[0];
  switch (/* XXX */spawnable.tag) {
    case "SPlayer" :
        return /* constructor */{
                tag: "Player",
                Arg0: spawnable.Arg0,
                Arg1: spr,
                Arg2: obj
              };
    case "SEnemy" :
        set_vel_to_speed(obj);
        return /* constructor */{
                tag: "Enemy",
                Arg0: spawnable.Arg0,
                Arg1: spr,
                Arg2: obj
              };
    case "SItem" :
        return /* constructor */{
                tag: "Item",
                Arg0: spawnable.Arg0,
                Arg1: spr,
                Arg2: obj
              };
    case "SBlock" :
        return /* constructor */{
                tag: "Block",
                Arg0: spawnable.Arg0,
                Arg1: spr,
                Arg2: obj
              };
    
  }
}

function get_sprite(param) {
  return param.Arg1;
}

function get_obj(param) {
  return param.Arg2;
}

function is_player(param) {
  if (/* XXX */param.tag === "Player") {
    return true;
  } else {
    return false;
  }
}

function is_enemy(param) {
  if (/* XXX */param.tag === "Enemy") {
    return true;
  } else {
    return false;
  }
}

function equals(col1, col2) {
  return col1.Arg2[/* id */3] === col2.Arg2[/* id */3];
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
            case "CLeft" :
                if (player$1[/* crouch */10]) {
                  return 0;
                } else {
                  if (player$1[/* vel */2][/* x */0] > -player$1[/* params */0][/* speed */1]) {
                    player$1[/* vel */2][/* x */0] = player$1[/* vel */2][/* x */0] - (0.4 - lr_acc);
                  }
                  player$1[/* dir */6] = "Left";
                  return /* () */0;
                }
            case "CRight" :
                if (player$1[/* crouch */10]) {
                  return 0;
                } else {
                  if (player$1[/* vel */2][/* x */0] < player$1[/* params */0][/* speed */1]) {
                    player$1[/* vel */2][/* x */0] = player$1[/* vel */2][/* x */0] + (0.4 + lr_acc);
                  }
                  player$1[/* dir */6] = "Right";
                  return /* () */0;
                }
            case "CUp" :
                if (!player$1[/* jumping */4] && player$1[/* grounded */5]) {
                  player$1[/* jumping */4] = true;
                  player$1[/* grounded */5] = false;
                  player$1[/* vel */2][/* y */1] = Caml_primitive.caml_float_max(player$1[/* vel */2][/* y */1] - (5.7 + Math.abs(player$1[/* vel */2][/* x */0]) * 0.25), -6);
                  return /* () */0;
                } else {
                  return 0;
                }
            case "CDown" :
                if (!player$1[/* jumping */4] && player$1[/* grounded */5]) {
                  player$1[/* crouch */10] = true;
                  return /* () */0;
                } else {
                  return 0;
                }
            
          }
        }), keys);
  var v = player[/* vel */2][/* x */0] * 0.9;
  var vel_damped = Math.abs(v) < 0.1 ? 0 : v;
  player[/* vel */2][/* x */0] = vel_damped;
  var pl_typ = player[/* health */9] <= 1 ? "SmallM" : "BigM";
  if (!prev_jumping && player[/* jumping */4]) {
    return /* tuple */[
            pl_typ,
            make(/* constructor */{
                  tag: "SPlayer",
                  Arg0: pl_typ,
                  Arg1: "Jumping"
                }, player[/* dir */6], context)
          ];
  } else if (prev_dir !== player[/* dir */6] || prev_vx === 0 && Math.abs(player[/* vel */2][/* x */0]) > 0 && !player[/* jumping */4]) {
    return /* tuple */[
            pl_typ,
            make(/* constructor */{
                  tag: "SPlayer",
                  Arg0: pl_typ,
                  Arg1: "Running"
                }, player[/* dir */6], context)
          ];
  } else if (prev_dir !== player[/* dir */6] && player[/* jumping */4] && prev_jumping) {
    return /* tuple */[
            pl_typ,
            make(/* constructor */{
                  tag: "SPlayer",
                  Arg0: pl_typ,
                  Arg1: "Jumping"
                }, player[/* dir */6], context)
          ];
  } else if (player[/* vel */2][/* y */1] === 0 && player[/* crouch */10]) {
    return /* tuple */[
            pl_typ,
            make(/* constructor */{
                  tag: "SPlayer",
                  Arg0: pl_typ,
                  Arg1: "Crouching"
                }, player[/* dir */6], context)
          ];
  } else if (player[/* vel */2][/* y */1] === 0 && player[/* vel */2][/* x */0] === 0) {
    return /* tuple */[
            pl_typ,
            make(/* constructor */{
                  tag: "SPlayer",
                  Arg0: pl_typ,
                  Arg1: "Standing"
                }, player[/* dir */6], context)
          ];
  } else {
    return ;
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
    obj[/* kill */8] = true;
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
  var check_x = $staropt$star !== undefined ? $staropt$star : true;
  switch (dir) {
    case "North" :
        obj[/* vel */2][/* y */1] = -0.001;
        return /* () */0;
    case "South" :
        obj[/* vel */2][/* y */1] = 0;
        obj[/* grounded */5] = true;
        obj[/* jumping */4] = false;
        return /* () */0;
    case "East" :
    case "West" :
        break;
    
  }
  if (check_x) {
    obj[/* vel */2][/* x */0] = 0;
    return /* () */0;
  } else {
    return 0;
  }
}

function opposite_dir(dir) {
  if (dir !== "Left") {
    return "Left";
  } else {
    return "Right";
  }
}

function reverse_left_right(obj) {
  obj[/* vel */2][/* x */0] = -obj[/* vel */2][/* x */0];
  obj[/* dir */6] = opposite_dir(obj[/* dir */6]);
  return /* () */0;
}

function evolve_enemy(player_dir, typ, spr, obj, context) {
  switch (typ) {
    case "Goomba" :
        obj[/* kill */8] = true;
        return ;
    case "GKoopa" :
        var match = make$2(undefined, obj[/* dir */6], /* constructor */{
              tag: "SEnemy",
              Arg0: "GKoopaShell"
            }, context, /* tuple */[
              obj[/* pos */1][/* x */0],
              obj[/* pos */1][/* y */1]
            ]);
        var new_obj = match[1];
        var new_spr = match[0];
        normalize_pos(new_obj[/* pos */1], spr[/* params */0], new_spr[/* params */0]);
        return /* constructor */{
                tag: "Enemy",
                Arg0: "GKoopaShell",
                Arg1: new_spr,
                Arg2: new_obj
              };
    case "RKoopa" :
        var match$1 = make$2(undefined, obj[/* dir */6], /* constructor */{
              tag: "SEnemy",
              Arg0: "RKoopaShell"
            }, context, /* tuple */[
              obj[/* pos */1][/* x */0],
              obj[/* pos */1][/* y */1]
            ]);
        var new_obj$1 = match$1[1];
        var new_spr$1 = match$1[0];
        normalize_pos(new_obj$1[/* pos */1], spr[/* params */0], new_spr$1[/* params */0]);
        return /* constructor */{
                tag: "Enemy",
                Arg0: "RKoopaShell",
                Arg1: new_spr$1,
                Arg2: new_obj$1
              };
    case "GKoopaShell" :
    case "RKoopaShell" :
        break;
    
  }
  obj[/* dir */6] = player_dir;
  if (obj[/* vel */2][/* x */0] !== 0) {
    obj[/* vel */2][/* x */0] = 0;
  } else {
    set_vel_to_speed(obj);
  }
  return ;
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
    obj[/* kill */8] = true;
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
  var match = make$2(undefined, undefined, /* constructor */{
        tag: "SBlock",
        Arg0: "QBlockUsed"
      }, context, /* tuple */[
        obj[/* pos */1][/* x */0],
        obj[/* pos */1][/* y */1]
      ]);
  return /* constructor */{
          tag: "Block",
          Arg0: "QBlockUsed",
          Arg1: match[0],
          Arg2: match[1]
        };
}

function spawn_above(player_dir, obj, typ, context) {
  var item = spawn(/* constructor */{
        tag: "SItem",
        Arg0: typ
      }, context, /* tuple */[
        obj[/* pos */1][/* x */0],
        obj[/* pos */1][/* y */1]
      ]);
  var item_obj = item.Arg2;
  item_obj[/* pos */1][/* y */1] = item_obj[/* pos */1][/* y */1] - item.Arg1[/* params */0][/* frame_size */3][1];
  item_obj[/* dir */6] = opposite_dir(player_dir);
  set_vel_to_speed(item_obj);
  return item;
}

function get_aabb(obj) {
  var spr = obj.Arg1[/* params */0];
  var obj$1 = obj.Arg2;
  var match = spr[/* bbox_offset */5];
  var box = obj$1[/* pos */1][/* x */0] + match[0];
  var boy = obj$1[/* pos */1][/* y */1] + match[1];
  var match$1 = spr[/* bbox_size */6];
  var sy = match$1[1];
  var sx = match$1[0];
  return /* record */[
          /* center : record */[
            /* x */box + sx / 2,
            /* y */boy + sy / 2
          ],
          /* half : record */[
            /* x */sx / 2,
            /* y */sy / 2
          ]
        ];
}

function col_bypass(c1, c2) {
  var o1 = c1.Arg2;
  var o2 = c2.Arg2;
  var ctypes;
  switch (/* XXX */c1.tag) {
    case "Player" :
        ctypes = /* XXX */c2.tag === "Enemy" ? c1.Arg2[/* invuln */7] > 0 : false;
        break;
    case "Enemy" :
        ctypes = /* XXX */c2.tag === "Item" ? true : false;
        break;
    case "Item" :
        switch (/* XXX */c2.tag) {
          case "Enemy" :
          case "Item" :
              ctypes = true;
              break;
          case "Player" :
          case "Block" :
              ctypes = false;
              break;
          
        }
        break;
    case "Block" :
        ctypes = false;
        break;
    
  }
  if (o1[/* kill */8] || o2[/* kill */8]) {
    return true;
  } else {
    return ctypes;
  }
}

function check_collision(c1, c2) {
  var b1 = get_aabb(c1);
  var b2 = get_aabb(c2);
  var o1 = c1.Arg2;
  if (col_bypass(c1, c2)) {
    return ;
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
          return "North";
        } else {
          o1[/* pos */1][/* y */1] = o1[/* pos */1][/* y */1] - oy;
          return "South";
        }
      } else if (vx > 0) {
        o1[/* pos */1][/* x */0] = o1[/* pos */1][/* x */0] + ox;
        return "West";
      } else {
        o1[/* pos */1][/* x */0] = o1[/* pos */1][/* x */0] - ox;
        return "East";
      }
    } else {
      return ;
    }
  }
}

function kill(collid, ctx) {
  switch (/* XXX */collid.tag) {
    case "Player" :
        return "[]";
    case "Enemy" :
        var o = collid.Arg2;
        var pos_000 = o[/* pos */1][/* x */0];
        var pos_001 = o[/* pos */1][/* y */1];
        var pos = /* tuple */[
          pos_000,
          pos_001
        ];
        var score = o[/* score */11] > 0 ? /* constructor */({
              tag: "::",
              Arg0: make_score(o[/* score */11], pos, ctx),
              Arg1: "[]"
            }) : "[]";
        var remains;
        remains = collid.Arg0 === "Goomba" ? /* constructor */({
              tag: "::",
              Arg0: make$1(undefined, undefined, "GoombaSquish", pos, ctx),
              Arg1: "[]"
            }) : "[]";
        return Pervasives.$at(score, remains);
    case "Item" :
        var o$1 = collid.Arg2;
        if (collid.Arg0 === "Mushroom") {
          return /* constructor */{
                  tag: "::",
                  Arg0: make_score(o$1[/* score */11], /* tuple */[
                        o$1[/* pos */1][/* x */0],
                        o$1[/* pos */1][/* y */1]
                      ], ctx),
                  Arg1: "[]"
                };
        } else {
          return "[]";
        }
    case "Block" :
        var o$2 = collid.Arg2;
        var tmp = collid.Arg0;
        if (typeof tmp === "string" && tmp === "Brick") {
          var pos_000$1 = o$2[/* pos */1][/* x */0];
          var pos_001$1 = o$2[/* pos */1][/* y */1];
          var pos$1 = /* tuple */[
            pos_000$1,
            pos_001$1
          ];
          var p1 = make$1(/* tuple */[
                -5,
                -5
              ], /* tuple */[
                0,
                0.2
              ], "BrickChunkL", pos$1, ctx);
          var p2 = make$1(/* tuple */[
                -3,
                -4
              ], /* tuple */[
                0,
                0.2
              ], "BrickChunkL", pos$1, ctx);
          var p3 = make$1(/* tuple */[
                3,
                -4
              ], /* tuple */[
                0,
                0.2
              ], "BrickChunkR", pos$1, ctx);
          var p4 = make$1(/* tuple */[
                5,
                -5
              ], /* tuple */[
                0,
                0.2
              ], "BrickChunkR", pos$1, ctx);
          return /* constructor */{
                  tag: "::",
                  Arg0: p1,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: p2,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: p3,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: p4,
                        Arg1: "[]"
                      }
                    }
                  }
                };
        } else {
          return "[]";
        }
    
  }
}

var $$Object = {
  invuln: 60,
  dampen_jump: 4,
  get_sprite: get_sprite,
  get_obj: get_obj,
  spawn: spawn,
  equals: equals,
  is_player: is_player,
  is_enemy: is_enemy,
  normalize_origin: normalize_origin,
  normalize_pos: normalize_pos,
  kill: kill,
  process_obj: process_obj,
  update_player: update_player,
  check_collision: check_collision,
  evolve_enemy: evolve_enemy,
  evolve_block: evolve_block,
  dec_health: dec_health,
  rev_dir: rev_dir,
  reverse_left_right: reverse_left_right,
  collide_block: collide_block,
  spawn_above: spawn_above
};

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
  var score_string = String(score);
  var coin_string = String(coins);
  var context = canvas.getContext("2d");
  context.font = "10px 'Press Start 2P'";
  context.fillText("Score: " + score_string, canvas.width - 140, 18);
  context.fillText("Coins: " + coin_string, 120, 18);
  return /* () */0;
}

function fps(canvas, fps_val) {
  var fps_str = String(fps_val | 0);
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

var Draw = {
  render: render,
  clear_canvas: clear_canvas,
  draw_bgd: draw_bgd,
  render_bbox: render_bbox,
  fps: fps,
  hud: hud,
  game_win: game_win,
  game_loss: game_loss
};

function make$3(param, param$1) {
  return /* record */[
          /* pos : record */[
            /* x */0,
            /* y */0
          ],
          /* v_dim : record */[
            /* x */param[0],
            /* y */param[1]
          ],
          /* m_dim : record */[
            /* x */param$1[0],
            /* y */param$1[1]
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
    return y <= v_max_y;
  } else {
    return false;
  }
}

function out_of_viewport_below(v, y) {
  var v_max_y = v[/* pos */0][/* y */1] + v[/* v_dim */1][/* y */1];
  return y >= v_max_y;
}

function coord_to_viewport(viewport, coord) {
  return /* record */[
          /* x */coord[/* x */0] - viewport[/* pos */0][/* x */0],
          /* y */coord[/* y */1] - viewport[/* pos */0][/* y */1]
        ];
}

function update(vpt, ctr) {
  var new_x = calc_viewport_point(ctr[/* x */0], vpt[/* v_dim */1][/* x */0], vpt[/* m_dim */2][/* x */0]);
  var new_y = calc_viewport_point(ctr[/* y */1], vpt[/* v_dim */1][/* y */1], vpt[/* m_dim */2][/* y */1]);
  var pos = /* record */[
    /* x */new_x,
    /* y */new_y
  ];
  return /* record */[
          /* pos */pos,
          /* v_dim */vpt[/* v_dim */1],
          /* m_dim */vpt[/* m_dim */2]
        ];
}

var Viewport = {
  make: make$3,
  calc_viewport_point: calc_viewport_point,
  in_viewport: in_viewport,
  out_of_viewport_below: out_of_viewport_below,
  coord_to_viewport: coord_to_viewport,
  update: update
};

var pressed_keys = /* record */[
  /* left */false,
  /* right */false,
  /* up */false,
  /* down */false,
  /* bbox */0
];

var collid_objs = /* record */[/* contents */"[]"];

var particles = /* record */[/* contents */"[]"];

var last_time = /* record */[/* contents */0];

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
  var o1$3;
  switch (/* XXX */c1.tag) {
    case "Player" :
        var o1$4 = c1.Arg2;
        var s1$2 = c1.Arg1;
        switch (/* XXX */c2.tag) {
          case "Player" :
              return /* tuple */[
                      undefined,
                      undefined
                    ];
          case "Enemy" :
              var o2$3 = c2.Arg2;
              var s2$2 = c2.Arg1;
              var typ$1 = c2.Arg0;
              if (dir === "South") {
                s1 = s1$2;
                o1 = o1$4;
                typ = typ$1;
                s2 = s2$2;
                o2 = o2$3;
                exit = 1;
              } else {
                s1$1 = s1$2;
                o1$1 = o1$4;
                t2 = typ$1;
                s2$1 = s2$2;
                o2$1 = o2$3;
                exit = 2;
              }
              break;
          case "Item" :
              o1$2 = o1$4;
              t2$1 = c2.Arg0;
              o2$2 = c2.Arg2;
              exit = 3;
              break;
          case "Block" :
              var o2$4 = c2.Arg2;
              var t = c2.Arg0;
              if (dir === "North") {
                if (typeof t === "string") {
                  switch (t) {
                    case "Brick" :
                        if (c1.Arg0 === "BigM") {
                          collide_block(undefined, dir, o1$4);
                          dec_health(o2$4);
                          return /* tuple */[
                                  undefined,
                                  undefined
                                ];
                        } else {
                          collide_block(undefined, dir, o1$4);
                          return /* tuple */[
                                  undefined,
                                  undefined
                                ];
                        }
                    case "Panel" :
                        game_win(state[/* ctx */1]);
                        return /* tuple */[
                                undefined,
                                undefined
                              ];
                    default:
                      collide_block(undefined, dir, o1$4);
                      return /* tuple */[
                              undefined,
                              undefined
                            ];
                  }
                } else {
                  var updated_block = evolve_block(o2$4, context);
                  var spawned_item = spawn_above(o1$4[/* dir */6], o2$4, t.Arg0, context);
                  collide_block(undefined, dir, o1$4);
                  return /* tuple */[
                          spawned_item,
                          updated_block
                        ];
                }
              } else {
                if (typeof t === "string" && t === "Panel") {
                  game_win(state[/* ctx */1]);
                  return /* tuple */[
                          undefined,
                          undefined
                        ];
                }
                if (dir === "South") {
                  state[/* multiplier */6] = 1;
                  collide_block(undefined, dir, o1$4);
                  return /* tuple */[
                          undefined,
                          undefined
                        ];
                } else {
                  collide_block(undefined, dir, o1$4);
                  return /* tuple */[
                          undefined,
                          undefined
                        ];
                }
              }
              break;
          
        }
        break;
    case "Enemy" :
        var o1$5 = c1.Arg2;
        var s1$3 = c1.Arg1;
        var t1 = c1.Arg0;
        switch (/* XXX */c2.tag) {
          case "Player" :
              var o1$6 = c2.Arg2;
              var s1$4 = c2.Arg1;
              if (dir === "North") {
                s1 = s1$4;
                o1 = o1$6;
                typ = t1;
                s2 = s1$3;
                o2 = o1$5;
                exit = 1;
              } else {
                s1$1 = s1$4;
                o1$1 = o1$6;
                t2 = t1;
                s2$1 = s1$3;
                o2$1 = o1$5;
                exit = 2;
              }
              break;
          case "Enemy" :
              var t1$1 = t1;
              var s1$5 = s1$3;
              var o1$7 = o1$5;
              var t2$2 = c2.Arg0;
              var s2$3 = c2.Arg1;
              var o2$5 = c2.Arg2;
              var dir$1 = dir;
              var exit$1 = 0;
              switch (t1$1) {
                case "GKoopaShell" :
                    switch (t2$2) {
                      case "GKoopaShell" :
                      case "RKoopaShell" :
                          exit$1 = 1;
                          break;
                      default:
                        exit$1 = 2;
                    }
                    break;
                case "RKoopaShell" :
                    switch (t2$2) {
                      case "GKoopaShell" :
                      case "RKoopaShell" :
                          exit$1 = 1;
                          break;
                      default:
                        exit$1 = 2;
                    }
                    break;
                default:
                  switch (t2$2) {
                    case "GKoopaShell" :
                    case "RKoopaShell" :
                        exit$1 = 3;
                        break;
                    default:
                      switch (dir$1) {
                        case "North" :
                        case "South" :
                            return /* tuple */[
                                    undefined,
                                    undefined
                                  ];
                        case "East" :
                        case "West" :
                            break;
                        
                      }
                      rev_dir(o1$7, t1$1, s1$5);
                      rev_dir(o2$5, t2$2, s2$3);
                      return /* tuple */[
                              undefined,
                              undefined
                            ];
                  }
              }
              switch (exit$1) {
                case 1 :
                    dec_health(o1$7);
                    dec_health(o2$5);
                    return /* tuple */[
                            undefined,
                            undefined
                          ];
                case 2 :
                    if (o1$7[/* vel */2][/* x */0] === 0) {
                      rev_dir(o2$5, t2$2, s2$3);
                      return /* tuple */[
                              undefined,
                              undefined
                            ];
                    } else {
                      dec_health(o2$5);
                      return /* tuple */[
                              undefined,
                              undefined
                            ];
                    }
                case 3 :
                    if (o2$5[/* vel */2][/* x */0] === 0) {
                      rev_dir(o1$7, t1$1, s1$5);
                      return /* tuple */[
                              undefined,
                              undefined
                            ];
                    } else {
                      dec_health(o1$7);
                      return /* tuple */[
                              undefined,
                              undefined
                            ];
                    }
                
              }
          case "Item" :
              return /* tuple */[
                      undefined,
                      undefined
                    ];
          case "Block" :
              var o2$6 = c2.Arg2;
              var t2$3 = c2.Arg0;
              switch (dir) {
                case "North" :
                case "South" :
                    o1$3 = o1$5;
                    exit = 4;
                    break;
                case "East" :
                case "West" :
                    break;
                
              }
              switch (t1) {
                case "GKoopaShell" :
                case "RKoopaShell" :
                    break;
                default:
                  rev_dir(o1$5, t1, s1$3);
                  return /* tuple */[
                          undefined,
                          undefined
                        ];
              }
              if (typeof t2$3 === "string") {
                if (t2$3 === "Brick") {
                  dec_health(o2$6);
                  reverse_left_right(o1$5);
                  return /* tuple */[
                          undefined,
                          undefined
                        ];
                } else {
                  rev_dir(o1$5, t1, s1$3);
                  return /* tuple */[
                          undefined,
                          undefined
                        ];
                }
              } else {
                var updated_block$1 = evolve_block(o2$6, context);
                var spawned_item$1 = spawn_above(o1$5[/* dir */6], o2$6, t2$3.Arg0, context);
                rev_dir(o1$5, t1, s1$3);
                return /* tuple */[
                        updated_block$1,
                        spawned_item$1
                      ];
              }
              break;
          
        }
        break;
    case "Item" :
        var o2$7 = c1.Arg2;
        switch (/* XXX */c2.tag) {
          case "Player" :
              o1$2 = c2.Arg2;
              t2$1 = c1.Arg0;
              o2$2 = o2$7;
              exit = 3;
              break;
          case "Enemy" :
          case "Item" :
              return /* tuple */[
                      undefined,
                      undefined
                    ];
          case "Block" :
              switch (dir) {
                case "North" :
                case "South" :
                    o1$3 = o2$7;
                    exit = 4;
                    break;
                case "East" :
                case "West" :
                    reverse_left_right(o2$7);
                    return /* tuple */[
                            undefined,
                            undefined
                          ];
                
              }
              break;
          
        }
        break;
    case "Block" :
        return /* tuple */[
                undefined,
                undefined
              ];
    
  }
  switch (exit) {
    case 1 :
        var o1$8 = o1;
        var typ$2 = typ;
        var s2$4 = s2;
        var o2$8 = o2;
        var state$1 = state;
        var context$1 = context;
        o1$8[/* invuln */7] = 10;
        o1$8[/* jumping */4] = false;
        o1$8[/* grounded */5] = true;
        switch (typ$2) {
          case "GKoopaShell" :
          case "RKoopaShell" :
              break;
          default:
            dec_health(o2$8);
            o1$8[/* vel */2][/* y */1] = -4;
            if (state$1[/* multiplier */6] === 8) {
              update_score(state$1, 800);
              o2$8[/* score */11] = 800;
              return /* tuple */[
                      undefined,
                      evolve_enemy(o1$8[/* dir */6], typ$2, s2$4, o2$8, context$1)
                    ];
            } else {
              var score = Caml_int32.imul(100, state$1[/* multiplier */6]);
              update_score(state$1, score);
              o2$8[/* score */11] = score;
              state$1[/* multiplier */6] = (state$1[/* multiplier */6] << 1);
              return /* tuple */[
                      undefined,
                      evolve_enemy(o1$8[/* dir */6], typ$2, s2$4, o2$8, context$1)
                    ];
            }
        }
        var r2 = evolve_enemy(o1$8[/* dir */6], typ$2, s2$4, o2$8, context$1);
        o1$8[/* vel */2][/* y */1] = -4;
        o1$8[/* pos */1][/* y */1] = o1$8[/* pos */1][/* y */1] - 5;
        return /* tuple */[
                undefined,
                r2
              ];
    case 2 :
        var o1$9 = o1$1;
        var t2$4 = t2;
        var s2$5 = s2$1;
        var o2$9 = o2$1;
        var context$2 = context;
        switch (t2$4) {
          case "GKoopaShell" :
          case "RKoopaShell" :
              break;
          default:
            dec_health(o1$9);
            o1$9[/* invuln */7] = 60;
            return /* tuple */[
                    undefined,
                    undefined
                  ];
        }
        var r2$1 = o2$9[/* vel */2][/* x */0] === 0 ? evolve_enemy(o1$9[/* dir */6], t2$4, s2$5, o2$9, context$2) : (dec_health(o1$9), o1$9[/* invuln */7] = 60, undefined);
        return /* tuple */[
                undefined,
                r2$1
              ];
    case 3 :
        switch (t2$1) {
          case "Mushroom" :
              dec_health(o2$2);
              if (o1$2[/* health */9] !== 2) {
                o1$2[/* health */9] = o1$2[/* health */9] + 1 | 0;
              }
              o1$2[/* vel */2][/* x */0] = 0;
              o1$2[/* vel */2][/* y */1] = 0;
              update_score(state, 1000);
              o2$2[/* score */11] = 1000;
              return /* tuple */[
                      undefined,
                      undefined
                    ];
          case "FireFlower" :
          case "Star" :
              break;
          case "Coin" :
              state[/* coins */5] = state[/* coins */5] + 1 | 0;
              dec_health(o2$2);
              update_score(state, 100);
              return /* tuple */[
                      undefined,
                      undefined
                    ];
          
        }
        dec_health(o2$2);
        update_score(state, 1000);
        return /* tuple */[
                undefined,
                undefined
              ];
    case 4 :
        collide_block(undefined, dir, o1$3);
        return /* tuple */[
                undefined,
                undefined
              ];
    
  }
}

function broad_phase(collid, all_collids, state) {
  var obj = collid.Arg2;
  return List.filter((function (c) {
                  if (in_viewport(state[/* vpt */2], obj[/* pos */1]) || is_player(collid)) {
                    return true;
                  } else {
                    return out_of_viewport_below(state[/* vpt */2], obj[/* pos */1][/* y */1]);
                  }
                }))(all_collids);
}

function check_collisions(collid, all_collids, state) {
  if (/* XXX */collid.tag === "Block") {
    return "[]";
  } else {
    var broad = broad_phase(collid, all_collids, state);
    var c = collid;
    var cs = broad;
    var state$1 = state;
    var c$1 = c;
    var _cs = cs;
    var state$2 = state$1;
    var _acc = "[]";
    while(true) {
      var acc = _acc;
      var cs$1 = _cs;
      if (cs$1 !== "[]") {
        var h = cs$1.Arg0;
        var c_obj = c$1.Arg2;
        var new_objs;
        if (equals(c$1, h)) {
          new_objs = /* tuple */[
            undefined,
            undefined
          ];
        } else {
          var match = check_collision(c$1, h);
          new_objs = match !== undefined && h.Arg2[/* id */3] !== c_obj[/* id */3] ? process_collision(match, c$1, h, state$2) : /* tuple */[
              undefined,
              undefined
            ];
        }
        var match$1 = new_objs[0];
        var acc$1;
        if (match$1 !== undefined) {
          var match$2 = new_objs[1];
          var o = match$1;
          acc$1 = match$2 !== undefined ? /* constructor */({
                tag: "::",
                Arg0: o,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: match$2,
                  Arg1: acc
                }
              }) : /* constructor */({
                tag: "::",
                Arg0: o,
                Arg1: acc
              });
        } else {
          var match$3 = new_objs[1];
          acc$1 = match$3 !== undefined ? /* constructor */({
                tag: "::",
                Arg0: match$3,
                Arg1: acc
              }) : acc;
        }
        _acc = acc$1;
        _cs = cs$1.Arg1;
        continue ;
      } else {
        return acc;
      }
    };
  }
}

function update_collidable(state, collid, all_collids) {
  var obj = collid.Arg2;
  var spr = collid.Arg1;
  obj[/* invuln */7] = obj[/* invuln */7] > 0 ? obj[/* invuln */7] - 1 | 0 : 0;
  var viewport_filter = in_viewport(state[/* vpt */2], obj[/* pos */1]) || is_player(collid) || out_of_viewport_below(state[/* vpt */2], obj[/* pos */1][/* y */1]);
  if (!obj[/* kill */8] && viewport_filter) {
    obj[/* grounded */5] = false;
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
    return "[]";
  }
}

function translate_keys(param) {
  var ctrls = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      pressed_keys[/* left */0],
      "CLeft"
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        pressed_keys[/* right */1],
        "CRight"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          pressed_keys[/* up */2],
          "CUp"
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            pressed_keys[/* down */3],
            "CDown"
          ],
          Arg1: "[]"
        }
      }
    }
  };
  return List.fold_left((function (a, x) {
                if (x[0]) {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x[1],
                          Arg1: a
                        };
                } else {
                  return a;
                }
              }), "[]", ctrls);
}

function run_update_collid(state, collid, all_collids) {
  if (/* XXX */collid.tag === "Player") {
    var o = collid.Arg2;
    var keys = translate_keys(/* () */0);
    o[/* crouch */10] = false;
    var match = update_player(o, keys, state[/* ctx */1]);
    var player;
    if (match !== undefined) {
      var match$1 = match;
      var new_spr = match$1[1];
      normalize_pos(o[/* pos */1], collid.Arg1[/* params */0], new_spr[/* params */0]);
      player = /* constructor */{
        tag: "Player",
        Arg0: match$1[0],
        Arg1: new_spr,
        Arg2: o
      };
    } else {
      player = collid;
    }
    var evolved = update_collidable(state, player, all_collids);
    collid_objs[0] = Pervasives.$at(collid_objs[0], evolved);
    return player;
  } else {
    var obj = collid.Arg2;
    var evolved$1 = update_collidable(state, collid, all_collids);
    if (!obj[/* kill */8]) {
      collid_objs[0] = /* constructor */{
        tag: "::",
        Arg0: collid,
        Arg1: Pervasives.$at(collid_objs[0], evolved$1)
      };
    }
    var new_parts = obj[/* kill */8] ? kill(collid, state[/* ctx */1]) : "[]";
    particles[0] = Pervasives.$at(particles[0], new_parts);
    return collid;
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
    /* vpt */update(viewport, player.Arg2[/* pos */1]),
    /* map */map_dim[1],
    /* score */0,
    /* coins */0,
    /* multiplier */1,
    /* game_over */false
  ];
  state[/* ctx */1].scale(1, 1);
  var update_helper = function (time, state, player, objs, parts) {
    if (state[/* game_over */7] === true) {
      return game_win(state[/* ctx */1]);
    } else {
      collid_objs[0] = "[]";
      particles[0] = "[]";
      var fps$1 = calc_fps(last_time[0], time);
      last_time[0] = time;
      clear_canvas(canvas);
      var vpos_x_int = state[/* vpt */2][/* pos */0][/* x */0] / 5 | 0;
      var bgd_width = state[/* bgd */0][/* params */0][/* frame_size */3][0] | 0;
      draw_bgd(state[/* bgd */0], Caml_int32.mod_(vpos_x_int, bgd_width));
      var player$1 = run_update_collid(state, player, objs);
      if (player$1.Arg2[/* kill */8] === true) {
        return game_loss(state[/* ctx */1]);
      } else {
        var state$1 = /* record */[
          /* bgd */state[/* bgd */0],
          /* ctx */state[/* ctx */1],
          /* vpt */update(state[/* vpt */2], player$1.Arg2[/* pos */1]),
          /* map */state[/* map */3],
          /* score */state[/* score */4],
          /* coins */state[/* coins */5],
          /* multiplier */state[/* multiplier */6],
          /* game_over */state[/* game_over */7]
        ];
        List.iter((function (obj) {
                run_update_collid(state$1, obj, objs);
                return /* () */0;
              }), objs);
        List.iter((function (part) {
                var state$2 = state$1;
                var part$1 = part;
                $$process(part$1);
                var x = part$1[/* pos */2][/* x */0] - state$2[/* vpt */2][/* pos */0][/* x */0];
                var y = part$1[/* pos */2][/* y */1] - state$2[/* vpt */2][/* pos */0][/* y */1];
                render(part$1[/* params */0][/* sprite */0], /* tuple */[
                      x,
                      y
                    ]);
                if (part$1[/* kill */5]) {
                  return 0;
                } else {
                  particles[0] = /* constructor */{
                    tag: "::",
                    Arg0: part$1,
                    Arg1: particles[0]
                  };
                  return /* () */0;
                }
              }), parts);
        fps(canvas, fps$1);
        hud(canvas, state$1[/* score */4], state$1[/* coins */5]);
        requestAnimationFrame((function (t) {
                return update_helper(t, state$1, player$1, collid_objs[0], particles[0]);
              }));
        return /* () */0;
      }
    }
  };
  return update_helper(0, state, player, param[1], "[]");
}

function keydown(evt) {
  var match = evt.keyCode;
  if (match >= 41) {
    switch (match) {
      case 65 :
          pressed_keys[/* left */0] = true;
          break;
      case 66 :
          pressed_keys[/* bbox */4] = (pressed_keys[/* bbox */4] + 1 | 0) % 2;
          break;
      case 68 :
          pressed_keys[/* right */1] = true;
          break;
      case 83 :
          pressed_keys[/* down */3] = true;
          break;
      case 67 :
      case 69 :
      case 70 :
      case 71 :
      case 72 :
      case 73 :
      case 74 :
      case 75 :
      case 76 :
      case 77 :
      case 78 :
      case 79 :
      case 80 :
      case 81 :
      case 82 :
      case 84 :
      case 85 :
      case 86 :
          break;
      case 87 :
          pressed_keys[/* up */2] = true;
          break;
      default:
        
    }
  } else if (match >= 32) {
    switch (match - 32 | 0) {
      case 1 :
      case 2 :
      case 3 :
      case 4 :
          break;
      case 5 :
          pressed_keys[/* left */0] = true;
          break;
      case 0 :
      case 6 :
          pressed_keys[/* up */2] = true;
          break;
      case 7 :
          pressed_keys[/* right */1] = true;
          break;
      case 8 :
          pressed_keys[/* down */3] = true;
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
          pressed_keys[/* right */1] = false;
        }
      } else {
        pressed_keys[/* up */2] = false;
      }
    } else {
      pressed_keys[/* down */3] = false;
    }
  } else if (match >= 41) {
    if (match === 65) {
      pressed_keys[/* left */0] = false;
    }
    
  } else if (match >= 32) {
    switch (match - 32 | 0) {
      case 1 :
      case 2 :
      case 3 :
      case 4 :
          break;
      case 5 :
          pressed_keys[/* left */0] = false;
          break;
      case 0 :
      case 6 :
          pressed_keys[/* up */2] = false;
          break;
      case 7 :
          pressed_keys[/* right */1] = false;
          break;
      case 8 :
          pressed_keys[/* down */3] = false;
          break;
      
    }
  }
  return true;
}

var Director = {
  update_loop: update_loop,
  keydown: keydown,
  keyup: keyup
};

function mem_loc(checkloc, _loclist) {
  while(true) {
    var loclist = _loclist;
    if (loclist !== "[]") {
      if (Caml_obj.caml_equal(checkloc, loclist.Arg0[1])) {
        return true;
      } else {
        _loclist = loclist.Arg1;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function convert_list(lst) {
  if (lst !== "[]") {
    var h = lst.Arg0;
    return Pervasives.$at(/* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  h[0],
                  /* tuple */[
                    h[1][0] * 16,
                    h[1][1] * 16
                  ]
                ],
                Arg1: "[]"
              }, convert_list(lst.Arg1));
  } else {
    return "[]";
  }
}

function choose_enemy_typ(typ) {
  switch (typ) {
    case 0 :
        return "RKoopa";
    case 1 :
        return "GKoopa";
    case 2 :
        return "Goomba";
    default:
      throw [
            Caml_builtin_exceptions.failure,
            "Shouldn't reach here"
          ];
  }
}

function choose_sblock_typ(typ) {
  switch (typ) {
    case 0 :
        return "Brick";
    case 1 :
        return "UnBBlock";
    case 2 :
        return "Cloud";
    case 3 :
        return /* constructor */{
                tag: "QBlock",
                Arg0: "Mushroom"
              };
    case 4 :
        return "Ground";
    default:
      throw [
            Caml_builtin_exceptions.failure,
            "Shouldn't reach here"
          ];
  }
}

function avoid_overlap(_lst, currentLst) {
  while(true) {
    var lst = _lst;
    if (lst !== "[]") {
      var t = lst.Arg1;
      var h = lst.Arg0;
      if (mem_loc(h[1], currentLst)) {
        _lst = t;
        continue ;
      } else {
        return Pervasives.$at(/* constructor */{
                    tag: "::",
                    Arg0: h,
                    Arg1: "[]"
                  }, avoid_overlap(t, currentLst));
      }
    } else {
      return "[]";
    }
  };
}

function trim_edges(_lst, blockw, blockh) {
  while(true) {
    var lst = _lst;
    if (lst !== "[]") {
      var t = lst.Arg1;
      var h = lst.Arg0;
      var cx = h[1][0];
      var cy = h[1][1];
      var pixx = blockw * 16;
      var pixy = blockh * 16;
      if (cx < 128 || pixx - cx < 528 || cy === 0 || pixy - cy < 48) {
        _lst = t;
        continue ;
      } else {
        return Pervasives.$at(/* constructor */{
                    tag: "::",
                    Arg0: h,
                    Arg1: "[]"
                  }, trim_edges(t, blockw, blockh));
      }
    } else {
      return "[]";
    }
  };
}

function generate_clouds(cbx, cby, typ, num) {
  if (num === 0) {
    return "[]";
  } else {
    return Pervasives.$at(/* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  typ,
                  /* tuple */[
                    cbx,
                    cby
                  ]
                ],
                Arg1: "[]"
              }, generate_clouds(cbx + 1, cby, typ, num - 1 | 0));
  }
}

function generate_coins(_block_coord) {
  while(true) {
    var block_coord = _block_coord;
    var place_coin = Random.$$int(2);
    if (block_coord !== "[]") {
      var t = block_coord.Arg1;
      var h = block_coord.Arg0;
      if (place_coin === 0) {
        var xc = h[1][0];
        var yc = h[1][1];
        return Pervasives.$at(/* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      0,
                      /* tuple */[
                        xc,
                        yc - 16
                      ]
                    ],
                    Arg1: "[]"
                  }, generate_coins(t));
      } else {
        _block_coord = t;
        continue ;
      }
    } else {
      return "[]";
    }
  };
}

function choose_block_pattern(blockw, blockh, cbx, cby, prob) {
  if (cbx > blockw || cby > blockh) {
    return "[]";
  } else {
    var block_typ = Random.$$int(4);
    var stair_typ = Random.$$int(2);
    var life_block_chance = Random.$$int(5);
    var middle_block = life_block_chance === 0 ? 3 : stair_typ;
    switch (prob) {
      case 0 :
          if (blockw - cbx > 2) {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      stair_typ,
                      /* tuple */[
                        cbx,
                        cby
                      ]
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        middle_block,
                        /* tuple */[
                          cbx + 1,
                          cby
                        ]
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          stair_typ,
                          /* tuple */[
                            cbx + 2,
                            cby
                          ]
                        ],
                        Arg1: "[]"
                      }
                    }
                  };
          } else if (blockw - cbx > 1) {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      block_typ,
                      /* tuple */[
                        cbx,
                        cby
                      ]
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        block_typ,
                        /* tuple */[
                          cbx + 1,
                          cby
                        ]
                      ],
                      Arg1: "[]"
                    }
                  };
          } else {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      block_typ,
                      /* tuple */[
                        cbx,
                        cby
                      ]
                    ],
                    Arg1: "[]"
                  };
          }
      case 1 :
          var num_clouds = Random.$$int(5) + 5 | 0;
          if (cby < 5) {
            return generate_clouds(cbx, cby, 2, num_clouds);
          } else {
            return "[]";
          }
      case 2 :
          if (blockh - cby === 1) {
            var cbx$1 = cbx;
            var cby$1 = cby;
            var typ = stair_typ;
            var four = /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                typ,
                /* tuple */[
                  cbx$1,
                  cby$1
                ]
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  typ,
                  /* tuple */[
                    cbx$1 + 1,
                    cby$1
                  ]
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    typ,
                    /* tuple */[
                      cbx$1 + 2,
                      cby$1
                    ]
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      typ,
                      /* tuple */[
                        cbx$1 + 3,
                        cby$1
                      ]
                    ],
                    Arg1: "[]"
                  }
                }
              }
            };
            var three = /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                typ,
                /* tuple */[
                  cbx$1 + 1,
                  cby$1 - 1
                ]
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  typ,
                  /* tuple */[
                    cbx$1 + 2,
                    cby$1 - 1
                  ]
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    typ,
                    /* tuple */[
                      cbx$1 + 3,
                      cby$1 - 1
                    ]
                  ],
                  Arg1: "[]"
                }
              }
            };
            var two = /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                typ,
                /* tuple */[
                  cbx$1 + 2,
                  cby$1 - 2
                ]
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  typ,
                  /* tuple */[
                    cbx$1 + 3,
                    cby$1 - 2
                  ]
                ],
                Arg1: "[]"
              }
            };
            var one = /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                typ,
                /* tuple */[
                  cbx$1 + 3,
                  cby$1 - 3
                ]
              ],
              Arg1: "[]"
            };
            return Pervasives.$at(four, Pervasives.$at(three, Pervasives.$at(two, one)));
          } else {
            return "[]";
          }
      case 3 :
          if (stair_typ === 0 && blockh - cby > 3) {
            var cbx$2 = cbx;
            var cby$2 = cby;
            var typ$1 = stair_typ;
            var three$1 = /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                typ$1,
                /* tuple */[
                  cbx$2,
                  cby$2
                ]
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  typ$1,
                  /* tuple */[
                    cbx$2 + 1,
                    cby$2
                  ]
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    typ$1,
                    /* tuple */[
                      cbx$2 + 2,
                      cby$2
                    ]
                  ],
                  Arg1: "[]"
                }
              }
            };
            var two$1 = /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                typ$1,
                /* tuple */[
                  cbx$2 + 2,
                  cby$2 + 1
                ]
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  typ$1,
                  /* tuple */[
                    cbx$2 + 3,
                    cby$2 + 1
                  ]
                ],
                Arg1: "[]"
              }
            };
            var one$1 = /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                typ$1,
                /* tuple */[
                  cbx$2 + 5,
                  cby$2 + 2
                ]
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  typ$1,
                  /* tuple */[
                    cbx$2 + 6,
                    cby$2 + 2
                  ]
                ],
                Arg1: "[]"
              }
            };
            return Pervasives.$at(three$1, Pervasives.$at(two$1, one$1));
          } else if (blockh - cby > 2) {
            var cbx$3 = cbx;
            var cby$3 = cby;
            var typ$2 = stair_typ;
            var one$2 = /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                typ$2,
                /* tuple */[
                  cbx$3,
                  cby$3
                ]
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  typ$2,
                  /* tuple */[
                    cbx$3 + 1,
                    cby$3
                  ]
                ],
                Arg1: "[]"
              }
            };
            var two$2 = /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                typ$2,
                /* tuple */[
                  cbx$3 + 3,
                  cby$3 - 1
                ]
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  typ$2,
                  /* tuple */[
                    cbx$3 + 4,
                    cby$3 - 1
                  ]
                ],
                Arg1: "[]"
              }
            };
            var three$2 = /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                typ$2,
                /* tuple */[
                  cbx$3 + 4,
                  cby$3 - 2
                ]
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  typ$2,
                  /* tuple */[
                    cbx$3 + 5,
                    cby$3 - 2
                  ]
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    typ$2,
                    /* tuple */[
                      cbx$3 + 6,
                      cby$3 - 2
                    ]
                  ],
                  Arg1: "[]"
                }
              }
            };
            return Pervasives.$at(one$2, Pervasives.$at(two$2, three$2));
          } else {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      stair_typ,
                      /* tuple */[
                        cbx,
                        cby
                      ]
                    ],
                    Arg1: "[]"
                  };
          }
      case 4 :
          if (cby + 3 - blockh === 2) {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      stair_typ,
                      /* tuple */[
                        cbx,
                        cby
                      ]
                    ],
                    Arg1: "[]"
                  };
          } else if (cby + 3 - blockh === 1) {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      stair_typ,
                      /* tuple */[
                        cbx,
                        cby
                      ]
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        stair_typ,
                        /* tuple */[
                          cbx,
                          cby + 1
                        ]
                      ],
                      Arg1: "[]"
                    }
                  };
          } else {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      stair_typ,
                      /* tuple */[
                        cbx,
                        cby
                      ]
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        stair_typ,
                        /* tuple */[
                          cbx,
                          cby + 1
                        ]
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          stair_typ,
                          /* tuple */[
                            cbx,
                            cby + 2
                          ]
                        ],
                        Arg1: "[]"
                      }
                    }
                  };
          }
      case 5 :
          return /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    3,
                    /* tuple */[
                      cbx,
                      cby
                    ]
                  ],
                  Arg1: "[]"
                };
      default:
        throw [
              Caml_builtin_exceptions.failure,
              "Shouldn't reach here"
            ];
    }
  }
}

function generate_enemies(blockw, blockh, _cbx, _cby, acc) {
  while(true) {
    var cby = _cby;
    var cbx = _cbx;
    if (cbx > blockw - 32) {
      return "[]";
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
        var enemy = /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            prob,
            /* tuple */[
              cbx * 16,
              cby * 16
            ]
          ],
          Arg1: "[]"
        };
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
    if (block_coord !== "[]") {
      var t = block_coord.Arg1;
      var h = block_coord.Arg0;
      if (place_enemy === 0) {
        var xc = h[1][0];
        var yc = h[1][1];
        return Pervasives.$at(/* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      enemy_typ,
                      /* tuple */[
                        xc,
                        yc - 16
                      ]
                    ],
                    Arg1: "[]"
                  }, generate_block_enemies(t));
      } else {
        _block_coord = t;
        continue ;
      }
    } else {
      return "[]";
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
  return spawn(/* constructor */{
              tag: "SBlock",
              Arg0: "Panel"
            }, context, /* tuple */[
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
      var newacc = Pervasives.$at(acc, /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              4,
              /* tuple */[
                inc * 16,
                blockh * 16
              ]
            ],
            Arg1: "[]"
          });
      if (skip === 7 && blockw - inc > 32) {
        _inc = inc + 1;
        continue ;
      } else {
        _acc = newacc;
        _inc = inc + 1;
        continue ;
      }
    } else {
      var newacc$1 = Pervasives.$at(acc, /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              4,
              /* tuple */[
                inc * 16,
                blockh * 16
              ]
            ],
            Arg1: "[]"
          });
      _acc = newacc$1;
      _inc = inc + 1;
      continue ;
    }
  };
}

function convert_to_block_obj(lst, context) {
  if (lst !== "[]") {
    var h = lst.Arg0;
    var sblock_typ = choose_sblock_typ(h[0]);
    var ob = spawn(/* constructor */{
          tag: "SBlock",
          Arg0: sblock_typ
        }, context, h[1]);
    return Pervasives.$at(/* constructor */{
                tag: "::",
                Arg0: ob,
                Arg1: "[]"
              }, convert_to_block_obj(lst.Arg1, context));
  } else {
    return "[]";
  }
}

function convert_to_enemy_obj(lst, context) {
  if (lst !== "[]") {
    var h = lst.Arg0;
    var senemy_typ = choose_enemy_typ(h[0]);
    var ob = spawn(/* constructor */{
          tag: "SEnemy",
          Arg0: senemy_typ
        }, context, h[1]);
    return Pervasives.$at(/* constructor */{
                tag: "::",
                Arg0: ob,
                Arg1: "[]"
              }, convert_to_enemy_obj(lst.Arg1, context));
  } else {
    return "[]";
  }
}

function convert_to_coin_obj(lst, context) {
  if (lst !== "[]") {
    var ob = spawn(/* constructor */{
          tag: "SItem",
          Arg0: "Coin"
        }, context, lst.Arg0[1]);
    return Pervasives.$at(/* constructor */{
                tag: "::",
                Arg0: ob,
                Arg1: "[]"
              }, convert_to_coin_obj(lst.Arg1, context));
  } else {
    return "[]";
  }
}

function generate_helper(blockw, blockh, cx, cy, context) {
  var block_locs = generate_block_locs(blockw, blockh, 0, 0, "[]");
  var converted_block_locs = trim_edges(convert_list(block_locs), blockw, blockh);
  var obj_converted_block_locs = convert_to_block_obj(converted_block_locs, context);
  var ground_blocks = generate_ground(blockw, blockh, 0, "[]");
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
  return Pervasives.$at(all_blocks, Pervasives.$at(obj_converted_enemies, Pervasives.$at(coin_objects, Pervasives.$at(obj_enemy_blocks, /* constructor */{
                          tag: "::",
                          Arg0: obj_panel,
                          Arg1: "[]"
                        }))));
}

function generate(w, h, context) {
  var blockw = w / 16;
  var blockh = h / 16 - 1;
  var collide_list = generate_helper(blockw, blockh, 0, 0, context);
  var player = spawn(/* constructor */{
        tag: "SPlayer",
        Arg0: "SmallM",
        Arg1: "Standing"
      }, context, /* tuple */[
        100,
        224
      ]);
  return /* tuple */[
          player,
          collide_list
        ];
}

function init(param) {
  return Random.self_init(/* () */0);
}

var Procedural_generator = {
  init: init,
  generate: generate
};

var loadCount = /* record */[/* contents */0];

function load(param) {
  Random.self_init(/* () */0);
  var canvas_id = "canvas";
  var match = document.getElementById(canvas_id);
  var canvas;
  if (match !== null) {
    canvas = match;
  } else {
    Curry._1(Printf.printf(/* constructor */{
              tag: "Format",
              Arg0: /* constructor */{
                tag: "String_literal",
                Arg0: "cant find canvas ",
                Arg1: /* constructor */{
                  tag: "String",
                  Arg0: "No_padding",
                  Arg1: /* constructor */{
                    tag: "String_literal",
                    Arg0: " \n",
                    Arg1: "End_of_format"
                  }
                }
              },
              Arg1: "cant find canvas %s \n"
            }), canvas_id);
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

function inc_counter(param) {
  loadCount[0] = loadCount[0] + 1 | 0;
  if (loadCount[0] === 4) {
    return load(/* () */0);
  } else {
    return /* () */0;
  }
}

function preload(param) {
  return List.map((function (img_src) {
                var img_src$1 = "sprites/" + img_src;
                var img = document.createElement("img");
                img.src = img_src$1;
                img.addEventListener("load", (function (ev) {
                        inc_counter(/* () */0);
                        return true;
                      }), true);
                return /* () */0;
              }), /* constructor */{
              tag: "::",
              Arg0: "blocks.png",
              Arg1: /* constructor */{
                tag: "::",
                Arg0: "items.png",
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: "enemies.png",
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: "mario-small.png",
                    Arg1: "[]"
                  }
                }
              }
            });
}

window.onload = (function (param) {
    preload(/* () */0);
    return true;
  });

var Main = {
  Html: 0,
  Pg: 0,
  loadCount: loadCount,
  imgsToLoad: 4,
  level_width: 2400,
  level_height: 256,
  load: load,
  inc_counter: inc_counter,
  preload: preload
};

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
