[@@@warning "-a"]

module Actors : sig 
#1 "actors.mli"
type dir_1d = | Left | Right 
type dir_2d = | North | South | East | West

(* Generic xy record for easy position access *)
type xy = {
  mutable x: float;
  mutable y: float;
}

(* Controls correspond to keyboard input *)
type controls =
  | CLeft
  | CRight
  | CUp
  | CDown

(* Player ability type *)
type pl_typ =
  | BigM
  | SmallM

type item_typ =
  | Mushroom
  | FireFlower
  | Star
  | Coin

type enemy_typ =
  | Goomba
  | GKoopa
  | RKoopa
  | GKoopaShell
  | RKoopaShell

type block_typ =
  | QBlock of item_typ
  | QBlockUsed
  | Brick
  | UnBBlock
  | Cloud
  | Panel
  | Ground

(* Player action type *)
type player_typ =
  | Standing
  | Jumping
  | Running
  | Crouching

(* Particle Type *)
type part_typ =
  | GoombaSquish
  | BrickChunkL
  | BrickChunkR
  | Score100
  | Score200
  | Score400
  | Score800
  | Score1000
  | Score2000
  | Score4000
  | Score8000

(*type unbblock_typ =
  | Wood
  | Earth
  | Brick
| *)

type spawn_typ =
  | SPlayer of pl_typ * player_typ
  | SEnemy of enemy_typ
  | SItem of item_typ
  | SBlock of block_typ
  (*| SGround of ground_typ*)


end = struct
#1 "actors.ml"
type dir_1d = | Left | Right
type dir_2d = | North | South | East | West

type xy = {
  mutable x: float;
  mutable y: float;
}

type controls =
  | CLeft
  | CRight
  | CUp
  | CDown

type pl_typ =
  | BigM
  | SmallM

type item_typ =
  | Mushroom
  | FireFlower
  | Star
  | Coin

type enemy_typ =
  | Goomba
  | GKoopa
  | RKoopa
  | GKoopaShell
  | RKoopaShell

type block_typ =
  | QBlock of item_typ
  | QBlockUsed
  | Brick
  | UnBBlock
  | Cloud
  | Panel
  | Ground

type player_typ =
  | Standing
  | Jumping
  | Running
  | Crouching

type part_typ =
  | GoombaSquish
  | BrickChunkL
  | BrickChunkR
  | Score100
  | Score200
  | Score400
  | Score800
  | Score1000
  | Score2000
  | Score4000
  | Score8000

type spawn_typ =
  | SPlayer of pl_typ * player_typ
  | SEnemy of enemy_typ
  | SItem of item_typ
  | SBlock of block_typ
end
module Dom_html
= struct
#1 "dom_html.ml"
type imageElement
type canvasRenderingContext2D
type canvasElement

external document: Dom.document = "" [@@bs.val]
external window: Dom.window = "" [@@bs.val]

(* external createImg: (_ [@bs.as "img"]) -> document -> imageElement = "createElement" [@@bs.send] *)
external createImg: Dom.document -> (_ [@bs.as "img"]) -> imageElement = "createElement" [@@bs.send]
external requestAnimationFrame : (float -> unit) -> unit = ""[@@bs.val ]
external getElementById : Dom.document -> string -> Dom.element option = ""[@@bs.return null_to_opt][@@bs.send]
external addEventListener : Dom.document -> string -> ('a Dom.event_like -> bool) -> bool -> unit = "" [@@bs.send]
external addEventListenerImg : imageElement -> string -> ('a Dom.event_like -> bool) -> bool -> unit = "addEventListener" [@@bs.send]

(* unsafe casts *)
external imageElementToJsObj : imageElement -> < .. > Js.t = "%identity"
external canvasRenderingContext2DToJsObj : canvasRenderingContext2D -> < .. > Js.t = "%identity"
external canvasElementToJsObj : canvasElement -> < .. > Js.t = "%identity"
external keyboardEventToJsObj : Dom.keyboardEvent -> < .. > Js.t = "%identity"
external elementToCanvasElement : Dom.element -> canvasElement = "%identity"
external windowToJsObj : Dom.window -> < .. > Js.t = "%identity"


end
module Sprite : sig 
#1 "sprite.mli"
open Actors

(* Represents an xy vector *)
type xy = float * float (* x, y *)

(* Inherent sprite parameters from which to create the sprite *)
type sprite_params =
  {
    max_frames: int;
    max_ticks: int;
    img_src: string;
    frame_size: xy;
    src_offset: xy;
    bbox_offset: xy;
    bbox_size: xy;
    loop: bool;
  }

(* Concrete sprite created to visually represent an object *)
type sprite =
  {
    mutable params: sprite_params;
    context: Dom_html.canvasRenderingContext2D;
    frame: int ref;
    ticks: int ref;
    mutable img: Dom_html.imageElement;
  }


(* Sets up a sprite to create *)
val setup_sprite : ?loop:bool -> ?bb_off:float*float-> ?bb_sz:float*float
        -> string -> int -> int -> xy -> xy
                          -> sprite_params

(* Creates a sprite given the actor type *)
val make : Actors.spawn_typ -> Actors.dir_1d
   -> Dom_html.canvasRenderingContext2D
   -> sprite

(* Make a background *)
val make_bgd : Dom_html.canvasRenderingContext2D  -> sprite

(* Make a particle corresponding to the given type *)
val make_particle : Actors.part_typ
    -> Dom_html.canvasRenderingContext2D -> sprite

(* Transform an enemy sprite based on direction *)
val transform_enemy : Actors.enemy_typ -> sprite -> Actors.dir_1d -> unit

(* Updates the sprite's animation *)
val update_animation : sprite -> unit


end = struct
#1 "sprite.ml"
open Actors

type xy = float * float

type sprite_params =
  {
    max_frames: int;
    max_ticks: int;
    img_src: string;
    frame_size: xy;
    src_offset: xy;
    bbox_offset: xy;
    bbox_size: xy;
    loop: bool;
  }

type sprite =
  {
    mutable params: sprite_params;
    context: Dom_html.canvasRenderingContext2D;
    frame: int ref;
    ticks: int ref;
    mutable img: Dom_html.imageElement;
  }

(*setup_sprite is used to initialize a sprite.*)
let setup_sprite  ?loop:(loop=true) ?bb_off:(bbox_offset=(0.,0.))
          ?bb_sz:(bbox_size=(0.,0.))
                 img_src max_frames max_ticks frame_size src_offset =
  let bbox_size = if bbox_size = (0.,0.) then frame_size else bbox_size in
  let img_src = "./sprites/" ^ img_src in
  {
    img_src;
    max_frames;
    max_ticks;
    frame_size;
    src_offset;
    bbox_offset;
    bbox_size;
    loop;
  }

(*The following functions are used in order to define sprite animations
 *from their sprite sheets. Also creates bounding boxes if necessary.*)

(*Sets sprite for small mario.*)
let make_small_player (typ, dir) =
  match dir with
    (* 16x16 grid with 0x0 offset*)
    | Left -> begin match typ with
      | Standing -> setup_sprite "mario-small.png" ~bb_off:(3.,1.) ~bb_sz:(11.,15.) 1 0 (16.,16.) (0.,0.)
      | Jumping -> setup_sprite "mario-small.png" ~bb_off:(2.,1.) ~bb_sz:(13.,15.) 2 10 (16.,16.) (16.,16.)
      | Running -> setup_sprite "mario-small.png" ~bb_off:(2.,1.) ~bb_sz:(12.,15.) 3 5 (16.,16.) (16.,0.)
      | Crouching -> setup_sprite "mario-small.png" ~bb_off:(1.,5.) ~bb_sz:(14.,10.) 1 0 (16.,16.) (0.,64.)
      end
    | Right -> begin match typ with
      | Standing -> setup_sprite "mario-small.png" ~bb_off:(1.,1.) ~bb_sz:(11.,15.) 1 0 (16.,16.) (0.,32.)
      | Jumping -> setup_sprite "mario-small.png" ~bb_off:(2.,1.) ~bb_sz:(13.,15.) 2 10 (16.,16.) (16.,48.)
      | Running -> setup_sprite "mario-small.png" ~bb_off:(2.,1.) ~bb_sz:(12.,15.) 3 5 (16.,16.) (16.,32.)
      | Crouching -> setup_sprite "mario-small.png" ~bb_off:(1.,5.) ~bb_sz:(14.,10.) 1 0 (16.,16.) (0.,64.)
      end

(*Sets sprite for big mario.*)
let make_big_player (typ, dir) =
  match dir with
  | Left -> begin match typ with
    | Standing -> setup_sprite "mario-big.png" 1 0 ~bb_off:(2.,1.) ~bb_sz:(13.,25.) (16.,27.) (16.,5.)
    | Jumping -> setup_sprite "mario-big.png" 1 0 ~bb_off:(2.,1.) ~bb_sz:(12.,25.) (16.,26.) (48.,6.)
    | Running -> setup_sprite "mario-big.png" 4 10 ~bb_off:(2.,1.) ~bb_sz:(13.,25.) (16.,27.)(0.,37.)
    | Crouching -> setup_sprite "mario-big.png" 1 0 ~bb_off:(2.,10.) ~bb_sz:(13.,17.) (16.,27.) (32.,5.)
    end
  | Right -> begin match typ with
    | Standing -> setup_sprite "mario-big.png" 1 0 ~bb_off:(1.,1.) ~bb_sz:(13.,25.) (16.,26.) (16.,69.)
    | Jumping -> setup_sprite "mario-big.png" 1 0 ~bb_off:(2.,1.) ~bb_sz:(12.,25.) (16.,26.) (48.,70.)
    | Running -> setup_sprite "mario-big.png" 4 10 ~bb_off:(2.,1.) ~bb_sz:(13.,25.) (16.,27.) (0.,101.)
    | Crouching -> setup_sprite "mario-big.png" 1 0 ~bb_off:(2.,10.) ~bb_sz:(13.,17.) (16.,27.) (32.,69.)
    end

(*Sets sprites for enemies: Goomba, Red Koopa, Green Koopa.*)
let make_enemy (typ, dir) =
  match (typ, dir) with
      | (Goomba,_) -> setup_sprite "enemies.png" ~bb_off:(1.,1.) ~bb_sz:(14.,14.) 2 10 (16.,16.) (0.,128.)
      | (GKoopa,Left) -> setup_sprite "enemies.png" ~bb_off:(4.,10.) ~bb_sz:(11.,16.) 2 10 (16.,27.) (0.,69.)
      | (GKoopa,Right) -> setup_sprite "enemies.png" ~bb_off:(1.,10.) ~bb_sz:(11.,16.) 2 10 (16.,27.) (32.,69.)
      | (RKoopa,Left) -> setup_sprite "enemies.png" ~bb_off:(4.,10.) ~bb_sz:(11.,16.) 2 10 (16.,27.) (0.,5.)
      | (RKoopa,Right) -> setup_sprite "enemies.png" ~bb_off:(1.,10.) ~bb_sz:(11.,16.) 2 10 (16.,27.) (32.,5.)
      | (GKoopaShell,_) -> setup_sprite "enemies.png" ~bb_off:(2.,2.) ~bb_sz:(12.,13.) 4 10 (16.,16.) (0.,96.)
      | (RKoopaShell,_) -> setup_sprite "enemies.png" ~bb_off:(2.,2.) ~bb_sz:(12.,13.) 4 10 (16.,16.) (0.,32.)

(*Sets sprites for items: coin, fireflower, mushroom, star.*)
let make_item = function
  (* 16x16 grid with 0x0 offset *)
  | Coin -> setup_sprite "items.png" ~bb_off:(3.,0.) ~bb_sz:(12.,16.) 3 15 (16.,16.) (0.,80.)
  | FireFlower -> setup_sprite "items.png" 1 0 (16.,16.) (0.,188.)
  | Mushroom -> setup_sprite "items.png" ~bb_off:(2.,0.) ~bb_sz: (12.,16.) 1 0 (16.,16.) (0.,0.)
  | Star -> setup_sprite "items.png" 1 0 (16.,16.) (16.,48.)

(*Sets sprites for blocks: brick, question block, unbreakable block, cloud block
* panel block, ground block.*)
let make_block = function
  (* 16x16 grid with 0x0 offset *)
  | Brick -> setup_sprite "blocks.png" 5 10 (16.,16.) (0.,0.)
  | QBlock _ -> setup_sprite "blocks.png" 4 15 (16.,16.) (0.,16.)
  | QBlockUsed -> setup_sprite "blocks.png" 1 0 (16.,16.) (0.,32.)
  | UnBBlock -> setup_sprite "blocks.png" 1 0 (16.,16.) (0.,48.)
  | Cloud -> setup_sprite "blocks.png" 1 0 (16., 16.) (0., 64.)
  | Panel -> setup_sprite "panel.png" 3 15 (26., 26.) (0., 0.)
  | Ground -> setup_sprite "ground.png" 1 0 (16., 16.) (0., 32.)

(*Sets sprites for particles, squished goomba, brick chunks (upon destruction
* of brick), score text.*)
let make_particle = function
  | GoombaSquish -> setup_sprite "enemies.png" 1 0 (16.,16.) (0.,144.)
  | BrickChunkL -> setup_sprite "chunks.png" 1 0 (8.,8.) (0.,0.)
  | BrickChunkR -> setup_sprite "chunks.png" 1 0 (8.,8.) (8.,0.)
  | Score100 -> setup_sprite "score.png" 1 0 (12.,8.) (0.,0.)
  | Score200 -> setup_sprite "score.png" 1 0 (12.,9.) (0.,9.)
  | Score400 -> setup_sprite "score.png" 1 0 (12.,9.) (0.,18.)
  | Score800 -> setup_sprite "score.png" 1 0 (12.,9.) (0.,27.)
  | Score1000 -> setup_sprite "score.png" 1 0 (14.,9.) (13.,0.)
  | Score2000 -> setup_sprite "score.png" 1 0 (14.,9.) (13.,9.)
  | Score4000 -> setup_sprite "score.png" 1 0 (14.,9.) (13.,18.)
  | Score8000 -> setup_sprite "score.png" 1 0 (14.,9.) (13.,27.)

(*Calls to set sprite for either big or small mario.*)
let make_player pt spr_type =
  match pt with
  | BigM -> make_big_player spr_type
  | SmallM -> make_small_player spr_type

(*Calls to set sprites for each type of object.*)
let make_type typ (dir : Actors.dir_1d) =
  match typ with
  | SPlayer(pt,st) -> make_player pt (st,dir)
  | SEnemy t -> make_enemy (t,dir)
  | SItem t -> make_item t
  | SBlock t -> make_block t

(* Makes a sprite from provided [params]. *)
let make_from_params params context =
  let img = (Dom_html.createImg Dom_html.document) in
  (Dom_html.imageElementToJsObj img)##src #= (params.img_src) ;
  {
    params;
    context;
    img;
    frame = ref 0;
    ticks = ref 0;
  }

(*Make is the wrapper function to cycle through sprite animations*)
let make spawn dir context  =
  let params = make_type spawn dir in
  make_from_params params context

(* Make a background *)
let make_bgd context =
  let params = setup_sprite "bgd-1.png" 1 0 (512.,256.) (0.,0.) in
  make_from_params params context

(* Make a particle from the given particle type *)
let make_particle ptyp context =
  let params = make_particle ptyp in
  make_from_params params context

(*Transform_enemy is used in order to switch the direction an enemy faces.*)
let transform_enemy enemy_typ spr dir =
  let params = make_enemy  (enemy_typ,dir) in
  let img = (Dom_html.createImg Dom_html.document) in
  (Dom_html.imageElementToJsObj img)##src #= (params.img_src) ;
  spr.params <- params;
  spr.img <- img

(*update_animation is the main method to cycle through sprite animations*)
let update_animation (spr: sprite) =
  (* Only advance frame when ticked *)
  let curr_ticks = !(spr.ticks) in
  if curr_ticks >= spr.params.max_ticks then begin
    spr.ticks := 0;
    if spr.params.loop then
    spr.frame := (!(spr.frame) + 1) mod spr.params.max_frames
  end else spr.ticks := curr_ticks + 1

end
module Particle : sig 
#1 "particle.mli"
open Actors
open Sprite

(* Template params associated with a particle *)
type part_params = {
  sprite: Sprite.sprite;  (* Backing sprite *)
  rot: float;             (* Rotation *)
  lifetime: int;          (* Life span *)
}

type particle = {
  params: part_params;
  part_type: Actors.part_typ;
  pos:  Actors.xy;
  vel:  Actors.xy;
  acc:  Actors.xy;
  mutable kill: bool;     (* Kill the particle in the next frame *)
  mutable life: int;      (* Remaining lifespan of particle *)
}

(* Makes a new particle of the given particle type with at a position. *)
val make : ?vel:float*float -> ?acc:float*float -> Actors.part_typ
    -> float*float -> Dom_html.canvasRenderingContext2D -> particle

(* Make a score particle. The first int indicates the score to spawn *)
val make_score : int -> float*float -> Dom_html.canvasRenderingContext2D
          -> particle

(* Process a particle, updating its velocity and position. Also marks it as
 * killable if it exceeds its lifespan *)
val process : particle -> unit

end = struct
#1 "particle.ml"
open Actors
open Sprite

type part_params = {
  sprite: Sprite.sprite;
  rot: float;
  lifetime: int;
}

type particle = {
  params: part_params;
  part_type: Actors.part_typ;
  pos:  Actors.xy;
  vel:  Actors.xy;
  acc:  Actors.xy;
  mutable kill: bool;
  mutable life: int;
}

(* Converts an x,y [pair] to an Actors.xy record *)
let pair_to_xy pair = {
  x = fst pair;
  y = snd pair;
}

(* Function wrapper to assist in generating the template paramss for a
 * particle. *)
let make_params sprite rot lifetime = 
  {
    sprite;
    rot;
    lifetime;
  }

(* Generate the template for a specific particle type *)
let make_type typ ctx =
  match typ with
  | GoombaSquish as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | BrickChunkL as t -> make_params (Sprite.make_particle t ctx) 0. 300
  | BrickChunkR as t -> make_params (Sprite.make_particle t ctx) 0. 300
  | Score100 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score200 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score400 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score800 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score1000 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score2000 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score4000 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score8000 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  
let make ?vel:(vel=(0.,0.)) ?acc:(acc=(0.,0.)) part_type pos ctx =
  let params = make_type part_type ctx in
  let pos = pair_to_xy pos and vel = pair_to_xy vel 
                           and acc = pair_to_xy acc in
  {
    params;
    part_type;
    pos;
    vel;
    acc;
    kill = false;
    life = params.lifetime;
  }
   
let make_score score pos ctx = 
  let t = match score with
  | 100 -> Score100
  | 200 -> Score200
  | 400 -> Score400
  | 800 -> Score800
  | 1000 -> Score1000
  | 2000 -> Score2000
  | 4000 -> Score4000
  | 8000 -> Score8000
  | _ -> Score100
  in make ~vel:(0.5,-0.7) t pos ctx

(* Mutably update the velocity of a particle *)
let update_vel part =
  part.vel.x <- (part.vel.x +. part.acc.x);
  part.vel.y <- (part.vel.y +. part.acc.y) 

(* Mutably update the position of a particle *)
let update_pos part =
  part.pos.x <- (part.vel.x +. part.pos.x);
  part.pos.y <- (part.vel.y +. part.pos.y)
  
let process part =
  part.life <- part.life - 1;
  if part.life = 0 then (part.kill <- true);
  update_vel part;
  update_pos part 

end
module Object : sig 
#1 "object.mli"
open Sprite
open Actors
open Particle

val invuln : int (* # of frames of invulnerability *)
val dampen_jump : float (* Boost to jump when enemy jumped on *)

type aabb = {
  center: xy;
  half: xy;
}

type obj_params = {
  has_gravity: bool;
  speed: float;
}
type obj = {
  params: obj_params;
  pos: xy;
  vel: xy;
  id: int;
  mutable jumping: bool;
  mutable grounded: bool;
  mutable dir: Actors.dir_1d;
  mutable invuln: int;
  mutable kill: bool;
  mutable health: int;
  mutable crouch: bool;
  mutable score: int;
}

type collidable =
  | Player of pl_typ * sprite * obj
  | Enemy of enemy_typ * sprite * obj
  | Item of item_typ * sprite * obj
  | Block of block_typ * sprite * obj


(* Returns the sprite associated with the object *)
val get_sprite : collidable -> Sprite.sprite

val get_obj : collidable -> obj

(* Creates a new object with a given
 * actor type on the the canvas at a given position *)
val spawn : Actors.spawn_typ  -> Dom_html.canvasRenderingContext2D
          -> float*float -> collidable

val equals : collidable -> collidable -> bool

val is_player : collidable -> bool
val is_enemy : collidable -> bool

val normalize_origin : xy -> Sprite.sprite -> unit

val normalize_pos : xy -> Sprite.sprite_params -> Sprite.sprite_params -> unit

(* Destroys the object, returning a list of destruction effect objects *)
val kill : collidable -> Dom_html.canvasRenderingContext2D
                      ->  particle list

val process_obj : obj -> float -> unit

val update_player : obj -> Actors.controls list
                        -> Dom_html.canvasRenderingContext2D
                        -> (pl_typ * sprite) option

(* Checks whether a collision occured between two objects, returning the
 * direction of the collision if one occurred. *)
val check_collision : collidable -> collidable -> Actors.dir_2d option

val evolve_enemy : Actors.dir_1d -> Actors.enemy_typ -> Sprite.sprite -> obj
        -> Dom_html.canvasRenderingContext2D -> collidable option

val evolve_block : obj -> Dom_html.canvasRenderingContext2D -> collidable
val dec_health : obj -> unit

val rev_dir : obj -> Actors.enemy_typ -> Sprite.sprite -> unit

val reverse_left_right : obj -> unit

val collide_block : ?check_x: bool -> Actors.dir_2d -> obj -> unit

val spawn_above : Actors.dir_1d -> obj -> Actors.item_typ
            -> Dom_html.canvasRenderingContext2D -> collidable

end = struct
#1 "object.ml"
open Sprite
open Actors
open Particle

(*Variables*)
let friction = 0.9
let gravity = 0.2
let max_y_vel = 4.5
let player_speed = 2.8
let player_jump = 5.7
let player_max_jump = -6.
let dampen_jump = 4.
let invuln = 60

type aabb = {
  center: xy;
  half: xy;
}

type obj_params = {
  has_gravity: bool;
  speed: float;
}

let id_counter = ref min_int

type obj = {
  params: obj_params;
  pos: xy;
  vel: xy;
  id: int;
  mutable jumping: bool;
  mutable grounded: bool;
  mutable dir: Actors.dir_1d;
  mutable invuln: int;
  mutable kill: bool;
  mutable health: int;
  mutable crouch: bool;
  mutable score: int;
}

type collidable =
  | Player of pl_typ * sprite * obj
  | Enemy of enemy_typ * sprite * obj
  | Item of item_typ * sprite * obj
  | Block of block_typ * sprite * obj


(*setup_obj is used to set gravity and speed, with default values true and 1.*)
let setup_obj ?g:(has_gravity=true) ?spd:(speed=1.) () =
  {
    has_gravity;
    speed;
  }

(* Sets an object's x velocity to the speed specified in its params based on
 * its direction *)
let set_vel_to_speed obj =
  let speed = obj.params.speed in
  match obj.dir with
  | Left -> obj.vel.x <- ~-.speed
  | Right -> obj.vel.x <- speed

(* The following make functions all set the objects' has_gravity and speed,
 * returning an [obj_params] that can be directly plugged into the [obj]
 * during creation. *)
let make_player () = setup_obj ~spd:player_speed ()

let make_item = function
  | Mushroom -> setup_obj ()
  | FireFlower -> setup_obj ()
  | Star -> setup_obj ()
  | Coin -> setup_obj ~g:false ()

let make_enemy = function
  | Goomba -> setup_obj ()
  | GKoopa -> setup_obj ()
  | RKoopa -> setup_obj ()
  | GKoopaShell -> setup_obj ~spd:3. ()
  | RKoopaShell -> setup_obj ~spd:3. ()

let make_block = function
  | QBlock i -> setup_obj ~g:false ()
  | QBlockUsed -> setup_obj ~g:false ()
  | Brick -> setup_obj ~g:false ()
  | UnBBlock -> setup_obj ~g:false ()
  | Cloud -> setup_obj ~g: false ()
  | Panel -> setup_obj ~g: false ()
  | Ground -> setup_obj ~g: false ()

let make_type = function
  | SPlayer(pt,t) -> make_player ()
  | SEnemy t -> make_enemy t
  | SItem t -> make_item t
  | SBlock t -> make_block t

(*Used in object creation and to compare two objects.*)
let new_id () =
  id_counter := !id_counter + 1;
  !id_counter

(*Used to return a new sprite and object of a created spawnable object*)
let make ?id:(id=None) ?dir:(dir=Left) spawnable context (posx, posy) =
  let spr = Sprite.make spawnable dir context in
  let params = make_type spawnable in
  let id = match id with
    | None -> new_id ()
    | Some n -> n
  in
  let obj = {
    params;
    pos = {x=posx; y=posy};
    vel = {x=0.0;y=0.0};
    id;
    jumping = false;
    grounded = false;
    dir;
    invuln = 0;
    kill = false;
    health = 1;
    crouch = false;
    score = 0;
  } in
  (spr,obj)

(*spawn returns a new collidable*)
let spawn spawnable context (posx, posy) =
  let (spr,obj) = make spawnable context (posx, posy) in
  match spawnable with
  | SPlayer(typ,t) -> Player(typ,spr,obj)
  | SEnemy t ->
      set_vel_to_speed obj;
      Enemy(t,spr,obj)
  | SItem t -> Item(t,spr,obj)
  | SBlock t -> Block(t,spr,obj)

(*Helper methods for getting sprites and objects from their collidables*)
let get_sprite = function
  | Player (_,s,_) | Enemy (_,s, _) | Item (_,s, _) | Block (_,s, _)  -> s

let get_obj = function
  | Player (_,_,o) | Enemy (_,_,o) | Item (_,_,o) | Block (_,_,o) -> o

let is_player = function
  | Player(_,_,_) -> true
  | _ -> false

let is_enemy = function
  | Enemy(_,_,_) -> true
  | _ -> false

let equals col1 col2 = (get_obj col1).id = (get_obj col2).id

(*Matches the controls being used and updates each of the player's params.*)
let update_player_keys (player : obj) (controls : controls) : unit =
  let lr_acc = player.vel.x *. 0.2 in
  match controls with
  | CLeft ->
    if not player.crouch then begin
      if player.vel.x > ~-.(player.params.speed)
      then player.vel.x <- player.vel.x -. (0.4 -. lr_acc);
      player.dir <- Left
    end
  | CRight ->
    if not player.crouch then begin
      if player.vel.x < player.params.speed
      then player.vel.x <- player.vel.x +. (0.4 +. lr_acc);
      player.dir <- Right
    end
  | CUp ->
    if (not player.jumping && player.grounded) then begin
      player.jumping <- true;
      player.grounded <- false;
      player.vel.y <-
        max (player.vel.y -.(player_jump +. abs_float player.vel.x *. 0.25))
            player_max_jump
    end
  | CDown ->
    if (not player.jumping && player.grounded) then
      player.crouch <- true

(*Used for sprite changing. If sprites change to different dimensions as a result
 *of some action, the new sprite must be normalized so that things aren't
 *jumpy*)
let normalize_pos pos (p1:Sprite.sprite_params) (p2:Sprite.sprite_params) =
    let (box1,boy1) = p1.bbox_offset and (box2,boy2) = p2.bbox_offset in
    let (bw1,bh1) = p1.bbox_size and (bw2,bh2) = p2.bbox_size in
    pos.x <- pos.x -. (bw2 +. box2) +. (bw1 +. box1);
    pos.y <- pos.y -. (bh2 +. boy2) +. (bh1 +. boy1)

(*Update player is constantly being called to check for if big or small
 *Mario sprites/collidables should be used.*)
let update_player player keys context =
  let prev_jumping = player.jumping in
  let prev_dir = player.dir and prev_vx = abs_float player.vel.x in
  List.iter (update_player_keys player) keys;
  let v = player.vel.x *. friction in
  let vel_damped = if abs_float v < 0.1 then 0. else v in
  player.vel.x <- vel_damped;
  let pl_typ = if player.health <= 1 then SmallM else BigM in
  if not prev_jumping && player.jumping
  then Some (pl_typ, (Sprite.make (SPlayer(pl_typ,Jumping)) player.dir context))
  else if prev_dir<>player.dir || (prev_vx=0. && (abs_float player.vel.x) > 0.)
          && not player.jumping
  then Some (pl_typ, (Sprite.make (SPlayer(pl_typ,Running)) player.dir context))
  else if prev_dir <> player.dir && player.jumping && prev_jumping
  then Some (pl_typ, (Sprite.make (SPlayer(pl_typ,Jumping)) player.dir context))
  else if player.vel.y = 0. && player.crouch
  then Some (pl_typ, (Sprite.make (SPlayer(pl_typ,Crouching)) player.dir context))
  else if player.vel.y = 0. && player.vel.x = 0.
  then Some (pl_typ, (Sprite.make (SPlayer(pl_typ,Standing)) player.dir context))
  else None

(*The following two helper methods update velocity and position of the player*)
let update_vel obj =
  if obj.grounded then obj.vel.y <- 0.
  else if obj.params.has_gravity then
    obj.vel.y <- min (obj.vel.y +. gravity +. abs_float obj.vel.y *. 0.01) max_y_vel

let update_pos obj =
  obj.pos.x <- (obj.vel.x +. obj.pos.x);
  if obj.params.has_gravity then obj.pos.y <- (obj.vel.y +. obj.pos.y)

(*Calls two above helper functions to update velocity and position of player.*)
let process_obj obj mapy =
  update_vel obj;
  update_pos obj;
  if obj.pos.y > mapy then obj.kill <- true

(* Converts an origin based on the bottom left of the bounding box to the top
 * right of the sprite, to make it easier to place objects flush with the ground.*)
let normalize_origin pos (spr:Sprite.sprite) =
  let p = spr.params in
  let (box,boy) = p.bbox_offset and (_,bh) = p.bbox_size in
  pos.x <- pos.x -. box;
  pos.y <- pos.y -. (boy +. bh)

(*Checks upon collision of block and updates the values of the object.*)
let collide_block ?check_x:(check_x=true) dir obj =
  match dir with
  | North -> obj.vel.y <- -0.001
  | South ->
      obj.vel.y <- 0.;
      obj.grounded <- true;
      obj.jumping <- false;
  | East | West -> if check_x then obj.vel.x <- 0.

(*Simple helper method that reverses the direction in question*)
let opposite_dir dir =
  match dir with
  | Left -> Right
  | Right -> Left

(*Used for enemy-enemy collisions*)
let reverse_left_right obj =
  obj.vel.x <- ~-.(obj.vel.x);
  obj.dir <- opposite_dir obj.dir

(*Actually creates a new enemy and deletes the previous. The positions must be
 *normalized. This method is typically called when enemies are killed and a
 *new sprite must be used (i.e., koopa to koopa shell). *)
let evolve_enemy player_dir typ (spr:Sprite.sprite) obj context =
  match typ with
  | GKoopa ->
      let (new_spr,new_obj) =
        make ~dir:obj.dir (SEnemy GKoopaShell) context (obj.pos.x,obj.pos.y) in
      normalize_pos new_obj.pos spr.params new_spr.params;
      Some(Enemy(GKoopaShell,new_spr,new_obj))
  | RKoopa ->
      let (new_spr,new_obj) =
        make ~dir:obj.dir (SEnemy RKoopaShell) context (obj.pos.x,obj.pos.y) in
      normalize_pos new_obj.pos spr.params new_spr.params;
      Some(Enemy(RKoopaShell,new_spr,new_obj))
  | GKoopaShell |RKoopaShell ->
      obj.dir <- player_dir;
      if obj.vel.x <> 0. then obj.vel.x <- 0. else set_vel_to_speed obj;
      None
  | _ -> obj.kill <- true; None

(*Updates the direction of the sprite. *)
let rev_dir o t (s:sprite) =
  reverse_left_right o;
  let old_params = s.params in
  Sprite.transform_enemy t s o.dir;
  normalize_pos o.pos old_params s.params

(*Used for killing enemies, or to make big Mario into small Mario*)
let dec_health obj =
  let health = obj.health - 1 in
  if health = 0 then obj.kill <- true else
  if obj.invuln = 0 then
    obj.health <- health

(*Used for deleting a block and replacing it with a used block*)
let evolve_block obj context =
  dec_health obj;
  let (new_spr,new_obj) =
    make (SBlock QBlockUsed) context (obj.pos.x, obj.pos.y) in
  Block(QBlockUsed,new_spr,new_obj)

(*Used for making a small Mario into a Big Mario*)
let evolve_player (spr : Sprite.sprite) obj context =
  let (new_spr,new_obj) =
    make (SPlayer (BigM,Standing)) context (obj.pos.x, obj.pos.y) in
  normalize_pos new_obj.pos spr.params new_spr.params ;
  Player(BigM,new_spr,new_obj)

(*Used for spawning items above question mark blocks*)
let spawn_above player_dir obj typ context =
  let item = spawn (SItem typ) context (obj.pos.x, obj.pos.y) in
  let item_obj = get_obj item in
  item_obj.pos.y <- item_obj.pos.y -. (snd (get_sprite item).params.frame_size);
  item_obj.dir <- opposite_dir player_dir;
  set_vel_to_speed item_obj;
  item

(*Used to get the bounding box.*)
let get_aabb obj  =
  let spr = ((get_sprite obj).params)  in
  let obj = get_obj obj in
  let (offx, offy) = spr.bbox_offset in
  let (box,boy) = (obj.pos.x+.offx,obj.pos.y+.offy) in
  let (sx,sy) = spr.bbox_size in
  {
    center = {x=(box+.sx/.2.);y=(boy+.sy/.2.)};
    half = {x=sx/.2.;y=sy/.2.};
  }

let col_bypass c1 c2 =
  let o1 = get_obj c1 and o2 = get_obj c2 in
  let ctypes = match(c1,c2) with
  | (Item(_,_,_), Enemy(_,_,_))
  | (Enemy(_,_,_), Item(_,_,_))
  | (Item(_,_,_), Item(_,_,_)) -> true
  | (Player(_,_,o1), Enemy(_,_,_)) -> if o1.invuln > 0 then true else false
  | _ -> false
  in o1.kill || o2.kill || ctypes

(*Used for checking if collisions occur. Compares half-widths and half-heights
 *and adjusts for when collisions do occur, by changing position so that
 *a second collision does not occur again immediately. This causes snapping.*)
let check_collision c1 c2 =
  let b1 = get_aabb c1 and b2 = get_aabb c2 in
  let o1 = get_obj c1 in
  if col_bypass c1 c2 then None else
  let vx = (b1.center.x) -. (b2.center.x) in
  let vy = (b1.center.y) -. (b2.center.y) in
  let hwidths = (b1.half.x) +. (b2.half.x) in
  let hheights = (b1.half.y) +. (b2.half.y) in
  if abs_float vx < hwidths && abs_float vy < hheights then begin
    let ox = hwidths -. abs_float vx in
    let oy = hheights -. abs_float vy in
    if ox >= oy then begin
      if vy > 0. then (o1.pos.y <- (o1.pos.y+.oy);  Some North)
      else (o1.pos.y <- (o1.pos.y -. oy);  Some South)
    end else begin
      if vx > 0. then (o1.pos.x <- o1.pos.x +.ox; Some West)
      else (o1.pos.x <- o1.pos.x -. ox;  Some East)
    end
  end else None

(*"Kills" the matched object by setting certain parameters for each.*)
let kill collid ctx =
  match collid with
  | Enemy(t,s,o) ->
      let pos = (o.pos.x,o.pos.y) in
      let score = if o.score > 0 then [Particle.make_score o.score pos ctx] else [] in
      let remains = begin match t with
      | Goomba -> [Particle.make GoombaSquish pos ctx]
      | _ -> []
      end in
      score @ remains
  | Block(t,s,o) ->
      begin match t with
      | Brick ->
          let pos = (o.pos.x,o.pos.y) in
          let p1 = Particle.make ~vel:(-5.,-5.) ~acc:(0.,0.2) BrickChunkL pos ctx in
          let p2 = Particle.make ~vel:(-3.,-4.) ~acc:(0.,0.2) BrickChunkL pos ctx in
          let p3 = Particle.make ~vel:(3.,-4.) ~acc:(0.,0.2) BrickChunkR pos ctx in
          let p4 = Particle.make ~vel:(5.,-5.) ~acc:(0.,0.2) BrickChunkR pos ctx in
          [p1;p2;p3;p4]
      | _ -> []
      end
  | Item(t,s,o) ->
      begin match t with
      | Mushroom -> [Particle.make_score o.score (o.pos.x,o.pos.y) ctx]
      | _ -> []
      end
  | _ -> []

end
module Draw : sig 
#1 "draw.mli"

(* Renders a given object on the canvas *)
val render : Sprite.sprite -> float * float  -> unit

(* Clears the canvas *)
val clear_canvas : Dom_html.canvasElement -> unit

(* Draw the given sprite as a background *)
val draw_bgd : Sprite.sprite -> float -> unit

(* Draws the axis aligned bounding box of the sprite at the position *)
val render_bbox : Sprite.sprite -> float * float -> unit

(* Draws the fps on the canvas *)
val fps : Dom_html.canvasElement -> float -> unit

(* Draw the heads up display *)
val hud : Dom_html.canvasElement -> int -> int -> unit

(* Draw the game win screen *)
val game_win : Dom_html.canvasRenderingContext2D -> unit

(* Draw the game loss screen *)
val game_loss : Dom_html.canvasRenderingContext2D -> unit

end = struct
#1 "draw.ml"
open Object
open Sprite
module Html = Dom_html
let document = Html.document

let get_context canvas = canvas##getContext "2d"

let render_bbox sprite (posx,posy) =
  let context = Dom_html.canvasRenderingContext2DToJsObj sprite.context in
  let (bbox,bboy) = sprite.params.bbox_offset in
  let (bbsx,bbsy) = sprite.params.bbox_size in
  context##strokeStyle #= "#FF0000";
  context##strokeRect (posx+.bbox) (posy+.bboy) bbsx bbsy

(*Draws a sprite onto the canvas.*)
let render sprite (posx,posy) =
  let context = Dom_html.canvasRenderingContext2DToJsObj sprite.context in
  let (sx, sy) = sprite.params.src_offset in
  let (sw, sh) = sprite.params.frame_size in
  let (dx, dy) = (posx,posy) in
  let (dw, dh) = sprite.params.frame_size in
  let sx = sx +. (float_of_int !(sprite.frame)) *. sw in
  (*print_endline (string_of_int !(sprite.frame));*)
  (*context##clearRect(0.,0.,sw, sh);*)
  context##drawImage sprite.img sx sy sw sh dx dy dw dh

(*Draws two background images, which needs to be done because of the
 *constantly changing viewport, which is always at most going to be
 *between two background images.*)
let draw_bgd bgd off_x =
  render bgd (~-.off_x,0.);
  render bgd ((fst bgd.params.frame_size) -. off_x, 0.)

(*Used for animation updating. Canvas is cleared each frame and redrawn.*)
let clear_canvas canvas =
  let canvas = Dom_html.canvasElementToJsObj canvas in
  let context = Dom_html.canvasRenderingContext2DToJsObj (canvas##getContext "2d") in
  let cwidth = float_of_int canvas##width in
  let cheight = float_of_int canvas##height in
  ignore @@ context##clearRect 0. 0. cwidth cheight

(*Displays the text for score and coins.*)
let hud canvas score coins =
  let score_string = string_of_int score in
  let coin_string = string_of_int coins in
  let canvas = Dom_html.canvasElementToJsObj canvas in
  let context = Dom_html.canvasRenderingContext2DToJsObj (canvas##getContext "2d") in
  ignore @@ context##font #= ( ("10px 'Press Start 2P'"));
  ignore @@ context##fillText  ("Score: "^score_string) ((float_of_int canvas##width) -. 140.) 18.;
  ignore @@ context##fillText  ("Coins: "^coin_string) 120. 18.

(*Displays the fps.*)
let fps canvas fps_val =
  let fps_str = int_of_float fps_val |> string_of_int in
  let canvas = Dom_html.canvasElementToJsObj canvas in
  let context = Dom_html.canvasRenderingContext2DToJsObj (canvas##getContext "2d") in
  ignore @@ context##fillText fps_str 10. 18.

(*game_win displays a black screen when you finish a game.*)
let game_win ctx =
  let ctx = Dom_html.canvasRenderingContext2DToJsObj ctx in
  ctx##rect 0. 0. 512. 512.;
  ctx##fillStyle #= ( "black");
  ctx##fill ();
  ctx##fillStyle #= ( "white");
  ctx##font #= ( "20px 'Press Start 2P'");
  ctx##fillText ("You win!") 180. 128.;
  failwith "Game over."

(*gave_loss displays a black screen stating a loss to finish that level play.*)
let game_loss ctx =
  let ctx = Dom_html.canvasRenderingContext2DToJsObj ctx in
  ctx##rect 0. 0. 512. 512.;
  ctx##fillStyle #= ( "black");
  ctx##fill ();
  ctx##fillStyle #= ( "white");
  ctx##font #= ( "20px 'Press Start 2P'");
  ctx##fillText ( "GAME OVER. You lose!") 60. 128.;
  failwith "Game over."

let draw_background_color canvas = failwith "todo"


end
module Viewport : sig 
#1 "viewport.mli"
open Actors

type viewport = {
  pos: Actors.xy;     (* Absolute position of viewport relative to map *)
  v_dim: Actors.xy;   (* Dimensions of viewport *)
  m_dim: Actors.xy;   (* Dimensions of map *)
}

(* Makes a new viewport of viewport dimensions and map dimensions*)
val make : float*float -> float*float -> viewport

(* Calculates the viewport origin point *)
val calc_viewport_point : float -> float -> float -> float

(* Whether the supplied position is outside of the viewport *)
val in_viewport : viewport -> Actors.xy -> bool

(* Whether the supplied position is below the viewport *)
val out_of_viewport_below : viewport -> float -> bool

(* Converts absolute coordinates to viewport coodinates *)
val coord_to_viewport : viewport -> Actors.xy -> Actors.xy

(* Update the viewport *)
val update : viewport -> Actors.xy -> viewport

end = struct
#1 "viewport.ml"
open Actors

type viewport = {
  pos: Actors.xy;
  v_dim: Actors.xy;
  m_dim: Actors.xy;
}

let make (vx,vy) (mx,my) = 
  {
    pos = {x = 0.; y = 0.;};
    v_dim = {x = vx; y = vy};
    m_dim = {x = mx; y = my};
  }

(* Calculates the viewport origin coordinate given the centering coordinate
 * [cc], the canvas coordinate [vc], and the map coordinate [mc]. This function
 * works for both x and y. At the extreme points, it will ensure that the
 * viewport is always within bounds of the map, even if it is no longer
 * centered about the origin point. *) 
let calc_viewport_point cc vc mc = 
  let vc_half = vc /. 2. in
  min ( max (cc -. vc_half) 0. ) ( min (mc -. vc) (abs_float(cc -. vc_half)) )

(* Returns whether a coordinate pair [pos] is inside the viewport [v] *)
let in_viewport v pos = 
  let margin = 32. in
  let (v_min_x,v_max_x) = (v.pos.x -. margin, v.pos.x +. v.v_dim.x) in
  let (v_min_y,v_max_y) = (v.pos.y -. margin, v.pos.y +. v.v_dim.y) in
  let (x,y) = (pos.x, pos.y) in 
  x >= v_min_x && x <= v_max_x && y >= v_min_y && y<= v_max_y

(* Returns whether an object is outside of the viewport and below it. This is
 * useful for determining whether to process falling out of screen normally. *)
let out_of_viewport_below v y = 
  let v_max_y = v.pos.y +. v.v_dim.y in
  y >= v_max_y

(* Converts a x,y [coord] pair in absolute coordinates to coordinates relative
 * to the viewport *)
let coord_to_viewport viewport coord = 
  { 
    x = coord.x -. viewport.pos.x;
    y = coord.y -. viewport.pos.y;
  }

(* Update the viewport [vpt] given the new center x,y coordinate pair [ctr] *)
let update vpt ctr =
  let new_x = calc_viewport_point ctr.x vpt.v_dim.x vpt.m_dim.x in
  let new_y = calc_viewport_point ctr.y vpt.v_dim.y vpt.m_dim.y in
  let pos = {x = new_x; y = new_y} in
  {vpt with pos}


end
module Director : sig 
#1 "director.mli"
(* Initiates the main game loop *)
val update_loop : Dom_html.canvasElement
                  -> (Object.collidable * Object.collidable list)
                  -> float*float
                  -> unit

(* Keydown event handler function *)
val keydown : Dom.keyboardEvent -> bool

(* Keyup event handler function *)
val keyup : Dom.keyboardEvent -> bool

end = struct
#1 "director.ml"
open Sprite
open Object
open Actors
open Viewport
open Particle

(* Represents the values of relevant key bindings. *)
type keys = {
  mutable left: bool;
  mutable right: bool;
  mutable up: bool;
  mutable down: bool;
  mutable bbox: int;
}


(*st represents the state of the game. It includes a background sprite (e.g.,
 * (e.g., hills), a context (used for rendering onto the page), a viewport
 * (used for moving the player's "camera"), a score (which is kept track
 * throughout the game), coins (also kept track through the game),
 * a multiplier (used for when you kill multiple enemies before ever touching
 * the ground, as in the actual Super Mario), and a game_over bool (which
 * is only true when the game is over). *)
type st = {
  bgd: sprite;
  ctx: Dom_html.canvasRenderingContext2D;
  vpt: viewport;
  map: float;
  mutable score: int;
  mutable coins: int;
  mutable multiplier: int;
  mutable game_over: bool;
}

(*pressed_keys instantiates the keys.*)
let pressed_keys = {
  left = false;
  right = false;
  up = false;
  down = false;
  bbox = 0;
}

let collid_objs = ref [] (* List of next iteration collidable objects *)
let particles = ref [] (* List of next iteration particles *)
let last_time = ref 0. (* Used for calculating fps *)


(* Calculates fps as the difference between [t0] and [t1] *)
let calc_fps t0 t1 =
  let delta = (t1 -. t0) /. 1000. in
  1. /. delta

(* Adds [i] to the score in [state] *)
let update_score state i =
  state.score <- state.score + i

(*player_attack_enemy is called for a player hitting an enemy from the north.
 *This causes the player to either kill the enemy or move the enemy, in the
 *case that the enemy is a shell. Invulnerability, jumping, and grounded
 *are used for fine tuning the movements.*)
let player_attack_enemy s1 o1 typ s2 o2 state context =
  o1.invuln <- 10;
  o1.jumping <- false;
  o1.grounded <- true;
  begin match typ with
  | GKoopaShell | RKoopaShell ->
      let r2 = evolve_enemy o1.dir typ s2 o2 context in
      o1.vel.y <- ~-. dampen_jump;
      o1.pos.y <- o1.pos.y -. 5.;
      (None,r2)
  | _ ->
      dec_health o2;
      o1.vel.y <- ~-. dampen_jump;
      if state.multiplier = 8 then begin
        update_score state 800;
        o2.score <- 800;
        (None, evolve_enemy o1.dir typ s2 o2 context)
      end else begin
        let score = 100 * state.multiplier in
        update_score state score;
        o2.score <- score;
        state.multiplier <- state.multiplier * 2;
        (None,(evolve_enemy o1.dir typ s2 o2 context))
      end
  end

(*enemy_attack_player is used when an enemy kills a player.*)
let enemy_attack_player s1 (o1:Object.obj) t2 s2 (o2:Object.obj) context =
  begin match t2 with
  | GKoopaShell |RKoopaShell ->
      let r2 = if o2.vel.x = 0. then evolve_enemy o1.dir t2 s2 o2 context
              else (dec_health o1; o1.invuln <- invuln; None) in
      (None,r2)
  | _ -> dec_health o1; o1.invuln <- invuln; (None,None)
  end

(*In the case that two enemies collide, they are to reverse directions. However,
 *in the case that one or more of the two enemies is a koopa shell, then
 *the koopa shell kills the other enemy. *)
let col_enemy_enemy t1 s1 o1 t2 s2 o2 dir =
  begin match (t1, t2) with
  | (GKoopaShell, GKoopaShell)
  | (GKoopaShell, RKoopaShell)
  | (RKoopaShell, RKoopaShell)
  | (RKoopaShell, GKoopaShell) ->
      dec_health o1;
      dec_health o2;
      (None,None)
  | (RKoopaShell, _) | (GKoopaShell, _) -> if o1.vel.x = 0. then
      (rev_dir o2 t2 s2;
      (None,None) )
      else ( dec_health o2; (None,None) )
  | (_, RKoopaShell) | (_, GKoopaShell) -> if o2.vel.x = 0. then
      (rev_dir o1 t1 s1;
      (None,None) )
      else ( dec_health o1; (None,None) )
  | (_, _) ->
      begin match dir with
      | West | East ->
          rev_dir o1 t1 s1;
          rev_dir o2 t2 s2;
          (None,None)
      | _ -> (None,None)
      end
  end

(* Gets the object at a given position *)
let obj_at_pos dir (pos: xy) (collids: Object.collidable list)
                                            : Object.collidable list =
  match dir with
  | Left -> List.filter (fun (col: Object.collidable) ->
      (get_obj col).pos.y = pos.y && (get_obj col).pos.x = pos.x -. 16.)
            collids
  | _ -> List.filter (fun (col: Object.collidable) ->
      (get_obj col).pos.y = pos.y && (get_obj col).pos.x = pos.x +. 16.)
            collids

(* Returns whether the object at a given position is a block *)
let is_block dir pos collids =
  match obj_at_pos dir pos collids with
  | [] -> false
  | [Block (_,_,_)] -> true
  | _ -> false

(* Returns whether the given object is a red koopa *)
let is_rkoopa collid =
  match collid with
  | Enemy(RKoopa,_,_) -> true
  | _ -> false

(* Process collision is called to match each of the possible collisions that
 * may occur. Returns a pair of collidable options, representing objects that
 * were created from the existing ones. That is, the first element represents
 * a new item spawned as a result of the first collidable. None indicates that
 * no new item should be spawned. Transformations to existing objects occur
 * mutably, as many changes are side-effectual.*)
let process_collision (dir : Actors.dir_2d) (c1 : Object.collidable)
  (c2 : Object.collidable) (state : st) : (Object.collidable option * Object.collidable option) =
  let context = state.ctx in
  match (c1, c2, dir) with
  | (Player(_,s1,o1), Enemy(typ,s2,o2), South)
  | (Enemy(typ,s2,o2),Player(_,s1,o1), North) ->
      player_attack_enemy s1 o1 typ s2 o2 state context
  | (Player(_,s1,o1), Enemy(t2,s2,o2), _)
  | (Enemy(t2,s2,o2), Player(_,s1,o1), _) ->
      enemy_attack_player s1 o1 t2 s2 o2 context
  | (Player(_,s1,o1), Item(t2,s2,o2), _)
  | (Item(t2,s2,o2), Player(_,s1,o1), _) ->
      begin match t2 with
      | Mushroom ->
          dec_health o2;
          (if o1.health = 2 then () else o1.health <- o1.health + 1);
          o1.vel.x <- 0.;
          o1.vel.y <- 0.;
          update_score state 1000;
          o2.score <- 1000;
          (None, None)
      | Coin -> state.coins <- state.coins + 1; dec_health o2;
          update_score state 100;
          (None, None)
      | _ -> dec_health o2; update_score state 1000; (None, None)
      end
  | (Enemy(t1,s1,o1), Enemy(t2,s2,o2), dir) ->
      col_enemy_enemy t1 s1 o1 t2 s2 o2 dir
  | (Enemy(t1,s1,o1), Block(t2,s2,o2), East)
  | (Enemy(t1,s1,o1), Block(t2,s2,o2), West)->
    begin match (t1,t2) with (* FIXME *)
    | (RKoopaShell, Brick) | (GKoopaShell, Brick) ->
        dec_health o2;
        reverse_left_right o1;
        (None,None)
    | (RKoopaShell, QBlock typ) | (GKoopaShell, QBlock typ) ->
        let updated_block = evolve_block o2 context in
        let spawned_item = spawn_above o1.dir o2 typ context in
         rev_dir o1 t1 s1;
        (Some updated_block, Some spawned_item)
    | (_,_) ->
        rev_dir o1 t1 s1;
      (None,None)
    end
  | (Item(_,s1,o1), Block(typ2,s2,o2), East)
  | (Item(_,s1,o1), Block(typ2,s2,o2), West) ->
      reverse_left_right o1;
      (None, None)
  | (Enemy(_,s1,o1), Block(typ2,s2,o2), _)
  | (Item(_,s1,o1), Block(typ2,s2,o2), _) ->
      collide_block dir o1;
      (None, None)
  | (Player(t1,s1,o1), Block(t,s2,o2), North) ->
      begin match t with
      | QBlock typ ->
          let updated_block = evolve_block o2 context in
          let spawned_item = spawn_above o1.dir o2 typ context in
          collide_block dir o1;
          (Some spawned_item, Some updated_block)
      | Brick -> if t1 = BigM then begin
        collide_block dir o1; dec_health o2; (None, None) end
                 else (collide_block dir o1; (None,None))
      | Panel -> Draw.game_win state.ctx; (None,None)
      | _ -> collide_block dir o1; (None,None)
      end
  | (Player(_,s1,o1), Block(t,s2,o2), _) ->
    begin match t with
    | Panel -> Draw.game_win state.ctx; (None,None)
    | _ ->
        begin match dir with
        | South -> state.multiplier <- 1 ; collide_block dir o1; (None, None)
        | _ -> collide_block dir o1; (None, None)
        end
    end
  | (_, _, _) -> (None,None)

(* Run the broad phase object filtering *)
let broad_phase collid all_collids state =
  let obj = get_obj collid in
  List.filter (fun c ->
    in_viewport state.vpt obj.pos || is_player collid ||
      out_of_viewport_below state.vpt obj.pos.y) all_collids

(*narrow_phase of collision is used in order to continuously loop through
 *each of the collidable objects to constantly check if collisions are
 *occurring.*)
let rec narrow_phase c cs state =
  let rec narrow_helper c cs state acc =
    match cs with
    | [] -> acc
    | h::t ->
      let c_obj = get_obj c in
      let new_objs = if not (equals c h) then
        begin match Object.check_collision c h with
        | None -> (None,None)
        | Some dir ->
          if (get_obj h).id <> c_obj.id
          then begin
            (*( (if (if is_rkoopa c then
            begin match c_obj.dir with
            | Left -> is_block c_obj.dir {x= c_obj.pos.x -. 16.; y= c_obj.pos.y -. 27.} cs
            | _ -> is_block c_obj.dir {x= c_obj.pos.x +. 16.; y= c_obj.pos.y -. 27.} cs
            end else false) then rev_dir c_obj RKoopa (Object.get_sprite c) else
            ());*)
            process_collision dir c h state
          end
          else (None,None)
      end else (None,None) in
      let acc = match new_objs with
        | (None, Some o) -> o::acc
        | (Some o, None) -> o::acc
        | (Some o1, Some o2) -> o1::o2::acc
        | (None, None) -> acc
      in
      narrow_helper c t state acc
  in narrow_helper c cs state []

(* This is an optimization setp to determine which objects require narrow phase
 * checking. This excludes static collidables, allowing collision to only be
 * checked with moving objects. This method is called once per collidable.
 * Collision detection proceeds as follows:
   * 1. Broad phase - filter collidables that cannot possibly collide with
   *    this object.
   * 2. Narrow phase - compare against all objects to determine whether there
   *    is a collision, and process the collision.
 * This method returns a list of objects that are created, which should be
 * added to the list of collidables for the next iteration.
 * *)
let check_collisions collid all_collids state =
  match collid with
  | Block(_,_,_) -> []
  | _ ->
    let broad = broad_phase collid all_collids state in
    narrow_phase collid broad state

(* Returns whether the bounding box should be drawn *)
let check_bbox_enabled () = pressed_keys.bbox = 1

(* update_collidable is the primary update method for collidable objects,
 * checking the collision, updating the object, and drawing to the canvas.*)
let update_collidable state (collid:Object.collidable) all_collids =
 (* TODO: optimize. Draw static elements only once *)
  let obj = Object.get_obj collid in
  let spr = Object.get_sprite collid in
  obj.invuln <- if obj.invuln > 0 then obj.invuln - 1 else 0;
  (* Prevent position from being updated outside of viewport *)
  let viewport_filter = in_viewport state.vpt obj.pos || is_player collid ||
      out_of_viewport_below state.vpt obj.pos.y in
  if not obj.kill &&  viewport_filter then begin
    obj.grounded <- false;
    Object.process_obj obj state.map;
    (* Run collision detection if moving object*)
    let evolved = check_collisions collid all_collids state in
    (* Render and update animation *)
    let vpt_adj_xy = coord_to_viewport state.vpt obj.pos in
    Draw.render spr (vpt_adj_xy.x,vpt_adj_xy.y);
    if check_bbox_enabled()
      then Draw.render_bbox spr (vpt_adj_xy.x,vpt_adj_xy.y);

    if obj.vel.x <> 0. || not (is_enemy collid)
      then Sprite.update_animation spr;
    evolved
  end else []

(* Converts a keypress to a list of control keys, allowing more than one key
 * to be processed each frame. *)
let translate_keys () =
  let k = pressed_keys in
  let ctrls = [(k.left,CLeft);(k.right,CRight);(k.up,CUp);(k.down,CDown)] in
  List.fold_left (fun a x -> if fst x then (snd x)::a else a) [] ctrls

(* run_update is used to update all of the collidables at once. Primarily used
 * as a wrapper method. This method is necessary to differentiate between
 * the player collidable and the remaining collidables, as special operations
 * such as viewport centering only occur with the player.*)
let run_update_collid state collid all_collids =
  match collid with
  | Player(t,s,o) as p ->
      let keys = translate_keys () in
      o.crouch <- false;
      let player = begin match Object.update_player o keys state.ctx with
        | None -> p
        | Some (new_typ, new_spr) ->
            Object.normalize_pos o.pos s.params new_spr.params;
            Player(new_typ,new_spr,o)
      end in
      let evolved = update_collidable state player all_collids in
      collid_objs := !collid_objs @ evolved;
      player
  | _ ->
      let obj = get_obj collid in
      let evolved = update_collidable state collid all_collids in
      if not obj.kill then (collid_objs := collid::(!collid_objs@evolved));
      let new_parts = if obj.kill then Object.kill collid state.ctx else [] in
      particles := !particles @ new_parts;
      collid

(* Primary update function to update and persist a particle *)
let run_update_particle state part =
  Particle.process part;
  let x=part.pos.x -. state.vpt.pos.x and y=part.pos.y -. state.vpt.pos.y in
  Draw.render part.params.sprite (x,y);
  if not part.kill then particles := part :: !particles

(*update_loop is constantly being called to check for collisions and to
 *update each of the objects in the game.*)
let update_loop canvas (player,objs) map_dim =
  let scale = 1. in
  let ctx = (Dom_html.canvasElementToJsObj canvas)##getContext "2d" in
  let cwidth = (float_of_int (Dom_html.canvasElementToJsObj canvas)##width) /. scale in
  let cheight = (float_of_int (Dom_html.canvasElementToJsObj canvas)##height) /. scale in
  let viewport = Viewport.make (cwidth,cheight) map_dim in
  let state = {
      bgd = Sprite.make_bgd ctx;
      vpt = Viewport.update viewport (get_obj player).pos;
      ctx;
      score = 0;
      coins = 0;
      multiplier = 1;
      map = snd map_dim;
      game_over = false;
  } in
  (Dom_html.canvasRenderingContext2DToJsObj state.ctx)##scale scale scale;
  let rec update_helper time state player objs parts =
      if state.game_over = true then Draw.game_win state.ctx else begin
        collid_objs := [];
        particles := [];

        let fps = calc_fps !last_time time in
        last_time := time;

        Draw.clear_canvas canvas;

        (* Parallax background *)
        let vpos_x_int = int_of_float (state.vpt.pos.x /. 5.)  in
        let bgd_width = int_of_float (fst state.bgd.params.frame_size) in
        Draw.draw_bgd state.bgd (float_of_int (vpos_x_int mod bgd_width));

        let player = run_update_collid state player objs in

        if (get_obj player).kill = true
        then Draw.game_loss state.ctx else begin
          let state = {
            state with vpt = Viewport.update state.vpt (get_obj player).pos} in
          List.iter (fun obj -> ignore (run_update_collid state obj objs)) objs;
          List.iter (fun part -> run_update_particle state part) parts;
          Draw.fps canvas fps;
          Draw.hud canvas state.score state.coins;
          ignore @@ Dom_html.requestAnimationFrame(
            fun (t:float) ->
              update_helper t state player !collid_objs !particles)
        end
      end
  in update_helper 0. state player objs []

(* Keydown event handler translates a key press *)
let keydown evt =
  let evt = Dom_html.keyboardEventToJsObj evt in
  let () = match evt##keyCode with
  | 38 | 32 | 87 -> pressed_keys.up <- true
  | 39 | 68 -> pressed_keys.right <- true
  | 37 | 65 -> pressed_keys.left <- true
  | 40 | 83 -> pressed_keys.down <- true
  | 66 -> pressed_keys.bbox <- (pressed_keys.bbox + 1) mod 2
  | _ -> ()
  in Js.true_

(* Keyup event handler translates a key release *)
let keyup evt =
  let evt = Dom_html.keyboardEventToJsObj evt in
  let () = match evt##keyCode with
  | 38 | 32 | 87 -> pressed_keys.up <- false
  | 39 | 68 -> pressed_keys.right <- false
  | 37 | 65 -> pressed_keys.left <- false
  | 40 | 83 -> pressed_keys.down <- false
  | _ -> ()
  in Js.true_

end
module Procedural_generator : sig 
#1 "procedural_generator.mli"
open Object
open Actors

type obj_coord

val init : unit -> unit

(* Procedurally generates a new map of default size*)
val generate : float -> float -> Dom_html.canvasRenderingContext2D ->
               collidable * collidable list

end = struct
#1 "procedural_generator.ml"
open Actors
open Object

(*Note: Canvas is 512 by 256 (w*h) -> 32 by 16 blocks*)

(*Holds obj typ and its coordinates. (int, (x-coord, y-coord))*)
type obj_coord =  int * (float * float)

(*Checks if the given location checkloc is already part of the list of locations
* in loclist.*)
let rec mem_loc (checkloc: float * float) (loclist: obj_coord list) : bool =
  match loclist with
  |[] -> false
  |h::t -> if (checkloc = (snd h)) then true
           else mem_loc checkloc t

(*Converts list of locations from blocksize to pixelsize by multiplying (x,y) by
* 16.*)
let rec convert_list (lst:obj_coord list) :obj_coord list =
  match lst with
  |[] -> []
  |(h::t) -> [(fst h, ((fst (snd h))*.16.,(snd (snd h))*.16.))]@(convert_list t)

(*Chooses what type of enemy should be instantiated given typ number*)
let choose_enemy_typ (typ:int) : enemy_typ =
  match typ with
  |0 -> RKoopa
  |1 -> GKoopa
  |2 -> Goomba
  |_ -> failwith "Shouldn't reach here"

(*Chooses what type of block should be instantiated given typ number*)
let choose_sblock_typ (typ:int) : block_typ =
  match typ with
  |0 -> Brick
  |1 -> UnBBlock
  |2 -> Cloud
  |3 -> QBlock Mushroom
  |4 -> Ground
  |_ -> failwith "Shouldn't reach here"

(*Optimizes lst such that there are no two items in the list that have the same
* coordinates. If there is one, it is removed.*)
let rec avoid_overlap (lst:obj_coord list) (currentLst:obj_coord list)
                      : obj_coord list =
  match lst with
  |[] -> []
  |h::t -> if(mem_loc (snd h) currentLst) then avoid_overlap t currentLst
           else [h]@(avoid_overlap t currentLst)

(*Gets rid of objects with coordinates in the ending frame, within 128 pixels of
* the start, at the very top, and two blocks from the ground.*)
let rec trim_edges (lst: obj_coord list) (blockw:float) (blockh: float)
                   : obj_coord list =
  match lst with
  |[] -> []
  |h::t -> let cx = fst(snd h) in
           let cy = snd(snd h) in
           let pixx = blockw*.16. in
           let pixy = blockh*.16. in
           if(cx<128. || pixx-.cx<528. || cy = 0. || pixy-.cy<48.)
            then trim_edges t blockw blockh
           else [h]@trim_edges t blockw blockh

(*Generates a stair formation with block typ being dependent on typ. This type
* of stair formation requires that the first step be on the ground.*)
let generate_ground_stairs cbx cby typ =
  let four = [(typ, (cbx, cby));(typ, (cbx+.1., cby));(typ, (cbx+.2., cby));
             (typ, (cbx+.3., cby))] in
  let three = [(typ,(cbx +. 1., cby -. 1.));(typ,(cbx +. 2., cby -. 1.));
              (typ,(cbx +. 3., cby -. 1.))] in
  let two = [(typ,(cbx +. 2., cby -. 2.));(typ,(cbx +. 3., cby -. 2.))] in
  let one = [(typ,(cbx +. 3., cby -. 3.))] in
  four@three@two@one

(*Generates a stair formation going upwards.*)
let generate_airup_stairs cbx cby typ =
  let one = [(typ,(cbx, cby));(typ,(cbx +. 1., cby))] in
  let two = [(typ,(cbx +. 3., cby -. 1.));(typ,(cbx +. 4., cby -. 1.))] in
  let three = [(typ,(cbx +. 4., cby -. 2.));(typ,(cbx +. 5., cby -. 2.));
              (typ,(cbx +. 6., cby -. 2.))] in
  one@two@three

(*Generates a stair formation going downwards*)
let generate_airdown_stairs cbx cby typ =
  let three = [(typ,(cbx, cby));(typ,(cbx +. 1., cby));(typ,(cbx +. 2., cby))]in
  let two = [(typ,(cbx +. 2., cby +. 1.));(typ,(cbx +. 3., cby +. 1.))] in
  let one = [(typ,(cbx +. 5., cby +. 2.));(typ,(cbx +. 6., cby +. 2.))] in
  three@two@one

(*Generates a cloud block platform with some length num.*)
let rec generate_clouds cbx cby typ num =
  if(num = 0) then []
  else [(typ,(cbx, cby))]@generate_clouds (cbx+.1.) cby typ (num-1)

(*Generates an obj_coord list (typ, coordinates) of coins to be placed.*)
let rec generate_coins (block_coord: obj_coord list) : obj_coord list =
  let place_coin = Random.int 2 in
  match block_coord with
  |[] -> []
  |h::t ->  if(place_coin = 0) then
              let xc = fst(snd h) in
              let yc = snd(snd h) in
              [(0,(xc,(yc-.16.)))]@generate_coins t
            else generate_coins t

(*Chooses the form of the blocks to be placed.
* When called, leaves a 1 block gap from canvas size.
* 1. If current xblock or yblock is greater than canvas width or height
*    respectively, return an empty list.
* 2. If current xblock or yblock is within 10 blocks of the left and right sides
*    of the level map, prevent any objects from being initialized.
* 3. Else call helper methods to created block formations and return obj_coord
*    list.
**)
let choose_block_pattern (blockw:float) (blockh: float) (cbx:float) (cby:float)
                         (prob:int) : obj_coord list=
  if(cbx > blockw || cby > blockh) then []
  else
    let block_typ = Random.int 4 in
    let stair_typ = Random.int 2 in
    let life_block_chance = Random.int 5 in
    let middle_block = if(life_block_chance = 0) then 3 else stair_typ in
    let obj_coord =
    match prob with
    |0 -> if(blockw -. cbx > 2.) then [(stair_typ, (cbx, cby));
            (middle_block,(cbx +. 1., cby));(stair_typ,(cbx +. 2., cby))]
          else if (blockw -. cbx > 1.) then [(block_typ,(cbx, cby));
            (block_typ,(cbx +. 1., cby))]
          else [(block_typ,(cbx, cby))]
    |1 -> let num_clouds = (Random.int 5) + 5 in
          if(cby < 5.) then generate_clouds cbx cby 2 num_clouds
          else []
    |2 -> if(blockh-.cby = 1.) then generate_ground_stairs cbx cby stair_typ
          else []
    |3 -> if(stair_typ = 0 && blockh -. cby > 3.) then
          generate_airdown_stairs cbx cby stair_typ
          else if (blockh-.cby>2.) then generate_airup_stairs cbx cby stair_typ
          else [(stair_typ,(cbx, cby))]
    |4 -> if ((cby +. 3.) -. blockh = 2.) then [(stair_typ,(cbx, cby))]
          else if ((cby +. 3.) -. blockh = 1.) then [(stair_typ, (cbx,cby));
          (stair_typ, (cbx, cby +. 1.))]
          else [(stair_typ,(cbx, cby)); (stair_typ,(cbx, cby +. 1.));
          (stair_typ,(cbx, cby +. 2.))]
    |5 -> [(3,(cbx, cby))]
    |_ -> failwith "Shouldn't reach here" in
    obj_coord

(*Generates a list of enemies to be placed on the ground.*)
let rec generate_enemies (blockw: float) (blockh: float) (cbx: float)
                    (cby: float) (acc: obj_coord list) =
  if(cbx > (blockw-.32.)) then []
  else if (cby > (blockh-. 1.) ||  cbx < 15.) then
    generate_enemies blockw blockh (cbx +. 1.) 0. acc
  else if(mem_loc (cbx, cby) acc || cby = 0.) then
    generate_enemies blockw blockh cbx (cby+.1.) acc
  else
    let prob = Random.int 30 in
    let enem_prob = 3 in
      if(prob < enem_prob && (blockh -. 1. = cby)) then
        let enemy = [(prob,(cbx*.16.,cby*.16.))] in
        enemy@(generate_enemies blockw blockh cbx (cby+.1.) acc)
      else generate_enemies blockw blockh cbx (cby+.1.) acc

(*Generates a list of enemies to be placed upon the block objects.*)
let rec generate_block_enemies (block_coord: obj_coord list) : obj_coord list =
  let place_enemy = Random.int 20 in
  let enemy_typ = Random.int 3 in
  match block_coord with
  |[] -> []
  |h::t ->  if(place_enemy = 0) then
              let xc = fst(snd h) in
              let yc = snd(snd h) in
              [(enemy_typ,(xc,(yc-.16.)))]@generate_block_enemies t
            else generate_block_enemies t

(*Generates an obj_coord list (typ, coordinates) of blocks to be placed.*)
let rec generate_block_locs (blockw: float) (blockh: float) (cbx: float)
                    (cby: float) (acc: obj_coord list) : obj_coord list =
  if(blockw-.cbx<33.) then acc
  else if (cby > (blockh-. 1.)) then
    generate_block_locs blockw blockh (cbx+.1.) 0. acc
  else if(mem_loc (cbx, cby) acc || cby = 0.) then
    generate_block_locs blockw blockh cbx (cby+.1.) acc
  else
    let prob = Random.int 100 in
    let block_prob = 5 in
      if(prob < block_prob) then
        let newacc = choose_block_pattern blockw blockh cbx cby prob in
        let undup_lst = avoid_overlap newacc acc in
        let called_acc = acc@undup_lst in
        generate_block_locs blockw blockh cbx (cby+.1.) called_acc
      else generate_block_locs blockw blockh cbx (cby+.1.) acc

(*Generates the ending item panel at the end of the level. Games ends upon
* collision with player.*)
let generate_panel (context:Dom_html.canvasRenderingContext2D)
                   (blockw: float) (blockh: float) : collidable =
  let ob = Object.spawn (SBlock Panel) context
    ((blockw*.16.)-.256., (blockh *. 16.)*.2./.3.) in
  ob

(*Generates the list of brick locations needed to display the ground.
* 1/10 chance that a ground block is skipped each call to create holes.*)
let rec generate_ground (blockw:float) (blockh:float) (inc:float)
                        (acc: obj_coord list) : obj_coord list =
  if(inc > blockw) then acc
  else
    if(inc > 10.) then
      let skip = Random.int 10 in
      let newacc = acc@[(4, (inc*. 16.,blockh *. 16.))] in
      if (skip = 7 && blockw-.inc>32.)
        then generate_ground blockw blockh (inc +. 1.) acc
      else  generate_ground blockw blockh (inc +. 1.) newacc
    else let newacc = acc@[(4, (inc*. 16.,blockh *. 16.))] in
      generate_ground blockw blockh (inc +. 1.) newacc

(*Converts the obj_coord list called by generate_block_locs to a list of objects
* with the coordinates given from the obj_coord list. *)
let rec convert_to_block_obj (lst:obj_coord list)
  (context:Dom_html.canvasRenderingContext2D) : collidable list =
  match lst with
  |[] -> []
  |h::t ->
    let sblock_typ = choose_sblock_typ (fst h) in
    let ob = Object.spawn (SBlock sblock_typ) context (snd h) in
    [ob]@(convert_to_block_obj t context)

(*Converts the obj_coord list called by generate_enemies to a list of objects
* with the coordinates given from the obj_coord list. *)
let rec convert_to_enemy_obj (lst:obj_coord list)
            (context:Dom_html.canvasRenderingContext2D) : collidable list =
  match lst with
  |[] -> []
  |h::t ->
    let senemy_typ = choose_enemy_typ (fst h) in
    let ob = Object.spawn (SEnemy senemy_typ) context (snd h) in
    [ob]@(convert_to_enemy_obj t context)

(*Converts the list of coordinates into a list of Coin objects*)
let rec convert_to_coin_obj (lst:obj_coord list)
            (context:Dom_html.canvasRenderingContext2D) : collidable list =
  match lst with
  |[] -> []
  |h::t ->
    let sitem_typ = Coin in
    let ob = Object.spawn (SItem sitem_typ) context (snd h) in
    [ob]@(convert_to_coin_obj t context)

(*Procedurally generates a list of collidables given canvas width, height and
* context. Arguments block width (blockw) and block height (blockh) are in
* block form, not pixels.*)
let generate_helper (blockw:float) (blockh:float) (cx:float) (cy:float)
            (context:Dom_html.canvasRenderingContext2D) : collidable list =
  let block_locs = generate_block_locs blockw blockh 0. 0. [] in
  let converted_block_locs = trim_edges (convert_list block_locs)
    blockw blockh in
  let obj_converted_block_locs = convert_to_block_obj converted_block_locs
    context in
  let ground_blocks = generate_ground blockw blockh 0. [] in
  let obj_converted_ground_blocks = convert_to_block_obj ground_blocks
    context in
  let block_locations = block_locs@ground_blocks in
  let all_blocks = obj_converted_block_locs@obj_converted_ground_blocks in
  let enemy_locs = generate_enemies blockw blockh 0. 0. block_locations in
  let obj_converted_enemies = convert_to_enemy_obj enemy_locs context in
  let coin_locs = generate_coins converted_block_locs in
  let undup_coin_locs = trim_edges(avoid_overlap coin_locs converted_block_locs)
    blockw blockh in
  let converted_block_coin_locs = converted_block_locs@coin_locs in
  let enemy_block_locs = generate_block_enemies converted_block_locs in
  let undup_enemy_block_locs = avoid_overlap enemy_block_locs
    converted_block_coin_locs in
  let obj_enemy_blocks = convert_to_enemy_obj undup_enemy_block_locs context in
  let coin_objects = convert_to_coin_obj undup_coin_locs context in
  let obj_panel = generate_panel context blockw blockh in
  all_blocks@obj_converted_enemies@coin_objects@obj_enemy_blocks@[obj_panel]

(*Main function called to procedurally generate the level map. w and h args
* are in pixel form. Converts to block form to call generate_helper. Spawns
* the list of collidables received from generate_helper to display on canvas.*)
let generate (w:float) (h:float)
                    (context:Dom_html.canvasRenderingContext2D) :
                    (collidable * collidable list) =
  let blockw = w/.16. in
  let blockh = (h/.16.) -. 1. in
  let collide_list = generate_helper blockw blockh 0. 0. context in
  let player = Object.spawn (SPlayer(SmallM,Standing)) context (100.,224.) in
  (player, collide_list)

(*Makes sure level map is uniquely generated at each call.*)
let init () =
  Random.self_init();

end
module Main
= struct
#1 "main.ml"
open Actors
open Sprite
open Object
module Html = Dom_html
module Pg = Procedural_generator

let loadCount =  ref 0
let imgsToLoad = 4
let level_width = 2400.
let level_height = 256.

(*Canvas is chosen from the index.html file. The context is obtained from
 *the canvas. Listeners are added. A level is generated and the general
 *update_loop method is called to make the level playable.*)
let load _ =
  Random.self_init();
  let canvas_id = "canvas" in
  let canvas = match Dom_html.getElementById Dom_html.document canvas_id with
    | None ->
      Printf.printf "cant find canvas %s \n" canvas_id;
      failwith "fail"
    | Some el -> Dom_html.elementToCanvasElement el
  in
  let context = (Dom_html.canvasElementToJsObj canvas)##getContext "2d" in
  let _ = Dom_html.addEventListener Dom_html.document "keydown" (Director.keydown) Js.true_ in
  let _ = Dom_html.addEventListener Dom_html.document "keyup" (Director.keyup) Js.true_ in
  let () = Pg.init () in
  let _ = Director.update_loop canvas (Pg.generate level_width level_height context) (level_width,level_height) in
  print_endline "asd";
  ()

let inc_counter _ =
  loadCount := !loadCount + 1;
  if !loadCount = imgsToLoad then load() else ()

(*Used for concurrency issues.*)
let preload _ =
  let root_dir = "sprites/" in
  let imgs = [ "blocks.png";"items.png";"enemies.png";"mario-small.png" ] in
  List.map (fun img_src ->
    let img_src = root_dir ^ img_src in
    let img = (Html.createImg Dom_html.document) in
    (Dom_html.imageElementToJsObj img)##src #= (img_src) ;
    ignore(Dom_html.addEventListenerImg  img "load"
    ( (fun ev ->  inc_counter(); Js.true_)) Js.true_)) imgs


let _ = (Dom_html.windowToJsObj Dom_html.window)##onload #= (fun _ -> ignore (preload()); Js.true_)

end
