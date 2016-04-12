module StackPanel = struct
  type t
  external create : unit -> t  = "" [@@ bs.new ]
  (** what would be like 
      {[ 
      type orientation = [`vertical|`horizontal]
      external orientation : t -> orientation 
      ]} 
      ppx 
      {[
      %property{
      type t 
      orientation : [`vertical|`horizontal] [@read|@set]
      }
      ]}
   *)
  external set_orientation : t -> [`vertical|`horizontal] = "" [@@bs.set]
  external get_orientation : t -> [`vertical|hor] = "" [@@bs.get]

  external set_minHeight : t -> int -> unit = "" [@@bs.set]    
  external get_minHeight : t ->  int = "" [@@bs.get]

  external addChild : t -> widget -> unit = "" [@@bs.send]    
  (** content *)    
      (* type _ event=  *)
      (*   | Click of (e ->  .. ) event  *)
      (*   | *)
  external on : t -> string -> (unit -> unit ) -> unit = "" [@@bs.send]       
end

class type stackpanel = object
    method set_orientation : string -> unit 
end
module Textarea = struct
  type t
  external create : unit -> t  = "" [@@ bs.new ]
  external set_orientation : t -> [`vertical|`horizontal] = "" [@@bs.set]
  external get_orientation : t -> [`vertical|hor] = "" [@@bs.get]

  external set_minHeight : t -> int -> unit = "" [@@bs.set]    
  external get_minHeight : t ->  int = "" [@@bs.get]

  external addChild : t -> widget -> unit = "" [@@bs.send]    
end


;; let  v = StackPanel.create () in
begin 
  set_orientation v `vertical;
  set_minHeight v 300;
end

