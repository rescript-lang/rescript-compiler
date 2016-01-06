module StackPanel = struct
  type t
  external create : unit -> t  = "" [@@ js.new ]
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
  external set_orientation : t -> [`vertical|`horizontal] = "" [@@js.set]
  external get_orientation : t -> [`vertical|hor] = "" [@@js.get]

  external set_minHeight : t -> int -> unit = "" [@@js.set]    
  external get_minHeight : t ->  int = "" [@@js.get]

  external addChild : t -> widget -> unit = "" [@@js.send]    
  (** content *)    
      (* type _ event=  *)
      (*   | Click of (e ->  .. ) event  *)
      (*   | *)
  external on : t -> string -> (unit -> unit ) -> unit = "" [@@js.send]       
end

class type stackpanel = object
    method set_orientation : string -> unit 
end
module Textarea = struct
  type t
  external create : unit -> t  = "" [@@ js.new ]
  external set_orientation : t -> [`vertical|`horizontal] = "" [@@js.set]
  external get_orientation : t -> [`vertical|hor] = "" [@@js.get]

  external set_minHeight : t -> int -> unit = "" [@@js.set]    
  external get_minHeight : t ->  int = "" [@@js.get]

  external addChild : t -> widget -> unit = "" [@@js.send]    
end


;; let  v = StackPanel.create () in
begin 
  set_orientation v `vertical;
  set_minHeight v 300;
end

