class type titlex =
  object
    method title : string [@@bs.set] [@@bs.get {null; undefined}]
  end[@bs]

class type widget =
  object
    method on : string -> ((event -> unit)[@bs]) -> unit
  end[@bs]

and event =
  object
    method source : widget

    method target : widget
  end[@bs]

class type title =
  object
    method title : string [@@bs.set]
  end[@bs]

class type text =
  object
    method text : string [@@bs.set]
  end[@bs]

class type measure =
  object
    method minHeight : int [@@bs.set]

    method minWidth : int [@@bs.set]

    method maxHeight : int [@@bs.set]

    method maxWidth : int [@@bs.set]
  end[@bs]

class type layout =
  object
    method orientation : string [@@bs.set]
  end[@bs]

class type applicationContext =
  object
    method exit : int -> unit
  end[@bs]

class type contentable =
  object
    method content : #widget Js.t [@@bs.set]

    method contentWidth : int [@@bs.set]
  end[@bs]

class type hostedWindow =
  object
    inherit widget

    inherit title

    inherit contentable

    method show : unit -> unit

    method hide : unit -> unit

    method focus : unit -> unit

    method appContext : applicationContext [@@bs.set]
  end[@bs]

class type hostedContent =
  object
    inherit widget

    inherit contentable
  end[@bs]

class type stackPanel =
  object
    inherit measure

    inherit layout

    inherit widget

    method addChild : #widget Js.t -> unit
  end[@bs]

class type grid =
  object
    inherit widget

    inherit measure

    method columns : [%bs.obj: < width: int ; .. > ] array [@@bs.set]

    method titleRows : [%bs.obj: < label: < text: string ; .. > ; .. > ] array [@@bs.set
                                                                        ]

    method dataSource :
      [%bs.obj: < label: < text: string ; .. > ; .. > ] array array [@@bs.set]
  end[@bs]

class type button =
  object
    inherit widget

    inherit text

    inherit measure
  end[@bs]

class type textArea =
  object
    inherit widget

    inherit measure

    inherit text
  end[@bs]

external set_interval : ((unit -> unit)[@bs]) -> float -> unit = "setInterval"
  [@@bs.module "@runtime", "Runtime"]

external toFixed : float -> int -> string = "toFixed" [@@bs.send]
