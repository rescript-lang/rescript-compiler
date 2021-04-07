
class type titlex = 
  object 
    method title : string [@@bs.set] [@@bs.get {null ; undefined}]
  end

class type widget = 
  object 
      method on : string ->  (event -> unit [@bs]) -> unit 
  end
and  event = 
  object 
    method source : widget
    method target : widget
  end


class type title = 
  object 
    method title : string [@@bs.set]
  end

class type text = 
  object
    method text : string [@@set]
  end

class type measure =
    object
      method minHeight : int [@@set]
      method minWidth : int [@@bs.set]
      method maxHeight : int  [@@bs.set]
      method maxWidth : int [@@bs.set]
    end

class type layout = 
    object 
      method orientation : string [@@bs.set]
    end

class type applicationContext = 
  object 
    method exit : int -> unit 
  end
class type contentable = 
  object
    method content : #widget  [@@bs.set]
    method contentWidth : int  [@@bs.set]
  end

class type hostedWindow =
  object
    inherit widget 
    inherit title
    inherit contentable
    method show : unit -> unit 
    method hide : unit -> unit 
    method focus : unit -> unit 
    method appContext : applicationContext [@@bs.set]
  end

class type hostedContent =
  object 
    inherit widget
    inherit contentable
  end


class type stackPanel = 
  object
    inherit measure
    inherit layout 
    inherit widget

    method addChild : #widget  -> unit 

  end

class type grid  = 
  object
    inherit widget
    inherit measure
    method columns :  <width : int; .. >    array [@@bs.set]
    method titleRows : 
       <label : <text : string; .. >    ; ..>    array [@@bs.set]
    method dataSource :
       <label : <text : string; .. >   ; ..>    array array [@@bs.set]
  end


class type button = 
  object
    inherit widget
    inherit text
    inherit measure
  end

class type textArea = 
  object
    inherit widget
    inherit measure
    inherit text 
  end


external set_interval : (unit -> unit [@bs]) -> float -> unit  =  "setInterval"
     [@@bs.module "@runtime", "Runtime"]

external toFixed : float -> int -> string = "toFixed" [@@send]
