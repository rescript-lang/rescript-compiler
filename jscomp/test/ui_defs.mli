
(* external stackPanel : unit -> stackPanel = ""  
    [@@js.new] [@@js.module "blp/ui" "UI"] 
   
https://github.com/jaked/ocamljs/issues/2
 *)

(* external dom : widget = "x" [@@js.global ] *)

class type dom = object 
end

type html_element
class type doc = 
  object
    method getElementById : string -> html_element
  end
external doc :  doc = "doc" [@@js.global]
