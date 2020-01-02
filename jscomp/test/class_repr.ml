

class x0 v = 
  object (* no pat means -> selfpat-*)
    val x = v + 2 (* Tcf_val *)
  end  (* only new_variables *)

class x (v : int) = object 
  val x = v 
  method get_x = x 
    (* Tcf_method -> Texp_function (self-
       Texp_instvar x/1019
    *)
end

(* let () = [%bs.debugger] *)
let v = new x 3 (*Texp_new
{[
   | Texp_new (cl, {Location.loc=loc}, _) ->
      Lapply{ap_should_be_tailcall=false;
             ap_loc=loc;
             ap_func=Lprim(Pfield (0, Fld_na), [transl_class_path ~loc e.exp_env cl], loc);
             ap_args=[lambda_unit];
             ap_inlined=Default_inline;
             ap_specialised=Default_specialise}
]}
*)
let u = Oo.copy v 

let () = assert (v#get_x = 3) (* Texp_send*)

let () = assert (u#get_x = 3)

class xx (x : float)= object (self)
  val money = x 
  method get_money = money (* Texp_instvar *)
  method incr = {< money = 2. *. x +. self#get_money >}
    (* Texp_instvar *)
end

let v1 = new xx 3.
let v2 = v1#incr 

let () = assert (v1#get_money = 3.)

    (* if Sys.backend_type = Other "BS" then *)
#if BS then    
let () = 
    Js.log (v1#get_money, v2#get_money)
#end    

let () = assert (v2#get_money = 9.)
(*
{[
  Texp_function
    Nolabel
    [
      <case>
      pattern 
        Tpat_alias "self-3/1036"
        pattern 
        Tpat_var "self-*/1035"
        expression 
        Texp_poly
        None
        Texp_override
        [
          <override> ""money/1033""
                       expression 
                       Texp_apply
                       expression 
                       Texp_ident "Pervasives!.*."
                       [
                         <arg>
                         Nolabel
                           expression 
                           Texp_constant Const_float 2.
                         <arg>
                         Nolabel
                           expression 
                           Texp_instvar "x/1030"
                       ]
        ]
    ]
]}
*)

(* check dispatch of s#get_x*)
class point = 
object (s) 
  val mutable x = 0 
  method get_x = x 
  method get_x5 = s#get_x + 5 
end 

let v = new point 

let () = 
  assert (v#get_x5 = 5)

class xx0 (x : float)= object (self)
  val money = x 
  val a0 = 0
  val a1 = 1
  val a2 = 2
  method get_money = money (* Texp_instvar *)
  method incr = {< money = 2. *. x +. self#get_money; a0 = 2 >}
  (* Texp_instvar *) (* camlinternalOO.copy is inlined here *)
end
