let should_transform name = !Js_config.embeds |> List.mem name

let extract_extension str =
  match String.split_on_char '.' str with
  | ["generated"; tag] -> Some (tag, None)
  | ["generated"; tag; fn_name] -> Some (tag, Some fn_name)
  | [tag] -> Some (tag, None)
  | [tag; fn_name] -> Some (tag, Some fn_name)
  | _ -> None

let transformed_count = Hashtbl.create 10

let increment_transformed_count (ext_name : string) =
  match Hashtbl.find_opt transformed_count ext_name with
  | None -> Hashtbl.add transformed_count ext_name 1
  | Some count -> Hashtbl.replace transformed_count ext_name (count + 1)

let get_transformed_count ext_name =
  match Hashtbl.find_opt transformed_count ext_name with
  | None -> 0
  | Some count -> count

type transformMode = LetBinding | ModuleBinding

let make_lident ?fn_name ~extension_name ~transform_mode filename =
  Longident.parse
    (Printf.sprintf "%s__%s.M%i%s"
       (if String.ends_with filename ~suffix:".res" then
          Filename.(chop_suffix (basename filename) ".res")
        else Filename.(chop_suffix (basename filename) ".resi"))
       extension_name
       (get_transformed_count extension_name)
       (match (transform_mode, fn_name) with
       | LetBinding, Some fn_name -> "." ^ fn_name
       | LetBinding, None -> ".default"
       | ModuleBinding, _ -> ""))

let transform_expr expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_extension
      ( {txt = ext_name},
        PStr
          [
            {
              pstr_desc =
                Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (_, _))}, _);
            };
          ] )
    when should_transform ext_name -> (
    match extract_extension ext_name with
    | None -> expr
    | Some (extension_name, fn_name) ->
      increment_transformed_count extension_name;
      let loc = expr.pexp_loc in
      let filename = loc.loc_start.pos_fname in
      let lid =
        make_lident ?fn_name ~extension_name ~transform_mode:LetBinding filename
      in
      Ast_helper.Exp.ident ~loc {txt = lid; loc})
  | _ -> expr

let structure_item structure_item =
  match structure_item.Parsetree.pstr_desc with
  | Pstr_value
      ( recFlag,
        [
          ({
             pvb_expr =
               {pexp_desc = Pexp_extension ({txt = ext_name}, _)} as expr;
           } as valueBinding);
        ] )
    when should_transform ext_name -> (
    match extract_extension ext_name with
    | None -> structure_item
    | Some _ ->
      {
        structure_item with
        pstr_desc =
          Pstr_value
            (recFlag, [{valueBinding with pvb_expr = transform_expr expr}]);
      })
  | Pstr_include
      ({
         pincl_mod =
           {pmod_desc = Pmod_extension ({txt = ext_name; loc}, _)} as pmod;
       } as pincl)
    when ext_name |> should_transform -> (
    match extract_extension ext_name with
    | None -> structure_item
    | Some (extension_name, _fn_name) ->
      increment_transformed_count extension_name;
      {
        structure_item with
        pstr_desc =
          Pstr_include
            {
              pincl with
              pincl_mod =
                {
                  pmod with
                  pmod_desc =
                    Pmod_ident
                      {
                        txt =
                          make_lident loc.loc_start.pos_fname ~extension_name
                            ~transform_mode:ModuleBinding;
                        loc;
                      };
                };
            };
      })
  | Pstr_module
      ({
         pmb_expr =
           {pmod_desc = Pmod_extension ({txt = ext_name; loc}, _)} as pmod;
       } as pmb)
    when ext_name |> should_transform -> (
    match extract_extension ext_name with
    | None -> structure_item
    | Some (extension_name, _fn_name) ->
      increment_transformed_count extension_name;
      {
        structure_item with
        pstr_desc =
          Pstr_module
            {
              pmb with
              pmb_expr =
                {
                  pmod with
                  pmod_desc =
                    Pmod_ident
                      {
                        txt =
                          make_lident loc.loc_start.pos_fname ~extension_name
                            ~transform_mode:ModuleBinding;
                        loc;
                      };
                };
            };
      })
  | _ -> structure_item
