
type variant_representation = String | Int | Other

let find_as_attribute_payload (attributes : Parsetree.attribute list) =
  attributes
  |> List.find_map (fun (({txt = name}, payload) : Parsetree.attribute) ->
         match (name, payload) with
         | ( "as",
             PStr
               [
                 {
                   pstr_desc =
                     Pstr_eval ({pexp_desc = Pexp_constant constant}, _);
                 };
               ] ) ->
           Some constant
         | _ -> None)

let constructors_representations
    (constructors : Types.constructor_declaration list) =
  constructors
  |> List.map (fun (c : Types.constructor_declaration) ->
         match (c.cd_args, c.cd_attributes |> find_as_attribute_payload) with
         | Cstr_tuple [], (Some (Pconst_string _) | None) -> String
         | Cstr_tuple [], Some (Pconst_integer _) -> Int
         | _ -> Other)

(* TODO: Improve error messages? Say why we can't coerce. *)

let can_coerce_to_string (constructors : Types.constructor_declaration list) =
  constructors |> constructors_representations |> List.for_all (( = ) String)

let can_coerce_to_int (constructors : Types.constructor_declaration list) =
  constructors |> constructors_representations |> List.for_all (( = ) Int)
