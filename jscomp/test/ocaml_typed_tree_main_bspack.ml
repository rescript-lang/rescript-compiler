let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

let v str = 
  str  
  |> Lexing.from_string 
  |> Parse.implementation

let ( ) = 
  begin 
    Clflags.dont_write_files := true;
    Clflags.unsafe_string := false;
    Clflags.debug := true;
    Clflags.record_event_when_debug := false;
    Clflags.binary_annotations := false; 
    Clflags.nopervasives := true;
    Clflags.assume_no_mli := Mli_non_exists
  end

let x = 
  let modulename = "Test" in
  Typemod.type_implementation modulename modulename modulename Env.empty (v {|
type int
external ( ~- ) : int -> int = "%negint"
external ( ~+ ) : int -> int = "%identity"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external ( mod ) : int -> int -> int = "%modint"
 let f x y = x + y|});;


let () = 
  match x with 
  | ({Typedtree.str_items =
        {Typedtree.str_desc =
           Typedtree.Tstr_type
             [{Typedtree.typ_id = { name = "int"; flags = 0};
               typ_name =
                 {Asttypes.txt = "int";
                  loc =
                    {Location.loc_start =
                       {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1;
                        pos_cnum = 6};
                     loc_end =
                       {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1;
                        pos_cnum = 9};
                     loc_ghost = false}};
               typ_params = [];
               typ_type =
                 {Types.type_params = []; type_arity = 0;
                  type_kind = Types.Type_abstract; type_private = Asttypes.Public;
                  type_manifest = None; type_variance = [];
                  type_newtype_level = None;
                  type_loc =
                    {Location.loc_start =
                       {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1;
                        pos_cnum = 1};
                     loc_end =
                       {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1;
                        pos_cnum = 9};
                     loc_ghost = false};
                  type_attributes = []};
               typ_cstrs = []; typ_kind = Typedtree.Ttype_abstract;
               typ_private = Asttypes.Public; typ_manifest = None;
               typ_loc =
                 {Location.loc_start =
                    {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1; pos_cnum = 1};
                  loc_end =
                    {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1; pos_cnum = 9};
                  loc_ghost = false};
               typ_attributes = []}];
         str_loc =
           {Location.loc_start =
              {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1; pos_cnum = 1};
            loc_end =
              {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 1; pos_cnum = 9};
            loc_ghost = false};
         str_env = _} ::
      {Typedtree.str_desc =
         Typedtree.Tstr_primitive
           {Typedtree.val_id = { name = "~-"; flags = 0};
            val_name =
              {Asttypes.txt = "~-";
               loc =
                 {Location.loc_start =
                    {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                     pos_cnum = 19};
                  loc_end =
                    {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                     pos_cnum = 25};
                  loc_ghost = false}};
            val_desc =
              {Typedtree.ctyp_desc =
                 Typedtree.Ttyp_arrow ("",
                                       {Typedtree.ctyp_desc =
                                          Typedtree.Ttyp_constr
                                            (Path.Pident { name = "int"; flags = 0},
                                             {Asttypes.txt = Longident.Lident "int";
                                              loc =
                                                {Location.loc_start =
                                                   {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                                                    pos_cnum = 28};
                                                 loc_end =
                                                   {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                                                    pos_cnum = 31};
                                                 loc_ghost = false}},
                                             []);
                                        ctyp_type =
                                          {Types.desc =
                                             Types.Tconstr
                                               (Path.Pident {name = "int"; flags = 0},
                                                [], {contents = Types.Mnil});
                                           };
                                        ctyp_env = _;
                                                   ctyp_loc =
                                                   {Location.loc_start =
                                                      {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                                                       pos_cnum = 28};
                                                    loc_end =
                                                      {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                                                       pos_cnum = 31};
                                                    loc_ghost = false};
                                        ctyp_attributes = []},
                                       {Typedtree.ctyp_desc =
                                          Typedtree.Ttyp_constr
                                            (Path.Pident { name = "int"; flags = 0},
                                             {Asttypes.txt = Longident.Lident "int";
                                              loc =
                                                {Location.loc_start =
                                                   {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                                                    pos_cnum = 35};
                                                 loc_end =
                                                   {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                                                    pos_cnum = 38};
                                                 loc_ghost = false}},
                                             []);
                                        ctyp_type =
                                          {Types.desc =
                                             Types.Tconstr
                                               (Path.Pident { name = "int"; flags = 0},
                                                [], {contents = Types.Mnil});
                                          };
                                        ctyp_env = _;
                                                   ctyp_loc =
                                                   {Location.loc_start =
                                                      {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                                                       pos_cnum = 35};
                                                    loc_end =
                                                      {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                                                       pos_cnum = 38};
                                                    loc_ghost = false};
                                        ctyp_attributes = []});
               ctyp_type =
                 {Types.desc =
                    Types.Tarrow ("",
                                  {Types.desc =
                                     Types.Tconstr
                                       (Path.Pident { name = "int"; flags = 0},
                                        [], {contents = Types.Mnil});
                                    },
                                  {Types.desc =
                                     Types.Tconstr
                                       (Path.Pident {name = "int"; flags = 0},
                                        [], {contents = Types.Mnil});
                                   },
                                  Types.Cok);
                   };
               ctyp_env = _;
                          ctyp_loc =
                          {Location.loc_start =
                             {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                              pos_cnum = 28};
                           loc_end =
                             {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                              pos_cnum = 38};
                           loc_ghost = false};
               ctyp_attributes = []};
            val_val =
              {Types.val_type =
                 {Types.desc =
                    Types.Tarrow ("",
                                  {Types.desc =
                                     Types.Tconstr
                                       (Path.Pident { name = "int"; flags = 0},
                                        [], {contents = Types.Mnil});
                                  },
                                  {Types.desc =
                                     Types.Tconstr
                                       (Path.Pident { name = "int"; flags = 0},
                                        [], {contents = Types.Mnil});
                                   },
                                  Types.Cok);
                  };
               val_kind =
                 Types.Val_prim
                   {Primitive.prim_name = "%negint"; prim_arity = 1;
                    prim_alloc = true; prim_native_name = "";
                    prim_native_float = false};
               val_loc =
                 {Location.loc_start =
                    {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                     pos_cnum = 10};
                  loc_end =
                    {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 10;
                     pos_cnum = 50};
                  loc_ghost = false};
               val_attributes = []};
            val_prim = _; val_loc = _ ; val_attributes = _};
       str_loc = _ ; str_env = _} :: _
     ;
      str_type = _; str_final_env = _},
     _) -> eq __LOC__ true true
  | _ -> eq __LOC__ true false
    (* [%debugger] ; Js.log x *)

let () = 
  Mt.from_pair_suites __MODULE__ !suites
(* local variables: *)
(* compile-command: "ocamlc.opt -c -I +compiler-libs ocaml_typed_tree_main.ml" *)
(* end: *)
