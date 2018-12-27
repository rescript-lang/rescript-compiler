let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites




(* #directory "+compiler-libs";; *)
(* #load "ocamlcommon.cma";; *)

let v str = 
  str  
  |> Lexing.from_string 
  |> Parse.implementation


let _ =
  match v {|let v str = 
  str  
  |> Lexing.from_string 
  |> Parse.implementation
|} with 
  | {Parsetree.pstr_desc =
   Parsetree.Pstr_value (Asttypes.Nonrecursive,
    [{Parsetree.pvb_pat =
       {Parsetree.ppat_desc =
         Parsetree.Ppat_var
          {Asttypes.txt = "v";
           loc =
            {Location.loc_start =
              {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
               pos_cnum = 4};
             loc_end =
              {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
               pos_cnum = 5};
             loc_ghost = false}};
        ppat_loc =
         {Location.loc_start =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 4};
          loc_end =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 5};
          loc_ghost = false};
        ppat_attributes = []};
      pvb_expr =
       {Parsetree.pexp_desc =
         Parsetree.Pexp_fun ("", None,
          {Parsetree.ppat_desc =
            Parsetree.Ppat_var
             {Asttypes.txt = "str";
              loc =
               {Location.loc_start =
                 {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                  pos_cnum = 6};
                loc_end =
                 {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
                  pos_cnum = 9};
                loc_ghost = false}};
           ppat_loc =
            {Location.loc_start =
              {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
               pos_cnum = 6};
             loc_end =
              {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0;
               pos_cnum = 9};
             loc_ghost = false};
           ppat_attributes = []},
          {Parsetree.pexp_desc =
            Parsetree.Pexp_apply
             ({Parsetree.pexp_desc =
                Parsetree.Pexp_ident
                 {Asttypes.txt = Longident.Lident "|>";
                  loc =
                   {Location.loc_start =
                     {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 46;
                      pos_cnum = 48};
                    loc_end =
                     {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 46;
                      pos_cnum = 50};
                    loc_ghost = false}};
               pexp_loc =
                {Location.loc_start =
                  {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 46;
                   pos_cnum = 48};
                 loc_end =
                  {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 46;
                   pos_cnum = 50};
                 loc_ghost = false};
               pexp_attributes = []},
             [("",
               {Parsetree.pexp_desc =
                 Parsetree.Pexp_apply
                  ({Parsetree.pexp_desc =
                     Parsetree.Pexp_ident
                      {Asttypes.txt = Longident.Lident "|>";
                       loc =
                        {Location.loc_start =
                          {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 21;
                           pos_cnum = 23};
                         loc_end =
                          {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 21;
                           pos_cnum = 25};
                         loc_ghost = false}};
                    pexp_loc =
                     {Location.loc_start =
                       {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 21;
                        pos_cnum = 23};
                      loc_end =
                       {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 21;
                        pos_cnum = 25};
                      loc_ghost = false};
                    pexp_attributes = []},
                  [("",
                    {Parsetree.pexp_desc =
                      Parsetree.Pexp_ident
                       {Asttypes.txt = Longident.Lident "str";
                        loc =
                         {Location.loc_start =
                           {Lexing.pos_fname = ""; pos_lnum = 2;
                            pos_bol = 13; pos_cnum = 15};
                          loc_end =
                           {Lexing.pos_fname = ""; pos_lnum = 2;
                            pos_bol = 13; pos_cnum = 18};
                          loc_ghost = false}};
                     pexp_loc =
                      {Location.loc_start =
                        {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 13;
                         pos_cnum = 15};
                       loc_end =
                        {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 13;
                         pos_cnum = 18};
                       loc_ghost = false};
                     pexp_attributes = []});
                   ("",
                    {Parsetree.pexp_desc =
                      Parsetree.Pexp_ident
                       {Asttypes.txt =
                         Longident.Ldot (Longident.Lident "Lexing",
                          "from_string");
                        loc =
                         {Location.loc_start =
                           {Lexing.pos_fname = ""; pos_lnum = 3;
                            pos_bol = 21; pos_cnum = 26};
                          loc_end =
                           {Lexing.pos_fname = ""; pos_lnum = 3;
                            pos_bol = 21; pos_cnum = 44};
                          loc_ghost = false}};
                     pexp_loc =
                      {Location.loc_start =
                        {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 21;
                         pos_cnum = 26};
                       loc_end =
                        {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 21;
                         pos_cnum = 44};
                       loc_ghost = false};
                     pexp_attributes = []})]);
                pexp_loc =
                 {Location.loc_start =
                   {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 13;
                    pos_cnum = 15};
                  loc_end =
                   {Lexing.pos_fname = ""; pos_lnum = 3; pos_bol = 21;
                    pos_cnum = 44};
                  loc_ghost = false};
                pexp_attributes = []});
              ("",
               {Parsetree.pexp_desc =
                 Parsetree.Pexp_ident
                  {Asttypes.txt =
                    Longident.Ldot (Longident.Lident "Parse",
                     "implementation");
                   loc =
                    {Location.loc_start =
                      {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 46;
                       pos_cnum = 51};
                     loc_end =
                      {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 46;
                       pos_cnum = 71};
                     loc_ghost = false}};
                pexp_loc =
                 {Location.loc_start =
                   {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 46;
                    pos_cnum = 51};
                  loc_end =
                   {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 46;
                    pos_cnum = 71};
                  loc_ghost = false};
                pexp_attributes = []})]);
           pexp_loc =
            {Location.loc_start =
              {Lexing.pos_fname = ""; pos_lnum = 2; pos_bol = 13;
               pos_cnum = 15};
             loc_end =
              {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 46;
               pos_cnum = 71};
             loc_ghost = false};
           pexp_attributes = []});
        pexp_loc =
         {Location.loc_start =
           {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 6};
          loc_end =
           {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 46; pos_cnum = 71};
          loc_ghost = true};
        pexp_attributes = []};
      pvb_attributes = [];
      pvb_loc =
       {Location.loc_start =
         {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
        loc_end =
         {Lexing.pos_fname = ""; pos_lnum = 4; pos_bol = 46; pos_cnum = 71};
        loc_ghost = false}}]);
  pstr_loc = _} :: _
    -> eq __LOC__ true true
  | _ -> eq __LOC__ true false




let () = Mt.from_pair_suites __MODULE__ !suites
