let suites : Mt.pair_suites ref = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y
let lex, parse = (Parser_api.from_string, Parser_api.implementation)

;;
match
  parse
    (lex
       {|let v str = 
  str  
  |> Lexing.from_string 
  |> Parse.implementation
|})
with
| { pstr_desc=
      Pstr_value
        ( Nonrecursive
        , [ { pvb_pat=
                { ppat_desc=
                    Ppat_var
                      { txt= "v"
                      ; loc=
                          { loc_start=
                              { Lexing.pos_fname= ""
                              ; pos_lnum= 1
                              ; pos_bol= 0
                              ; pos_cnum= 4 }
                          ; loc_end=
                              { Lexing.pos_fname= ""
                              ; pos_lnum= 1
                              ; pos_bol= 0
                              ; pos_cnum= 5 }
                          ; loc_ghost= false } }
                ; ppat_loc=
                    { loc_start=
                        { Lexing.pos_fname= ""
                        ; pos_lnum= 1
                        ; pos_bol= 0
                        ; pos_cnum= 4 }
                    ; loc_end=
                        { Lexing.pos_fname= ""
                        ; pos_lnum= 1
                        ; pos_bol= 0
                        ; pos_cnum= 5 }
                    ; loc_ghost= false }
                ; ppat_attributes= [] }
            ; pvb_expr=
                { pexp_desc=
                    Pexp_fun
                      ( ""
                      , None
                      , { ppat_desc=
                            Ppat_var
                              { txt= "str"
                              ; loc=
                                  { loc_start=
                                      { Lexing.pos_fname= ""
                                      ; pos_lnum= 1
                                      ; pos_bol= 0
                                      ; pos_cnum= 6 }
                                  ; loc_end=
                                      { Lexing.pos_fname= ""
                                      ; pos_lnum= 1
                                      ; pos_bol= 0
                                      ; pos_cnum= 9 }
                                  ; loc_ghost= false } }
                        ; ppat_loc=
                            { loc_start=
                                { Lexing.pos_fname= ""
                                ; pos_lnum= 1
                                ; pos_bol= 0
                                ; pos_cnum= 6 }
                            ; loc_end=
                                { Lexing.pos_fname= ""
                                ; pos_lnum= 1
                                ; pos_bol= 0
                                ; pos_cnum= 9 }
                            ; loc_ghost= false }
                        ; ppat_attributes= [] }
                      , { pexp_desc=
                            Pexp_apply
                              ( { pexp_desc=
                                    Pexp_ident
                                      { txt= Lident "|>"
                                      ; loc=
                                          { loc_start=
                                              { Lexing.pos_fname= ""
                                              ; pos_lnum= 4
                                              ; pos_bol= 46
                                              ; pos_cnum= 48 }
                                          ; loc_end=
                                              { Lexing.pos_fname= ""
                                              ; pos_lnum= 4
                                              ; pos_bol= 46
                                              ; pos_cnum= 50 }
                                          ; loc_ghost= false } }
                                ; pexp_loc=
                                    { loc_start=
                                        { Lexing.pos_fname= ""
                                        ; pos_lnum= 4
                                        ; pos_bol= 46
                                        ; pos_cnum= 48 }
                                    ; loc_end=
                                        { Lexing.pos_fname= ""
                                        ; pos_lnum= 4
                                        ; pos_bol= 46
                                        ; pos_cnum= 50 }
                                    ; loc_ghost= false }
                                ; pexp_attributes= [] }
                              , [ ( ""
                                  , { pexp_desc=
                                        Pexp_apply
                                          ( { pexp_desc=
                                                Pexp_ident
                                                  { txt= Lident "|>"
                                                  ; loc=
                                                      { loc_start=
                                                          { Lexing.pos_fname= ""
                                                          ; pos_lnum= 3
                                                          ; pos_bol= 21
                                                          ; pos_cnum= 23 }
                                                      ; loc_end=
                                                          { Lexing.pos_fname= ""
                                                          ; pos_lnum= 3
                                                          ; pos_bol= 21
                                                          ; pos_cnum= 25 }
                                                      ; loc_ghost= false } }
                                            ; pexp_loc=
                                                { loc_start=
                                                    { Lexing.pos_fname= ""
                                                    ; pos_lnum= 3
                                                    ; pos_bol= 21
                                                    ; pos_cnum= 23 }
                                                ; loc_end=
                                                    { Lexing.pos_fname= ""
                                                    ; pos_lnum= 3
                                                    ; pos_bol= 21
                                                    ; pos_cnum= 25 }
                                                ; loc_ghost= false }
                                            ; pexp_attributes= [] }
                                          , [ ( ""
                                              , { pexp_desc=
                                                    Pexp_ident
                                                      { txt= Lident "str"
                                                      ; loc=
                                                          { loc_start=
                                                              { Lexing.pos_fname=
                                                                  ""
                                                              ; pos_lnum= 2
                                                              ; pos_bol= 13
                                                              ; pos_cnum= 15 }
                                                          ; loc_end=
                                                              { Lexing.pos_fname=
                                                                  ""
                                                              ; pos_lnum= 2
                                                              ; pos_bol= 13
                                                              ; pos_cnum= 18 }
                                                          ; loc_ghost= false }
                                                      }
                                                ; pexp_loc=
                                                    { loc_start=
                                                        { Lexing.pos_fname= ""
                                                        ; pos_lnum= 2
                                                        ; pos_bol= 13
                                                        ; pos_cnum= 15 }
                                                    ; loc_end=
                                                        { Lexing.pos_fname= ""
                                                        ; pos_lnum= 2
                                                        ; pos_bol= 13
                                                        ; pos_cnum= 18 }
                                                    ; loc_ghost= false }
                                                ; pexp_attributes= [] } )
                                            ; ( ""
                                              , { pexp_desc=
                                                    Pexp_ident
                                                      { txt=
                                                          Ldot
                                                            ( Lident "Lexing"
                                                            , "from_string" )
                                                      ; loc=
                                                          { loc_start=
                                                              { Lexing.pos_fname=
                                                                  ""
                                                              ; pos_lnum= 3
                                                              ; pos_bol= 21
                                                              ; pos_cnum= 26 }
                                                          ; loc_end=
                                                              { Lexing.pos_fname=
                                                                  ""
                                                              ; pos_lnum= 3
                                                              ; pos_bol= 21
                                                              ; pos_cnum= 44 }
                                                          ; loc_ghost= false }
                                                      }
                                                ; pexp_loc=
                                                    { loc_start=
                                                        { Lexing.pos_fname= ""
                                                        ; pos_lnum= 3
                                                        ; pos_bol= 21
                                                        ; pos_cnum= 26 }
                                                    ; loc_end=
                                                        { Lexing.pos_fname= ""
                                                        ; pos_lnum= 3
                                                        ; pos_bol= 21
                                                        ; pos_cnum= 44 }
                                                    ; loc_ghost= false }
                                                ; pexp_attributes= [] } ) ] )
                                    ; pexp_loc=
                                        { loc_start=
                                            { Lexing.pos_fname= ""
                                            ; pos_lnum= 2
                                            ; pos_bol= 13
                                            ; pos_cnum= 15 }
                                        ; loc_end=
                                            { Lexing.pos_fname= ""
                                            ; pos_lnum= 3
                                            ; pos_bol= 21
                                            ; pos_cnum= 44 }
                                        ; loc_ghost= false }
                                    ; pexp_attributes= [] } )
                                ; ( ""
                                  , { pexp_desc=
                                        Pexp_ident
                                          { txt=
                                              Ldot
                                                ( Lident "Parse"
                                                , "implementation" )
                                          ; loc=
                                              { loc_start=
                                                  { Lexing.pos_fname= ""
                                                  ; pos_lnum= 4
                                                  ; pos_bol= 46
                                                  ; pos_cnum= 51 }
                                              ; loc_end=
                                                  { Lexing.pos_fname= ""
                                                  ; pos_lnum= 4
                                                  ; pos_bol= 46
                                                  ; pos_cnum= 71 }
                                              ; loc_ghost= false } }
                                    ; pexp_loc=
                                        { loc_start=
                                            { Lexing.pos_fname= ""
                                            ; pos_lnum= 4
                                            ; pos_bol= 46
                                            ; pos_cnum= 51 }
                                        ; loc_end=
                                            { Lexing.pos_fname= ""
                                            ; pos_lnum= 4
                                            ; pos_bol= 46
                                            ; pos_cnum= 71 }
                                        ; loc_ghost= false }
                                    ; pexp_attributes= [] } ) ] )
                        ; pexp_loc=
                            { loc_start=
                                { Lexing.pos_fname= ""
                                ; pos_lnum= 2
                                ; pos_bol= 13
                                ; pos_cnum= 15 }
                            ; loc_end=
                                { Lexing.pos_fname= ""
                                ; pos_lnum= 4
                                ; pos_bol= 46
                                ; pos_cnum= 71 }
                            ; loc_ghost= false }
                        ; pexp_attributes= [] } )
                ; pexp_loc=
                    { loc_start=
                        { Lexing.pos_fname= ""
                        ; pos_lnum= 1
                        ; pos_bol= 0
                        ; pos_cnum= 6 }
                    ; loc_end=
                        { Lexing.pos_fname= ""
                        ; pos_lnum= 4
                        ; pos_bol= 46
                        ; pos_cnum= 71 }
                    ; loc_ghost= true }
                ; pexp_attributes= [] }
            ; pvb_attributes= []
            ; pvb_loc=
                { loc_start=
                    {Lexing.pos_fname= ""; pos_lnum= 1; pos_bol= 0; pos_cnum= 0}
                ; loc_end=
                    { Lexing.pos_fname= ""
                    ; pos_lnum= 4
                    ; pos_bol= 46
                    ; pos_cnum= 71 }
                ; loc_ghost= false } } ] )
  ; pstr_loc= _ }
  :: _ ->
    eq __LOC__ true true
| _ -> eq __LOC__ true false

;;
Mt.from_pair_suites __MODULE__ !suites
