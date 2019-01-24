(* Tests for matchings on integers and characters *)

(* Dense integer switch *)

let f = function 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4 | 5 -> 5 | 6 -> 6 | _ -> 0

(* Sparse integer switch *)

let g = function 303 -> 1 | 401 -> 2 | _ -> 0

(* Very sparse integer switch *)

let iszero = function 0 -> true | _ -> false

(* Simple matching on characters *)

let h = function
    'a' -> "a"
  | 'e' -> "e"
  | 'i' -> "i"
  | 'o' -> "o"
  | 'u' -> "u"
  | _ -> "?"

(* Matching with orpats *)

let k = function
    ' ' | '\t' | '\n' | '\r' -> "blk"
  | 'A'..'Z' | 'a'..'z' | '\192'..'\255' -> "letr"
  | '0'..'9' -> "dig"
  | '!'|'%'|'&'|'$'|'#'|'+'|'/'|':'|'<'|'='|'>'|'?'|'@'|'\\'|
             '~'|'^'|'|'|'*' -> "oper"
  | _ -> "othr"

(* Matching on arrays *)

let p = function [| x |] -> x | _ -> assert false

let q = function [| x |] -> x | _ -> 0

let r = function [| x |] -> x | _ -> 0.0

let l = function
    [||] -> 0
  | [|x|] -> x + 1
  | [|x;y|] -> x + y
  | [|x;y;z|] -> x + y + z
  | _ -> assert false

(* The test *)

open Printf

external bytes_create: int -> bytes = "caml_create_bytes"
external unsafe_chr: int -> char = "%identity"
external bytes_unsafe_set : bytes -> int -> char -> unit
                           = "%bytes_unsafe_set"

external unsafe_to_string : bytes -> string = "%bytes_to_string"

(* The following function is roughly equivalent to Char.escaped,
   except that it is locale-independent. *)
let escaped = function
  | '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | '\b' -> "\\b"
  | c ->
    if ((k c) <> "othr") && ((Char.code c) <= 191) then begin
      let s = bytes_create 1 in
      bytes_unsafe_set s 0 c;
      unsafe_to_string s
    end else begin
      let n = Char.code c in
      let s = bytes_create 4 in
      bytes_unsafe_set s 0 '\\';
      bytes_unsafe_set s 1 (unsafe_chr (48 + n / 100));
      bytes_unsafe_set s 2 (unsafe_chr (48 + (n / 10) mod 10));
      bytes_unsafe_set s 3 (unsafe_chr (48 + n mod 10));
      unsafe_to_string s
    end

let _ =
  for i = -5 to 10 do printf "f(%d) = %d\n" i (f i) done;
  List.iter (fun i -> printf "g(%d) = %d\n" i (g i))
            [0;300;303;305;400;401;402;999];
  for i = -2 to 2 do printf "iszero(%d) = %B\n" i (iszero i) done;
  for i = 97 to 126 do
    let c = Char.chr i in
    printf "h(%c) = %s\n" c (h c)
  done;
  for i = 0 to 255 do
    let c = Char.chr i in
    printf "\tk(%s) = %s" (escaped c) (k c)
  done;
  printf "\n";
  printf "p([|\"hello\"|]) = %s\n" (p [|"hello"|]);
  printf "p([|1.0|]) = %f\n" (p [|1.0|]);
  printf "q([|2|]) = %d\n" (q [|2|]);
  printf "r([|3.0|]) = %f\n" (r [|3.0|]);
  printf "l([||]) = %d\n" (l [||]);
  printf "l([|1|]) = %d\n" (l [|1|]);
  printf "l([|2;3|]) = %d\n" (l [|2;3|]);
  printf "l([|4;5;6|]) = %d\n" (l [|4;5;6|])

(* PR #5992 *)
(* Was segfaulting *)

let f = function
 | lazy (), _, {contents=None} -> 0
 | _, lazy (), {contents=Some x} -> 1

let s = ref None
let set_true = lazy (s := Some 1)
let set_false = lazy (s := None)

let () =
  let _r = try f (set_true, set_false, s) with Match_failure _ -> 2 in
  printf "PR#5992=Ok\n"

(* PR #5788, was giving wrong result 3 *)
exception Foo
exception Bar = Foo

let test e b =
  match e, b with
  | Foo, true -> 1
  | Bar, false -> 2
  | _, _ -> 3

let () =
  let r = test Bar false in
  if r = 2 then printf "PR#5788=Ok\n"

let test e b =
  match e, b with
  | Bar, false -> 0
  | (Foo|Bar), true -> 1
  | Foo, false -> 2
  | _, _ -> 3


let () =
  let r = test Foo false in
  if r = 0 then printf "PR#5788=Ok\n"

(* PR#6646 Avoid explosion of default cases when there are many constructors *)

(* This took forever to compile *)

type token =
  | Abs
  | Acload
  | After
  | And
  | Annotate
  | Apply
  | Arc
  | Array
  | Arraymacro
  | Arrayrelatedinfo
  | Arraysite
  | Assign
  | Atleast
  | Atmost
  | Author
  | Basearray
  | Becomes
  | Between
  | Block
  | Boolean
  | Booleandisplay
  | Booleanmap
  | Booleanvalue
  | Borderpattern
  | Borderwidth
  | Boundingbox
  | Ceiling
  | Cell
  | Cellref
  | Celltype
  | Change
  | Circle
  | Color
  | Comment
  | Commentgraphics
  | Compound
  | Concat
  | Connectlocation
  | Constant
  | Constraint
  | Contents
  | Cornertype
  | Criticality
  | Currentmap
  | Curve
  | Cycle
  | Dataorigin
  | Dcfaninload
  | Dcfanoutload
  | Dcmaxfanin
  | Dcmaxfanout
  | Delay
  | Delta
  | Derivation
  | Design
  | Designator
  | Difference
  | Direction
  | Display
  | Divide
  | Dominates
  | Dot
  | Duration
  | E
  | Edif
  | Ediflevel
  | Edifversion
  | Else
  | Enclosuredistance
  | Endtype
  | Entry
  | Equal
  | Escape
  | Event
  | Exactly
  | External
  | Fabricate
  | False
  | Figure
  | Figurearea
  | Figuregroup
  | Figuregroupobject
  | Figuregroupoverride
  | Figuregroupref
  | Figureperimeter
  | Figurewidth
  | Fillpattern
  | Fix
  | Floor
  | Follow
  | Forbiddenevent
  | Form
  | Globalportref
  | Greaterthan
  | Gridmap
  | If
  | Ignore
  | Includefiguregroup
  | Increasing
  | Initial
  | Instance
  | Instancebackannotate
  | Instancegroup
  | Instancemap
  | Instancenamedef
  | Instanceref
  | Integer
  | Integerdisplay
  | Interface
  | Interfiguregroupspacing
  | Intersection
  | Intrafiguregroupspacing
  | Inverse
  | Isolated
  | Iterate
  | Joined
  | Justify
  | Keyworddisplay
  | Keywordlevel
  | Keywordmap
  | Lessthan
  | Library
  | Libraryref
  | Listofnets
  | Listofports
  | Loaddelay
  | Logicassign
  | Logicinput
  | Logiclist
  | Logicmapinput
  | Logicmapoutput
  | Logiconeof
  | Logicoutput
  | Logicport
  | Logicref
  | Logicvalue
  | Logicwaveform
  | Maintain
  | Match
  | Max
  | Member
  | Min
  | Minomax
  | Minomaxdisplay
  | Mnm
  | Mod
  | Multiplevalueset
  | Mustjoin
  | Name
  | Negate
  | Net
  | Netbackannotate
  | Netbundle
  | Netdelay
  | Netgroup
  | Netmap
  | Netref
  | Nochange
  | Nonpermutable
  | Not
  | Notallowed
  | Notchspacing
  | Number
  | Numberdefinition
  | Numberdisplay
  | Offpageconnector
  | Offsetevent
  | Openshape
  | Or
  | Orientation
  | Origin
  | Overhangdistance
  | Overlapdistance
  | Oversize
  | Owner
  | Page
  | Pagesize
  | Parameter
  | Parameterassign
  | Parameterdisplay
  | Path
  | Pathdelay
  | Pathwidth
  | Permutable
  | Physicaldesignrule
  | Plug
  | Point
  | Pointdisplay
  | Pointlist
  | Pointsubtract
  | Pointsum
  | Polygon
  | Port
  | Portbackannotate
  | Portbundle
  | Portdelay
  | Portgroup
  | Portimplementation
  | Portinstance
  | Portlist
  | Portlistalias
  | Portmap
  | Portref
  | Product
  | Program
  | Property
  | Propertydisplay
  | Protectionframe
  | Pt
  | Rangevector
  | Rectangle
  | Rectanglesize
  | Rename
  | Resolves
  | Scale
  | Scalex
  | Scaley
  | Section
  | Shape
  | Simulate
  | Simulationinfo
  | Singlevalueset
  | Site
  | Socket
  | Socketset
  | Statement
  | Status
  | Steady
  | Strictlyincreasing
  | String
  | Stringdisplay
  | Strong
  | Subtract
  | Sum
  | Symbol
  | Symmetry
  | Table
  | Tabledefault
  | Technology
  | Textheight
  | Then
  | Timeinterval
  | Timestamp
  | Timing
  | Transform
  | Transition
  | Trigger
  | True
  | Typedvalue
  | Unconstrained
  | Undefined
  | Union
  | Unit
  | Unused
  | Userdata
  | Valuenameref
  | Variable
  | Version
  | View
  | Viewlist
  | Viewmap
  | Viewref
  | Viewtype
  | Visible
  | Voltagemap
  | Wavevalue
  | Weak
  | Weakjoined
  | When
  | While
  | Written
  | Xcoord
  | Xor
  | Ycoord
  | ILLEGAL of (char)
  | ID of (string)
  | TLIST of (token list)
  | TLIST2 of (token list*token list)
  | ITEM of (token*token)
  | ITEM2 of (token*token*token)
  | STRING of (string)
  | INT of (int)
  | ENDOFFILE
  | EOL
  | LPAREN
  | RPAREN
  | EMPTY

let test_match tok = match tok with
  | ITEM2(Array, ITEM (Rename, TLIST [ID id; STRING str]), INT idx) ->
      1
  | ITEM2(Cellref, TLIST [ID id], TLIST lst) ->
      2
  | ITEM2(Cell, TLIST [ID cellid], TLIST lst) ->
      3
  | ITEM2(Contents, TLIST lst1, TLIST lst2) ->
      4
  | ITEM2(Design, TLIST [ID id], TLIST lst) ->
      5
  | ITEM2(Edif, TLIST [ID id], TLIST lst) ->
      6
  | ITEM2(Instance,
          TLIST [ID instid],
          TLIST[ITEM2(Viewref, TLIST [ID netlist],
                      TLIST[ITEM(Cellref, TLIST [ID cellid])])]) ->
      7


  | ITEM2(Instance,
          TLIST [ID instid],
          TLIST[ITEM2(Viewref, TLIST [ID netlist],
                      TLIST[ITEM2(Cellref, TLIST [ID cellid],
                                  TLIST [ITEM (Libraryref,
                                               TLIST [ID libid])])])]) ->
      8
(* *)
  | ITEM2(Instance, TLIST [ID instid],
                    TLIST [ITEM2(viewref,
                                 TLIST [ID netlist],
                                 TLIST [ITEM2(cellref,
                                              TLIST [ID cellid],
                                              TLIST [ITEM(libraryref,
                                                          TLIST [ID libid])])]);
                    ITEM2(property, TLIST [ID xstlib],
                                    TLIST [ITEM2(bool1,
                                                 TLIST [],
                                                 TLIST [ITEM(True, TLIST [])]);
                                    ITEM(owner, TLIST [str])])]) ->  9
(* *)
  | ITEM2(Interface, TLIST [], TLIST lst) -> 100
  | ITEM2(Joined, TLIST [], TLIST lst) -> 10
  | ITEM2(Keywordmap, TLIST lst1, TLIST lst2) -> 11
  | ITEM2(Library, TLIST [], TLIST lst) -> 12
  | ITEM2(Library, TLIST [ID libid], TLIST lst) -> 13
  | ITEM2(Net, TLIST [], TLIST [ITEM (Rename, TLIST [ID oldid; STRING newid]);
        ITEM2(Joined, TLIST [],
          TLIST portlst)]) -> 14
  | ITEM2(Net, TLIST [ID netid], TLIST [ITEM2(Joined, TLIST [],
          TLIST portlst)]) -> 15
  | ITEM2(Net, _, _) -> 16
  | ITEM2(Port, TLIST [], TLIST lst) -> 17
  | ITEM2(Port, TLIST [ID id], TLIST lst) -> 18
  | ITEM2(Portref, TLIST [ID id], TLIST [ITEM (Instanceref, TLIST [ID ref])]) ->
      19
  | ITEM2(Portref, TLIST [], TLIST [ITEM (Member, TLIST [ID mref; INT idx])]) ->
      20
  | ITEM2(Portref, TLIST [], TLIST[ITEM (Member, TLIST [ID mref; INT idx]);
                                   ITEM (Instanceref, TLIST [ID instref])]) ->
      21
  | ITEM2(Program, TLIST [STRING progid], TLIST lst) ->21
  | ITEM2(Property, TLIST [ID part], TLIST lst) -> 22
  | ITEM2(Status, TLIST lst1, TLIST lst2) -> 23
  | ITEM2(Technology, TLIST lst1, TLIST lst2) -> 24
  | ITEM2(View, TLIST [ID netlist], TLIST lst) -> 25
  | ITEM2(Viewref, TLIST [ID "netlist"], TLIST lst) -> 26
  | ITEM2(Written, TLIST lst1, TLIST lst2) ->  27
  | ITEM2(External, TLIST lst1, TLIST lst2) -> 28
  | ITEM(Integer, TLIST [INT n]) -> 29
  | ITEM (Author, TLIST [STRING author]) -> 30
  | ITEM (Cellref, TLIST [ID id]) -> 31
  | ITEM (Celltype, TLIST [ID "GENERIC"]) -> 32
  | ITEM (Direction, TLIST [ID dir]) -> 32 (* print_endline dir *)
  | ITEM (Ediflevel, TLIST [INT 0]) -> 32
  | ITEM (Edifversion, TLIST [INT 2; INT 0; INT 0]) -> 32
  | ITEM (Instanceref, TLIST [ID id]) -> 32
  | ITEM (Keywordlevel, TLIST [INT 0]) -> 32
  | ITEM (Libraryref, TLIST [ID "work"]) -> 32
  | ITEM (Libraryref, TLIST [ID "xilinx"]) -> 32
  | ITEM (Member, TLIST [ID id; INT n]) -> 32
  | ITEM (Numberdefinition, TLIST []) -> 32
  | ITEM (Owner, TLIST [STRING "\"Xilinx\""]) -> 32
  | ITEM (Portref, TLIST [ID id]) -> 32
  | ITEM (Rename, TLIST [ID id; STRING str]) -> 33
  | ITEM (String, TLIST [STRING str]) -> 32
  | ITEM (String, TLIST lst) -> 34
  | ITEM (Timestamp, TLIST [INT yr; INT mon; INT day; INT hour; INT min;
                            INT sec]) ->
      32
  | ITEM (Version, TLIST [STRING str]) -> 32
  | ITEM (Viewtype, TLIST [ID "NETLIST"]) -> 32
  | ITEM (Designator, TLIST lst) -> 34
  | Abs -> failwith " Abs "
  | Acload -> failwith " Acload "
  | After -> failwith " After "
  | And -> failwith " And "
  | Annotate -> failwith " Annotate "
  | Apply -> failwith " Apply "
  | Arc -> failwith " Arc "
  | Array -> failwith " Array "
  | Arraymacro -> failwith " Arraymacro "
  | Arrayrelatedinfo -> failwith " Arrayrelatedinfo "
  | Arraysite -> failwith " Arraysite "
  | Assign -> failwith " Assign "
  | Atleast -> failwith " Atleast "
  | Atmost -> failwith " Atmost "
  | Author -> failwith " Author "
  | Basearray -> failwith " Basearray "
  | Becomes -> failwith " Becomes "
  | Between -> failwith " Between "
  | Block -> failwith " Block "
  | Boolean -> failwith " Boolean "
  | Booleandisplay -> failwith " Booleandisplay "
  | Booleanmap -> failwith " Booleanmap "
  | Booleanvalue -> failwith " Booleanvalue "
  | Borderpattern -> failwith " Borderpattern "
  | Borderwidth -> failwith " Borderwidth "
  | Boundingbox -> failwith " Boundingbox "
  | Ceiling -> failwith " Ceiling "
  | Cell -> failwith " Cell "
  | Cellref -> failwith " Cellref "
  | Celltype -> failwith " Celltype "
  | Change -> failwith " Change "
  | Circle -> failwith " Circle "
  | Color -> failwith " Color "
  | Comment -> failwith " Comment "
  | Commentgraphics -> failwith " Commentgraphics "
  | Compound -> failwith " Compound "
  | Concat -> failwith " Concat "
  | Connectlocation -> failwith " Connectlocation "
  | Constant -> failwith " Constant "
  | Constraint -> failwith " Constraint "
  | Contents -> failwith " Contents "
  | Cornertype -> failwith " Cornertype "
  | Criticality -> failwith " Criticality "
  | Currentmap -> failwith " Currentmap "
  | Curve -> failwith " Curve "
  | Cycle -> failwith " Cycle "
  | Dataorigin -> failwith " Dataorigin "
  | Dcfaninload -> failwith " Dcfaninload "
  | Dcfanoutload -> failwith " Dcfanoutload "
  | Dcmaxfanin -> failwith " Dcmaxfanin "
  | Dcmaxfanout -> failwith " Dcmaxfanout "
  | Delay -> failwith " Delay "
  | Delta -> failwith " Delta "
  | Derivation -> failwith " Derivation "
  | Design -> failwith " Design "
  | Designator -> failwith " Designator "
  | Difference -> failwith " Difference "
  | Direction -> failwith " Direction "
  | Display -> failwith " Display "
  | Divide -> failwith " Divide "
  | Dominates -> failwith " Dominates "
  | Dot -> failwith " Dot "
  | Duration -> failwith " Duration "
  | E -> failwith " E "
  | Edif -> failwith " Edif "
  | Ediflevel -> failwith " Ediflevel "
  | Edifversion -> failwith " Edifversion "
  | Else -> failwith " Else "
  | Enclosuredistance -> failwith " Enclosuredistance "
  | Endtype -> failwith " Endtype "
  | Entry -> failwith " Entry "
  | Equal -> failwith " Equal "
  | Escape -> failwith " Escape "
  | Event -> failwith " Event "
  | Exactly -> failwith " Exactly "
  | External -> failwith " External "
  | Fabricate -> failwith " Fabricate "
  | False -> failwith " False "
  | Figure -> failwith " Figure "
  | Figurearea -> failwith " Figurearea "
  | Figuregroup -> failwith " Figuregroup "
  | Figuregroupobject -> failwith " Figuregroupobject "
  | Figuregroupoverride -> failwith " Figuregroupoverride "
  | Figuregroupref -> failwith " Figuregroupref "
  | Figureperimeter -> failwith " Figureperimeter "
  | Figurewidth -> failwith " Figurewidth "
  | Fillpattern -> failwith " Fillpattern "
  | Fix -> failwith " Fix "
  | Floor -> failwith " Floor "
  | Follow -> failwith " Follow "
  | Forbiddenevent -> failwith " Forbiddenevent "
  | Form -> failwith " Form "
  | Globalportref -> failwith " Globalportref "
  | Greaterthan -> failwith " Greaterthan "
  | Gridmap -> failwith " Gridmap "
  | If -> failwith " If "
  | Ignore -> failwith " Ignore "
  | Includefiguregroup -> failwith " Includefiguregroup "
  | Increasing -> failwith " Increasing "
  | Initial -> failwith " Initial "
  | Instance -> failwith " Instance "
  | Instancebackannotate -> failwith " Instancebackannotate "
  | Instancegroup -> failwith " Instancegroup "
  | Instancemap -> failwith " Instancemap "
  | Instancenamedef -> failwith " Instancenamedef "
  | Instanceref -> failwith " Instanceref "
  | Integer -> failwith " Integer "
  | Integerdisplay -> failwith " Integerdisplay "
  | Interface -> failwith " Interface "
  | Interfiguregroupspacing -> failwith " Interfiguregroupspacing "
  | Intersection -> failwith " Intersection "
  | Intrafiguregroupspacing -> failwith " Intrafiguregroupspacing "
  | Inverse -> failwith " Inverse "
  | Isolated -> failwith " Isolated "
  | Iterate -> failwith " Iterate "
  | Joined -> failwith " Joined "
  | Justify -> failwith " Justify "
  | Keyworddisplay -> failwith " Keyworddisplay "
  | Keywordlevel -> failwith " Keywordlevel "
  | Keywordmap -> failwith " Keywordmap "
  | Lessthan -> failwith " Lessthan "
  | Library -> failwith " Library "
  | Libraryref -> failwith " Libraryref "
  | Listofnets -> failwith " Listofnets "
  | Listofports -> failwith " Listofports "
  | Loaddelay -> failwith " Loaddelay "
  | Logicassign -> failwith " Logicassign "
  | Logicinput -> failwith " Logicinput "
  | Logiclist -> failwith " Logiclist "
  | Logicmapinput -> failwith " Logicmapinput "
  | Logicmapoutput -> failwith " Logicmapoutput "
  | Logiconeof -> failwith " Logiconeof "
  | Logicoutput -> failwith " Logicoutput "
  | Logicport -> failwith " Logicport "
  | Logicref -> failwith " Logicref "
  | Logicvalue -> failwith " Logicvalue "
  | Logicwaveform -> failwith " Logicwaveform "
  | Maintain -> failwith " Maintain "
  | Match -> failwith " Match "
  | Max -> failwith " Max "
  | Member -> failwith " Member "
  | Min -> failwith " Min "
  | Minomax -> failwith " Minomax "
  | Minomaxdisplay -> failwith " Minomaxdisplay "
  | Mnm -> failwith " Mnm "
  | Mod -> failwith " Mod "
  | Multiplevalueset -> failwith " Multiplevalueset "
  | Mustjoin -> failwith " Mustjoin "
  | Name -> failwith " Name "
  | Negate -> failwith " Negate "
  | Net -> failwith " Net "
  | Netbackannotate -> failwith " Netbackannotate "
  | Netbundle -> failwith " Netbundle "
  | Netdelay -> failwith " Netdelay "
  | Netgroup -> failwith " Netgroup "
  | Netmap -> failwith " Netmap "
  | Netref -> failwith " Netref "
  | Nochange -> failwith " Nochange "
  | Nonpermutable -> failwith " Nonpermutable "
  | Not -> failwith " Not "
  | Notallowed -> failwith " Notallowed "
  | Notchspacing -> failwith " Notchspacing "
  | Number -> failwith " Number "
  | Numberdefinition -> failwith " Numberdefinition "
  | Numberdisplay -> failwith " Numberdisplay "
  | Offpageconnector -> failwith " Offpageconnector "
  | Offsetevent -> failwith " Offsetevent "
  | Openshape -> failwith " Openshape "
  | Or -> failwith " Or "
  | Orientation -> failwith " Orientation "
  | Origin -> failwith " Origin "
  | Overhangdistance -> failwith " Overhangdistance "
  | Overlapdistance -> failwith " Overlapdistance "
  | Oversize -> failwith " Oversize "
  | Owner -> failwith " Owner "
  | Page -> failwith " Page "
  | Pagesize -> failwith " Pagesize "
  | Parameter -> failwith " Parameter "
  | Parameterassign -> failwith " Parameterassign "
  | Parameterdisplay -> failwith " Parameterdisplay "
  | Path -> failwith " Path "
  | Pathdelay -> failwith " Pathdelay "
  | Pathwidth -> failwith " Pathwidth "
  | Permutable -> failwith " Permutable "
  | Physicaldesignrule -> failwith " Physicaldesignrule "
  | Plug -> failwith " Plug "
  | Point -> failwith " Point "
  | Pointdisplay -> failwith " Pointdisplay "
  | Pointlist -> failwith " Pointlist "
  | Pointsubtract -> failwith " Pointsubtract "
  | Pointsum -> failwith " Pointsum "
  | Polygon -> failwith " Polygon "
  | Port -> failwith " Port "
  | Portbackannotate -> failwith " Portbackannotate "
  | Portbundle -> failwith " Portbundle "
  | Portdelay -> failwith " Portdelay "
  | Portgroup -> failwith " Portgroup "
  | Portimplementation -> failwith " Portimplementation "
  | Portinstance -> failwith " Portinstance "
  | Portlist -> failwith " Portlist "
  | Portlistalias -> failwith " Portlistalias "
  | Portmap -> failwith " Portmap "
  | Portref -> failwith " Portref "
  | Product -> failwith " Product "
  | Program -> failwith " Program "
  | Property -> failwith " Property "
  | Propertydisplay -> failwith " Propertydisplay "
  | Protectionframe -> failwith " Protectionframe "
  | Pt -> failwith " Pt "
  | Rangevector -> failwith " Rangevector "
  | Rectangle -> failwith " Rectangle "
  | Rectanglesize -> failwith " Rectanglesize "
  | Rename -> failwith " Rename "
  | Resolves -> failwith " Resolves "
  | Scale -> failwith " Scale "
  | Scalex -> failwith " Scalex "
  | Scaley -> failwith " Scaley "
  | Section -> failwith " Section "
  | Shape -> failwith " Shape "
  | Simulate -> failwith " Simulate "
  | Simulationinfo -> failwith " Simulationinfo "
  | Singlevalueset -> failwith " Singlevalueset "
  | Site -> failwith " Site "
  | Socket -> failwith " Socket "
  | Socketset -> failwith " Socketset "
  | Statement -> failwith " Statement "
  | Status -> failwith " Status "
  | Steady -> failwith " Steady "
  | Strictlyincreasing -> failwith " Strictlyincreasing "
  | String -> failwith " String "
  | Stringdisplay -> failwith " Stringdisplay "
  | Strong -> failwith " Strong "
  | Subtract -> failwith " Subtract "
  | Sum -> failwith " Sum "
  | Symbol -> failwith " Symbol "
  | Symmetry -> failwith " Symmetry "
  | Table -> failwith " Table "
  | Tabledefault -> failwith " Tabledefault "
  | Technology -> failwith " Technology "
  | Textheight -> failwith " Textheight "
  | Then -> failwith " Then "
  | Timeinterval -> failwith " Timeinterval "
  | Timestamp -> failwith " Timestamp "
  | Timing -> failwith " Timing "
  | Transform -> failwith " Transform "
  | Transition -> failwith " Transition "
  | Trigger -> failwith " Trigger "
  | True -> failwith " True "
  | Typedvalue -> failwith " Typedvalue "
  | Unconstrained -> failwith " Unconstrained "
  | Undefined -> failwith " Undefined "
  | Union -> failwith " Union "
  | Unit -> failwith " Unit "
  | Unused -> failwith " Unused "
  | Userdata -> failwith " Userdata "
  | Valuenameref -> failwith " Valuenameref "
  | Variable -> failwith " Variable "
  | Version -> failwith " Version "
  | View -> failwith " View "
  | Viewlist -> failwith " Viewlist "
  | Viewmap -> failwith " Viewmap "
  | Viewref -> failwith " Viewref "
  | Viewtype -> failwith " Viewtype "
  | Visible -> failwith " Visible "
  | Voltagemap -> failwith " Voltagemap "
  | Wavevalue -> failwith " Wavevalue "
  | Weak -> failwith " Weak "
  | Weakjoined -> failwith " Weakjoined "
  | When -> failwith " When "
  | While -> failwith " While "
  | Written -> failwith " Written "
  | Xcoord -> failwith " Xcoord "
  | Xor -> failwith " Xor "
  | Ycoord -> failwith " Ycoord "
  | ILLEGAL _ -> failwith " ILLEGAL _ "
  | ID _ -> failwith " ID _ "
  | TLIST _ -> failwith " TLIST _ "
  | TLIST2 _ -> failwith " TLIST2 _ "
  | STRING _ -> failwith " STRING _ "
  | INT _ -> failwith " INT _ "
  | ENDOFFILE -> failwith " ENDOFFILE "
  | EOL -> failwith " EOL "
  | LPAREN -> failwith " LPAREN "
  | RPAREN -> failwith " RPAREN "
  | EMPTY -> failwith " EMPTY "

  | ITEM2(Abs, _, _) -> failwith " ITEM2(Abs, _, _) "
  | ITEM2(Acload, _, _) -> failwith " ITEM2(Acload, _, _) "
  | ITEM2(After, _, _) -> failwith " ITEM2(After, _, _) "
  | ITEM2(And, _, _) -> failwith " ITEM2(And, _, _) "
  | ITEM2(Annotate, _, _) -> failwith " ITEM2(Annotate, _, _) "
  | ITEM2(Apply, _, _) -> failwith " ITEM2(Apply, _, _) "
  | ITEM2(Arc, _, _) -> failwith " ITEM2(Arc, _, _) "
  | ITEM2(Array, _, _) -> failwith " ITEM2(Array, _, _) "
  | ITEM2(Arraymacro, _, _) -> failwith " ITEM2(Arraymacro, _, _) "
  | ITEM2(Arrayrelatedinfo, _, _) -> failwith " ITEM2(Arrayrelatedinfo, _, _) "
  | ITEM2(Arraysite, _, _) -> failwith " ITEM2(Arraysite, _, _) "
  | ITEM2(Assign, _, _) -> failwith " ITEM2(Assign, _, _) "
  | ITEM2(Atleast, _, _) -> failwith " ITEM2(Atleast, _, _) "
  | ITEM2(Atmost, _, _) -> failwith " ITEM2(Atmost, _, _) "
  | ITEM2(Author, _, _) -> failwith " ITEM2(Author, _, _) "
  | ITEM2(Basearray, _, _) -> failwith " ITEM2(Basearray, _, _) "
  | ITEM2(Becomes, _, _) -> failwith " ITEM2(Becomes, _, _) "
  | ITEM2(Between, _, _) -> failwith " ITEM2(Between, _, _) "
  | ITEM2(Block, _, _) -> failwith " ITEM2(Block, _, _) "
  | ITEM2(Boolean, _, _) -> failwith " ITEM2(Boolean, _, _) "
  | ITEM2(Booleandisplay, _, _) -> failwith " ITEM2(Booleandisplay, _, _) "
  | ITEM2(Booleanmap, _, _) -> failwith " ITEM2(Booleanmap, _, _) "
  | ITEM2(Booleanvalue, _, _) -> failwith " ITEM2(Booleanvalue, _, _) "
  | ITEM2(Borderpattern, _, _) -> failwith " ITEM2(Borderpattern, _, _) "
  | ITEM2(Borderwidth, _, _) -> failwith " ITEM2(Borderwidth, _, _) "
  | ITEM2(Boundingbox, _, _) -> failwith " ITEM2(Boundingbox, _, _) "
  | ITEM2(Ceiling, _, _) -> failwith " ITEM2(Ceiling, _, _) "
  | ITEM2(Cell, _, _) -> failwith " ITEM2(Cell, _, _) "
  | ITEM2(Cellref, _, _) -> failwith " ITEM2(Cellref, _, _) "
  | ITEM2(Celltype, _, _) -> failwith " ITEM2(Celltype, _, _) "
  | ITEM2(Change, _, _) -> failwith " ITEM2(Change, _, _) "
  | ITEM2(Circle, _, _) -> failwith " ITEM2(Circle, _, _) "
  | ITEM2(Color, _, _) -> failwith " ITEM2(Color, _, _) "
  | ITEM2(Comment, _, _) -> failwith " ITEM2(Comment, _, _) "
  | ITEM2(Commentgraphics, _, _) -> failwith " ITEM2(Commentgraphics, _, _) "
  | ITEM2(Compound, _, _) -> failwith " ITEM2(Compound, _, _) "
  | ITEM2(Concat, _, _) -> failwith " ITEM2(Concat, _, _) "
  | ITEM2(Connectlocation, _, _) -> failwith " ITEM2(Connectlocation, _, _) "
  | ITEM2(Constant, _, _) -> failwith " ITEM2(Constant, _, _) "
  | ITEM2(Constraint, _, _) -> failwith " ITEM2(Constraint, _, _) "
  | ITEM2(Contents, _, _) -> failwith " ITEM2(Contents, _, _) "
  | ITEM2(Cornertype, _, _) -> failwith " ITEM2(Cornertype, _, _) "
  | ITEM2(Criticality, _, _) -> failwith " ITEM2(Criticality, _, _) "
  | ITEM2(Currentmap, _, _) -> failwith " ITEM2(Currentmap, _, _) "
  | ITEM2(Curve, _, _) -> failwith " ITEM2(Curve, _, _) "
  | ITEM2(Cycle, _, _) -> failwith " ITEM2(Cycle, _, _) "
  | ITEM2(Dataorigin, _, _) -> failwith " ITEM2(Dataorigin, _, _) "
  | ITEM2(Dcfaninload, _, _) -> failwith " ITEM2(Dcfaninload, _, _) "
  | ITEM2(Dcfanoutload, _, _) -> failwith " ITEM2(Dcfanoutload, _, _) "
  | ITEM2(Dcmaxfanin, _, _) -> failwith " ITEM2(Dcmaxfanin, _, _) "
  | ITEM2(Dcmaxfanout, _, _) -> failwith " ITEM2(Dcmaxfanout, _, _) "
  | ITEM2(Delay, _, _) -> failwith " ITEM2(Delay, _, _) "
  | ITEM2(Delta, _, _) -> failwith " ITEM2(Delta, _, _) "
  | ITEM2(Derivation, _, _) -> failwith " ITEM2(Derivation, _, _) "
  | ITEM2(Design, _, _) -> failwith " ITEM2(Design, _, _) "
  | ITEM2(Designator, _, _) -> failwith " ITEM2(Designator, _, _) "
  | ITEM2(Difference, _, _) -> failwith " ITEM2(Difference, _, _) "
  | ITEM2(Direction, _, _) -> failwith " ITEM2(Direction, _, _) "
  | ITEM2(Display, _, _) -> failwith " ITEM2(Display, _, _) "
  | ITEM2(Divide, _, _) -> failwith " ITEM2(Divide, _, _) "
  | ITEM2(Dominates, _, _) -> failwith " ITEM2(Dominates, _, _) "
  | ITEM2(Dot, _, _) -> failwith " ITEM2(Dot, _, _) "
  | ITEM2(Duration, _, _) -> failwith " ITEM2(Duration, _, _) "
  | ITEM2(E, _, _) -> failwith " ITEM2(E, _, _) "
  | ITEM2(Edif, _, _) -> failwith " ITEM2(Edif, _, _) "
  | ITEM2(Ediflevel, _, _) -> failwith " ITEM2(Ediflevel, _, _) "
  | ITEM2(Edifversion, _, _) -> failwith " ITEM2(Edifversion, _, _) "
  | ITEM2(Else, _, _) -> failwith " ITEM2(Else, _, _) "
  | ITEM2(Enclosuredistance, _, _) ->
      failwith " ITEM2(Enclosuredistance, _, _) "
  | ITEM2(Endtype, _, _) -> failwith " ITEM2(Endtype, _, _) "
  | ITEM2(Entry, _, _) -> failwith " ITEM2(Entry, _, _) "
  | ITEM2(Equal, _, _) -> failwith " ITEM2(Equal, _, _) "
  | ITEM2(Escape, _, _) -> failwith " ITEM2(Escape, _, _) "
  | ITEM2(Event, _, _) -> failwith " ITEM2(Event, _, _) "
  | ITEM2(Exactly, _, _) -> failwith " ITEM2(Exactly, _, _) "
  | ITEM2(External, _, _) -> failwith " ITEM2(External, _, _) "
  | ITEM2(Fabricate, _, _) -> failwith " ITEM2(Fabricate, _, _) "
  | ITEM2(False, _, _) -> failwith " ITEM2(False, _, _) "
  | ITEM2(Figure, _, _) -> failwith " ITEM2(Figure, _, _) "
  | ITEM2(Figurearea, _, _) -> failwith " ITEM2(Figurearea, _, _) "
  | ITEM2(Figuregroup, _, _) -> failwith " ITEM2(Figuregroup, _, _) "
  | ITEM2(Figuregroupobject, _, _) ->
      failwith " ITEM2(Figuregroupobject, _, _) "
  | ITEM2(Figuregroupoverride, _, _) ->
      failwith " ITEM2(Figuregroupoverride, _, _) "
  | ITEM2(Figuregroupref, _, _) -> failwith " ITEM2(Figuregroupref, _, _) "
  | ITEM2(Figureperimeter, _, _) -> failwith " ITEM2(Figureperimeter, _, _) "
  | ITEM2(Figurewidth, _, _) -> failwith " ITEM2(Figurewidth, _, _) "
  | ITEM2(Fillpattern, _, _) -> failwith " ITEM2(Fillpattern, _, _) "
  | ITEM2(Fix, _, _) -> failwith " ITEM2(Fix, _, _) "
  | ITEM2(Floor, _, _) -> failwith " ITEM2(Floor, _, _) "
  | ITEM2(Follow, _, _) -> failwith " ITEM2(Follow, _, _) "
  | ITEM2(Forbiddenevent, _, _) -> failwith " ITEM2(Forbiddenevent, _, _) "
  | ITEM2(Form, _, _) -> failwith " ITEM2(Form, _, _) "
  | ITEM2(Globalportref, _, _) -> failwith " ITEM2(Globalportref, _, _) "
  | ITEM2(Greaterthan, _, _) -> failwith " ITEM2(Greaterthan, _, _) "
  | ITEM2(Gridmap, _, _) -> failwith " ITEM2(Gridmap, _, _) "
  | ITEM2(If, _, _) -> failwith " ITEM2(If, _, _) "
  | ITEM2(Ignore, _, _) -> failwith " ITEM2(Ignore, _, _) "
  | ITEM2(Includefiguregroup, _, _) ->
      failwith " ITEM2(Includefiguregroup, _, _) "
  | ITEM2(Increasing, _, _) -> failwith " ITEM2(Increasing, _, _) "
  | ITEM2(Initial, _, _) -> failwith " ITEM2(Initial, _, _) "
  | ITEM2(Instance, arg1, arg2) -> failwith (" ITEM2(Instance, ) ")
  | ITEM2(Instancebackannotate, _, _) ->
      failwith " ITEM2(Instancebackannotate, _, _) "
  | ITEM2(Instancegroup, _, _) -> failwith " ITEM2(Instancegroup, _, _) "
  | ITEM2(Instancemap, _, _) -> failwith " ITEM2(Instancemap, _, _) "
  | ITEM2(Instancenamedef, _, _) -> failwith " ITEM2(Instancenamedef, _, _) "
  | ITEM2(Instanceref, _, _) -> failwith " ITEM2(Instanceref, _, _) "
  | ITEM2(Integer, _, _) -> failwith " ITEM2(Integer, _, _) "
  | ITEM2(Integerdisplay, _, _) -> failwith " ITEM2(Integerdisplay, _, _) "
  | ITEM2(Interface, _, _) -> failwith " ITEM2(Interface, _, _) "
  | ITEM2(Interfiguregroupspacing, _, _) ->
      failwith " ITEM2(Interfiguregroupspacing, _, _) "
  | ITEM2(Intersection, _, _) -> failwith " ITEM2(Intersection, _, _) "
  | ITEM2(Intrafiguregroupspacing, _, _) ->
      failwith " ITEM2(Intrafiguregroupspacing, _, _) "
  | ITEM2(Inverse, _, _) -> failwith " ITEM2(Inverse, _, _) "
  | ITEM2(Isolated, _, _) -> failwith " ITEM2(Isolated, _, _) "
  | ITEM2(Iterate, _, _) -> failwith " ITEM2(Iterate, _, _) "
  | ITEM2(Joined, _, _) -> failwith " ITEM2(Joined, _, _) "
  | ITEM2(Justify, _, _) -> failwith " ITEM2(Justify, _, _) "
  | ITEM2(Keyworddisplay, _, _) -> failwith " ITEM2(Keyworddisplay, _, _) "
  | ITEM2(Keywordlevel, _, _) -> failwith " ITEM2(Keywordlevel, _, _) "
  | ITEM2(Keywordmap, _, _) -> failwith " ITEM2(Keywordmap, _, _) "
  | ITEM2(Lessthan, _, _) -> failwith " ITEM2(Lessthan, _, _) "
  | ITEM2(Library, _, _) -> failwith " ITEM2(Library, _, _) "
  | ITEM2(Libraryref, _, _) -> failwith " ITEM2(Libraryref, _, _) "
  | ITEM2(Listofnets, _, _) -> failwith " ITEM2(Listofnets, _, _) "
  | ITEM2(Listofports, _, _) -> failwith " ITEM2(Listofports, _, _) "
  | ITEM2(Loaddelay, _, _) -> failwith " ITEM2(Loaddelay, _, _) "
  | ITEM2(Logicassign, _, _) -> failwith " ITEM2(Logicassign, _, _) "
  | ITEM2(Logicinput, _, _) -> failwith " ITEM2(Logicinput, _, _) "
  | ITEM2(Logiclist, _, _) -> failwith " ITEM2(Logiclist, _, _) "
  | ITEM2(Logicmapinput, _, _) -> failwith " ITEM2(Logicmapinput, _, _) "
  | ITEM2(Logicmapoutput, _, _) -> failwith " ITEM2(Logicmapoutput, _, _) "
  | ITEM2(Logiconeof, _, _) -> failwith " ITEM2(Logiconeof, _, _) "
  | ITEM2(Logicoutput, _, _) -> failwith " ITEM2(Logicoutput, _, _) "
  | ITEM2(Logicport, _, _) -> failwith " ITEM2(Logicport, _, _) "
  | ITEM2(Logicref, _, _) -> failwith " ITEM2(Logicref, _, _) "
  | ITEM2(Logicvalue, _, _) -> failwith " ITEM2(Logicvalue, _, _) "
  | ITEM2(Logicwaveform, _, _) -> failwith " ITEM2(Logicwaveform, _, _) "
  | ITEM2(Maintain, _, _) -> failwith " ITEM2(Maintain, _, _) "
  | ITEM2(Match, _, _) -> failwith " ITEM2(Match, _, _) "
  | ITEM2(Max, _, _) -> failwith " ITEM2(Max, _, _) "
  | ITEM2(Member, _, _) -> failwith " ITEM2(Member, _, _) "
  | ITEM2(Min, _, _) -> failwith " ITEM2(Min, _, _) "
  | ITEM2(Minomax, _, _) -> failwith " ITEM2(Minomax, _, _) "
  | ITEM2(Minomaxdisplay, _, _) -> failwith " ITEM2(Minomaxdisplay, _, _) "
  | ITEM2(Mnm, _, _) -> failwith " ITEM2(Mnm, _, _) "
  | ITEM2(Mod, _, _) -> failwith " ITEM2(Mod, _, _) "
  | ITEM2(Multiplevalueset, _, _) -> failwith " ITEM2(Multiplevalueset, _, _) "
  | ITEM2(Mustjoin, _, _) -> failwith " ITEM2(Mustjoin, _, _) "
  | ITEM2(Name, _, _) -> failwith " ITEM2(Name, _, _) "
  | ITEM2(Negate, _, _) -> failwith " ITEM2(Negate, _, _) "
(*
  | ITEM2(Net, _, _) -> failwith " ITEM2(Net, _, _) "
*)
  | ITEM2(Netbackannotate, _, _) -> failwith " ITEM2(Netbackannotate, _, _) "
  | ITEM2(Netbundle, _, _) -> failwith " ITEM2(Netbundle, _, _) "
  | ITEM2(Netdelay, _, _) -> failwith " ITEM2(Netdelay, _, _) "
  | ITEM2(Netgroup, _, _) -> failwith " ITEM2(Netgroup, _, _) "
  | ITEM2(Netmap, _, _) -> failwith " ITEM2(Netmap, _, _) "
  | ITEM2(Netref, _, _) -> failwith " ITEM2(Netref, _, _) "
  | ITEM2(Nochange, _, _) -> failwith " ITEM2(Nochange, _, _) "
  | ITEM2(Nonpermutable, _, _) -> failwith " ITEM2(Nonpermutable, _, _) "
  | ITEM2(Not, _, _) -> failwith " ITEM2(Not, _, _) "
  | ITEM2(Notallowed, _, _) -> failwith " ITEM2(Notallowed, _, _) "
  | ITEM2(Notchspacing, _, _) -> failwith " ITEM2(Notchspacing, _, _) "
  | ITEM2(Number, _, _) -> failwith " ITEM2(Number, _, _) "
  | ITEM2(Numberdefinition, _, _) -> failwith " ITEM2(Numberdefinition, _, _) "
  | ITEM2(Numberdisplay, _, _) -> failwith " ITEM2(Numberdisplay, _, _) "
  | ITEM2(Offpageconnector, _, _) -> failwith " ITEM2(Offpageconnector, _, _) "
  | ITEM2(Offsetevent, _, _) -> failwith " ITEM2(Offsetevent, _, _) "
  | ITEM2(Openshape, _, _) -> failwith " ITEM2(Openshape, _, _) "
  | ITEM2(Or, _, _) -> failwith " ITEM2(Or, _, _) "
  | ITEM2(Orientation, _, _) -> failwith " ITEM2(Orientation, _, _) "
  | ITEM2(Origin, _, _) -> failwith " ITEM2(Origin, _, _) "
  | ITEM2(Overhangdistance, _, _) -> failwith " ITEM2(Overhangdistance, _, _) "
  | ITEM2(Overlapdistance, _, _) -> failwith " ITEM2(Overlapdistance, _, _) "
  | ITEM2(Oversize, _, _) -> failwith " ITEM2(Oversize, _, _) "
  | ITEM2(Owner, _, _) -> failwith " ITEM2(Owner, _, _) "
  | ITEM2(Page, _, _) -> failwith " ITEM2(Page, _, _) "
  | ITEM2(Pagesize, _, _) -> failwith " ITEM2(Pagesize, _, _) "
  | ITEM2(Parameter, _, _) -> failwith " ITEM2(Parameter, _, _) "
  | ITEM2(Parameterassign, _, _) -> failwith " ITEM2(Parameterassign, _, _) "
  | ITEM2(Parameterdisplay, _, _) -> failwith " ITEM2(Parameterdisplay, _, _) "
  | ITEM2(Path, _, _) -> failwith " ITEM2(Path, _, _) "
  | ITEM2(Pathdelay, _, _) -> failwith " ITEM2(Pathdelay, _, _) "
  | ITEM2(Pathwidth, _, _) -> failwith " ITEM2(Pathwidth, _, _) "
  | ITEM2(Permutable, _, _) -> failwith " ITEM2(Permutable, _, _) "
  | ITEM2(Physicaldesignrule, _, _) ->
      failwith " ITEM2(Physicaldesignrule, _, _) "
  | ITEM2(Plug, _, _) -> failwith " ITEM2(Plug, _, _) "
  | ITEM2(Point, _, _) -> failwith " ITEM2(Point, _, _) "
  | ITEM2(Pointdisplay, _, _) -> failwith " ITEM2(Pointdisplay, _, _) "
  | ITEM2(Pointlist, _, _) -> failwith " ITEM2(Pointlist, _, _) "
  | ITEM2(Pointsubtract, _, _) -> failwith " ITEM2(Pointsubtract, _, _) "
  | ITEM2(Pointsum, _, _) -> failwith " ITEM2(Pointsum, _, _) "
  | ITEM2(Polygon, _, _) -> failwith " ITEM2(Polygon, _, _) "
  | ITEM2(Port, _, _) -> failwith " ITEM2(Port, _, _) "
  | ITEM2(Portbackannotate, _, _) -> failwith " ITEM2(Portbackannotate, _, _) "
  | ITEM2(Portbundle, _, _) -> failwith " ITEM2(Portbundle, _, _) "
  | ITEM2(Portdelay, _, _) -> failwith " ITEM2(Portdelay, _, _) "
  | ITEM2(Portgroup, _, _) -> failwith " ITEM2(Portgroup, _, _) "
  | ITEM2(Portimplementation, _, _) ->
      failwith " ITEM2(Portimplementation, _, _) "
  | ITEM2(Portinstance, _, _) -> failwith " ITEM2(Portinstance, _, _) "
  | ITEM2(Portlist, _, _) -> failwith " ITEM2(Portlist, _, _) "
  | ITEM2(Portlistalias, _, _) -> failwith " ITEM2(Portlistalias, _, _) "
  | ITEM2(Portmap, _, _) -> failwith " ITEM2(Portmap, _, _) "
  | ITEM2(Portref, _, _) -> failwith " ITEM2(Portref, _, _) "
  | ITEM2(Product, _, _) -> failwith " ITEM2(Product, _, _) "
  | ITEM2(Program, _, _) -> failwith " ITEM2(Program, _, _) "
  | ITEM2(Property, _, _) -> failwith " ITEM2(Property, _, _) "
  | ITEM2(Propertydisplay, _, _) -> failwith " ITEM2(Propertydisplay, _, _) "
  | ITEM2(Protectionframe, _, _) -> failwith " ITEM2(Protectionframe, _, _) "
  | ITEM2(Pt, _, _) -> failwith " ITEM2(Pt, _, _) "
  | ITEM2(Rangevector, _, _) -> failwith " ITEM2(Rangevector, _, _) "
  | ITEM2(Rectangle, _, _) -> failwith " ITEM2(Rectangle, _, _) "
  | ITEM2(Rectanglesize, _, _) -> failwith " ITEM2(Rectanglesize, _, _) "
  | ITEM2(Rename, _, _) -> failwith " ITEM2(Rename, _, _) "
  | ITEM2(Resolves, _, _) -> failwith " ITEM2(Resolves, _, _) "
  | ITEM2(Scale, _, _) -> failwith " ITEM2(Scale, _, _) "
  | ITEM2(Scalex, _, _) -> failwith " ITEM2(Scalex, _, _) "
  | ITEM2(Scaley, _, _) -> failwith " ITEM2(Scaley, _, _) "
  | ITEM2(Section, _, _) -> failwith " ITEM2(Section, _, _) "
  | ITEM2(Shape, _, _) -> failwith " ITEM2(Shape, _, _) "
  | ITEM2(Simulate, _, _) -> failwith " ITEM2(Simulate, _, _) "
  | ITEM2(Simulationinfo, _, _) -> failwith " ITEM2(Simulationinfo, _, _) "
  | ITEM2(Singlevalueset, _, _) -> failwith " ITEM2(Singlevalueset, _, _) "
  | ITEM2(Site, _, _) -> failwith " ITEM2(Site, _, _) "
  | ITEM2(Socket, _, _) -> failwith " ITEM2(Socket, _, _) "
  | ITEM2(Socketset, _, _) -> failwith " ITEM2(Socketset, _, _) "
  | ITEM2(Statement, _, _) -> failwith " ITEM2(Statement, _, _) "
  | ITEM2(Status, _, _) -> failwith " ITEM2(Status, _, _) "
  | ITEM2(Steady, _, _) -> failwith " ITEM2(Steady, _, _) "
  | ITEM2(Strictlyincreasing, _, _) ->
      failwith " ITEM2(Strictlyincreasing, _, _) "
  | ITEM2(String, _, _) -> failwith " ITEM2(String, _, _) "
  | ITEM2(Stringdisplay, _, _) -> failwith " ITEM2(Stringdisplay, _, _) "
  | ITEM2(Strong, _, _) -> failwith " ITEM2(Strong, _, _) "
  | ITEM2(Subtract, _, _) -> failwith " ITEM2(Subtract, _, _) "
  | ITEM2(Sum, _, _) -> failwith " ITEM2(Sum, _, _) "
  | ITEM2(Symbol, _, _) -> failwith " ITEM2(Symbol, _, _) "
  | ITEM2(Symmetry, _, _) -> failwith " ITEM2(Symmetry, _, _) "
  | ITEM2(Table, _, _) -> failwith " ITEM2(Table, _, _) "
  | ITEM2(Tabledefault, _, _) -> failwith " ITEM2(Tabledefault, _, _) "
  | ITEM2(Technology, _, _) -> failwith " ITEM2(Technology, _, _) "
  | ITEM2(Textheight, _, _) -> failwith " ITEM2(Textheight, _, _) "
  | ITEM2(Then, _, _) -> failwith " ITEM2(Then, _, _) "
  | ITEM2(Timeinterval, _, _) -> failwith " ITEM2(Timeinterval, _, _) "
  | ITEM2(Timestamp, _, _) -> failwith " ITEM2(Timestamp, _, _) "
  | ITEM2(Timing, _, _) -> failwith " ITEM2(Timing, _, _) "
  | ITEM2(Transform, _, _) -> failwith " ITEM2(Transform, _, _) "
  | ITEM2(Transition, _, _) -> failwith " ITEM2(Transition, _, _) "
  | ITEM2(Trigger, _, _) -> failwith " ITEM2(Trigger, _, _) "
  | ITEM2(True, _, _) -> failwith " ITEM2(True, _, _) "
  | ITEM2(Typedvalue, _, _) -> failwith " ITEM2(Typedvalue, _, _) "
  | ITEM2(Unconstrained, _, _) -> failwith " ITEM2(Unconstrained, _, _) "
  | ITEM2(Undefined, _, _) -> failwith " ITEM2(Undefined, _, _) "
  | ITEM2(Union, _, _) -> failwith " ITEM2(Union, _, _) "
  | ITEM2(Unit, _, _) -> failwith " ITEM2(Unit, _, _) "
  | ITEM2(Unused, _, _) -> failwith " ITEM2(Unused, _, _) "
  | ITEM2(Userdata, _, _) -> failwith " ITEM2(Userdata, _, _) "
  | ITEM2(Valuenameref, _, _) -> failwith " ITEM2(Valuenameref, _, _) "
  | ITEM2(Variable, _, _) -> failwith " ITEM2(Variable, _, _) "
  | ITEM2(Version, _, _) -> failwith " ITEM2(Version, _, _) "
  | ITEM2(View, _, _) -> failwith " ITEM2(View, _, _) "
  | ITEM2(Viewlist, _, _) -> failwith " ITEM2(Viewlist, _, _) "
  | ITEM2(Viewmap, _, _) -> failwith " ITEM2(Viewmap, _, _) "
  | ITEM2(Viewref, _, _) -> failwith " ITEM2(Viewref, _, _) "
  | ITEM2(Viewtype, _, _) -> failwith " ITEM2(Viewtype, _, _) "
  | ITEM2(Visible, _, _) -> failwith " ITEM2(Visible, _, _) "
  | ITEM2(Voltagemap, _, _) -> failwith " ITEM2(Voltagemap, _, _) "
  | ITEM2(Wavevalue, _, _) -> failwith " ITEM2(Wavevalue, _, _) "
  | ITEM2(Weak, _, _) -> failwith " ITEM2(Weak, _, _) "
  | ITEM2(Weakjoined, _, _) -> failwith " ITEM2(Weakjoined, _, _) "
  | ITEM2(When, _, _) -> failwith " ITEM2(When, _, _) "
  | ITEM2(While, _, _) -> failwith " ITEM2(While, _, _) "
  | ITEM2(Written, _, _) -> failwith " ITEM2(Written, _, _) "
  | ITEM2(Xcoord, _, _) -> failwith " ITEM2(Xcoord, _, _) "
  | ITEM2(Xor, _, _) -> failwith " ITEM2(Xor, _, _) "
  | ITEM2(Ycoord, _, _) -> failwith " ITEM2(Ycoord, _, _) "
  | ITEM2(ILLEGAL _, _, _) -> failwith " ITEM2(ILLEGAL _, _, _) "
  | ITEM2(ID _, _, _) -> failwith " ITEM2(ID _, _, _) "
  | ITEM2(TLIST _, _, _) -> failwith " ITEM2(TLIST _, _, _) "
  | ITEM2(TLIST2 _, _, _) -> failwith " ITEM2(TLIST2 _, _, _) "
  | ITEM2(STRING _, _, _) -> failwith " ITEM2(STRING _, _, _) "
  | ITEM2(INT _, _, _) -> failwith " ITEM2(INT _, _, _) "
  | ITEM2(ENDOFFILE, _, _) -> failwith " ITEM2(ENDOFFILE, _, _) "
  | ITEM2(EOL, _, _) -> failwith " ITEM2(EOL, _, _) "
  | ITEM2(LPAREN, _, _) -> failwith " ITEM2(LPAREN, _, _) "
  | ITEM2(RPAREN, _, _) -> failwith " ITEM2(RPAREN, _, _) "
  | ITEM2(EMPTY, _, _) -> failwith " ITEM2(EMPTY, _, _) "

  | ITEM(Abs, _) -> failwith " ITEM(Abs, _) "
  | ITEM(Acload, _) -> failwith " ITEM(Acload, _) "
  | ITEM(After, _) -> failwith " ITEM(After, _) "
  | ITEM(And, _) -> failwith " ITEM(And, _) "
  | ITEM(Annotate, _) -> failwith " ITEM(Annotate, _) "
  | ITEM(Apply, _) -> failwith " ITEM(Apply, _) "
  | ITEM(Arc, _) -> failwith " ITEM(Arc, _) "
  | ITEM(Array, _) -> failwith " ITEM(Array, _) "
  | ITEM(Arraymacro, _) -> failwith " ITEM(Arraymacro, _) "
  | ITEM(Arrayrelatedinfo, _) -> failwith " ITEM(Arrayrelatedinfo, _) "
  | ITEM(Arraysite, _) -> failwith " ITEM(Arraysite, _) "
  | ITEM(Assign, _) -> failwith " ITEM(Assign, _) "
  | ITEM(Atleast, _) -> failwith " ITEM(Atleast, _) "
  | ITEM(Atmost, _) -> failwith " ITEM(Atmost, _) "
  | ITEM(Author, _) -> failwith " ITEM(Author, _) "
  | ITEM(Basearray, _) -> failwith " ITEM(Basearray, _) "
  | ITEM(Becomes, _) -> failwith " ITEM(Becomes, _) "
  | ITEM(Between, _) -> failwith " ITEM(Between, _) "
  | ITEM(Block, _) -> failwith " ITEM(Block, _) "
  | ITEM(Boolean, _) -> failwith " ITEM(Boolean, _) "
  | ITEM(Booleandisplay, _) -> failwith " ITEM(Booleandisplay, _) "
  | ITEM(Booleanmap, _) -> failwith " ITEM(Booleanmap, _) "
  | ITEM(Booleanvalue, _) -> failwith " ITEM(Booleanvalue, _) "
  | ITEM(Borderpattern, _) -> failwith " ITEM(Borderpattern, _) "
  | ITEM(Borderwidth, _) -> failwith " ITEM(Borderwidth, _) "
  | ITEM(Boundingbox, _) -> failwith " ITEM(Boundingbox, _) "
  | ITEM(Ceiling, _) -> failwith " ITEM(Ceiling, _) "
  | ITEM(Cell, _) -> failwith " ITEM(Cell, _) "
  | ITEM(Cellref, _) -> failwith " ITEM(Cellref, _) "
  | ITEM(Celltype, _) -> failwith " ITEM(Celltype, _) "
  | ITEM(Change, _) -> failwith " ITEM(Change, _) "
  | ITEM(Circle, _) -> failwith " ITEM(Circle, _) "
  | ITEM(Color, _) -> failwith " ITEM(Color, _) "
  | ITEM(Comment, _) -> 32
  | ITEM(Commentgraphics, _) -> failwith " ITEM(Commentgraphics, _) "
  | ITEM(Compound, _) -> failwith " ITEM(Compound, _) "
  | ITEM(Concat, _) -> failwith " ITEM(Concat, _) "
  | ITEM(Connectlocation, _) -> failwith " ITEM(Connectlocation, _) "
  | ITEM(Constant, _) -> failwith " ITEM(Constant, _) "
  | ITEM(Constraint, _) -> failwith " ITEM(Constraint, _) "
  | ITEM(Contents, _) -> failwith " ITEM(Contents, _) "
  | ITEM(Cornertype, _) -> failwith " ITEM(Cornertype, _) "
  | ITEM(Criticality, _) -> failwith " ITEM(Criticality, _) "
  | ITEM(Currentmap, _) -> failwith " ITEM(Currentmap, _) "
  | ITEM(Curve, _) -> failwith " ITEM(Curve, _) "
  | ITEM(Cycle, _) -> failwith " ITEM(Cycle, _) "
  | ITEM(Dataorigin, _) -> failwith " ITEM(Dataorigin, _) "
  | ITEM(Dcfaninload, _) -> failwith " ITEM(Dcfaninload, _) "
  | ITEM(Dcfanoutload, _) -> failwith " ITEM(Dcfanoutload, _) "
  | ITEM(Dcmaxfanin, _) -> failwith " ITEM(Dcmaxfanin, _) "
  | ITEM(Dcmaxfanout, _) -> failwith " ITEM(Dcmaxfanout, _) "
  | ITEM(Delay, _) -> failwith " ITEM(Delay, _) "
  | ITEM(Delta, _) -> failwith " ITEM(Delta, _) "
  | ITEM(Derivation, _) -> failwith " ITEM(Derivation, _) "
  | ITEM(Design, _) -> failwith " ITEM(Design, _) "
  | ITEM(Designator, _) -> failwith " ITEM(Designator, _) "
  | ITEM(Difference, _) -> failwith " ITEM(Difference, _) "
  | ITEM(Direction, _) -> failwith " ITEM(Direction, _) "
  | ITEM(Display, _) -> failwith " ITEM(Display, _) "
  | ITEM(Divide, _) -> failwith " ITEM(Divide, _) "
  | ITEM(Dominates, _) -> failwith " ITEM(Dominates, _) "
  | ITEM(Dot, _) -> failwith " ITEM(Dot, _) "
  | ITEM(Duration, _) -> failwith " ITEM(Duration, _) "
  | ITEM(E, _) -> failwith " ITEM(E, _) "
  | ITEM(Edif, _) -> failwith " ITEM(Edif, _) "
  | ITEM(Ediflevel, _) -> failwith " ITEM(Ediflevel, _) "
  | ITEM(Edifversion, _) -> failwith " ITEM(Edifversion, _) "
  | ITEM(Else, _) -> failwith " ITEM(Else, _) "
  | ITEM(Enclosuredistance, _) -> failwith " ITEM(Enclosuredistance, _) "
  | ITEM(Endtype, _) -> failwith " ITEM(Endtype, _) "
  | ITEM(Entry, _) -> failwith " ITEM(Entry, _) "
  | ITEM(Equal, _) -> failwith " ITEM(Equal, _) "
  | ITEM(Escape, _) -> failwith " ITEM(Escape, _) "
  | ITEM(Event, _) -> failwith " ITEM(Event, _) "
  | ITEM(Exactly, _) -> failwith " ITEM(Exactly, _) "
  | ITEM(External, _) -> failwith " ITEM(External, _) "
  | ITEM(Fabricate, _) -> failwith " ITEM(Fabricate, _) "
  | ITEM(False, _) -> failwith " ITEM(False, _) "
  | ITEM(Figure, _) -> failwith " ITEM(Figure, _) "
  | ITEM(Figurearea, _) -> failwith " ITEM(Figurearea, _) "
  | ITEM(Figuregroup, _) -> failwith " ITEM(Figuregroup, _) "
  | ITEM(Figuregroupobject, _) -> failwith " ITEM(Figuregroupobject, _) "
  | ITEM(Figuregroupoverride, _) -> failwith " ITEM(Figuregroupoverride, _) "
  | ITEM(Figuregroupref, _) -> failwith " ITEM(Figuregroupref, _) "
  | ITEM(Figureperimeter, _) -> failwith " ITEM(Figureperimeter, _) "
  | ITEM(Figurewidth, _) -> failwith " ITEM(Figurewidth, _) "
  | ITEM(Fillpattern, _) -> failwith " ITEM(Fillpattern, _) "
  | ITEM(Fix, _) -> failwith " ITEM(Fix, _) "
  | ITEM(Floor, _) -> failwith " ITEM(Floor, _) "
  | ITEM(Follow, _) -> failwith " ITEM(Follow, _) "
  | ITEM(Forbiddenevent, _) -> failwith " ITEM(Forbiddenevent, _) "
  | ITEM(Form, _) -> failwith " ITEM(Form, _) "
  | ITEM(Globalportref, _) -> failwith " ITEM(Globalportref, _) "
  | ITEM(Greaterthan, _) -> failwith " ITEM(Greaterthan, _) "
  | ITEM(Gridmap, _) -> failwith " ITEM(Gridmap, _) "
  | ITEM(If, _) -> failwith " ITEM(If, _) "
  | ITEM(Ignore, _) -> failwith " ITEM(Ignore, _) "
  | ITEM(Includefiguregroup, _) -> failwith " ITEM(Includefiguregroup, _) "
  | ITEM(Increasing, _) -> failwith " ITEM(Increasing, _) "
  | ITEM(Initial, _) -> failwith " ITEM(Initial, _) "
  | ITEM(Instance, _) -> failwith " ITEM(Instance, _) "
  | ITEM(Instancebackannotate, _) -> failwith " ITEM(Instancebackannotate, _) "
  | ITEM(Instancegroup, _) -> failwith " ITEM(Instancegroup, _) "
  | ITEM(Instancemap, _) -> failwith " ITEM(Instancemap, _) "
  | ITEM(Instancenamedef, _) -> failwith " ITEM(Instancenamedef, _) "
  | ITEM(Instanceref, _) -> failwith " ITEM(Instanceref, _) "
  | ITEM(Integer, _) -> failwith " ITEM(Integer, _) "
  | ITEM(Integerdisplay, _) -> failwith " ITEM(Integerdisplay, _) "
  | ITEM(Interface, _) -> failwith " ITEM(Interface, _) "
  | ITEM(Interfiguregroupspacing, _) ->
      failwith " ITEM(Interfiguregroupspacing, _) "
  | ITEM(Intersection, _) -> failwith " ITEM(Intersection, _) "
  | ITEM(Intrafiguregroupspacing, _) ->
      failwith " ITEM(Intrafiguregroupspacing, _) "
  | ITEM(Inverse, _) -> failwith " ITEM(Inverse, _) "
  | ITEM(Isolated, _) -> failwith " ITEM(Isolated, _) "
  | ITEM(Iterate, _) -> failwith " ITEM(Iterate, _) "
  | ITEM(Joined, _) -> failwith " ITEM(Joined, _) "
  | ITEM(Justify, _) -> failwith " ITEM(Justify, _) "
  | ITEM(Keyworddisplay, _) -> failwith " ITEM(Keyworddisplay, _) "
  | ITEM(Keywordlevel, _) -> failwith " ITEM(Keywordlevel, _) "
  | ITEM(Keywordmap, _) -> failwith " ITEM(Keywordmap, _) "
  | ITEM(Lessthan, _) -> failwith " ITEM(Lessthan, _) "
  | ITEM(Library, _) -> failwith " ITEM(Library, _) "
  | ITEM(Libraryref, _) -> failwith " ITEM(Libraryref, _) "
  | ITEM(Listofnets, _) -> failwith " ITEM(Listofnets, _) "
  | ITEM(Listofports, _) -> failwith " ITEM(Listofports, _) "
  | ITEM(Loaddelay, _) -> failwith " ITEM(Loaddelay, _) "
  | ITEM(Logicassign, _) -> failwith " ITEM(Logicassign, _) "
  | ITEM(Logicinput, _) -> failwith " ITEM(Logicinput, _) "
  | ITEM(Logiclist, _) -> failwith " ITEM(Logiclist, _) "
  | ITEM(Logicmapinput, _) -> failwith " ITEM(Logicmapinput, _) "
  | ITEM(Logicmapoutput, _) -> failwith " ITEM(Logicmapoutput, _) "
  | ITEM(Logiconeof, _) -> failwith " ITEM(Logiconeof, _) "
  | ITEM(Logicoutput, _) -> failwith " ITEM(Logicoutput, _) "
  | ITEM(Logicport, _) -> failwith " ITEM(Logicport, _) "
  | ITEM(Logicref, _) -> failwith " ITEM(Logicref, _) "
  | ITEM(Logicvalue, _) -> failwith " ITEM(Logicvalue, _) "
  | ITEM(Logicwaveform, _) -> failwith " ITEM(Logicwaveform, _) "
  | ITEM(Maintain, _) -> failwith " ITEM(Maintain, _) "
  | ITEM(Match, _) -> failwith " ITEM(Match, _) "
  | ITEM(Max, _) -> failwith " ITEM(Max, _) "
  | ITEM(Member, _) -> failwith " ITEM(Member, _) "
  | ITEM(Min, _) -> failwith " ITEM(Min, _) "
  | ITEM(Minomax, _) -> failwith " ITEM(Minomax, _) "
  | ITEM(Minomaxdisplay, _) -> failwith " ITEM(Minomaxdisplay, _) "
  | ITEM(Mnm, _) -> failwith " ITEM(Mnm, _) "
  | ITEM(Mod, _) -> failwith " ITEM(Mod, _) "
  | ITEM(Multiplevalueset, _) -> failwith " ITEM(Multiplevalueset, _) "
  | ITEM(Mustjoin, _) -> failwith " ITEM(Mustjoin, _) "
  | ITEM(Name, _) -> failwith " ITEM(Name, _) "
  | ITEM(Negate, _) -> failwith " ITEM(Negate, _) "
  | ITEM(Net, _) -> failwith " ITEM(Net, _) "
  | ITEM(Netbackannotate, _) -> failwith " ITEM(Netbackannotate, _) "
  | ITEM(Netbundle, _) -> failwith " ITEM(Netbundle, _) "
  | ITEM(Netdelay, _) -> failwith " ITEM(Netdelay, _) "
  | ITEM(Netgroup, _) -> failwith " ITEM(Netgroup, _) "
  | ITEM(Netmap, _) -> failwith " ITEM(Netmap, _) "
  | ITEM(Netref, _) -> failwith " ITEM(Netref, _) "
  | ITEM(Nochange, _) -> failwith " ITEM(Nochange, _) "
  | ITEM(Nonpermutable, _) -> failwith " ITEM(Nonpermutable, _) "
  | ITEM(Not, _) -> failwith " ITEM(Not, _) "
  | ITEM(Notallowed, _) -> failwith " ITEM(Notallowed, _) "
  | ITEM(Notchspacing, _) -> failwith " ITEM(Notchspacing, _) "
  | ITEM(Number, _) -> failwith " ITEM(Number, _) "
  | ITEM(Numberdefinition, _) -> failwith " ITEM(Numberdefinition, _) "
  | ITEM(Numberdisplay, _) -> failwith " ITEM(Numberdisplay, _) "
  | ITEM(Offpageconnector, _) -> failwith " ITEM(Offpageconnector, _) "
  | ITEM(Offsetevent, _) -> failwith " ITEM(Offsetevent, _) "
  | ITEM(Openshape, _) -> failwith " ITEM(Openshape, _) "
  | ITEM(Or, _) -> failwith " ITEM(Or, _) "
  | ITEM(Orientation, _) -> failwith " ITEM(Orientation, _) "
  | ITEM(Origin, _) -> failwith " ITEM(Origin, _) "
  | ITEM(Overhangdistance, _) -> failwith " ITEM(Overhangdistance, _) "
  | ITEM(Overlapdistance, _) -> failwith " ITEM(Overlapdistance, _) "
  | ITEM(Oversize, _) -> failwith " ITEM(Oversize, _) "
  | ITEM(Owner, _) -> failwith " ITEM(Owner, _) "
  | ITEM(Page, _) -> failwith " ITEM(Page, _) "
  | ITEM(Pagesize, _) -> failwith " ITEM(Pagesize, _) "
  | ITEM(Parameter, _) -> failwith " ITEM(Parameter, _) "
  | ITEM(Parameterassign, _) -> failwith " ITEM(Parameterassign, _) "
  | ITEM(Parameterdisplay, _) -> failwith " ITEM(Parameterdisplay, _) "
  | ITEM(Path, _) -> failwith " ITEM(Path, _) "
  | ITEM(Pathdelay, _) -> failwith " ITEM(Pathdelay, _) "
  | ITEM(Pathwidth, _) -> failwith " ITEM(Pathwidth, _) "
  | ITEM(Permutable, _) -> failwith " ITEM(Permutable, _) "
  | ITEM(Physicaldesignrule, _) -> failwith " ITEM(Physicaldesignrule, _) "
  | ITEM(Plug, _) -> failwith " ITEM(Plug, _) "
  | ITEM(Point, _) -> failwith " ITEM(Point, _) "
  | ITEM(Pointdisplay, _) -> failwith " ITEM(Pointdisplay, _) "
  | ITEM(Pointlist, _) -> failwith " ITEM(Pointlist, _) "
  | ITEM(Pointsubtract, _) -> failwith " ITEM(Pointsubtract, _) "
  | ITEM(Pointsum, _) -> failwith " ITEM(Pointsum, _) "
  | ITEM(Polygon, _) -> failwith " ITEM(Polygon, _) "
  | ITEM(Port, _) -> failwith " ITEM(Port, _) "
  | ITEM(Portbackannotate, _) -> failwith " ITEM(Portbackannotate, _) "
  | ITEM(Portbundle, _) -> failwith " ITEM(Portbundle, _) "
  | ITEM(Portdelay, _) -> failwith " ITEM(Portdelay, _) "
  | ITEM(Portgroup, _) -> failwith " ITEM(Portgroup, _) "
  | ITEM(Portimplementation, _) -> failwith " ITEM(Portimplementation, _) "
  | ITEM(Portinstance, _) -> failwith " ITEM(Portinstance, _) "
  | ITEM(Portlist, _) -> failwith " ITEM(Portlist, _) "
  | ITEM(Portlistalias, _) -> failwith " ITEM(Portlistalias, _) "
  | ITEM(Portmap, _) -> failwith " ITEM(Portmap, _) "
  | ITEM(Portref, _) -> failwith " ITEM(Portref, _) "
  | ITEM(Product, _) -> failwith " ITEM(Product, _) "
  | ITEM(Program, _) -> failwith " ITEM(Program, _) "
  | ITEM(Property, _) -> failwith " ITEM(Property, _) "
  | ITEM(Propertydisplay, _) -> failwith " ITEM(Propertydisplay, _) "
  | ITEM(Protectionframe, _) -> failwith " ITEM(Protectionframe, _) "
  | ITEM(Pt, _) -> failwith " ITEM(Pt, _) "
  | ITEM(Rangevector, _) -> failwith " ITEM(Rangevector, _) "
  | ITEM(Rectangle, _) -> failwith " ITEM(Rectangle, _) "
  | ITEM(Rectanglesize, _) -> failwith " ITEM(Rectanglesize, _) "
  | ITEM(Rename, _) -> failwith " ITEM(Rename, _) "
  | ITEM(Resolves, _) -> failwith " ITEM(Resolves, _) "
  | ITEM(Scale, _) -> failwith " ITEM(Scale, _) "
  | ITEM(Scalex, _) -> failwith " ITEM(Scalex, _) "
  | ITEM(Scaley, _) -> failwith " ITEM(Scaley, _) "
  | ITEM(Section, _) -> failwith " ITEM(Section, _) "
  | ITEM(Shape, _) -> failwith " ITEM(Shape, _) "
  | ITEM(Simulate, _) -> failwith " ITEM(Simulate, _) "
  | ITEM(Simulationinfo, _) -> failwith " ITEM(Simulationinfo, _) "
  | ITEM(Singlevalueset, _) -> failwith " ITEM(Singlevalueset, _) "
  | ITEM(Site, _) -> failwith " ITEM(Site, _) "
  | ITEM(Socket, _) -> failwith " ITEM(Socket, _) "
  | ITEM(Socketset, _) -> failwith " ITEM(Socketset, _) "
  | ITEM(Statement, _) -> failwith " ITEM(Statement, _) "
  | ITEM(Status, _) -> failwith " ITEM(Status, _) "
  | ITEM(Steady, _) -> failwith " ITEM(Steady, _) "
  | ITEM(Strictlyincreasing, _) -> failwith " ITEM(Strictlyincreasing, _) "
  | ITEM(String, _) -> failwith " ITEM(String, _) "
  | ITEM(Stringdisplay, _) -> failwith " ITEM(Stringdisplay, _) "
  | ITEM(Strong, _) -> failwith " ITEM(Strong, _) "
  | ITEM(Subtract, _) -> failwith " ITEM(Subtract, _) "
  | ITEM(Sum, _) -> failwith " ITEM(Sum, _) "
  | ITEM(Symbol, _) -> failwith " ITEM(Symbol, _) "
  | ITEM(Symmetry, _) -> failwith " ITEM(Symmetry, _) "
  | ITEM(Table, _) -> failwith " ITEM(Table, _) "
  | ITEM(Tabledefault, _) -> failwith " ITEM(Tabledefault, _) "
  | ITEM(Technology, _) -> failwith " ITEM(Technology, _) "
  | ITEM(Textheight, _) -> failwith " ITEM(Textheight, _) "
  | ITEM(Then, _) -> failwith " ITEM(Then, _) "
  | ITEM(Timeinterval, _) -> failwith " ITEM(Timeinterval, _) "
  | ITEM(Timestamp, _) -> failwith " ITEM(Timestamp, _) "
  | ITEM(Timing, _) -> failwith " ITEM(Timing, _) "
  | ITEM(Transform, _) -> failwith " ITEM(Transform, _) "
  | ITEM(Transition, _) -> failwith " ITEM(Transition, _) "
  | ITEM(Trigger, _) -> failwith " ITEM(Trigger, _) "
  | ITEM(True, _) -> failwith " ITEM(True, _) "
  | ITEM(Typedvalue, _) -> failwith " ITEM(Typedvalue, _) "
  | ITEM(Unconstrained, _) -> failwith " ITEM(Unconstrained, _) "
  | ITEM(Undefined, _) -> failwith " ITEM(Undefined, _) "
  | ITEM(Union, _) -> failwith " ITEM(Union, _) "
  | ITEM(Unit, _) -> failwith " ITEM(Unit, _) "
  | ITEM(Unused, _) -> failwith " ITEM(Unused, _) "
  | ITEM(Userdata, _) -> failwith " ITEM(Userdata, _) "
  | ITEM(Valuenameref, _) -> failwith " ITEM(Valuenameref, _) "
  | ITEM(Variable, _) -> failwith " ITEM(Variable, _) "
  | ITEM(Version, _) -> failwith " ITEM(Version, _) "
  | ITEM(View, _) -> failwith " ITEM(View, _) "
  | ITEM(Viewlist, _) -> failwith " ITEM(Viewlist, _) "
  | ITEM(Viewmap, _) -> failwith " ITEM(Viewmap, _) "
  | ITEM(Viewref, _) -> failwith " ITEM(Viewref, _) "
  | ITEM(Viewtype, _) -> failwith " ITEM(Viewtype, _) "
  | ITEM(Visible, _) -> failwith " ITEM(Visible, _) "
  | ITEM(Voltagemap, _) -> failwith " ITEM(Voltagemap, _) "
  | ITEM(Wavevalue, _) -> failwith " ITEM(Wavevalue, _) "
  | ITEM(Weak, _) -> failwith " ITEM(Weak, _) "
  | ITEM(Weakjoined, _) -> failwith " ITEM(Weakjoined, _) "
  | ITEM(When, _) -> failwith " ITEM(When, _) "
  | ITEM(While, _) -> failwith " ITEM(While, _) "
  | ITEM(Written, _) -> failwith " ITEM(Written, _) "
  | ITEM(Xcoord, _) -> failwith " ITEM(Xcoord, _) "
  | ITEM(Xor, _) -> failwith " ITEM(Xor, _) "
  | ITEM(Ycoord, _) -> failwith " ITEM(Ycoord, _) "
  | ITEM(ILLEGAL _, _) -> failwith " ITEM(ILLEGAL _, _) "
  | ITEM(ID _, _) -> failwith " ITEM(ID _, _) "
  | ITEM(TLIST _, _) -> failwith " ITEM(TLIST _, _) "
  | ITEM(TLIST2 _, _) -> failwith " ITEM(TLIST2 _, _) "
  | ITEM(STRING _, _) -> failwith " ITEM(STRING _, _) "
  | ITEM(INT _, _) -> failwith " ITEM(INT _, _) "
  | ITEM(ENDOFFILE, _) -> failwith " ITEM(ENDOFFILE, _) "
  | ITEM(EOL, _) -> failwith " ITEM(EOL, _) "
  | ITEM(LPAREN, _) -> failwith " ITEM(LPAREN, _) "
  | ITEM(RPAREN, _) -> failwith " ITEM(RPAREN, _) "
  | ITEM(EMPTY, _) -> failwith " ITEM(EMPTY, _) "
  | ITEM ((ITEM _|ITEM2 _), _) -> failwith " ITEM ((ITEM _|ITEM2 _), _) "
  | ITEM2 ((ITEM _|ITEM2 _), _, _) ->
      failwith " ITEM2 ((ITEM _|ITEM2 _), _, _) "

let () =  printf "PR#6646=Ok\n%!"

(* Simplified example, with application test *)

type t =
  | B of int
  | C of int
  | I of t list
  | A00
  | A01
  | A02
  | A03
  | A04
  | A05
  | A06
  | A07
  | A08
  | A09
  | A10
  | A11
  | A12
  | A13
  | A14
  | A15
  | A16
  | A17
  | A18
  | A19
  | A20
  | A21
  | A22
  | A23
  | A24
  | A25
  | A26
  | A27
  | A28
  | A29
  | A30
  | A31
  | A32
  | A33
  | A34
  | A35
  | A36
  | A37
  | A38
  | A39
  | A40
  | A41
  | A42
  | A43
  | A44
  | A45
  | A46
  | A47
  | A48
  | A49
  | A50
  | A51
  | A52
  | A53
  | A54
  | A55
  | A56
  | A57
  | A58
  | A59
  | A60
  | A61
  | A62
  | A63
  | A64
  | A65
  | A66
  | A67
  | A68
  | A69
  | A70
  | A71
  | A72
  | A73
  | A74
  | A75
  | A76
  | A77
  | A78
  | A79
  | A80
  | A81
  | A82
  | A83
  | A84
  | A85
  | A86
  | A87
  | A88
  | A89
  | A90
  | A91
  | A92
  | A93
  | A94
  | A95
  | A96
  | A97
  | A98
  | A99


let test = function
  | I [A00;I [I [A00;I [A00]]]] -> 1
  | I [A00;I [I [A00;I [A01]]]] -> 2
  | I [A00;I [I [A00;I [A02]]]] -> 3
  | I [A00;I [I [A00;I [A03]]]] -> -3
  | I [A00;I [I [A00;I [A04]]]] -> 4
  | I [A00;I [I [A00;I [A05]]]] -> 5
  | I [A00;I [I [A00;I [A06]]]] -> 6
  | I [A00;I [I [A00;I [A07]]]] -> 7
  | I [A00;I [I [A00;I [A08]]]] -> 8
  | I [A00;I [I [A00;I [A09]]]] -> 9

  | I [A00;I [I [_ ; I [A00]]]] -> 11
  | I [A00;I [I [_ ; I [A01]]]] -> 12
  | I [A00;I [I [_ ; I [A02]]]] -> 13
  | _ -> -1


let () =
  assert (test (I [A00;I [I [A00;I [A00]]]]) = 1) ;
  assert (test (I [A00;I [I [A20;I [A00]]]]) = 11) ;
  assert (test (I [A00;I [I [A00;I [A01]]]]) = 2) ;
  assert (test (I [A00;I [I [A20;I [A01]]]]) = 12) ;
  assert (test (I [A00;I [I [A00;I [A02]]]]) = 3) ;
  assert (test (I [A00;I [I [A20;I [A02]]]]) = 13) ;
  assert (test (I [A00;I [I [A00;I [A03]]]]) = -3) ;
  assert (test (I [A00;I [I [A20;I [A03]]]]) = -1) ;
  printf "PR#6646=Ok\n%!"

(* PR#6674, a compilation failure introduced by correcting PR#6646 *)

type t6674 =
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6
  | A7
  | A8
  | A9
  | A10
  | A11
  | A12
  | A13
  | A14
  | A15
  | A16
  | A17
  | A18
  | A19
  | A20
  | A21
  | A22
  | A23
  | A24
  | A25
  | A26
  | A27
  | A28
  | A29
  | A30
  | A31
  | A32
  | X of string

let f = function
  | X _ -> true
  | _ -> false

let () =  printf "PR#6676=Ok\n%!"

(* GPR#234, allow ``[]`` as a user defined constructor *)
module GPR234HList = struct

  type _ cell =
    | Int : int -> int cell
    | Pair : int * int -> (int * int) cell
    | StrInt : string -> string cell
    | List : int list -> int list cell

  type hlist =
    | [] : hlist
    | ( :: ) : 'a cell * hlist -> hlist

  type 'b foldf = {
    f: 'a. 'a cell -> 'b -> 'b
  }

  let fold_hlist : 'b foldf -> 'b -> hlist -> 'b = fun f init l ->
    let rec loop : hlist -> 'b -> 'b = fun l acc ->
      match l with
      | [] -> acc
      | hd :: tl -> loop tl (f.f hd acc) in
    loop l init

  let to_int_fold : type a. a cell -> int -> int = fun cell acc ->
    match cell with
    | Int x -> x + acc
    | Pair (x, y) -> x + y + acc
    | StrInt str -> int_of_string str + acc
    | List l -> acc + List.fold_left (+) 0 l

  let sum l = fold_hlist {f=to_int_fold} 0 l

  let l = List [1; 2; 3] (* still fine to use normal list here *)

  let ll = [Int 3; Pair (4, 5); StrInt "30"; l]

  let test () = Printf.printf "%d\n" (sum ll)

end

let () = GPR234HList.test ()

let () = printf "GPR#234=Ok\n%!"

module MPR7761 = struct

  let zyva msg c r =
    if r <> c then begin
      Printf.printf "'%c' pas bon pour %s (should be '%c')\n%!"r  msg c
    end else
      Printf.printf "%s -> '%c'\n%!" msg r

  module A = struct
    type t = ..
    type t +=
      | A
      | B

    let f x y = match x, y with
    | (A | B), A -> 'a'
    | (A | B), B -> 'b'
    | _, _ -> '_'

    let () =
      zyva "f A A" 'a' (f A A) ;
      zyva "f A B" 'b' (f A B) ;
      printf "PR#7661-A=Ok\n%!"
  end

  module B = struct
    type t = ..
    type t +=
      | A
      | B

    type t += C
    type t += D

    let f x y = match x, y with
    | B, C       -> 'x'
    | (A | B), A -> 'a'
    | (A | B), B -> 'b'
    | (C | D), (A|B|C) -> 'c'
    | _, _ -> '_'

    let g x y = match x, y with
    | Some B, C       -> 'x'
    | (Some A | Some B), A -> 'a'
    | (Some A | Some B), B -> 'b'
    | (Some C | Some D), (A|B|C) -> 'c'
    | _, _ -> '_'

    let () =
      zyva "f B C" 'x' (f B C) ;
      zyva "f A A" 'a' (f A A) ;
      zyva "f B A" 'a' (f B A) ;
      zyva "f A B" 'b' (f A B) ;
      zyva "f B B" 'b' (f B B) ;
      zyva "f C B" 'c' (f C B) ;
      zyva "f D B" 'c' (f D B) ;
      zyva "f C A" 'c' (f C A) ;
      zyva "f D A" 'c' (f D A) ;
      zyva "f C C" 'c' (f C C) ;
      zyva "f D C" 'c' (f D C) ;
      zyva "f A D" '_' (f A D) ;
      zyva "f C D" '_' (f C D) ;
(***************)
      zyva "g (Some B) C" 'x' (g (Some B) C) ;
      zyva "g (Some A) A" 'a' (g (Some A) A) ;
      zyva "g (Some B) A" 'a' (g (Some B) A) ;
      zyva "g (Some A) B" 'b' (g (Some A) B) ;
      zyva "g (Some B) B" 'b' (g (Some B) B) ;
      zyva "g (Some C) B" 'c' (g (Some C) B) ;
      zyva "g (Some D) B" 'c' (g (Some D) B) ;
      zyva "g (Some C) A" 'c' (g (Some C) A) ;
      zyva "g (Some D) A" 'c' (g (Some D) A) ;
      zyva "g (Some C) C" 'c' (g (Some C) C) ;
      zyva "g (Some D) C" 'c' (g (Some D) C) ;
      zyva "g (Some A) D" '_' (g (Some A) D) ;
      zyva "g (Some C) D" '_' (g (Some C) D) ;
(***************)
      printf "PR#7661-B=Ok\n%!"
  end

  module C = struct
    type t = ..
    type t +=
      | A
      | B

    type t += C
    type t += D=A

    let f x y = match x, y with
    | B, C       -> 'x'
    | (A | B), A -> 'a'
    | (A | B), B -> 'b'
    | (C | D), (A|B|C) -> 'c'
    | _, _ -> '_'

    let g x y = match x, y with
    | Some B, C       -> 'x'
    | (Some A | Some B), A -> 'a'
    | (Some A | Some B), B -> 'b'
    | (Some C | Some D), (A|B|C) -> 'c'
    | _, _ -> '_'

    let () =
      zyva "f B C" 'x' (f B C) ;
      zyva "f A A" 'a' (f A A) ;
      zyva "f B A" 'a' (f B A) ;
      zyva "f A B" 'b' (f A B) ;
      zyva "f B B" 'b' (f B B) ;
      zyva "f C B" 'c' (f C B) ;
      zyva "f D B" 'b' (f D B) ;
      zyva "f C A" 'c' (f C A) ;
      zyva "f D A" 'a' (f D A) ;
      zyva "f C C" 'c' (f C C) ;
      zyva "f D C" 'c' (f D C) ;
      zyva "f A D" 'a' (f A D) ;
      zyva "f B D" 'a' (f B D) ;
      zyva "f C D" 'c' (f C D) ;
      zyva "f D D" 'a' (f D D) ;
(***************)
      zyva "g (Some B) C" 'x' (g (Some B) C) ;
      zyva "g (Some A) A" 'a' (g (Some A) A) ;
      zyva "g (Some B) A" 'a' (g (Some B) A) ;
      zyva "g (Some A) B" 'b' (g (Some A) B) ;
      zyva "g (Some B) B" 'b' (g (Some B) B) ;
      zyva "g (Some C) B" 'c' (g (Some C) B) ;
      zyva "g (Some D) B" 'b' (g (Some D) B) ;
      zyva "g (Some C) A" 'c' (g (Some C) A) ;
      zyva "g (Some D) A" 'a' (g (Some D) A) ;
      zyva "g (Some C) C" 'c' (g (Some C) C) ;
      zyva "g (Some D) C" 'c' (g (Some D) C) ;
      zyva "g (Some A) D" 'a' (g (Some A) D) ;
      zyva "g (Some B) D" 'a' (g (Some B) D) ;
      zyva "g (Some C) D" 'c' (g (Some C) D) ;
      zyva "g (Some D) D" 'a' (g (Some D) D) ;
(***************)
      printf "PR#7661-C=Ok\n%!"
  end

  module D = struct
    type t = ..
    type t += A | B of int
    type t += C=A

    let f x y = match x,y with
    | true,A -> 'a'
    | _,B _  -> 'b'
    | false,A -> 'c'
    | _,_ -> '_'

    let g x y = match x,y with
    | true,A -> 'a'
    | _,C  -> 'b'
    | false,A -> 'c'
    | _,_ -> '_'

    let () =
      zyva "f true A" 'a' (f true A) ;
      zyva "f true (B 0)" 'b' (f true (B 0)) ;
      zyva "f false A" 'c' (f false A) ;
      zyva "g true A" 'a' (g true A) ;
      zyva "g false A" 'b' (g false A) ;
      zyva "g true (B 0)" '_' (g true (B 0)) ;
(***************)
      printf "PR#7661-D=Ok\n%!"
  end

  module E = struct

    module type S = sig
      type t = ..
      type t += A|B|C
      type u = X|Y|Z

      val fAYX : char
      val gAYX : char
      val fAZY : char
      val gAZY : char
    end

    module Z(T:S) : sig end = struct

      open T

      let f x y z = match x,y,z with
      | A,X,_ -> '1'
      | _,X,X -> '2'
      | B,_,X -> '3'
      | C,_,X -> '4'
      | C,_,Y -> '5'
      | _,_,_ -> '_'

      let g x y z = match x,y,z with
      | A,X,_     -> '1'
      | _,X,X     -> '2'
      | (B|C),_,X -> '3'
      | C,_,Y     -> '5'
      | _,_,_     -> '_'

      let () =
        zyva "f A Y X" fAYX  (f A Y X) ;
        zyva "g A Y X" gAYX  (g A Y X) ;
        zyva "f A Z Y" fAZY  (f A Z Y) ;
        zyva "g A Z Y" gAZY  (g A Z Y) ;
        ()
    end

    module A =
      Z
        (struct
          type t = ..
          type t += A|B
          type t += C=A
          type u = X|Y|Z

          let fAYX = '4'
          and gAYX = '3'
          and fAZY = '5'
          and gAZY = '5'
        end)

    module B =
      Z
        (struct
          type t = ..
          type t += A|B
          type t += C
          type u = X|Y|Z

          let fAYX = '_'
          and gAYX = '_'
          and fAZY = '_'
          and gAZY = '_'
        end)

    let () = printf "PR#7661-E=Ok\n%!"
  end
end
