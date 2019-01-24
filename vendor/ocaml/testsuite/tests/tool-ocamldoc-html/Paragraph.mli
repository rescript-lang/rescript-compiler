(** This file tests the generation of paragraph within module comments.


    At least three points should be exercised in this tests

    - First, all text should be tagged
    - Second, no paragraph should contain only spaces characters
    - Third, the mixing of different text style should not create
    invalid p tags


    See also {{: http://caml.inria.fr/mantis/view.php?id=7352} MPR:7352},
    {{: http://caml.inria.fr/mantis/view.php?id=7353} MPR:7353}

    {2:here Testing non-text elements }

    [code x ] {i should } be inside a p.


    {e But} {b not}
    {[
      let complex_code = ()
    ]}
    here.

    + An enumerated list first element
    + second element

    {L Alignement test: left}
    {R Right}
    {C Center}


    Other complex text{_ in subscript }{^ and superscript}
    {V Verbatim V}

    There is also {%html: html specific %} elements.

    @author: Florian Angeletti
    @version: 1
*)

(** *)

type t
(**
    And cross-reference {! t}.
   {!modules: Paragraph}
   {!indexlist}
*)
