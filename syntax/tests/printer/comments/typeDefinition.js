/* before */ type t /* after */
/* before */ type /* before name */ t /* after name */ /* after */

/* before */ type t = .. /* after */
/* before */ type /* before name */ t /* after name */ = .. /* after */

/* before */ type t</* c0 */'err /* c1 */ , /* c2 */ 'success /* c3 */> /* after */
/* before */ type /* before name */ t /* after name */ </* c0 */'err /* c1 */ , /* c2 */ 'success /* c3 */> /* after */

/* before */ type t = /* before manifest */ string /* after */
/* before */ type /* before name */ t /* after name */ = /* before manifest */ string /* after */
/* before */ type /* before name */ t /* after name */ </* c0 */'err /* c1 */ , /* c2 */ 'success /* c3 */> = /* before manifest */ string /* after */

/* before */ type /* before name */ t /* after name */ =
  // above Red
  | Red // trailing Red
  // above Blue
  | Blue // trailing Blue
  // above Green
  | Green // trailing Green

/* before */ type /* before name */ t /* after name */ = /* before manifest */ Colour.t /* after manifest */ =
  // above Red
  | /* before Red */ Red // trailing Red
  // above Blue
  | /* before Blue */ Blue // trailing Blue
  // above Green
  | /* before Green */ Green /* trailing Green */; // test

type color =
  | /* before Red */ Red /* after Red */ : /* before gadt */ color /* after gadt */
  | /* before Blue */ Blue /* after Blue */ : /* before gadt */ color /* after gadt */

type color = /* before constr */ Rgb/*after constructor */( /* before red */ red, green, blue)

type color = | /* before constr */ Rgb/*after constructor */(
  /* before red */ red /* after red */,
  /* before green */ green /* after green */,
  /* before blue */ blue /* after blue */
)

type color = | /* before constr */ Rgb/*after constructor */({
  /* before red */ red/* after red */: /* before typ */ someNumber /* after typ */ ,
  /* before green */ green/* after green */: /* before typ */someNumber /* after typ */,
  /* before blue */ blue/* after blue */: /* before typ */someNumber  /* after typ */
})

type color = {
  /* before red */ red/* after red */: /* before typ */ someNumber /* after typ */ ,
  /* before green */ green/* after green */: /* before typ */someNumber /* after typ */,
  /* before blue */ blue/* after blue */: /* before typ */someNumber  /* after typ */
}

type /* c0 */ color /* c1 */ = /* before manifest */ Colour.t /* after manifest */ = {
  /* before red */ red/* after red */: /* before typ */ someNumber /* after typ */ ,
  /* before green */ green/* after green */: /* before typ */someNumber /* after typ */,
  /* before blue */ blue/* after blue */: /* before typ */someNumber  /* after typ */
}

type domProps =  {
  @bs.optional
  viewTarget: string,
  @bs.optional
  visibility: string,
  /*width::string? =>*/
  @bs.optional
  widths: string,
  @bs.optional
  wordSpacing: string,
  @bs.optional
  writingMode: string,
  @bs.optional
  x: string,
  @bs.optional
  x1: string,
}

@bs.deriving(abstract)
type t = {
  /* MDX shortnames for more advanced components */
  @bs.as("Cite")
  cite: React.component<{
    "author": option<string>,
    "children": React.element,
  }>,
  @bs.as("Info") @bs.optional
  info: React.component<props>,
}

type rec t = {
  facing: facing,
  // frames
}
