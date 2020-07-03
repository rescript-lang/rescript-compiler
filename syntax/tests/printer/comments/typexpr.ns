// Ptyp_any
type t = /* before */ _ /* after */

// Ptyp_var
type t = /* before */ 'a /* after */

// Ptyp_tuple
// sigh… conflicts

// Ptyp_constr
type t = /* before */ string /* after */
type t = /* before */ option /* c0 */< /*c1 */string /* c2 */> /* after */ 
type t = /* before */ result /* c0 */< /*c1 */success /* c2 */, /* c3 */ err /*c4*/> /* after */ 

// Ptyp_extension
type t = /* before */ %ext(/* before */ "here" /* after */) /* after */

// Ptyp_package
type t = /* c0 */ module(/* before */ S /* after */) /* c1 */

type t = /* c0 */ module(/* c1 */Hashmap /* c2 */ with type /* c3 */ key /* c4 */ = /* c5 */ string /* c6 */)

// Ptyp_alias
type t = /* c0 */ string /* c1 */ as 'x // after

// Ptyp_poly
type fn = {
  f: /* c0 */ 'a /* c1 */ 'b /* c2*/ . /* c3 */ string /* c4 */
}

// Ptyp_arrow
type add = /* before */ ( /* c0 */ int /* c1 */, /* c2 */ int /* c3 */) => /* before return */ int /* after */
type add = /* before */ ( /* c0 */ ~a:int /* c1 */, /* c2 */ ~b:int /* c3 */) => /* before return */ int /* after */
type multiply = /* before */ ( /* c0 */ ~fn: (/* cinner0 */ int /* cinner1 */, /* cinner2 */int /*cinner3 */) => /*cx*/ int /* c1 */, /* c2 */ ~b:int /* c3 */) => int /* after */

// Ptyp_object
type jsUser = /* before */ {
  // above name
  /* before name */"name" /* after name */: /* before typ */ string /* after typ */,
  // above age
  /* before age */"age" /* after age */: /*before int */ int /*after int */
} /* after */
