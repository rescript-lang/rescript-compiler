/* bs error */
let app = [@bs] (f, x) => f(x);

app(((x) => x + 1), 2);
/*
File "./tmp/bucklescript_case0.re", line 4, characters 0-3:
Error: This expression has type ((('a) => 'b, 'a) => 'b) [@bs]
       This is not a function; it cannot be applied.

=====

  We've found a bug for you!
  ./tmp/bucklescript_case0.re 4:1-3
  
  2 │ let app = [@bs] (f, x) => f(x);
  3 │ 
  4 │ app(((x) => x + 1), 2);
  
  This is an uncurried bucklescript function. It must be applied with [@bs].
*/
