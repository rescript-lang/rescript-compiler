/*
 * Js_of_ocaml runtime support
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published * by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

external \".![]": (string, int) => int = "%string_unsafe_get"
external \".!()": (string, int) => char = "%string_unsafe_get"

let code_0 = 48 // '0'
let code_a = 97 // 'a'
let code_A = 65 // 'A'

module Caml_char = {
  external code: char => int = "%identity"
  external unsafe_chr: int => char = "%identity"
}

let failwith = s => raise(Failure(s))
/* let invalid_argument s= raise (Invalid_argument s ) */

let \">>>" = Caml_nativeint_extern.shift_right_logical

let \"+~" = Caml_nativeint_extern.add
let \"*~" = Caml_nativeint_extern.mul

let parse_digit = c =>
  switch c {
  | '0' .. '9' => Caml_char.code(c) - code_0
  | 'A' .. 'Z' => Caml_char.code(c) - (code_A - 10)
  | 'a' .. 'z' => Caml_char.code(c) - (code_a - 10)
  | _ => -1
  }

type of_string_base =
  | Oct
  | Hex
  | Dec
  | Bin

let int_of_string_base = x =>
  switch x {
  | Oct => 8
  | Hex => 16
  | Dec => 10
  | Bin => 2
  }

let parse_sign_and_base = (s: string) => {
  let sign = ref(1)
  let base = ref(Dec)
  let i = ref(0)
  switch \".!()"(s, i.contents) {
  | '-' =>
    sign.contents = -1
    i.contents = i.contents + 1
  | '+' => i.contents = i.contents + 1
  | _ => ()
  }
  if \".!()"(s, i.contents) == '0' {
    switch \".!()"(s, i.contents + 1) {
    | 'x' | 'X' =>
      base.contents = Hex
      i.contents = i.contents + 2
    | 'o' | 'O' =>
      base.contents = Oct
      i.contents = i.contents + 2
    | 'b' | 'B' =>
      base.contents = Bin
      i.contents = i.contents + 2
    | 'u' | 'U' => i.contents = i.contents + 2
    | _ => ()
    }
  }
  (i.contents, sign.contents, base.contents)
}

let int_of_string = (s: string): int => {
  let (i, sign, hbase) = parse_sign_and_base(s)
  let base = int_of_string_base(hbase)
  let threshold = \">>>"(-1, 0)
  let len = Caml_string_extern.length(s)
  let c = if i < len {
    \".!()"(s, i)
  } else {
    '\000'
  }
  let d = parse_digit(c)
  let () = if d < 0 || d >= base {
    failwith("int_of_string")
  }
  /* let () = [%debugger]  in */
  let rec aux = (acc, k) =>
    if k == len {
      acc
    } else {
      let a = \".!()"(s, k)
      if a == '_' {
        aux(acc, k + 1)
      } else {
        let v = parse_digit(a)
        if v < 0 || v >= base {
          failwith("int_of_string")
        } else {
          let acc = \"+~"(\"*~"(base, acc), v)
          if acc > threshold {
            failwith("int_of_string")
          } else {
            aux(acc, k + 1)
          }
        }
      }
    }

  let res = \"*~"(sign, aux(d, i + 1))
  let or_res = lor(res, 0)
  if base == 10 && res != or_res {
    failwith("int_of_string")
  }
  or_res
}

let (hex_threshold, dec_threshold, oct_threshold, bin_threshold) = (
  1152921504606846975L,
  1844674407370955161L,
  2305843009213693951L,
  9223372036854775807L,
)

let int64_of_string = s => {
  let (i, sign, hbase) = parse_sign_and_base(s)
  let base = Caml_int64_extern.of_int(int_of_string_base(hbase))
  let sign = Caml_int64_extern.of_int(sign)
  let threshold = switch hbase {
  | Hex => /* 2 ^ 64 - 1 / 16 */
    hex_threshold
  | Dec => dec_threshold
  | Oct => oct_threshold
  | Bin => bin_threshold
  }

  let len = Caml_string_extern.length(s)
  let c = if i < len {
    \".!()"(s, i)
  } else {
    '\000'
  }
  let d = Caml_int64_extern.of_int(parse_digit(c))
  let () = if d < 0L || d >= base {
    failwith("int64_of_string")
  }
  let \"+~" = Caml_int64_extern.add
  let \"*~" = Caml_int64_extern.mul

  let rec aux = (acc, k) =>
    if k == len {
      acc
    } else {
      let a = \".!()"(s, k)
      if a == '_' {
        aux(acc, k + 1)
      } else {
        let v = Caml_int64_extern.of_int(parse_digit(a))
        if v < 0L || (v >= base || acc > threshold) {
          failwith("int64_of_string")
        } else {
          let acc = \"+~"(\"*~"(base, acc), v)
          aux(acc, k + 1)
        }
      }
    }

  let res = \"*~"(sign, aux(d, i + 1))
  let or_res = Caml_int64_extern.logor(res, 0L)
  if base == 10L && res != or_res {
    failwith("int64_of_string")
  }
  or_res
}

type base = Oct | Hex | Dec
let int_of_base = x =>
  switch x {
  | Oct => 8
  | Hex => 16
  | Dec => 10
  }

type fmt = {
  mutable justify: string,
  mutable signstyle: string,
  mutable filter: string,
  mutable alternate: bool,
  mutable base: base,
  mutable signedconv: bool,
  mutable width: int,
  mutable uppercase: bool,
  mutable sign: int,
  mutable prec: int,
  mutable conv: string,
}

let lowercase = (c: char): char =>
  if (c >= 'A' && c <= 'Z') || ((c >= '\192' && c <= '\214') || c >= '\216' && c <= '\222') {
    Caml_char.unsafe_chr(Caml_char.code(c) + 32)
  } else {
    c
  }

let parse_format = fmt => {
  module String = Caml_string_extern
  let len = Caml_string_extern.length(fmt)
  if len > 31 {
    raise(Invalid_argument("format_int: format too long"))
  }
  let rec aux = (f: fmt, i): fmt =>
    if i >= len {
      f
    } else {
      let c = String.get(fmt, i)
      switch c {
      | '-' =>
        f.justify = "-"
        aux(f, i + 1)
      | '+' | ' ' =>
        f.signstyle = Caml_string_extern.of_char(c)
        aux(f, i + 1)
      | '#' =>
        f.alternate = true
        aux(f, i + 1)
      | '0' =>
        f.filter = "0"
        aux(f, i + 1)
      | '1' .. '9' =>
        f.width = 0
        let j = ref(i)

        while {
          let w = \".![]"(fmt, j.contents) - code_0
          w >= 0 && w <= 9
        } {
          f.width = f.width * 10 + \".![]"(fmt, j.contents) - code_0
          j.contents = j.contents + 1
        }
        aux(f, j.contents)
      | '.' =>
        f.prec = 0
        let j = ref(i + 1)
        while {
          let w = \".![]"(fmt, j.contents) - code_0
          w >= 0 && w <= 9
        } {
          f.prec = f.prec * 10 + \".![]"(fmt, j.contents) - code_0
          j.contents = j.contents + 1
        }
        aux(f, j.contents)
      | 'd'
      | 'i' =>
        f.signedconv = true
        f.base = Dec
        aux(f, i + 1)
      | 'u' =>
        f.base = Dec
        aux(f, i + 1)
      | 'x' =>
        f.base = Hex
        aux(f, i + 1)
      | 'X' =>
        f.base = Hex
        f.uppercase = true
        aux(f, i + 1)
      | 'o' =>
        f.base = Oct
        aux(f, i + 1)
      /* | 'O' -> base .contents<- 8; uppercase .contents<- true no uppercase for oct */
      | 'e' | 'f' | 'g' =>
        f.signedconv = true
        f.conv = Caml_string_extern.of_char(c)
        aux(f, i + 1)
      | 'E' | 'F' | 'G' =>
        f.signedconv = true
        f.uppercase = true
        f.conv = Caml_string_extern.of_char(lowercase(c))
        aux(f, i + 1)
      | _ => aux(f, i + 1)
      }
    }

  aux(
    {
      justify: "+",
      signstyle: "-",
      filter: " ",
      alternate: false,
      base: Dec,
      signedconv: false,
      width: 0,
      uppercase: false,
      sign: 1,
      prec: -1,
      conv: "f",
    },
    0,
  )
}

let finish_formatting = (config: fmt, rawbuffer) => {
  let {
    justify,
    signstyle,
    filter,
    alternate,
    base,
    signedconv,
    width,
    uppercase,
    sign,
    prec: _,
    conv: _,
  } = config
  let len = ref(Caml_string_extern.length(rawbuffer))
  if signedconv && (sign < 0 || signstyle != "-") {
    len.contents = len.contents + 1
  }
  if alternate {
    if base == Oct {
      len.contents = len.contents + 1
    } else if base == Hex {
      len.contents = len.contents + 2
    } else {
      ()
    }
  }
  let buffer = ref("")

  /* let (+=) buffer s = buffer .contents<- buffer.contents ^ s in 
     FIXME: should get inlined
 */
  /* let (+:) s = buffer .contents<- buffer.contents ^ s in */
  if justify == "+" && filter == " " {
    for _ in len.contents to width - 1 {
      buffer.contents = buffer.contents ++ filter
    }
  }
  if signedconv {
    if sign < 0 {
      buffer.contents = buffer.contents ++ "-"
    } else if signstyle != "-" {
      buffer.contents = buffer.contents ++ signstyle
    } else {
      ()
    }
  }
  if alternate && base == Oct {
    buffer.contents = buffer.contents ++ "0"
  }
  if alternate && base === Hex {
    buffer.contents = buffer.contents ++ "0x"
  }

  if justify == "+" && filter == "0" {
    for _ in len.contents to width - 1 {
      buffer.contents = buffer.contents ++ filter
    }
  }
  if uppercase {
    buffer.contents = buffer.contents ++ Caml_string_extern.toUpperCase(rawbuffer)
  } else {
    buffer.contents = buffer.contents ++ rawbuffer
  }
  if justify == "-" {
    for _ in len.contents to width - 1 {
      buffer.contents = buffer.contents ++ " "
    }
  }
  buffer.contents
}

let aux = (f, i: int): string => {
  let i = if i < 0 {
    if f.signedconv {
      f.sign = -1
      \">>>"(-i, 0)
      /* when i is min_int, [-i] could overflow */
    } else {
      \">>>"(i, 0)
    }
  } else {
    i
  }
  let s = ref(Caml_string_extern.of_int(i, ~base=int_of_base(f.base)))
  if f.prec >= 0 {
    f.filter = " "
    let n = f.prec - Caml_string_extern.length(s.contents)
    if n > 0 {
      s.contents = Caml_string_extern.repeat("0", n) ++ s.contents
    }
  }
  finish_formatting(f, s.contents)
}

let format_int = (fmt, i) =>
  if fmt == "%d" {
    Caml_nativeint_extern.to_string(i)
  } else {
    let f = parse_format(fmt)
    aux(f, i)
  }

/* This can handle unsigned integer (-1L) and print it as "%Lu" which 
   will overflow signed integer in general
*/
let dec_of_pos_int64 = x =>
  if x < 0L {
    let wbase = 10L
    let cvtbl = "0123456789"
    let y = Caml_int64.discard_sign(x)
    /* 2 ^  63 + y `div_mod` 10 */

    let quotient_l = 922337203685477580L /* 2 ^ 63 / 10 */
    /* {lo =   -858993460n; hi =  214748364n} */
    /* TODO:  int64 constant folding so that we can do idiomatic code
     2 ^ 63 / 10 */
    let modulus_l = 8L
    /* let c, d = Caml_int64.div_mod (Caml_int64.add y modulus_l) wbase in
        we can not do the code above, it can overflow when y is really large           
 */
    let (c, d) = Caml_int64.div_mod(y, wbase)
    let (e, f) = Caml_int64.div_mod(Caml_int64_extern.add(modulus_l, d), wbase)
    let quotient = Caml_int64_extern.add(Caml_int64_extern.add(quotient_l, c), e)
    Caml_int64.to_string(quotient) ++
    Caml_string_extern.get_string_unsafe(cvtbl, Caml_int64_extern.to_int(f))
  } else {
    Caml_int64.to_string(x)
  }

let oct_of_int64 = x => {
  let s = ref("")
  let wbase = 8L
  let cvtbl = "01234567"
  if x < 0L {
    let y = Caml_int64.discard_sign(x)
    /* 2 ^  63 + y `div_mod` 8 */
    let quotient_l = 1152921504606846976L
    /* {lo =   0n; hi =  268435456n } */ /* 2 ^ 31 / 8 */

    /* let c, d = Caml_int64.div_mod (Caml_int64.add y modulus_l) wbase in
          we can not do the code above, it can overflow when y is really large           
 */
    let (c, d) = Caml_int64.div_mod(y, wbase)

    let quotient = ref(Caml_int64_extern.add(quotient_l, c))
    let modulus = ref(d)
    s.contents =
      Caml_string_extern.get_string_unsafe(cvtbl, Caml_int64_extern.to_int(modulus.contents)) ++
      s.contents

    while quotient.contents != 0L {
      let (a, b) = Caml_int64.div_mod(quotient.contents, wbase)
      quotient.contents = a
      modulus.contents = b
      s.contents =
        Caml_string_extern.get_string_unsafe(cvtbl, Caml_int64_extern.to_int(modulus.contents)) ++
        s.contents
    }
  } else {
    let (a, b) = Caml_int64.div_mod(x, wbase)
    let quotient = ref(a)
    let modulus = ref(b)
    s.contents =
      Caml_string_extern.get_string_unsafe(cvtbl, Caml_int64_extern.to_int(modulus.contents)) ++
      s.contents

    while quotient.contents != 0L {
      let (a, b) = Caml_int64.div_mod(quotient.contents, wbase)
      quotient.contents = a
      modulus.contents = b
      s.contents =
        Caml_string_extern.get_string_unsafe(cvtbl, Caml_int64_extern.to_int(modulus.contents)) ++
        s.contents
    }
  }
  s.contents
}

/* FIXME: improve codegen for such cases
   let div_mod (x : int64) (y : int64) : int64 * int64 =  
   let a, b = Caml_int64.(div_mod (unsafe_of_int64 x) (unsafe_of_int64 y)) in   
   Caml_int64.unsafe_to_int64 a , Caml_int64.unsafe_to_int64 b 
*/
let int64_format = (fmt, x) =>
  if fmt == "%d" {
    Caml_int64.to_string(x)
  } else {
    let f = parse_format(fmt)
    let x = if f.signedconv && x < 0L {
      f.sign = -1
      Caml_int64_extern.neg(x)
    } else {
      x
    }
    let s = switch f.base {
    | Hex => Caml_int64.to_hex(x)
    | Oct => oct_of_int64(x)
    | Dec => dec_of_pos_int64(x)
    }
    let fill_s = if f.prec >= 0 {
      f.filter = " "
      let n = f.prec - Caml_string_extern.length(s)
      if n > 0 {
        "0"->Caml_string_extern.repeat(n) ++ s
      } else {
        s
      }
    } else {
      s
    }

    finish_formatting(f, fill_s)
  }

let format_float = (fmt, x) => {
  module String = Caml_string_extern
  let f = parse_format(fmt)
  let prec = if f.prec < 0 {
    6
  } else {
    f.prec
  }
  let x = if x < 0. {
    f.sign = -1
    -.x
  } else {
    x
  }
  let s = ref("")
  if Caml_float_extern.isNaN(x) {
    s.contents = "nan"
    f.filter = " "
  } else if !Caml_float_extern.isFinite(x) {
    s.contents = "inf"
    f.filter = " "
  } else {
    switch f.conv {
    | "e" =>
      s.contents = Caml_float_extern.toExponentialWithPrecision(x, ~digits=prec)
      /* exponent should be at least two digits
           {[
             (3.3).toExponential()
               "3.3e+0"
               3.3e+00
           ]}
 */
      let i = Caml_string_extern.length(s.contents)
      if String.get(s.contents, i - 3) == 'e' {
        s.contents =
          Caml_string_extern.slice(s.contents, 0, i - 1) ++
          ("0" ++
          Caml_string_extern.slice_rest(s.contents, i - 1))
      }
    | "f" =>
      /* this will not work large numbers */
      /* ("%3.10f", 3e+56, "300000000000000005792779041490073052596128503513888063488.0000000000") */
      s.contents = Caml_float_extern.toFixedWithPrecision(x, ~digits=prec)
    | "g" =>
      let prec = if prec != 0 {
        prec
      } else {
        1
      }
      s.contents = Caml_float_extern.toExponentialWithPrecision(x, ~digits=prec - 1)
      let j = Caml_string_extern.index_of(s.contents, "e")
      let exp = Caml_float.int_of_float(
        Caml_float_extern.fromString(Caml_string_extern.slice_rest(s.contents, j + 1)),
      )
      if exp < -4 || (x >= 1e21 || Caml_string_extern.length(Caml_float_extern.toFixed(x)) > prec) {
        let i = ref(j - 1)
        while String.get(s.contents, i.contents) == '0' {
          i.contents = i.contents - 1
        }
        if String.get(s.contents, i.contents) == '.' {
          i.contents = i.contents - 1
        }
        s.contents =
          Caml_string_extern.slice(s.contents, 0, i.contents + 1) ++
          Caml_string_extern.slice_rest(s.contents, j)
        let i = Caml_string_extern.length(s.contents)
        if String.get(s.contents, i - 3) == 'e' {
          s.contents =
            Caml_string_extern.slice(s.contents, 0, i - 1) ++
            ("0" ++
            Caml_string_extern.slice_rest(s.contents, i - 1))
        } else {
          ()
        }
      } else {
        let p = ref(prec)
        if exp < 0 {
          p.contents = p.contents - (exp + 1)
          s.contents = Caml_float_extern.toFixedWithPrecision(x, ~digits=p.contents)
        } else {
          while {
            s.contents = Caml_float_extern.toFixedWithPrecision(x, ~digits=p.contents)
            Caml_string_extern.length(s.contents) > prec + 1
          } {
            p.contents = p.contents - 1
          }
        }
        if p.contents != 0 {
          let k = ref(Caml_string_extern.length(s.contents) - 1)
          while String.get(s.contents, k.contents) == '0' {
            k.contents = k.contents - 1
          }
          if String.get(s.contents, k.contents) == '.' {
            k.contents = k.contents - 1
          }
          s.contents = Caml_string_extern.slice(s.contents, 0, k.contents + 1)
        }
      }

    | _ => ()
    }
  }
  finish_formatting(f, s.contents)
}

let hexstring_of_float: (float, int, char) => string = %raw(`function(x,prec,style){
  if (!isFinite(x)) {
    if (isNaN(x)) return "nan";
    return x > 0 ? "infinity":"-infinity";
  }
  var sign = (x==0 && 1/x == -Infinity)?1:(x>=0)?0:1;
  if(sign) x = -x;
  var exp = 0;
  if (x == 0) { }
  else if (x < 1) {
    while (x < 1 && exp > -1022)  { x *= 2; exp-- }
  } else {
    while (x >= 2) { x /= 2; exp++ }
  }
  var exp_sign = exp < 0 ? '' : '+';
  var sign_str = '';
  if (sign) sign_str = '-'
  else {
    switch(style){
    case 43 /* '+' */: sign_str = '+'; break;
    case 32 /* ' ' */: sign_str = ' '; break;
    default: break;
    }
  }
  if (prec >= 0 && prec < 13) {
    /* If a precision is given, and is small, round mantissa accordingly */
      var cst = Math.pow(2,prec * 4);
      x = Math.round(x * cst) / cst;
  }
  var x_str = x.toString(16);
  if(prec >= 0){
      var idx = x_str.indexOf('.');
    if(idx<0) {
      x_str += '.' +  '0'.repeat(prec);
    }
    else {
      var size = idx+1+prec;
      if(x_str.length < size)
        x_str += '0'.repeat(size - x_str.length);
      else
        x_str = x_str.substr(0,size);
    }
  }
  return  (sign_str + '0x' + x_str + 'p' + exp_sign + exp.toString(10));
}`)

let float_of_string: (string, exn) => float = %raw(`function(s,exn){

    var res = +s;
    if ((s.length > 0) && (res === res))
        return res;
    s = s.replace(/_/g, "");
    res = +s;
    if (((s.length > 0) && (res === res)) || /^[+-]?nan$/i.test(s)) {
        return res;
    };
    var m = /^ *([+-]?)0x([0-9a-f]+)\.?([0-9a-f]*)p([+-]?[0-9]+)/i.exec(s);
    //            1        2             3           4
    if(m){
        var m3 = m[3].replace(/0+$/,'');
        var mantissa = parseInt(m[1] + m[2] + m3, 16);
        var exponent = (m[4]|0) - 4*m3.length;
        res = mantissa * Math.pow(2, exponent);
        return res;
    }
    if (/^\+?inf(inity)?$/i.test(s))
        return Infinity;
    if (/^-inf(inity)?$/i.test(s))
        return -Infinity;
    throw new Error(exn.RE_EXN_ID, { cause: exn });;
}
`)

@ocaml.doc("
   Pervasives.float_of_string : string -> float = \"?float_of_string\"
   Semantics is slightly different from javascript :
   console.assert(float_of_string('infinity')===Infinity)
   console.assert(float_of_string('Infinity')===Infinity
   parseFloat('Infinity') === Infinity
   parseFloat('infinity') === Nan
")
let float_of_string = (s: string): float => float_of_string(s, Failure("float_of_string"))
