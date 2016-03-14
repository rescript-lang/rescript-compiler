(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
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
 *)

(* Author: Hongbo Zhang  *)

let repeat = Caml_utils.repeat
let caml_failwith s = raise (Failure  s)
let caml_invalid_argument s= raise (Invalid_argument s )

let (>>>) = Nativeint.shift_right_logical
let (+~) = Nativeint.add 
let to_nat x = Nativeint.of_int x 
let of_nat x = Nativeint.to_int x 
let ( *~ ) = Nativeint.mul  
external string_of_char_code : char -> string = "String.fromCharCode" [@@js.call]
let parse_digit c = 
  match c with 
  | '0' .. '9' 
    -> Char.code c - Char.code '0'
  | 'A' .. 'Z'
    -> Char.code c - (Char.code 'A' - 10)
  | 'a' .. 'z'
    -> Char.code c - (Char.code 'a' - 10 )
  | _ -> -1

let parse_sign_and_base (s : string) = 
  let sign = ref 1n in
  let base = ref 10n in
  let i  = ref 0 in
  begin 
    (
      if s.[!i] = '-' then
        begin 
          sign :=  -1n;
          incr i
        end
    );
    (match s.[!i], s.[!i + 1] with 
     | '0', ('x' | 'X')
       -> base := 16n; i:=!i + 2 
     | '0', ( 'o' | 'O')
       -> base := 8n; i := !i + 2
     | '0', ('b' | 'B' )
       -> base := 2n; i := !i + 2 
     | _, _ -> ()); 
    (!i, !sign, !base)
  end

let caml_int_of_string s = 
  let i, sign, base = parse_sign_and_base s in 
  let threshold = (-1n >>> 0) in 
  let len = String.length s in  
  let c = if i < len then s.[i] else '\000' in
  let d = to_nat @@ parse_digit c in
  let () =
    if d < 0n || d >=  base then
      caml_failwith "int_of_string" in
  (* let () = [%js.debug]  in *)
  let rec aux acc k = 
    if k = len then acc 
    else 
      let a = s.[k] in
      if a  = '_' then aux acc ( k +  1) 
      else 
        let v = to_nat @@ parse_digit a in  
        if v < 0n || v >=  base then 
          caml_failwith "int_of_string"
        else 
          let acc = base *~ acc +~  v in 
          if acc > threshold then 
            caml_failwith "int_of_string"
          else aux acc  ( k +   1)
  in 
  let res = sign *~ aux d (i + 1) in 
  let or_res = Nativeint.logor res 0n in 
  (if base = 10n && res != or_res then 
    caml_failwith "int_of_string");
  or_res

type fmt = {
  mutable justify : string; 
  mutable signstyle : string;
  mutable filter : string ;
  mutable alternate : bool;
  mutable base : int;
  mutable signedconv : bool;
  mutable width :int;
  mutable uppercase : bool;
  mutable sign : int;
  mutable prec : int;
  mutable conv : string
}

let lowercase c =
  if (c >= 'A' && c <= 'Z')
  || (c >= '\192' && c <= '\214')
  || (c >= '\216' && c <= '\222')
  then Char.unsafe_chr(Char.code c + 32)
  else c

let _parse_format fmt = 
  let len = String.length fmt in 
  if len > 31 then 
    raise @@ Invalid_argument "format_int: format too long" ;
  let rec aux (f : fmt) i : fmt = 
    if i >= len then  f
    else
      let c = fmt.[i] in  
      match c with
      | '-' -> 
        f.justify <- "-";
        aux f (i + 1)
      | '+'|' ' 
        ->
        f.signstyle <- string_of_char_code c ; 
        aux f (i + 1)
      | '#' -> 
        f.alternate <- true;
        aux f (i + 1)
      | '0' -> 
        f.filter <- "0";
        aux f (i + 1)
      | '1' .. '9' 
        -> 
        begin 
          f.width <- 0;
          let j = ref i in 

          while (let w = Char.code fmt.[!j] - Char.code '0' in w >=0 && w <= 9  ) do 
            f.width <- f.width * 10 + Char.code fmt.[!j] - Char.code '0';
            incr j
          done;
          aux f !j
        end
      | '.' 
        -> 
        f.prec <- 0;
        let j = ref (i + 1 ) in 
        while (let w = Char.code fmt.[!j] - Char.code '0' in w >=0 && w <= 9  ) do 
          f.prec <- f.prec * 10 + Char.code fmt.[!j] - Char.code '0';
          incr j;
        done;
        aux f !j 
      | 'd' 
      | 'i' -> 
        f.signedconv <- true;
        f.base <- 10;
        aux f (i + 1)
      | 'u' -> 
        f.base <-10;
        aux f (i + 1)
      | 'x' -> 
        f.base <- 16;
        aux f (i + 1)
      | 'X' -> 
        f.base <- 16;
        f.uppercase <- true;
        aux f (i + 1)
      | 'o' -> 
        f.base <- 8;
        aux f (i + 1)
      (* | 'O' -> base := 8; uppercase := true no uppercase for oct *)
      | 'e' | 'f' | 'g' 
        -> 
        f.signedconv <- true;
        f.conv <- string_of_char_code c ;
        aux  f (i + 1)
      | 'E' | 'F' | 'G' 
        -> 
        f.signedconv <- true;
        f.uppercase <- true;
        f.conv <- string_of_char_code (lowercase c);
        aux f (i + 1)
      | _ -> 
        aux f (i + 1) 
  in 
  aux   { justify =  "+" ;
          signstyle =   "-";
          filter =  " " ;
          alternate =  false ;
          base=  0 ;
          signedconv=  false ;
          width =  0 ;
          uppercase=   false ;
          sign =  1 ;
          prec =  (-1); 
          conv =  "f"} 0 ;


external toUpperCase : string -> string = "toUpperCase" [@@js.send]
let _finish_formatting ({
  justify; 
  signstyle;
  filter ;
  alternate;
  base;
  signedconv;
  width;
  uppercase;
  sign;
  prec;
  conv
}) rawbuffer =  
  let len = ref (String.length rawbuffer) in 
  if signedconv && (sign < 0 || signstyle <> "-") then 
    incr len;
  if alternate then 
    begin 
      if base = 8 then
        incr len 
      else
      if base = 16 then 
        len := !len + 2
      else ()
    end ; 
  let buffer = ref "" in 
  (* let (+=) buffer s = buffer := !buffer ^ s in 
     FIXME: should get inlined
  *)
  (* let (+:) s = buffer := !buffer ^ s in *)
  if justify = "+" && filter = " " then
    for i = !len to width - 1 do 
       buffer := !buffer ^ filter
    done;
  if signedconv then 
    if sign < 0 then 
      buffer := !buffer ^ "-"
    else if signstyle <> "-" then 
      buffer := !buffer ^ signstyle
    else ()  ;
  if alternate && base = 8 then 
    buffer := !buffer ^ "0";
  if alternate && base == 16 then
    buffer := !buffer ^ "0x";
    
  if justify = "+" && filter = "0" then 
    for i = !len to width - 1 do 
      buffer := !buffer ^ filter;
    done;
  begin 
    if uppercase then 
      buffer := !buffer ^ toUpperCase rawbuffer
    else
      buffer := !buffer ^ rawbuffer
  end;
  if justify = "-" then 
    for i = !len to width - 1 do 
      buffer := !buffer ^ " ";
    done;
  !buffer

external int_to_string : int -> int -> string = "toString" [@@js.send]

(** TODO: depends on ES6 polyfill [String.prototype.repeat]*)
(* external duplicate : string -> int -> string = "repeat" [@@js.send] *)

let caml_format_int fmt i = 
  if fmt = "%d" then string_of_int i 
  else 
    let f = _parse_format fmt in 
    let i = 
      if i < 0 then 
        if f.signedconv then 
          begin 
            f.sign <- -1;
            -i
          end
        else 
          i lsr 0 
      else  i  in
    let s = ref @@ int_to_string i f.base in 
    if f.prec >= 0 then 
      begin 
      f.filter <- " ";
      let n = f.prec - String.length !s in 
      if n > 0 then
        s :=  repeat n "0"  ^ !s
      end
    ;
    _finish_formatting f !s
      
[%%js.raw{|

/**
 * external float_of_string : string -> float = "caml_float_of_string"
 * pervasives.ml
 * Semantics is slightly different from javascript :
 * console.assert(caml_float_of_string('infinity')===Infinity)
 * console.assert(caml_float_of_string('Infinity')===Infinity
 *
 * parseFloat('Infinity') === Infinity
 * parseFloat('infinity') === Nan
 * */
function $$caml_float_of_string(s) {
    //s = caml_bytes_of_string (s);
    var res = +s;
    if ((s.length > 0) && (res === res))
        return res;
    s = s.replace(/_/g, "");
    res = +s;
    if (((s.length > 0) && (res === res)) || /^[+-]?nan$/i.test(s)) {
        return res;
    }
    ;
    if (/^ *0x[0-9a-f_]+p[+-]?[0-9_]+/i.test(s)) {
        var pidx = s.indexOf('p');
        pidx = (pidx == -1) ? s.indexOf('P') : pidx;
        var exp = +s.substring(pidx + 1);
        res = +s.substring(0, pidx);
        return res * Math.pow(2, exp);
    }
    if (/^\+?inf(inity)?$/i.test(s))
        return Infinity;
    if (/^-inf(inity)?$/i.test(s))
        return -Infinity;
    caml_failwith("float_of_string");
}

/**
 * TODO: Check out how it compares with nodejs util.format
 * */
function $$parse_format(fmt) {
    //fmt = caml_bytes_of_string(fmt);
    var len = fmt.length;
    if (len > 31)
        caml_invalid_argument("format_int: format too long");
    var f = { justify: '+',
        signstyle: '-',
        filler: ' ',
        alternate: false,
        base: 0,
        signedconv: false,
        width: 0, uppercase: false,
        sign: 1,
        prec: -1,
        conv: 'f' };
    for (var i = 0; i < len; i++) {
        var c = fmt.charAt(i);
        switch (c) {
            case '-':
                f.justify = '-';
                break;
            case '+':
            case ' ':
                f.signstyle = c;
                break;
            case '#':
                f.alternate = true;
                break;
            case '0':
                f.filler = '0';
                break;
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                f.width = 0;
                var w;
                while (w = fmt.charCodeAt(i) - 48, w >= 0 && w <= 9) {
                    f.width = f.width * 10 + w;
                    i++;
                }
                i--;
                break;
            case '.':
                f.prec = 0;
                i++;
                var c1;
                while (c1 = fmt.charCodeAt(i) - 48, c1 >= 0 && c1 <= 9) {
                    f.prec = f.prec * 10 + c1;
                    i++;
                }
                i--;
                break;
            case 'd':
            case 'i':
                f.signedconv = true; // fall through to have f.base = 10
            case 'u':
                f.base = 10;
                break;
            case 'x':
                f.base = 16;
                break;
            case 'X':
                f.base = 16;
                f.uppercase = true;
                break;
            case 'o':
                f.base = 8;
                break;
            case 'e':
            case 'f':
            case 'g':
                f.signedconv = true;
                f.conv = c;
                break;
            case 'E':
            case 'F':
            case 'G':
                f.signedconv = true;
                f.uppercase = true;
                f.conv = c.toLowerCase();
                break;
        }
    }
    return f;
}

function $$finish_formatting(f, rawbuffer) {
    var len = rawbuffer.length;
    //if (f.uppercase) rawbuffer = rawbuffer.toUpperCase();
    /* Adjust len to reflect additional chars (sign, etc) */
    if (f.signedconv && (f.sign < 0 || f.signstyle != '-'))
        len++;
    if (f.alternate) {
        if (f.base == 8)
            len += 1;
        if (f.base == 16)
            len += 2;
    }
    /* Do the formatting */
    var buffer = "";
    if (f.justify == '+' && f.filler == ' ') {
        for (var i = len; i < f.width; i++)
            buffer += ' ';
    }
    if (f.signedconv) {
        if (f.sign < 0)
            buffer += '-';
        else if (f.signstyle != '-')
            buffer += f.signstyle;
    }
    if (f.alternate && f.base == 8) {
        buffer += '0';
    }
    if (f.alternate && f.base == 16) {
        buffer += "0x";
    }
    if (f.justify == '+' && f.filler == '0') {
        for (var i = len; i < f.width; i++)
            buffer += '0';
    }
    if (f.uppercase) {
        buffer += rawbuffer.toUpperCase();
    }
    else {
        buffer += rawbuffer;
    }
    //buffer += rawbuffer;
    if (f.justify == '-')
        for (var i = len; i < f.width; i++)
            buffer += ' ';
    return buffer;
}




/**
 * Noat that for {!Pervasives.float_of_string} the first parameter is fixed,
 * We should do code specialization for it
 * @param fmt
 * @param x
 * @returns {string}
 */
function $$caml_format_float(fmt, x) {
    var s;
    var f = $$parse_format(fmt);
    var prec = (f.prec < 0) ? 6 : f.prec;
    if (x < 0) {
        f.sign = -1;
        x = -x;
    }
    if (isNaN(x)) {
        s = "nan";
        f.filler = ' ';
    }
    else if (!isFinite(x)) {
        s = "inf";
        f.filler = ' ';
    }
    else {
        switch (f.conv) {
            case 'e':
                var s = x.toExponential(prec);
                // exponent should be at least two digits
                var i = s.length;
                if (s.charAt(i - 3) == 'e')
                    s = s.slice(0, i - 1) + '0' + s.slice(i - 1);
                break;
            case 'f':
                s = x.toFixed(prec);
                break;
            case 'g':
                prec = prec ? prec : 1;
                s = x.toExponential(prec - 1);
                var j = s.indexOf('e');
                var exp = +s.slice(j + 1);
                if (exp < -4 || x >= 1e21 || x.toFixed(0).length > prec) {
                    // remove trailing zeroes
                    var i = j - 1;
                    while (s.charAt(i) == '0')
                        i--;
                    if (s.charAt(i) == '.')
                        i--;
                    s = s.slice(0, i + 1) + s.slice(j);
                    i = s.length;
                    if (s.charAt(i - 3) == 'e')
                        s = s.slice(0, i - 1) + '0' + s.slice(i - 1);
                    break;
                }
                else {
                    var p = prec;
                    if (exp < 0) {
                        p -= exp + 1;
                        s = x.toFixed(p);
                    }
                    else
                        while (s = x.toFixed(p), s.length > prec + 1)
                            p--;
                    if (p) {
                        // remove trailing zeroes
                        var i = s.length - 1;
                        while (s.charAt(i) == '0')
                            i--;
                        if (s.charAt(i) == '.')
                            i--;
                        s = s.slice(0, i + 1);
                    }
                }
                break;
        }
    }
    return $$finish_formatting(f, s);
}

|}]

external caml_format_float : string -> float -> string =  "$$caml_format_float"
[@@js.call] [@@js.local]


let caml_nativeint_format = caml_format_int
let caml_int32_format = caml_format_int

external caml_float_of_string : string -> float = ""
[@@js.call "$$caml_float_of_string"] [@@js.local]


let caml_int32_of_string = caml_int_of_string
let caml_nativeint_of_string = caml_int32_of_string
