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


external is_nan : float -> bool = "isNaN" 
    [@@js.call]

external is_finite : float -> bool = "isFinite"
    [@@js.call]

external to_exponential : float -> int ->  string = "toExponential"
    [@@js.send]

external slice : string -> int -> int -> string = "slice" 
    [@@js.send]

external slice_rest : string -> int -> string = "slice" 
    [@@js.send]

external to_fixed : float -> int -> string = "toFixed" 
    [@@js.send]

external index_of : string -> string -> int = "indexOf"
    [@@js.send]

(** TODO: investigate why [caml_int_of_string] does not work here *)
[%%js.raw{|
function $$js_anything_to_number(x){
   return +x;
}
|}]
external to_number : 'a -> int = "$$js_anything_to_number" (* + conversion*)
[@@js.call] [@@js.local]

let caml_format_float fmt x = 
  let f = _parse_format fmt in 
  let prec = if f.prec < 0 then 6 else f.prec in 
  let x = if x < 0. then (f.sign <- (-1); -. x) else x in 
  let s = ref "" in 
  if is_nan x then 
    begin 
      s := "nan";
      f.filter <- " "
    end
  else if not @@ is_finite x then
    begin 
      s := "inf";
      f.filter <- " " 
    end
  else 
    begin 
      match f.conv with 
      | "e"
        -> 
        s := to_exponential x prec;
        (* exponent should be at least two digits
           {[
             (3.3).toExponential()
               "3.3e+0"
               3.3e+00
           ]}
        *)
        let  i = String.length !s in 
        if !s.[i-3] = 'e' then
          begin 
            s := slice !s 0 (i - 1) ^ "0" ^ slice_rest !s (i - 1)
          end
      | "f"
        -> 
        (*  this will not work large numbers *)
        (* ("%3.10f", 3e+56, "300000000000000005792779041490073052596128503513888063488.0000000000") *)
        s := to_fixed x prec 
      | "g" -> 
        let prec = if prec <> 0 then prec else 1 in
        s := to_exponential x (prec - 1);
        let j = index_of !s "e" in 
        let  exp = to_number @@ slice_rest !s (j + 1)  in 
        if exp < -4 || x >= 1e21 || String.length (to_fixed x 0) > prec then 
          let i = ref (j - 1)  in
          while !s.[!i] = '0' do 
            decr i 
          done;
          if !s.[!i] = '.' then 
            decr i ;
          s := slice !s 0 (!i+1) ^ slice_rest !s j ;
          let i = String.length !s in 
          if !s.[i - 3] = 'e' then 
            s := slice !s 0 (i - 1) ^ "0" ^ slice_rest !s (i - 1) 
          else ()
        else 
          let p = ref prec in 
          if exp < 0 then 
            begin 
              p := !p - (exp + 1);
              s := to_fixed x !p 
            end
          else 
            while (s := to_fixed x !p; String.length !s > prec + 1) do 
              decr p
            done ;
          if !p <> 0 then 
            let k = ref @@ String.length !s - 1 in 
            while !s.[!k] = '0' do 
              decr k
            done ;
            if !s.[!k] = '.' then 
              decr k ;
            s := slice !s 0 (!k + 1) 

      | _ -> ()
    end;
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

|}
]



let caml_nativeint_format = caml_format_int
let caml_int32_format = caml_format_int

external caml_float_of_string : string -> float = ""
[@@js.call "$$caml_float_of_string"] [@@js.local]


let caml_int32_of_string = caml_int_of_string
let caml_nativeint_of_string = caml_int32_of_string
