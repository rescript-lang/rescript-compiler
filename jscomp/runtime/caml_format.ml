

(*
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
 *)







(** *)

let repeat = Caml_utils.repeat
let caml_failwith s = raise (Failure  s)
let caml_invalid_argument s= raise (Invalid_argument s )
let (^) = Bs_string.append
let (>>>) = Nativeint.shift_right_logical

let to_nat x = Nativeint.of_int x 
let of_nat x = Nativeint.to_int x 
let (+~) = Nativeint.add 
let ( *~ ) = Nativeint.mul  

let parse_digit c = 
  match c with 
  | '0' .. '9' 
    -> Char.code c - Char.code '0'
  | 'A' .. 'Z'
    -> Char.code c - (Char.code 'A' - 10)
  | 'a' .. 'z'
    -> Char.code c - (Char.code 'a' - 10 )
  | _ -> -1

type of_string_base =
  | Oct
  | Hex
  | Dec
  | Bin

let int_of_string_base = function
  | Oct -> 8
  | Hex -> 16
  | Dec -> 10
  | Bin -> 2
    
let parse_sign_and_base (s : string) = 
  let sign = ref 1n in
  let base = ref Dec in
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
       -> base := Hex; i:=!i + 2 
     | '0', ( 'o' | 'O')
       -> base := Oct; i := !i + 2
     | '0', ('b' | 'B' )
       -> base := Bin; i := !i + 2 
     | _, _ -> ()); 
    (!i, !sign, !base)
  end

let caml_int_of_string s = 
  let i, sign, hbase = parse_sign_and_base s in
  let base  = Nativeint.of_int @@ int_of_string_base hbase in
  let threshold = (-1n >>> 0) in 
  let len =Bs_string.length s in  
  let c = if i < len then s.[i] else '\000' in
  let d = to_nat @@ parse_digit c in
  let () =
    if d < 0n || d >=  base then
      caml_failwith "int_of_string" in
  (* let () = [%bs.debugger]  in *)
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
  (if base = 10n && res <> or_res then 
    caml_failwith "int_of_string");
  or_res


let caml_int64_of_string s = 
  let i, sign, hbase = parse_sign_and_base s in
  let base  = Int64.of_int @@ int_of_string_base hbase in
  let sign = Int64.of_nativeint sign in
  let threshold =
    match hbase with
    | Hex -> (* 2 ^ 64 - 1 / 16*)
      1152921504606846975L
    | Dec ->
      1844674407370955161L
    | Oct ->
      2305843009213693951L
    | Bin ->
      9223372036854775807L
  in 
  let len =Bs_string.length s in  
  let c = if i < len then s.[i] else '\000' in
  let d = Int64.of_int @@ parse_digit c in
  let () =
    if d < 0L || d >=  base then
      caml_failwith "int64_of_string" in
  let (+~) = Int64.add  in
  let ( *~ ) = Int64.mul  in

  let rec aux acc k = 
    if k = len then acc 
    else             
      let a = s.[k] in
      if a  = '_' then aux acc ( k +  1) 
      else     
        let v = Int64.of_int @@ parse_digit a in  
        if v < 0L || v >=  base || acc > threshold then 
          caml_failwith "int64_of_string"
        else 
          let acc = base *~ acc +~  v in 
          aux acc  ( k +   1)
  in 
  let res = sign *~ aux d (i + 1) in 
  let or_res = Int64.logor res 0L in 
  (if base = 10L && res <> or_res then 
    caml_failwith "int64_of_string");
  or_res

type base = 
  | Oct | Hex | Dec
let int_of_base = function
  | Oct -> 8
  | Hex -> 16
  | Dec -> 10
    
type fmt = {
  mutable justify : string; 
  mutable signstyle : string;
  mutable filter : string ;
  mutable alternate : bool;
  mutable base : base;
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

let parse_format fmt = 
  let len =Bs_string.length fmt in 
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
        f.signstyle <- Bs_string.of_char c ; 
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
        f.base <- Dec;
        aux f (i + 1)
      | 'u' -> 
        f.base <- Dec;
        aux f (i + 1)
      | 'x' -> 
        f.base <- Hex;
        aux f (i + 1)
      | 'X' -> 
        f.base <- Hex;
        f.uppercase <- true;
        aux f (i + 1)
      | 'o' -> 
        f.base <- Oct;
        aux f (i + 1)
      (* | 'O' -> base := 8; uppercase := true no uppercase for oct *)
      | 'e' | 'f' | 'g' 
        -> 
        f.signedconv <- true;
        f.conv <- Bs_string.of_char c ;
        aux  f (i + 1)
      | 'E' | 'F' | 'G' 
        -> 
        f.signedconv <- true;
        f.uppercase <- true;
        f.conv <- Bs_string.of_char (lowercase c);
        aux f (i + 1)
      | _ -> 
        aux f (i + 1) 
  in 
  aux   { justify =  "+" ;
          signstyle =   "-";
          filter =  " " ;
          alternate =  false ;
          base=  Dec ;
          signedconv=  false ;
          width =  0 ;
          uppercase=   false ;
          sign =  1 ;
          prec =  (-1); 
          conv =  "f"} 0 



let finish_formatting ({
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
      if base = Oct then
        incr len 
      else
      if base = Hex then 
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
  if alternate && base = Oct then 
    buffer := !buffer ^ "0";
  if alternate && base == Hex then
    buffer := !buffer ^ "0x";
    
  if justify = "+" && filter = "0" then 
    for i = !len to width - 1 do 
      buffer := !buffer ^ filter;
    done;
  begin 
    if uppercase then 
      buffer := !buffer ^ Bs_string.toUpperCase rawbuffer
    else
      buffer := !buffer ^ rawbuffer
  end;
  if justify = "-" then 
    for i = !len to width - 1 do 
      buffer := !buffer ^ " ";
    done;
  !buffer



let aux f (i : nativeint)  = 
  let i = 
    if i < 0n then 
      if f.signedconv then 
        begin 
          f.sign <- -1;
          Nativeint.neg i
        end
      else 
        Nativeint.shift_right_logical i 0 
    else  i  in
  let s = ref @@ Bs_string.of_nativeint i ~base:(int_of_base f.base) in 
  if f.prec >= 0 then 
    begin 
      f.filter <- " ";
      let n = f.prec -Bs_string.length !s in 
      if n > 0 then
        s :=  repeat n "0" [@bs]  ^ !s
    end ;
  finish_formatting f !s

let caml_format_int fmt i = 
  if fmt = "%d" then Js_nativeint.to_string i 
  else 
    let f = parse_format fmt in 
    aux f i 

let caml_int64_format fmt x =
  let f = parse_format fmt in
  let x =
    if f.signedconv &&  x < 0L then
      begin
        f.sign <- -1;
        Int64.neg x
      end
    else x in
  let s = ref "" in

  begin match f.base with
    | Hex ->
      s := Js_int64.to_hex x ^ !s       
    | Oct ->
      let wbase  = 8L  in
      let  cvtbl = "01234567" in

      if  x < 0L then
        begin         
          let y = Js_int64.discard_sign x  in
          (* 2 ^  63 + y `div_mod` 8 *)        
          let quotient_l  = 1152921504606846976L (**)
            (* {lo =   0n; hi =  268435456n } *) (* 2 ^ 31 / 8 *)
          in 

          (* let c, d = Caml_int64.div_mod (Caml_int64.add y modulus_l) wbase in
             we can not do the code above, it can overflow when y is really large           
          *)
          let c, d = Js_int64.div_mod  y  wbase in

          let quotient =
            ref (Int64.add quotient_l c )  in
          let modulus = ref d in
          s :=
            Bs_string.of_char 
              cvtbl.[ Int64.to_int !modulus] ^ !s ;

          while  !quotient <> 0L do
            let a, b = Js_int64.div_mod (!quotient) wbase in
            quotient := a;
            modulus := b;
            s := Bs_string.of_char cvtbl.[Int64.to_int !modulus] ^ !s ;
          done;
        end
      else
        let a, b =  Js_int64.div_mod x wbase  in
        let quotient = ref a  in
        let modulus = ref b in
        s :=
          Bs_string.of_char 
            cvtbl.[ Int64.to_int !modulus] ^ !s ;

        while  !quotient <> 0L do
          let a, b = Js_int64.div_mod (!quotient) wbase in
          quotient := a;
          modulus := b;
          s := Bs_string.of_char cvtbl.[Int64.to_int !modulus] ^ !s ;
        done

    | Dec ->
      let wbase  =  10L  in
      let  cvtbl = "0123456789" in

      if  x < 0L then
        let y  = Js_int64.discard_sign x  in
        (* 2 ^  63 + y `div_mod` 10 *)        

        let quotient_l  = 922337203685477580L (* 2 ^ 63 / 10 *)
          (* {lo =   -858993460n; hi =  214748364n} *)
          (* TODO:  int64 constant folding so that we can do idiomatic code
             2 ^ 63 / 10 *)in 
        let modulus_l  =  8L  in
        (* let c, d = Caml_int64.div_mod (Caml_int64.add y modulus_l) wbase in
           we can not do the code above, it can overflow when y is really large           
        *)
        let c, d = Js_int64.div_mod  y  wbase in
        let e ,f = Js_int64.div_mod (Int64.add modulus_l d) wbase in        
        let quotient =
          ref (Int64.add (Int64.add quotient_l c )
                 e)  in
        let modulus = ref f in
        s :=
          Bs_string.of_char 
            cvtbl.[Int64.to_int !modulus] ^ !s ;

        while !quotient <> 0L do
          let a, b = Js_int64.div_mod (!quotient) wbase in
          quotient := a;
          modulus := b;
          s := Bs_string.of_char cvtbl.[Int64.to_int !modulus] ^ !s ;
        done;

      else
        let a, b =  Js_int64.div_mod x wbase  in
        let quotient = ref a  in
        let modulus = ref b in
        s :=
          Bs_string.of_char 
            cvtbl.[ Int64.to_int !modulus] ^ !s ;

        while  !quotient <> 0L do
          let a, b = Js_int64.div_mod (!quotient) wbase in
          quotient := a;
          modulus := b;
          s := Bs_string.of_char cvtbl.[Int64.to_int !modulus] ^ !s ;
        done;
  end;
  if f.prec >= 0 then
    begin
      f.filter <- " ";
      let n = f.prec -Bs_string.length !s in
      if n > 0 then
        s := repeat n "0" [@bs] ^ !s
    end;

  finish_formatting f !s

let caml_format_float fmt x = 
  let f = parse_format fmt in 
  let prec = if f.prec < 0 then 6 else f.prec in 
  let x = if x < 0. then (f.sign <- (-1); -. x) else x in 
  let s = ref "" in 
  if Js_float.isNaN x then 
    begin 
      s := "nan";
      f.filter <- " "
    end
  else if not @@ Js_float.isFinite x then
    begin 
      s := "inf";
      f.filter <- " " 
    end
  else 
    begin 
      match f.conv with 
      | "e"
        -> 
        s := Js_float.toExponentialWithPrecision x ~digits:prec;
        (* exponent should be at least two digits
           {[
             (3.3).toExponential()
               "3.3e+0"
               3.3e+00
           ]}
        *)
        let  i =Bs_string.length !s in 
        if !s.[i-3] = 'e' then
          begin 
            s := Bs_string.slice !s 0 (i - 1) ^ "0" ^ Bs_string.slice_rest !s (i - 1)
          end
      | "f"
        -> 
        (*  this will not work large numbers *)
        (* ("%3.10f", 3e+56, "300000000000000005792779041490073052596128503513888063488.0000000000") *)
        s := Js_float.toFixedWithPrecision x ~digits:prec 
      | "g" -> 
        let prec = if prec <> 0 then prec else 1 in
        s := Js_float.toExponentialWithPrecision x ~digits:(prec - 1);
        let j = Bs_string.index_of !s "e" in 
        let  exp = int_of_float @@ Js_float.fromString @@ Bs_string.slice_rest !s (j + 1)  in 
        if exp < -4 || x >= 1e21 ||Bs_string.length (Js_float.toFixed x) > prec then 
          let i = ref (j - 1)  in
          while !s.[!i] = '0' do 
            decr i 
          done;
          if !s.[!i] = '.' then 
            decr i ;
          s := Bs_string.slice !s 0 (!i+1) ^ Bs_string.slice_rest !s j ;
          let i =Bs_string.length !s in 
          if !s.[i - 3] = 'e' then 
            s := Bs_string.slice !s 0 (i - 1) ^ "0" ^ Bs_string.slice_rest !s (i - 1) 
          else ()
        else 
          let p = ref prec in 
          if exp < 0 then 
            begin 
              p := !p - (exp + 1);
              s := Js_float.toFixedWithPrecision x ~digits:!p 
            end
          else 
            while (s := Js_float.toFixedWithPrecision x ~digits:!p;Bs_string.length !s > prec + 1) do 
              decr p
            done ;
          if !p <> 0 then 
            let k = ref @@Bs_string.length !s - 1 in 
            while !s.[!k] = '0' do 
              decr k
            done ;
            if !s.[!k] = '.' then 
              decr k ;
            s := Bs_string.slice !s 0 (!k + 1) 

      | _ -> ()
    end;
  finish_formatting f !s


(**
 external float_of_string : string -> float = "caml_float_of_string"
 pervasives.ml
 Semantics is slightly different from javascript :
 console.assert(caml_float_of_string('infinity')===Infinity)
 console.assert(caml_float_of_string('Infinity')===Infinity

 parseFloat('Infinity') === Infinity
 parseFloat('infinity') === Nan

 FIXME: arity of float_of_string is not inferred correctly
*)

let float_of_string : string -> (string -> 'a) ->  float = [%bs.raw {|
  function (s, caml_failwith) {
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

let caml_float_of_string s = float_of_string s caml_failwith

let caml_nativeint_format = caml_format_int
let caml_int32_format = caml_format_int



let caml_int32_of_string = caml_int_of_string
let caml_nativeint_of_string = caml_int32_of_string

