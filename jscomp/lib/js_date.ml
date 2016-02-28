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

(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date *)
(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Default_parameters *)
(* https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split *)
(**
   JS API handles optional value inconsistent
   {[
      x= new Date();
      y= new Date(null);

      z = new Date(undefined);
   ]}   
   here [x] and [y] are behaving the same
   {[
     x = "ghos" ;
     x.split('');
     x.split('', undefined);
     x.split('', null);
   ]}
   here [split] and [split(undefined)] behaves the same
*)
type number = float
class type t = 
  object
    method toDateString__0 : string
    method toTimeString__0 : string
    method toLocaleString__0 : string
    method toLocaleDateString__0 : string
    method toLocaleTimeString__0 : string
    method valueOf__0 : number
    method getTime__0 : number
    method getFullYear__0 : number
    method getUTCFullYear__0 : number
    method getMonth__0 : number
    method getUTCMonth__0 : number
    method getDate__0 : number
    method getUTCDate__0 : number
    method getDay__0 : number
    method getUTCDay__0 : number
    method getHours__0 : number
    method getUTCHours__0 : number
    method getMinutes__0 :  number
    method getUTCMinutes__0 : number
    method getSeconds__0 : number
    method getUTCSeconds__0 : number
    method getMilliseconds__0 : number
    method getUTCMilliseconds__0 : number
    method getTimezoneOffset__0 : number
    method setTime__1  : number ->  number

    method setMilliseconds__1 : number ->  number
    method setUTCMilliseconds__1 : number ->  number

    method setSeconds__1 : number -> number
    method setSeconds__2 :  number -> number ->  number

    method setUTCSeconds__1 : number -> number
    method setUTCSeconds__2 : number -> number -> number

    method setMinutes__1 : number -> number
    method setMinutes__2 : number -> number -> number
    method setMinutes__3 : number -> number -> number -> number

    method setUTCMinutes__1 : number -> number
    method setUTCMinutes__2 : number -> number -> number
    method setUTCMinutes__3 : number -> number -> number -> number

    method setHours__1 : number -> number
    method setHours__2 : number -> number -> number
    method setHours__3 : number -> number -> number -> number
    method setHours__4 : number -> number -> number -> number -> number

    method setUTCHours__1 : number -> number
    method setUTCHours__2 : number -> number -> number
    method setUTCHours__3 : number -> number -> number -> number
    method setUTCHours__4 : number -> number -> number -> number -> number



    method setDate__1 :  number ->  number
    method setUTCDate__1 : number ->  number 
    method setMonth__1 : number -> number
    method setMonth__2 : number -> number -> number
    method setUTCMonth__1 : number -> number
    method setUTCMonth__2 : number -> number -> number


    method setFullYear__1 : number -> number
    method setFullYear__2 : number -> number -> number
    method setFullYear__3 : number -> number -> number -> number

    method setUTCFullYear__1 : number -> number
    method setUTCFullYear__2 : number -> number -> number
    method setUTCFullYear__3 : number -> number -> number -> number

    method toUTCString__ : string
    method toISOString__ : string
    method toJSON__ : string 
    method toJSON__1 : 'a -> string
  end

external current_date_as_string : unit -> string = "" 
  [@@js.call "Date"] [@@js.nullary]
(* Note here [Date(0)] is the same as [Date()], 
   but this is not always true
*)


external parse : string -> number = ""
  [@@js.call "Date.parse"]

external now : unit -> number = ""
  [@@js.call "Date.now"]

external current : unit -> t = ""
  [@@js.new "Date"] [@@js.nullary]
external of_string : string -> t = ""
  [@@js.new "Date"]



external utc_of_y_m :   
  year:number -> month:number -> unit -> number = ""
  [@@js.call "Date.UTC"]

external utc_of_y_m_d :  
  year:number -> month:number -> day:number ->   unit -> number = ""
  [@@js.call "Date.UTC"]

external utc_of_y_m_d_h :  
  year:number -> month:number -> 
  day:number -> hour:number ->
  unit -> number = ""
  [@@js.call "Date.UTC"]


external utc_of_y_m_d_h_m :
  year:number -> month:number -> 
  day:number -> hour:number ->
  minute:number -> 
  unit -> number = ""
  [@@js.call "Date.UTC"]


external utc_of_y_m_d_h_m_s :
  year:number -> month:number -> 
  day:number -> hour:number ->
  minute:number -> second:number -> 
  unit -> number = ""
  [@@js.call "Date.UTC"]

external utc_of_y_m_d_h_m_s_m :
  year:number -> month:number -> 
  day:number -> hour:number  -> 
  minute:number  -> second:number  ->
  millisecond:number -> unit -> t = ""
  [@@js.call "Date.UTC"]


external of_y_m:   year:number -> month:number -> unit -> t = ""
  [@@js.new "Date"]

external of_y_m_d:  year:number -> month:number -> day:number ->   unit -> t = ""
  [@@js.new "Date"]

external of_y_m_d_h:  
  year:number -> month:number -> 
  day:number -> hour:number ->
  unit -> t = ""
  [@@js.new "Date"]

external of_y_m_d_h_m :
  year:number -> month:number -> 
  day:number -> hour:number ->
  minute:number -> 
  unit -> t = ""
  [@@js.new "Date"]

external of_y_m_d_h_m_s :
  year:number -> month:number -> 
  day:number -> hour:number ->
  minute:number -> second:number -> 
  unit -> t = ""
  [@@js.new "Date"]

external of_y_m_d_h_m_s_m:
  year:number -> month:number -> 
  day:number -> hour:number  -> 
  minute:number  -> second:number  ->
  millisecond:number -> unit -> t = ""
  [@@js.new "Date"]




