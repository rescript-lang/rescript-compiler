(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** JavaScript Date API *)

type t

external valueOf : t -> float = "valueOf" [@@bs.send]
(** returns the primitive value of this date, equivalent to getTime *)

external make : unit -> t = "Date" [@@bs.new]
(** returns a date representing the current time *)

external fromFloat : float -> t = "Date" [@@bs.new]
external fromString : string -> t = "Date" [@@bs.new]

external makeWithYM : year:float -> month:float -> unit -> t = "Date" [@@bs.new]
external makeWithYMD : year:float -> month:float -> date:float -> unit -> t = "Date" [@@bs.new]
external makeWithYMDH : year:float -> month:float -> date:float -> hours:float -> unit -> t = "Date" [@@bs.new]
external makeWithYMDHM : year:float -> month:float -> date:float -> hours:float -> minutes:float -> unit -> t = "Date" [@@bs.new]
external makeWithYMDHMS : year:float -> month:float -> date:float -> hours:float -> minutes:float -> seconds:float -> unit -> t = "Date" [@@bs.new]

external utcWithYM : year:float -> month:float -> unit -> float = "" [@@bs.val "Date.UTC"]
external utcWithYMD : year:float -> month:float -> date:float -> unit -> float = "" [@@bs.val "Date.UTC"]
external utcWithYMDH : year:float -> month:float -> date:float -> hours:float -> unit -> float = "" [@@bs.val "Date.UTC"]
external utcWithYMDHM : year:float -> month:float -> date:float -> hours:float -> minutes:float -> unit -> float = "" [@@bs.val "Date.UTC"]
external utcWithYMDHMS : year:float -> month:float -> date:float -> hours:float -> minutes:float -> seconds:float -> unit -> float = "" [@@bs.val "Date.UTC"]

external now : unit -> float = "" [@@bs.val "Date.now"]
(** returns the number of milliseconds since Unix epoch *)

external parse : string -> t = "Date" [@@bs.new]
[@@deprecated "Please use `fromString` instead"]

external parseAsFloat : string -> float = "" [@@bs.val "parse"] [@@bs.scope "Date"]
(** returns NaN if passed invalid date string *)

external getDate : t -> float = "getDate" [@@bs.send]
(** return the day of the month (1-31) *)

external getDay : t -> float = "getDay" [@@bs.send]
(** returns the day of the week (0-6) *)

external getFullYear : t -> float = "getFullYear" [@@bs.send]
external getHours : t -> float = "getHours" [@@bs.send]
external getMilliseconds : t -> float = "getMilliseconds" [@@bs.send]
external getMinutes : t -> float = "getMinutes" [@@bs.send]
external getMonth : t -> float = "getMonth" [@@bs.send]
(** returns the month (0-11) *)

external getSeconds : t -> float = "getSeconds" [@@bs.send]
external getTime : t -> float = "getTime" [@@bs.send]
(** returns the number of milliseconds since Unix epoch *)

external getTimezoneOffset : t -> float = "getTimezoneOffset" [@@bs.send]
external getUTCDate : t -> float = "getUTCDate" [@@bs.send]
(** return the day of the month (1-31) *)

external getUTCDay : t -> float = "getUTCDay" [@@bs.send]
(** returns the day of the week (0-6) *)

external getUTCFullYear : t -> float = "getUTCFullYear" [@@bs.send]
external getUTCHours : t -> float = "getUTCHours" [@@bs.send]
external getUTCMilliseconds : t -> float = "getUTCMilliseconds" [@@bs.send]
external getUTCMinutes : t -> float = "getUTCMinutes" [@@bs.send]
external getUTCMonth : t -> float = "getUTCMonth" [@@bs.send]
(** returns the month (0-11) *)

external getUTCSeconds : t -> float = "getUTCSeconds" [@@bs.send]
external getYear : t -> float = "getYear" [@@bs.send]
[@@deprecated "use `getFullYear` instead"]

external setDate : t -> float -> float = "setDate" [@@bs.send]
external setFullYear : t -> float -> float = "setFullYear" [@@bs.send]
external setFullYearM : t -> year:float -> month:float -> unit -> float = "setFullYear" [@@bs.send]
external setFullYearMD : t -> year:float -> month:float -> date:float -> unit -> float = "setFullYear" [@@bs.send]
external setHours : t -> float -> float = "setHours" [@@bs.send]
external setHoursM : t -> hours:float -> minutes:float -> unit -> float = "setHours" [@@bs.send]
external setHoursMS : t -> hours:float -> minutes:float -> seconds:float -> unit -> float = "setHours" [@@bs.send]
external setHoursMSMs : t -> hours:float -> minutes:float -> seconds:float -> milliseconds:float -> unit -> float = "setHours" [@@bs.send]
external setMilliseconds : t -> float -> float = "setMilliseconds" [@@bs.send]
external setMinutes : t -> float -> float = "setMinutes" [@@bs.send]
external setMinutesS : t -> minutes:float -> seconds:float -> unit -> float = "setMinutes" [@@bs.send]
external setMinutesSMs : t -> minutes:float -> seconds:float -> milliseconds:float -> unit -> float = "setMinutes" [@@bs.send]
external setMonth : t -> float -> float = "setMonth" [@@bs.send]
external setMonthD : t -> month:float -> date:float -> unit -> float = "setMonth" [@@bs.send]
external setSeconds : t -> float -> float = "setSeconds" [@@bs.send]
external setSecondsMs : t -> seconds:float -> milliseconds:float -> unit -> float = "setSeconds" [@@bs.send]
external setTime : t -> float -> float = "setTime" [@@bs.send]
external setUTCDate : t -> float -> float = "setUTCDate" [@@bs.send]
external setUTCFullYear : t -> float -> float = "setUTCFullYear" [@@bs.send]
external setUTCFullYearM : t -> year:float -> month:float -> unit -> float = "setUTCFullYear" [@@bs.send]
external setUTCFullYearMD : t -> year:float -> month:float -> date:float -> unit -> float = "setUTCFullYear" [@@bs.send]
external setUTCHours : t -> float -> float = "setUTCHours" [@@bs.send]
external setUTCHoursM : t -> hours:float -> minutes:float -> unit -> float = "setUTCHours" [@@bs.send]
external setUTCHoursMS : t -> hours:float -> minutes:float -> seconds:float -> unit -> float = "setUTCHours" [@@bs.send]
external setUTCHoursMSMs : t -> hours:float -> minutes:float -> seconds:float -> milliseconds:float -> unit -> float = "setUTCHours" [@@bs.send]
external setUTCMilliseconds : t -> float -> float = "setUTCMilliseconds" [@@bs.send]
external setUTCMinutes : t -> float -> float = "setUTCMinutes" [@@bs.send]
external setUTCMinutesS : t -> minutes:float -> seconds:float -> unit -> float = "setUTCMinutes" [@@bs.send]
external setUTCMinutesSMs : t -> minutes:float -> seconds:float -> milliseconds:float -> unit -> float = "setUTCMinutes" [@@bs.send]
external setUTCMonth : t -> float -> float = "setUTCMonth" [@@bs.send]
external setUTCMonthD : t -> month:float -> date:float -> unit -> float = "setUTCMonth" [@@bs.send]
external setUTCSeconds : t -> float -> float = "setUTCSeconds" [@@bs.send]
external setUTCSecondsMs : t -> seconds:float -> milliseconds:float -> unit -> float = "setUTCSeconds" [@@bs.send]
external setUTCTime : t -> float -> float = "setTime" [@@bs.send]
external setYear : t -> float -> float = "setYear" [@@bs.send]
[@@deprecated "use `setFullYear` instead"]

external toDateString : t -> string = "toDateString" [@@bs.send]
external toGMTString : t -> string = "toGMTString" [@@bs.send]
[@@deprecated "use `toUTCString` instead"]

external toISOString : t -> string = "toISOString" [@@bs.send]
external toJSON : t -> string = "toJSON" [@@bs.send]
[@@deprecated "This method is unsafe. It will be changed to return option in a future release. Please use toJSONUnsafe instead."]
external toJSONUnsafe : t -> string = "toJSON" [@@bs.send]
external toLocaleDateString : t -> string = "toLocaleDateString" [@@bs.send] (* TODO: has overloads with somewhat poor browser support *)
external toLocaleString: t -> string = "toLocaleString" [@@bs.send] (* TODO: has overloads with somewhat poor browser support *)
external toLocaleTimeString: t -> string = "toLocaleTimeString" [@@bs.send] (* TODO: has overloads with somewhat poor browser support *)
external toString : t -> string = "toString" [@@bs.send]
external toTimeString : t -> string = "toTimeString" [@@bs.send]
external toUTCString : t -> string = "toUTCString" [@@bs.send]
