type t

external valueOf : t -> float = "" [@@bs.send]
(** returns the primitive value of this date, equivalent to getTime *)

external make : unit -> t = "Date" [@@bs.new]
(** returns a date representing the current time *)

external fromFloat : float -> t = "Date" [@@bs.new]
external fromString : string -> t = "Date" [@@bs.new]
let fromString s =
  let date = fromString s in
  if valueOf date |> Js_float.isNaN |> Js.to_bool  then
    None
  else
    Some date

external makeWithYM : year:float -> month:float -> t = "Date" [@@bs.new]
external makeWithYMD : year:float -> month:float -> date:float -> t = "Date" [@@bs.new]
external makeWithYMDH : year:float -> month:float -> date:float -> hours:float -> t = "Date" [@@bs.new]
external makeWithYMDHM : year:float -> month:float -> date:float -> hours:float -> minutes:float -> t = "Date" [@@bs.new]
external makeWithYMDHMS : year:float -> month:float -> date:float -> hours:float -> minutes:float -> seconds:float -> t = "Date" [@@bs.new]

external utcWithYM : year:float -> month:float -> float = "" [@@bs.val "Date.UTC"]
external utcWithYMD : year:float -> month:float -> date:float -> float = "" [@@bs.val "Date.UTC"]
external utcWithYMDH : year:float -> month:float -> date:float -> hours:float -> float = "" [@@bs.val "Date.UTC"]
external utcWithYMDHM : year:float -> month:float -> date:float -> hours:float -> minutes:float -> float = "" [@@bs.val "Date.UTC"]
external utcWithYMDHMS : year:float -> month:float -> date:float -> hours:float -> minutes:float -> seconds:float -> float = "" [@@bs.val "Date.UTC"]

external now : unit -> float = "" [@@bs.val "Date.now"]
(** returns the number of milliseconds since Unix epoch *)

external parse : string -> t = "Date" [@@bs.new]
(** returns NaN if passed invalid date string *)

external getDate : t -> float = "" [@@bs.send]
(** return the day of the month (1-31) *)
external getDay : t -> float = "" [@@bs.send]
(** returns the day of the week (0-6) *)
external getFullYear : t -> float = "" [@@bs.send]
external getHours : t -> float = "" [@@bs.send]
external getMilliseconds : t -> float = "" [@@bs.send]
external getMinutes : t -> float = "" [@@bs.send]
external getMonth : t -> float = "" [@@bs.send]
(** returns the month (0-11) *)
external getSeconds : t -> float = "" [@@bs.send]
external getTime : t -> float = "" [@@bs.send]
(** returns the number of milliseconds since Unix epoch *)
external getTimezoneOffset : t -> float = "" [@@bs.send]
external getUTCDate : t -> float = "" [@@bs.send]
(** return the day of the month (1-31) *)
external getUTCDay : t -> float = "" [@@bs.send]
(** returns the day of the week (0-6) *)
external getUTCFullYear : t -> float = "" [@@bs.send]
external getUTCHours : t -> float = "" [@@bs.send]
external getUTCMilliseconds : t -> float = "" [@@bs.send]
external getUTCMinutes : t -> float = "" [@@bs.send]
external getUTCMonth : t -> float = "" [@@bs.send]
(** returns the month (0-11) *)
external getUTCSeconds : t -> float = "" [@@bs.send]
external getYear : t -> float = "" [@@bs.send]
(** DEPRECATED, use `getFullYear` instead *)

external setDate : t -> float -> float = "" [@@bs.send]
external setFullYear : t -> float -> float = "" [@@bs.send]
external setFullYearM : t -> year:float -> month:float -> float = "setFullYear" [@@bs.send]
external setFullYearMD : t -> year:float -> month:float -> date:float -> float = "setFullYear" [@@bs.send]
external setHours : t -> float -> float = "" [@@bs.send]
external setHoursM : t -> hours:float -> minutes:float -> float = "setHours" [@@bs.send]
external setHoursMS : t -> hours:float -> minutes:float -> seconds:float -> float = "setHours" [@@bs.send]
external setHoursMSMs : t -> hours:float -> minutes:float -> seconds:float -> milliseconds:float -> float = "setHours" [@@bs.send]
external setMilliseconds : t -> float -> float = "" [@@bs.send]
external setMinutes : t -> float -> float = "" [@@bs.send]
external setMinutesS : t -> minutes:float -> seconds:float -> float = "setMinutes" [@@bs.send]
external setMinutesSMs : t -> minutes:float -> seconds:float -> milliseconds:float -> float = "setMinutes" [@@bs.send]
external setMonth : t -> float -> float = "" [@@bs.send]
external setMonthD : t -> month:float -> date:float -> float = "setMonth" [@@bs.send]
external setSeconds : t -> float -> float = "" [@@bs.send]
external setSecondsMs : t -> seconds:float -> milliseconds:float -> float = "setSeconds" [@@bs.send]
external setTime : t -> float -> float = "" [@@bs.send]
external setUTCDate : t -> float -> float = "" [@@bs.send]
external setUTCFullYear : t -> float -> float = "" [@@bs.send]
external setUTCFullYearM : t -> year:float -> month:float -> float = "setUTCFullYear" [@@bs.send]
external setUTCFullYearMD : t -> year:float -> month:float -> date:float -> float = "setUTCFullYear" [@@bs.send]
external setUTCHours : t -> float -> float = "" [@@bs.send]
external setUTCHoursM : t -> hours:float -> minutes:float -> float = "setUTCHours" [@@bs.send]
external setUTCHoursMS : t -> hours:float -> minutes:float -> seconds:float -> float = "setUTCHours" [@@bs.send]
external setUTCHoursMSMs : t -> hours:float -> minutes:float -> seconds:float -> milliseconds:float -> float = "setUTCHours" [@@bs.send]
external setUTCMilliseconds : t -> float -> float = "" [@@bs.send]
external setUTCMinutes : t -> float -> float = "" [@@bs.send]
external setUTCMinutesS : t -> minutes:float -> seconds:float -> float = "setUTCMinutes" [@@bs.send]
external setUTCMinutesSMs : t -> minutes:float -> seconds:float -> milliseconds:float -> float = "setUTCMinutes" [@@bs.send]
external setUTCMonth : t -> float -> float = "" [@@bs.send]
external setUTCMonthD : t -> month:float -> date:float -> float = "setUTCMonth" [@@bs.send]
external setUTCSeconds : t -> float -> float = "" [@@bs.send]
external setUTCSecondsMs : t -> seconds:float -> milliseconds:float -> float = "setUTCSeconds" [@@bs.send]
external setUTCTime : t -> float -> float = "" [@@bs.send]
external setYear : t -> float -> float = "" [@@bs.send]
(** DEPRECATED, use `setFullYear` instead *)

external toDateString : t -> string = "" [@@bs.send]
external toGMTString : t -> string = "" [@@bs.send]
(** DEPRECATED, use `toUTCString` instead *)
external toISOString : t -> string = "" [@@bs.send]
external toJSON : t -> string = "" [@@bs.send]
external toLocaleDateString : t -> string = "" [@@bs.send] (* TODO: has overloads with somewhat poor browser support *)
external toLocaleString: t -> string = "" [@@bs.send] (* TODO: has overloads with somewhat poor browser support *)
external toLocaleTimeString: t -> string = "" [@@bs.send] (* TODO: has overloads with somewhat poor browser support *)
external toString : t -> string = "" [@@bs.send]
external toTimeString : t -> string = "" [@@bs.send]
external toUTCString : t -> string = "" [@@bs.send]
