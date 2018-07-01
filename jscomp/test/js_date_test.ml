
let date () =
  Js.Date.fromString "1976-03-08T12:34:56.789+01:23"

let suites = Mt.[
    "valueOf", (fun _ ->
      Eq(195131516789., Js.Date.valueOf (date ())));

    "make", (fun _ ->
      Ok((Js.Date.make () |> Js.Date.getTime) > 1487223505382.));

    "parseAsFloat", (fun _ ->
      Eq(Js.Date.parseAsFloat "1976-03-08T12:34:56.789+01:23", 195131516789.));

    "parseAsFloat_invalid", (fun _ ->
      Ok(Js.Date.parseAsFloat "gibberish" |> Js_float.isNaN));

    "fromFloat", (fun _ ->
      Eq("1976-03-08T11:11:56.789Z",
         Js.Date.fromFloat 195131516789. |> Js.Date.toISOString));

    "fromString_valid", (fun _ ->
      Eq( 195131516789.,
          Js.Date.fromString "1976-03-08T12:34:56.789+01:23"
          |> Js.Date.getTime));

    "fromString_invalid", (fun _ ->
      Ok( Js.Date.fromString "gibberish"
          |> Js.Date.getTime
          |> Js_float.isNaN
          ));

    "makeWithYM", (fun _ ->
      let d = Js.Date.makeWithYM ~year:1984.
                                 ~month:4. () in

      Eq( (1984., 4.),
          (Js.Date.getFullYear d,
           Js.Date.getMonth d))
    );
    "makeWithYMD", (fun _ ->
      let d = Js.Date.makeWithYMD ~year:1984.
                                  ~month:4.
                                  ~date:6. () in

      Eq( (1984., 4., 6.),
          (Js.Date.getFullYear d,
           Js.Date.getMonth d,
           Js.Date.getDate d))
    );
    "makeWithYMDH", (fun _ ->
      let d = Js.Date.makeWithYMDH ~year:1984.
                                   ~month:4.
                                   ~date:6.
                                   ~hours:3. () in

      Eq( (1984., 4., 6., 3.),
          (Js.Date.getFullYear d,
           Js.Date.getMonth d,
           Js.Date.getDate d,
           Js.Date.getHours d))
    );
    "makeWithYMDHM", (fun _ ->
      let d = Js.Date.makeWithYMDHM ~year:1984.
                                    ~month:4.
                                    ~date:6.
                                    ~hours:3.
                                    ~minutes:59. () in

      Eq( (1984., 4., 6., 3., 59.),
          (Js.Date.getFullYear d,
           Js.Date.getMonth d,
           Js.Date.getDate d,
           Js.Date.getHours d,
           Js.Date.getMinutes d))
    );
    "makeWithYMDHMS", (fun _ ->
      let d = Js.Date.makeWithYMDHMS ~year:1984.
                                     ~month:4.
                                     ~date:6.
                                     ~hours:3.
                                     ~minutes:59.
                                     ~seconds:27. () in

      Eq( (1984., 4., 6., 3., 59., 27.),
          (Js.Date.getFullYear d,
           Js.Date.getMonth d,
           Js.Date.getDate d,
           Js.Date.getHours d,
           Js.Date.getMinutes d,
           Js.Date.getSeconds d))
    );

    "utcWithYM", (fun _ ->
      let d = Js.Date.utcWithYM ~year:1984.
                                ~month:4. () in
      let d = Js.Date.fromFloat d in

      Eq( (1984., 4.),
          (Js.Date.getUTCFullYear d,
           Js.Date.getUTCMonth d))
    );
    "utcWithYMD", (fun _ ->
      let d = Js.Date.utcWithYMD ~year:1984.
                                 ~month:4.
                                 ~date:6. () in
      let d = Js.Date.fromFloat d in

      Eq( (1984., 4., 6.),
          (Js.Date.getUTCFullYear d,
           Js.Date.getUTCMonth d,
           Js.Date.getUTCDate d))
    );
    "utcWithYMDH", (fun _ ->
      let d = Js.Date.utcWithYMDH ~year:1984.
                                  ~month:4.
                                  ~date:6.
                                  ~hours:3. () in
      let d = Js.Date.fromFloat d in

      Eq( (1984., 4., 6., 3.),
          (Js.Date.getUTCFullYear d,
           Js.Date.getUTCMonth d,
           Js.Date.getUTCDate d,
           Js.Date.getUTCHours d))
    );
    "utcWithYMDHM", (fun _ ->
      let d = Js.Date.utcWithYMDHM ~year:1984.
                                   ~month:4.
                                   ~date:6.
                                   ~hours:3.
                                   ~minutes:59. () in
      let d = Js.Date.fromFloat d in

      Eq( (1984., 4., 6., 3., 59.),
          (Js.Date.getUTCFullYear d,
           Js.Date.getUTCMonth d,
           Js.Date.getUTCDate d,
           Js.Date.getUTCHours d,
           Js.Date.getUTCMinutes d))
    );
    "utcWithYMDHMS", (fun _ ->
      let d = Js.Date.utcWithYMDHMS ~year:1984.
                                    ~month:4.
                                    ~date:6.
                                    ~hours:3.
                                    ~minutes:59.
                                    ~seconds:27. () in
      let d = Js.Date.fromFloat d in

      Eq( (1984., 4., 6., 3., 59., 27.),
          (Js.Date.getUTCFullYear d,
           Js.Date.getUTCMonth d,
           Js.Date.getUTCDate d,
           Js.Date.getUTCHours d,
           Js.Date.getUTCMinutes d,
           Js.Date.getUTCSeconds d))
    );

    (* locale dependent
    "getDate", (fun _ ->
      Eq(8., Js.Date.getDate (date ())));
    "getDay", (fun _ ->
      Eq(1., Js.Date.getDay (date ())));
    *)
    "getFullYear", (fun _ ->
      Eq(1976., Js.Date.getFullYear (date ())));
    (* locale dependent
    "getHours", (fun _ ->
      Eq(12., Js.Date.getHours (date ())));
    *)
    "getMilliseconds", (fun _ ->
      Eq(789., Js.Date.getMilliseconds (date ())));
    (* locale dependent
    "getMinutes", (fun _ ->
      Eq(11., Js.Date.getMinutes (date ())));
    "getMonth", (fun _ ->
      Eq(2., Js.Date.getMonth (date ())));
    *)
    "getSeconds", (fun _ ->
      Eq(56., Js.Date.getSeconds (date ())));
    "getTime", (fun _ ->
      Eq(195131516789., Js.Date.getTime (date ())));
    (* locale depdendent
    "getTimezoneOffset", (fun _ ->
      Eq(-60., Js.Date.getTimezoneOffset (date ())));
    *)
    "getUTCDate", (fun _ ->
      Eq(8., Js.Date.getUTCDate (date ())));
    "getUTCDay", (fun _ ->
      Eq(1., Js.Date.getUTCDay (date ())));
    "getUTCFUllYear", (fun _ ->
      Eq(1976., Js.Date.getUTCFullYear (date ())));
    "getUTCHours", (fun _ ->
      Eq(11., Js.Date.getUTCHours (date ())));
    "getUTCMilliseconds", (fun _ ->
      Eq(789., Js.Date.getUTCMilliseconds (date ())));
    "getUTCMinutes", (fun _ ->
      Eq(11., Js.Date.getUTCMinutes (date ())));
    "getUTCMonth", (fun _ ->
      Eq(2., Js.Date.getUTCMonth (date ())));
    "getUTCSeconds", (fun _ ->
      Eq(56., Js.Date.getUTCSeconds (date ())));
    "getYear", (fun _ ->
      Eq(76., Js.Date.getYear (date ())));

    "setDate", (fun _ ->
      let d = date () in
      let _ = Js.Date.setDate d 12. in

      Eq(12., Js.Date.getDate d)
    );
    "setFullYear", (fun _ ->
      let d = date () in
      let _ = Js.Date.setFullYear d 1986. in

      Eq(1986., Js.Date.getFullYear d)
    );
    "setFullYearM", (fun _ ->
      let d = date () in
      let _ = Js.Date.setFullYearM d ~year:1986.
                                     ~month:7. () in
      Eq( (1986., 7.),
          (Js.Date.getFullYear d,
           Js.Date.getMonth d))
    );
    "setFullYearMD", (fun _ ->
      let d = date () in
      let _ = Js.Date.setFullYearMD d ~year:1986.
                                      ~month:7.
                                      ~date:23. () in
      Eq( (1986., 7., 23.),
          (Js.Date.getFullYear d,
           Js.Date.getMonth d,
           Js.Date.getDate d))
    );
    "setHours", (fun _ ->
      let d = date () in
      let _ = Js.Date.setHours d 22. in

      Eq(22., Js.Date.getHours d)
    );
    "setHoursM", (fun _ ->
      let d = date () in
      let _ = Js.Date.setHoursM d ~hours:22.
                                  ~minutes:48. () in
      Eq( (22., 48.),
          (Js.Date.getHours d,
           Js.Date.getMinutes d))
    );
    "setHoursMS", (fun _ ->
      let d = date () in
      let _ = Js.Date.setHoursMS d ~hours:22.
                                   ~minutes:48.
                                   ~seconds:54. () in
      Eq( (22., 48., 54.),
          (Js.Date.getHours d,
           Js.Date.getMinutes d,
           Js.Date.getSeconds d))
    );
    "setMilliseconds", (fun _ ->
      let d = date () in
      let _ = Js.Date.setMilliseconds d 543. in

      Eq(543., Js.Date.getMilliseconds d)
    );
    "setMinutes", (fun _ ->
      let d = date () in
      let _ = Js.Date.setMinutes d 18. in

      Eq(18., Js.Date.getMinutes d)
    );
    "setMinutesS", (fun _ ->
      let d = date () in
      let _ = Js.Date.setMinutesS d ~minutes:18.
                                    ~seconds:42. () in
      Eq( (18., 42.),
         (Js.Date.getMinutes d,
          Js.Date.getSeconds d))
    );
    "setMinutesSMs", (fun _ ->
      let d = date () in
      let _ = Js.Date.setMinutesSMs d ~minutes:18.
                                      ~seconds:42.
                                      ~milliseconds:311. () in
      Eq( (18., 42., 311.),
          (Js.Date.getMinutes d,
           Js.Date.getSeconds d,
           Js.Date.getMilliseconds d))
    );
    "setMonth", (fun _ ->
      let d = date () in
      let _ = Js.Date.setMonth d 10. in

      Eq(10., Js.Date.getMonth d)
    );
    "setMonthD", (fun _ ->
      let d = date () in
      let _ = Js.Date.setMonthD d ~month:10.
                                  ~date:14. () in
      Eq( (10., 14.),
          (Js.Date.getMonth d,
           Js.Date.getDate d))
    );
    "setSeconds", (fun _ ->
      let d = date () in
      let _ = Js.Date.setSeconds d 36. in

      Eq(36., Js.Date.getSeconds d)
    );
    "setSecondsMs", (fun _ ->
      let d = date () in
      let _ = Js.Date.setSecondsMs d ~seconds:36.
                                     ~milliseconds:420. () in
      Eq( (36., 420.),
          (Js.Date.getSeconds d,
           Js.Date.getMilliseconds d))
    );
    "setUTCDate", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCDate d 12. in

      Eq(12., Js.Date.getUTCDate d)
    );
    "setUTCFullYear", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCFullYear d 1986. in

      Eq(1986., Js.Date.getUTCFullYear d)
    );
    "setUTCFullYearM", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCFullYearM d ~year:1986.
                                        ~month:7. () in
      Eq( (1986., 7.),
          (Js.Date.getUTCFullYear d,
           Js.Date.getUTCMonth d))
    );
    "setUTCFullYearMD", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCFullYearMD d ~year:1986.
                                         ~month:7.
                                         ~date:23. () in
      Eq( (1986., 7., 23.),
          (Js.Date.getUTCFullYear d,
           Js.Date.getUTCMonth d,
           Js.Date.getUTCDate d))
    );
    "setUTCHours", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCHours d 22. in

      Eq(22., Js.Date.getUTCHours d)
    );
    "setUTCHoursM", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCHoursM d ~hours:22.
                                     ~minutes:48. () in
      Eq( (22., 48.),
          (Js.Date.getUTCHours d,
           Js.Date.getUTCMinutes d))
    );
    "setUTCHoursMS", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCHoursMS d ~hours:22.
                                      ~minutes:48.
                                      ~seconds:54. () in
      Eq( (22., 48., 54.),
          (Js.Date.getUTCHours d,
           Js.Date.getUTCMinutes d,
           Js.Date.getUTCSeconds d))
    );
    "setUTCMilliseconds", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCMilliseconds d 543. in

      Eq(543., Js.Date.getUTCMilliseconds d)
    );
    "setUTCMinutes", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCMinutes d 18. in

      Eq(18., Js.Date.getUTCMinutes d)
    );
    "setUTCMinutesS", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCMinutesS d ~minutes:18.
                                       ~seconds:42. () in
      Eq( (18., 42.),
          (Js.Date.getUTCMinutes d,
           Js.Date.getUTCSeconds d))
    );
    "setUTCMinutesSMs", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCMinutesSMs d ~minutes:18.
                                         ~seconds:42.
                                        ~milliseconds:311. () in
      Eq( (18., 42., 311.),
          (Js.Date.getUTCMinutes d,
           Js.Date.getUTCSeconds d,
           Js.Date.getUTCMilliseconds d))
    );
    "setUTCMonth", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCMonth d 10. in

      Eq(10., Js.Date.getUTCMonth d)
    );
    "setUTCMonthD", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCMonthD d ~month:10.
                                     ~date:14. () in
      Eq( (10., 14.),
          (Js.Date.getUTCMonth d,
           Js.Date.getUTCDate d))
    );
    "setUTCSeconds", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCSeconds d 36. in

      Eq(36., Js.Date.getUTCSeconds d)
    );
    "setUTCSecondsMs", (fun _ ->
      let d = date () in
      let _ = Js.Date.setUTCSecondsMs d ~seconds:36.
                                        ~milliseconds:420. () in
      Eq( (36., 420.),
          (Js.Date.getUTCSeconds d,
           Js.Date.getUTCMilliseconds d))
    );

    "toDateString", (fun _ ->
      Eq("Mon Mar 08 1976", Js.Date.toDateString (date ())));
    "toGMTString", (fun _ ->
      Eq("Mon, 08 Mar 1976 11:11:56 GMT", Js.Date.toUTCString (date ())));
    "toISOString", (fun _ ->
      Eq("1976-03-08T11:11:56.789Z", Js.Date.toISOString (date ())));
    "toJSON", (fun _ ->
      Eq("1976-03-08T11:11:56.789Z", Js.Date.toJSON (date ())));
    (* locale dependent
    "toLocaleDateString", (fun _ ->
      Eq("3/8/1976", Js.Date.toLocaleDateString (date ())));
    "toLocaleString", (fun _ ->
      Eq("3/8/1976, 12:11:56 PM", Js.Date.toLocaleString (date ())));
    "toLocaleTimeString", (fun _ ->
      Eq("12:11:56 PM", Js.Date.toLocaleTimeString (date ())));
    "toString", (fun _ ->
      Eq("Mon Mar 08 1976 12:11:56 GMT+0100 (CET)", Js.Date.toString (date ())));
    "toTimeString", (fun _ ->
      Eq("12:11:56 GMT+0100 (CET)", Js.Date.toTimeString (date ())));
    *)
    "toUTCString", (fun _ ->
      Eq("Mon, 08 Mar 1976 11:11:56 GMT", Js.Date.toUTCString (date ())));
]

;; Mt.from_pair_suites __FILE__ suites
