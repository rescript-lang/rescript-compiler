
module N = Js.Date

let date () =
  N.fromString "1976-03-08T12:34:56.789+01:23"

let suites = Mt.[
    "valueOf", (fun _ ->
      Eq(195131516789., N.valueOf (date ())));

    "make", (fun _ ->
      Ok((N.make () |> N.getTime) > 1487223505382.));

    "parseAsFloat", (fun _ ->
      Eq(N.parseAsFloat "1976-03-08T12:34:56.789+01:23", 195131516789.));

    "parseAsFloat_invalid", (fun _ ->
      Ok(N.parseAsFloat "gibberish" |> Js_float.isNaN));

    "fromFloat", (fun _ ->
      Eq("1976-03-08T11:11:56.789Z",
         N.fromFloat 195131516789. |> N.toISOString));

    "fromString_valid", (fun _ ->
      Eq( 195131516789.,
          N.fromString "1976-03-08T12:34:56.789+01:23"
          |> N.getTime));

    "fromString_invalid", (fun _ ->
      Ok( N.fromString "gibberish"
          |> N.getTime
          |> Js_float.isNaN
          ));

    "makeWithYM", (fun _ ->
      let d = N.makeWithYM ~year:1984.
                                 ~month:4. () in

      Eq( (1984., 4.),
          (N.getFullYear d,
           N.getMonth d))
    );
    "makeWithYMD", (fun _ ->
      let d = N.makeWithYMD ~year:1984.
                                  ~month:4.
                                  ~date:6. () in

      Eq( (1984., 4., 6.),
          (N.getFullYear d,
           N.getMonth d,
           N.getDate d))
    );
    "makeWithYMDH", (fun _ ->
      let d = N.makeWithYMDH ~year:1984.
                                   ~month:4.
                                   ~date:6.
                                   ~hours:3. () in

      Eq( (1984., 4., 6., 3.),
          (N.getFullYear d,
           N.getMonth d,
           N.getDate d,
           N.getHours d))
    );
    "makeWithYMDHM", (fun _ ->
      let d = N.makeWithYMDHM ~year:1984.
                                    ~month:4.
                                    ~date:6.
                                    ~hours:3.
                                    ~minutes:59. () in

      Eq( (1984., 4., 6., 3., 59.),
          (N.getFullYear d,
           N.getMonth d,
           N.getDate d,
           N.getHours d,
           N.getMinutes d))
    );
    "makeWithYMDHMS", (fun _ ->
      let d = N.makeWithYMDHMS ~year:1984.
                                     ~month:4.
                                     ~date:6.
                                     ~hours:3.
                                     ~minutes:59.
                                     ~seconds:27. () in

      Eq( (1984., 4., 6., 3., 59., 27.),
          (N.getFullYear d,
           N.getMonth d,
           N.getDate d,
           N.getHours d,
           N.getMinutes d,
           N.getSeconds d))
    );

    "utcWithYM", (fun _ ->
      let d = N.utcWithYM ~year:1984.
                                ~month:4. () in
      let d = N.fromFloat d in

      Eq( (1984., 4.),
          (N.getUTCFullYear d,
           N.getUTCMonth d))
    );
    "utcWithYMD", (fun _ ->
      let d = N.utcWithYMD ~year:1984.
                                 ~month:4.
                                 ~date:6. () in
      let d = N.fromFloat d in

      Eq( (1984., 4., 6.),
          (N.getUTCFullYear d,
           N.getUTCMonth d,
           N.getUTCDate d))
    );
    "utcWithYMDH", (fun _ ->
      let d = N.utcWithYMDH ~year:1984.
                                  ~month:4.
                                  ~date:6.
                                  ~hours:3. () in
      let d = N.fromFloat d in

      Eq( (1984., 4., 6., 3.),
          (N.getUTCFullYear d,
           N.getUTCMonth d,
           N.getUTCDate d,
           N.getUTCHours d))
    );
    "utcWithYMDHM", (fun _ ->
      let d = N.utcWithYMDHM ~year:1984.
                                   ~month:4.
                                   ~date:6.
                                   ~hours:3.
                                   ~minutes:59. () in
      let d = N.fromFloat d in

      Eq( (1984., 4., 6., 3., 59.),
          (N.getUTCFullYear d,
           N.getUTCMonth d,
           N.getUTCDate d,
           N.getUTCHours d,
           N.getUTCMinutes d))
    );
    "utcWithYMDHMS", (fun _ ->
      let d = N.utcWithYMDHMS ~year:1984.
                                    ~month:4.
                                    ~date:6.
                                    ~hours:3.
                                    ~minutes:59.
                                    ~seconds:27. () in
      let d = N.fromFloat d in

      Eq( (1984., 4., 6., 3., 59., 27.),
          (N.getUTCFullYear d,
           N.getUTCMonth d,
           N.getUTCDate d,
           N.getUTCHours d,
           N.getUTCMinutes d,
           N.getUTCSeconds d))
    );

    (* locale dependent
    "getDate", (fun _ ->
      Eq(8., N.getDate (date ())));
    "getDay", (fun _ ->
      Eq(1., N.getDay (date ())));
    *)
    "getFullYear", (fun _ ->
      Eq(1976., N.getFullYear (date ())));
    (* locale dependent
    "getHours", (fun _ ->
      Eq(12., N.getHours (date ())));
    *)
    "getMilliseconds", (fun _ ->
      Eq(789., N.getMilliseconds (date ())));
    (* locale dependent
    "getMinutes", (fun _ ->
      Eq(11., N.getMinutes (date ())));
    "getMonth", (fun _ ->
      Eq(2., N.getMonth (date ())));
    *)
    "getSeconds", (fun _ ->
      Eq(56., N.getSeconds (date ())));
    "getTime", (fun _ ->
      Eq(195131516789., N.getTime (date ())));
    (* locale depdendent
    "getTimezoneOffset", (fun _ ->
      Eq(-60., N.getTimezoneOffset (date ())));
    *)
    "getUTCDate", (fun _ ->
      Eq(8., N.getUTCDate (date ())));
    "getUTCDay", (fun _ ->
      Eq(1., N.getUTCDay (date ())));
    "getUTCFUllYear", (fun _ ->
      Eq(1976., N.getUTCFullYear (date ())));
    "getUTCHours", (fun _ ->
      Eq(11., N.getUTCHours (date ())));
    "getUTCMilliseconds", (fun _ ->
      Eq(789., N.getUTCMilliseconds (date ())));
    "getUTCMinutes", (fun _ ->
      Eq(11., N.getUTCMinutes (date ())));
    "getUTCMonth", (fun _ ->
      Eq(2., N.getUTCMonth (date ())));
    "getUTCSeconds", (fun _ ->
      Eq(56., N.getUTCSeconds (date ())));
    "getYear", (fun _ ->
      Eq(1976., N.getFullYear (date ())));

    "setDate", (fun _ ->
      let d = date () in
      let _ = N.setDate d 12. in

      Eq(12., N.getDate d)
    );
    "setFullYear", (fun _ ->
      let d = date () in
      let _ = N.setFullYear d 1986. in

      Eq(1986., N.getFullYear d)
    );
    "setFullYearM", (fun _ ->
      let d = date () in
      let _ = N.setFullYearM d ~year:1986.
                                     ~month:7. () in
      Eq( (1986., 7.),
          (N.getFullYear d,
           N.getMonth d))
    );
    "setFullYearMD", (fun _ ->
      let d = date () in
      let _ = N.setFullYearMD d ~year:1986.
                                      ~month:7.
                                      ~date:23. () in
      Eq( (1986., 7., 23.),
          (N.getFullYear d,
           N.getMonth d,
           N.getDate d))
    );
    "setHours", (fun _ ->
      let d = date () in
      let _ = N.setHours d 22. in

      Eq(22., N.getHours d)
    );
    "setHoursM", (fun _ ->
      let d = date () in
      let _ = N.setHoursM d ~hours:22.
                                  ~minutes:48. () in
      Eq( (22., 48.),
          (N.getHours d,
           N.getMinutes d))
    );
    "setHoursMS", (fun _ ->
      let d = date () in
      let _ = N.setHoursMS d ~hours:22.
                                   ~minutes:48.
                                   ~seconds:54. () in
      Eq( (22., 48., 54.),
          (N.getHours d,
           N.getMinutes d,
           N.getSeconds d))
    );
    "setMilliseconds", (fun _ ->
      let d = date () in
      let _ = N.setMilliseconds d 543. in

      Eq(543., N.getMilliseconds d)
    );
    "setMinutes", (fun _ ->
      let d = date () in
      let _ = N.setMinutes d 18. in

      Eq(18., N.getMinutes d)
    );
    "setMinutesS", (fun _ ->
      let d = date () in
      let _ = N.setMinutesS d ~minutes:18.
                                    ~seconds:42. () in
      Eq( (18., 42.),
         (N.getMinutes d,
          N.getSeconds d))
    );
    "setMinutesSMs", (fun _ ->
      let d = date () in
      let _ = N.setMinutesSMs d ~minutes:18.
                                      ~seconds:42.
                                      ~milliseconds:311. () in
      Eq( (18., 42., 311.),
          (N.getMinutes d,
           N.getSeconds d,
           N.getMilliseconds d))
    );
    "setMonth", (fun _ ->
      let d = date () in
      let _ = N.setMonth d 10. in

      Eq(10., N.getMonth d)
    );
    "setMonthD", (fun _ ->
      let d = date () in
      let _ = N.setMonthD d ~month:10.
                                  ~date:14. () in
      Eq( (10., 14.),
          (N.getMonth d,
           N.getDate d))
    );
    "setSeconds", (fun _ ->
      let d = date () in
      let _ = N.setSeconds d 36. in

      Eq(36., N.getSeconds d)
    );
    "setSecondsMs", (fun _ ->
      let d = date () in
      let _ = N.setSecondsMs d ~seconds:36.
                                     ~milliseconds:420. () in
      Eq( (36., 420.),
          (N.getSeconds d,
           N.getMilliseconds d))
    );
    "setUTCDate", (fun _ ->
      let d = date () in
      let _ = N.setUTCDate d 12. in

      Eq(12., N.getUTCDate d)
    );
    "setUTCFullYear", (fun _ ->
      let d = date () in
      let _ = N.setUTCFullYear d 1986. in

      Eq(1986., N.getUTCFullYear d)
    );
    "setUTCFullYearM", (fun _ ->
      let d = date () in
      let _ = N.setUTCFullYearM d ~year:1986.
                                        ~month:7. () in
      Eq( (1986., 7.),
          (N.getUTCFullYear d,
           N.getUTCMonth d))
    );
    "setUTCFullYearMD", (fun _ ->
      let d = date () in
      let _ = N.setUTCFullYearMD d ~year:1986.
                                         ~month:7.
                                         ~date:23. () in
      Eq( (1986., 7., 23.),
          (N.getUTCFullYear d,
           N.getUTCMonth d,
           N.getUTCDate d))
    );
    "setUTCHours", (fun _ ->
      let d = date () in
      let _ = N.setUTCHours d 22. in

      Eq(22., N.getUTCHours d)
    );
    "setUTCHoursM", (fun _ ->
      let d = date () in
      let _ = N.setUTCHoursM d ~hours:22.
                                     ~minutes:48. () in
      Eq( (22., 48.),
          (N.getUTCHours d,
           N.getUTCMinutes d))
    );
    "setUTCHoursMS", (fun _ ->
      let d = date () in
      let _ = N.setUTCHoursMS d ~hours:22.
                                      ~minutes:48.
                                      ~seconds:54. () in
      Eq( (22., 48., 54.),
          (N.getUTCHours d,
           N.getUTCMinutes d,
           N.getUTCSeconds d))
    );
    "setUTCMilliseconds", (fun _ ->
      let d = date () in
      let _ = N.setUTCMilliseconds d 543. in

      Eq(543., N.getUTCMilliseconds d)
    );
    "setUTCMinutes", (fun _ ->
      let d = date () in
      let _ = N.setUTCMinutes d 18. in

      Eq(18., N.getUTCMinutes d)
    );
    "setUTCMinutesS", (fun _ ->
      let d = date () in
      let _ = N.setUTCMinutesS d ~minutes:18.
                                       ~seconds:42. () in
      Eq( (18., 42.),
          (N.getUTCMinutes d,
           N.getUTCSeconds d))
    );
    "setUTCMinutesSMs", (fun _ ->
      let d = date () in
      let _ = N.setUTCMinutesSMs d ~minutes:18.
                                         ~seconds:42.
                                        ~milliseconds:311. () in
      Eq( (18., 42., 311.),
          (N.getUTCMinutes d,
           N.getUTCSeconds d,
           N.getUTCMilliseconds d))
    );
    "setUTCMonth", (fun _ ->
      let d = date () in
      let _ = N.setUTCMonth d 10. in

      Eq(10., N.getUTCMonth d)
    );
    "setUTCMonthD", (fun _ ->
      let d = date () in
      let _ = N.setUTCMonthD d ~month:10.
                                     ~date:14. () in
      Eq( (10., 14.),
          (N.getUTCMonth d,
           N.getUTCDate d))
    );
    "setUTCSeconds", (fun _ ->
      let d = date () in
      let _ = N.setUTCSeconds d 36. in

      Eq(36., N.getUTCSeconds d)
    );
    "setUTCSecondsMs", (fun _ ->
      let d = date () in
      let _ = N.setUTCSecondsMs d ~seconds:36.
                                        ~milliseconds:420. () in
      Eq( (36., 420.),
          (N.getUTCSeconds d,
           N.getUTCMilliseconds d))
    );

    "toDateString", (fun _ ->
      Eq("Mon Mar 08 1976", N.toDateString (date ())));
    "toGMTString", (fun _ ->
      Eq("Mon, 08 Mar 1976 11:11:56 GMT", N.toUTCString (date ())));
    "toISOString", (fun _ ->
      Eq("1976-03-08T11:11:56.789Z", N.toISOString (date ())));
    "toJSON", (fun _ ->
      Eq("1976-03-08T11:11:56.789Z", N.toJSON (date ())));
    "toJSONUnsafe", (fun _ ->
      Eq("1976-03-08T11:11:56.789Z", N.toJSONUnsafe (date ())));
    (* locale dependent
    "toLocaleDateString", (fun _ ->
      Eq("3/8/1976", N.toLocaleDateString (date ())));
    "toLocaleString", (fun _ ->
      Eq("3/8/1976, 12:11:56 PM", N.toLocaleString (date ())));
    "toLocaleTimeString", (fun _ ->
      Eq("12:11:56 PM", N.toLocaleTimeString (date ())));
    "toString", (fun _ ->
      Eq("Mon Mar 08 1976 12:11:56 GMT+0100 (CET)", N.toString (date ())));
    "toTimeString", (fun _ ->
      Eq("12:11:56 GMT+0100 (CET)", N.toTimeString (date ())));
    *)
    "toUTCString", (fun _ ->
      Eq("Mon, 08 Mar 1976 11:11:56 GMT", N.toUTCString (date ())));
    "eq" , (fun _ -> 
      let a = Js.Date.fromString "2013-03-01T01:10:00" in 
      let b = Js.Date.fromString "2013-03-01T01:10:00" in 
      let c = Js.Date.fromString "2013-03-01T01:10:01" in 
      Ok (a = b && b <> c &&  c > b)
    );
]

;; Mt.from_pair_suites __MODULE__ suites
