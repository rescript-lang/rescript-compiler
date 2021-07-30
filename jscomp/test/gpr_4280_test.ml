[@@@bs.config {flags = [|(* "-bs-diagnose" *)|]}]
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 

let u = ref 0
let div ~children () = 
  for i = 0 to 1 do   
    u := 300;  
    Js.log "nonline"
  done

let string (s : string) =   
  for i = 0 to 1 do 
    u := 200;
    Js.log "no"
  done     

let fn authState route =
  match (authState, route) with
  | (`Unauthenticated,`Onboarding onboardingRoute)
  | (`Unverified _,`Onboarding onboardingRoute) ->
    Js.Console.log onboardingRoute;
    div
      ~children:[string
                   "Onboarding"] ();
      0
  | (`Unauthenticated,`SignIn)|(`Unauthenticated,`SignUp)
  |(`Unauthenticated,`Invite)|(`Unauthenticated,`PasswordReset) ->
    div
      ~children:[string
                   "LoggedOut"] ();1

  | (`Unverified user,_) ->
    Js.Console.log user;
    div
      ~children:[string
                   "VerifyEmail"] ();
      2
  | (`Unauthenticated,_) ->
    div
      ~children:[string
                   "Redirect"] ();3


;; eq __LOC__ (fn (`Unauthenticated) `Invite) 1
;; eq __LOC__ (fn (`Unauthenticated) (`Onboarding 0)) 0
;; eq __LOC__ (fn (`Unverified 0) `Invite) 2 
;; eq __LOC__ (fn `Unauthenticated `xx) 3 
;; Mt.from_pair_suites __FILE__ !suites
