
let u = ref 0
let div ~children () = 
  for i = 0 to 1 do   
    u := 300;  
    Js.log "nonline"
  done
module React = struct 
  let string (s : string) =   
    for i = 0 to 1 do 
      u := 200;
      Js.log "no"
    done     
end     
let fn authState route =
  match (authState, route) with
  | (`Unauthenticated,`Onboarding onboardingRoute)
  | (`Unverified _,`Onboarding onboardingRoute) ->
    Js.Console.log onboardingRoute;
    div
      ~children:[React.string
                   "Onboarding"] ();
      0
  | (`Unauthenticated,`SignIn)|(`Unauthenticated,`SignUp)
  |(`Unauthenticated,`Invite)|(`Unauthenticated,`PasswordReset) ->
    div
      ~children:[React.string
                   "LoggedOut"] ();1

  | (`Unverified user,_) ->
    Js.Console.log user;
    div
      ~children:[React.string
                   "VerifyEmail"] ();
      2
  | (`Unauthenticated,_) ->
    div
      ~children:[React.string
                   "Redirect"] ();3
;; assert (fn (`Unauthenticated) `Invite  = 1 ) (* == 1*)