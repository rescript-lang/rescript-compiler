(** To do the right [tail call] conversion when the arguments are functions
    which captures some variables, we need create a scope to capture it the
    naive version:
    {[
   var f = 
   function(n,acc)
   {f_tailcall_0001:
     while(/* true */1)
     {if(n===0)
       {return acc(/* () */0);}
      else
       {n=n-1;
        acc=
        function()
         {console.log(Pervasives.string_of_int(n));
          return acc(/* () */0);};
        continue f_tailcall_0001;}};};
   
    ]}

    A correct but less optimal version:
    {[
   var f = 
   function(n,acc)
   {f_tailcall_0001:
     while(/* true */1)
     { (funtcion(n,acc){if(n===0)
       {return acc(/* () */0);}
      else
       {n=n-1;
        acc=
        function()
         {console.log(Pervasives.string_of_int(n));
          return acc(/* () */0);};
        continue f_tailcall_0001;}}(n,acc)) };};   
    ]}

    It does not work, since `continue` can not cross functions

    the right version:
    {[
   var f = 
    function(n,acc)
      {f_tailcall_0001:
     while(/* true */1)
     {if(n===0)
       {return acc(/* () */0);}
      else
       {
        acc = (function(){
            var acc1 = acc ;
            var n1 = n;
            return function() {
                console.log(Pervasives.string_of_int(n1));
                return acc1(/* () */0);}
        }());
           n=n-1;
        continue f_tailcall_0001;}};};

    ]}

    with [let] *)

let rec f n acc =
  if n = 0 then acc ()
  else
    f (n - 1) (fun _ ->
        print_endline (string_of_int n) ;
        acc ())

(** Here a naive version would be
    {[
    for(var i = 0;i<=n;++i)
     {arr[i]=function(){return i;};}
    ]}

    The correct version should be
    {[
    for(var i = 0;i<=n;++i)
     {arr[i]= (function(){ var i$1 = i; function(){return i$1;};}())
    ]} *)
let test_closure () =
  let n = 6 in
  let arr = Array.make n (fun x -> x) in
  for i = 0 to n do
    arr.(i) <- (fun _ -> i)
  done ;
  arr

;;
f 10 (fun _ -> ())
