

let _ = 
  begin 
    assert (Js.Null_undefined.to_opt (Js.Null_undefined.return "" ) = Some "");
    assert (Js.Undefined.to_opt (Js.Undefined.return "" ) = Some "");
    assert (Js.Null.toOption (Js.Null.return "") = Some "") 
  end
