

let _ = 
  begin 
    assert (Js.Undefined.to_opt (Js.Undefined.return "" ) = Some "");
    assert (Js.Null.to_opt (Js.Null.return "") = Some "") 
  end
