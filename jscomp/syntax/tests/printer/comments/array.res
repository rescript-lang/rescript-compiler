a[ /* zz */ 0 ] =  7 

let _ = (
  /* zz */ a  
  )[0]

let _ = (
  a // zz
  )[0]

 (
    incidents
    ->Belt.Array.keep(({status}) => status === #OPEN)
    // This comment will vanish 
    ->Belt.SortArray.stableSortBy((a, b) =>
      compare(a.createdTime, b.createdTime)
    )
  )[0]