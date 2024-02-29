let x = <div @ @@@ />

let x = <Component prop1= prop2="value2" prop3=<C /> {...props} props=module(Foo) />

let x = <Component ?prop0 prop1=[1,2,3] prop2= prop3={value3} prop4=?value4 />

let x = <Component prop1=value1 prop2="value2" prop3=<C /> {...props} props4= />

let x = <Component className=Styles.something prop2={v => {f(v + 1)}} prop1= prop3=1 />

let x = 
  <a prop= href={j`https://$txExplererUrl/tx/$txHash`} target="_blank" rel="noopener noreferrer">
    {("View the transaction on " ++ txExplererUrl)->restr}
  </a>
