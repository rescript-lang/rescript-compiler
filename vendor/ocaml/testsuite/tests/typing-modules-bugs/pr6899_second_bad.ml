include struct
  let foo `Test = ()
  let wrap f `Test = f
  let bar = wrap ()
end
