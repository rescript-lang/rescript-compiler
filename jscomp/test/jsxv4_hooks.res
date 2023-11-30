@@bs.config({
   flags: ["-bs-jsx", "4"],
 })

let useEffectTest0 = () => {    
  React.useEffect(() => None, [])
}

let useEffectTest1 = (a: string) => {    
  React.useEffect(() => None, [a])
}

let useEffectTest2 = (a: string, b: int) => {    
  React.useEffect(() => None, [a, b])
}

let useEffectTest2 = (a: string, b: int) => {    
  React.useEffect(() => None, [a, b])
}

let useEffectTest3 = (a: string, b: int, c: float) => {    
  React.useEffect(() => None, [a, b, c])
}

let useLayoutEffectTest = (a: string, b: int, c: float) => {    
  React.useLayoutEffect(() => None, [a, b, c])
}

let useCallbackTest = (a: string, b: int, c: float) => {    
  React.useCallback(() => None, [a, b, c])
}

let useMemoTest = (a: string, b: int, c: float) => {    
  React.useMemo(() => None, [a, b, c])
}
