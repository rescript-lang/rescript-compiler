
[@@@config {
  flags = [|
  (* "-drawlambda"; *)
  (* "-dtypedtree"; *)
  "-bs-diagnose";
  (* "-dparsetree"; *)
  (* "-dsource"; *)
  |]
}]


type t =
  | Arg
  
  | Acos
  | Acosh
  | Add
  | ArcMinuteUnit
  | ArcSecondUnit
  | Asin
  | Asinh
  | Atan
  | Atanh
  | Bin
  | Conj
  | DecimalSeparator
  | DegreeFunction
  | DegreeUnit
  | Div
  | Dot
  | Factorial
  | Gamma
  | GradianFunction
  | GradianUnit
  | Hex
  | Im
  | Log
  | Mul
  | Oct
  | OpenBracket
  | Percent
  | Re
  | Sub
  | UnitConversion of int * int 
  
  | CloseBracketS
  | ConstES
  | ConstPiS
  | CosecS
  | CoshS
  | CosS
  | CotS
  | ImaginaryUnitS
  | LabelS of {mml: string}
  | N0_S
  | N1_S
  | N2_S
  | N3_S
  | N4_S
  | N5_S
  | N6_S
  | N7_S
  | N8_S
  | N9_S
  | NA_S
  | NB_S
  | NC_S
  | ND_S
  | NE_S
  | NF_S
  | RandS
  | SecS
  | SinhS
  | SinS
  | TanhS
  | TanS
  | CustomAtomS of {
      mml: string;
      value: string
    }
  | VariableS of string
  
  | Magnitude1
  | NLog1
  | Superscript1
  
  | Abs1S
  | Ceil1S
  | Floor1S
  | Round1S
  | Sqrt1S
  
  | Differential2
  | NCR2
  | NPR2
  | Product2
  | Sum2
  
  | Frac2S
  | Gcd2S
  | Lcm2S
  | Max2S
  | Min2S
  | NRoot2S
  | RandInt2S
  | Vector2S
  
  | Integral3
  
  | Vector3S
  
  | Matrix4S
  | Matrix9S
let elementIsStationary (element: t) =
  match (element) with
  | Acos
  | Acosh
  | Add
  | Asin
  | Asinh
  | Atan
  | Atanh
  | DegreeFunction
  | Div
  | Dot
  | Gamma
  | GradianFunction
  | Im
  | Log
  | Mul
  | Re
  | Sub
  | CosecS
  | CoshS
  | CosS
  | CotS
  | SecS
  | SinhS
  | SinS
  | TanhS
  | TanS
  | NLog1
  | Differential2
  | Product2
  | Sum2
  | Vector2S
  | Integral3
  | Vector3S
  | Matrix4S
  | Matrix9S -> true
  | Arg
  | ArcMinuteUnit
  | ArcSecondUnit
  | Bin
  | Conj
  | DecimalSeparator
  | DegreeUnit
  | Factorial
  | GradianUnit
  | Hex
  | Oct
  | OpenBracket
  | Percent
  | UnitConversion(_)
  | CloseBracketS
  | ConstES
  | ConstPiS
  | CustomAtomS(_)
  | ImaginaryUnitS
  | LabelS(_)
  | N0_S
  | N1_S
  | N2_S
  | N3_S
  | N4_S
  | N5_S
  | N6_S
  | N7_S
  | N8_S
  | N9_S
  | NA_S
  | NB_S
  | NC_S
  | ND_S
  | NE_S
  | NF_S
  | RandS
  | VariableS(_)
  | Magnitude1
  | Superscript1
  | Abs1S
  | Ceil1S
  | Floor1S
  | Round1S
  | Sqrt1S
  | NCR2
  | NPR2
  | Frac2S
  | Gcd2S
  | Lcm2S
  | Max2S
  | Min2S
  | NRoot2S
  | RandInt2S -> false
  
;; Js.log (elementIsStationary Differential2)  