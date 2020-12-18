let x = true
let y = false

let txt = "a string"

let txtWithEscapedChar = "foo\nbar"

let number = 1

let template = `amazing
  multine
  template
     string
`


let complexNumber = 1.6
let x = 0b0000_0001

let int32 = 42l
let int64 = 42L

let x = -44.20e99
let x = -44.20_34e99
let x = -44.20e+9
let x = -44.20e-9
let x = -44.20e-99_99
let x = -444_444.20e99
let x = 44.20e99
let x = 44.20_34e99
let x = 44.20e+9
let x = 44.20e-9
let x = 44.20e-99_99
let x = 44e99
let x = 44_44e99

let x = 0x0
let x = 0X0

let x = 0xAA
let x = 0XA_A
let x = 0xAA.ff
let x = 0xAA.ff_ff
let x = 0xAA.ff_ffp10
let x = 0xAA.ff_ffp+10
let x = 0xAA.ff_ffp-10
let x = 0xAA.ff_ffp100_00
let x = 0xAA.ff_ffp+100_00
let x = 0xAA.ff_ffp-100_00

let x = 'a' // char
let x = '\\'
let x = '\''
let x = '\n'
let x = '\t'
let x = '\b'
let x = '\r'
let x = ' '
let x = '\o021'
let x = '\xAA'
let x = '\179'

let () = {
  getResult()
  -10
}

let x = "foo\010bar"
let x = "foo\x0Abar"
let x = "foo\o012bar"
let x = "\\abc"
let x = "\'bar"
let x = "\nbar"
let x = "\tbar"
let x = "\bbar"
let x = "\rbar"
let x = "\ bar"
