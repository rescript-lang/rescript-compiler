@genType type promise<'a> = Js.Promise.t<'a>

@genType
type fromPayload = {
  x: int,
  s: string,
}

@genType type toPayload = {result: string}

@genType let convert = Js.Promise.then_(({s}) => Js.Promise.resolve({result: s}))

@genType let barx = (~x=Js.Promise.resolve(Some("a")), ()) => x == x

@genType
@tag("status")
type settledResult<+'a> =
  | @as("fulfilled") Fulfilled({value: 'a}) | @as("rejected") Rejected({reason: unknown})

@genType
type settled = settledResult<string>
