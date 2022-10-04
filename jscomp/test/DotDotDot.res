type a = {x: int}

type b = {dotdotdot: a, y: string}

type c = {dotdotdot: b, z: string}

let v: c = {x: 10, y: "", z: ""}

type vw = {v: float, w: float}

type cvw = {dotdotdot: c, dotdotdot: vw}

let v2: cvw = {x: 10, y: "", z: "", v: 1.0, w: 2.0}

type globalProps = {
  id?: string,
  name?: string,
  title?: string,
  className?: string,
}

type anchorProps = {
  dotdotdot: globalProps,
  download?: string,
  href?: string,
  target?: [#_self | #_blank | #_parent | #_top],
}

// globalProps only case?
type divProps = {dotdotdot: globalProps}

type svgProps = {
  dotdotdot: globalProps,
  x?: string,
  y?: string,
}
