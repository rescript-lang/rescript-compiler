let customLayoutThreshold = 2

type t = {
  customLayout: int;
  mutable uncurried_config: Res_uncurried.config;
  mutable customInfix: Res_custom_infix.t;
}

let copy t =
  {
    customLayout = t.customLayout;
    uncurried_config = t.uncurried_config;
    customInfix = t.customInfix;
  }

let init =
  {customLayout = 0; uncurried_config = Res_uncurried.init; customInfix = []}

let nextCustomLayout t = {t with customLayout = t.customLayout + 1}

let shouldBreakCallback t = t.customLayout > customLayoutThreshold
