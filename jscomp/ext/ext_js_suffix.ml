type t =
  | Js
  | Bs_js
  | Mjs
  | Bs_mjs
  | Cjs
  | Bs_cjs
  | Unknown_extension

let to_string (x : t) =
  match x with
  | Js -> Literals.suffix_js
  | Bs_js -> Literals.suffix_bs_js
  | Mjs -> Literals.suffix_mjs
  | Bs_mjs -> Literals.suffix_bs_mjs
  | Cjs -> Literals.suffix_cjs
  | Bs_cjs -> Literals.suffix_bs_cjs
  | Unknown_extension -> assert false

let of_string (x : string) : t =
  match () with
  | () when x = Literals.suffix_js -> Js
  | () when x = Literals.suffix_bs_js -> Bs_js
  | () when x = Literals.suffix_mjs -> Mjs
  | () when x = Literals.suffix_bs_mjs -> Bs_mjs
  | () when x = Literals.suffix_cjs -> Cjs
  | () when x = Literals.suffix_bs_cjs -> Bs_cjs
  | _ -> Unknown_extension
