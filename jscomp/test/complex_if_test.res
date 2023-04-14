let rec fib = x =>
  switch x {
  | 1 | 23 => 11111123
  | n => fib(n - 1) + fib(n - 2)
  }

open Bytes
external char_code: char => int = "%identity"
external char_chr: int => char = "%identity"

let escaped = s => {
  let n = ref(0)
  for i in 0 to length(s) - 1 {
    n :=
      n.contents +
      switch unsafe_get(s, i) {
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' => 2
      | ' ' .. '~' => 1
      | _ => 4
      }
  }
  if n.contents == length(s) {
    copy(s)
  } else {
    let s' = create(n.contents)
    n := 0
    for i in 0 to length(s) - 1 {
      switch unsafe_get(s, i) {
      | ('"' | '\\') as c =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, c)
      | '\n' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 'n')
      | '\t' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 't')
      | '\r' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 'r')
      | '\b' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 'b')
      | ' ' .. '~' as c => unsafe_set(s', n.contents, c)
      | c =>
        let a = char_code(c)
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, char_chr(48 + a / 100))
        incr(n)
        unsafe_set(s', n.contents, char_chr(48 + mod(a / 10, 10)))
        incr(n)
        unsafe_set(s', n.contents, char_chr(48 + mod(a, 10)))
      }
      incr(n)
    }
    s'
  }
}
let string_escaped = s => Bytes.to_string(escaped(Bytes.of_string(s)))

/* let s = (let v = ref "" in for i = 0 to 255 do v := !v ^ (String.make 1 (Char.chr i)) done ; !v);; */
let suites = {
  open Mt
  list{
    (
      "complete_escape",
      _ => Eq(
        string_escaped(
          "\000\001\002\003\004\005\006\007\b\t\n\011\012\r\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159\160\161\162\163\164\165\166\167\168\169\170\171\172\173\174\175\176\177\178\179\180\181\182\183\184\185\186\187\188\189\190\191\192\193\194\195\196\197\198\199\200\201\202\203\204\205\206\207\208\209\210\211\212\213\214\215\216\217\218\219\220\221\222\223\224\225\226\227\228\229\230\231\232\233\234\235\236\237\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\253\254\255",
        ),
        "\\000\\001\\002\\003\\004\\005\\006\\007\\b\\t\\n\\011\\012\\r\\014\\015\\016\\017\\018\\019\\020\\021\\022\\023\\024\\025\\026\\027\\028\\029\\030\\031 !\\\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\\127\\128\\129\\130\\131\\132\\133\\134\\135\\136\\137\\138\\139\\140\\141\\142\\143\\144\\145\\146\\147\\148\\149\\150\\151\\152\\153\\154\\155\\156\\157\\158\\159\\160\\161\\162\\163\\164\\165\\166\\167\\168\\169\\170\\171\\172\\173\\174\\175\\176\\177\\178\\179\\180\\181\\182\\183\\184\\185\\186\\187\\188\\189\\190\\191\\192\\193\\194\\195\\196\\197\\198\\199\\200\\201\\202\\203\\204\\205\\206\\207\\208\\209\\210\\211\\212\\213\\214\\215\\216\\217\\218\\219\\220\\221\\222\\223\\224\\225\\226\\227\\228\\229\\230\\231\\232\\233\\234\\235\\236\\237\\238\\239\\240\\241\\242\\243\\244\\245\\246\\247\\248\\249\\250\\251\\252\\253\\254\\255",
      ),
    ),
  }
}
Mt.from_pair_suites(__MODULE__, suites)
