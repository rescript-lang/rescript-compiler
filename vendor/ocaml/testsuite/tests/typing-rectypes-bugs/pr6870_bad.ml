module type T = sig type 'a t end
module Fix (T : T) = struct type r = ('r T.t as 'r) end
