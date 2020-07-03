type t<'a, 'b>
type t<+'a, -'b>

type t<'superlongthing, 'superlongthing, 'superlongthing, 'superlongthing, 'superlongthing>

type t<+'superlongthing, -'superlongthing, +'superlongthing, -'superlongthing, +'superlongthing>

type t<+'superlongthing, -'superlongthing, +'superlongthing, -'superlongthing, +'superlongthing>
  constraint 't = ('state, 'action) => 'nextSubtree
