type t = Dom.mutationObserver

@new external make: ((array<Dom.mutationRecord>, t) => unit) => t = "MutationObserver"

@bs.send.pipe(: t) external observe: (Dom.node_like<'a>, {..}) => unit = ""
@bs.send.pipe(: t) external disconnect: unit = ""
@bs.send.pipe(: t) external takeRecords: array<Dom.mutationRecord> = ""
