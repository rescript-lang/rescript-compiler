type t = {
  requireEmitterEarly : string list;
  exportEmitterEarly : string list;
  requireEmitter : string list;
  importEmitter : string list;
  exportEmitter : string list;
}

let initial =
  {
    requireEmitterEarly = [];
    exportEmitterEarly = [];
    requireEmitter = [];
    importEmitter = [];
    exportEmitter = [];
  }

let string ~emitter s = s :: emitter

let requireEarly ~emitters s =
  {
    emitters with
    requireEmitterEarly = s |> string ~emitter:emitters.requireEmitterEarly;
  }

let exportEarly ~emitters s =
  {
    emitters with
    exportEmitterEarly = s |> string ~emitter:emitters.exportEmitterEarly;
  }

let require ~emitters s =
  {
    emitters with
    requireEmitter = s |> string ~emitter:emitters.requireEmitter;
  }

let import ~emitters s =
  { emitters with importEmitter = s |> string ~emitter:emitters.importEmitter }

let export ~emitters s =
  { emitters with exportEmitter = s |> string ~emitter:emitters.exportEmitter }

let toString ~separator emitters =
  [
    emitters.requireEmitterEarly |> List.rev;
    emitters.exportEmitterEarly |> List.rev;
    emitters.requireEmitter |> List.rev;
    emitters.importEmitter |> List.rev;
    emitters.exportEmitter |> List.rev;
  ]
  |> List.concat |> String.concat separator
