let 1 = 1
let "string" = 1
let 3.14 = 1

let print = (ppf, i) =>
  switch i.stamp {
  | 0 => fprintf(ppf, "%s!", i.name)
  | -1 => fprintf(ppf, "%s#", i.name)
  | +1 => fprintf(ppf, "%s#", i.name)
  | -1. => fprintf(ppf, "%s#", i.name)
  | +1. => fprintf(ppf, "%s#", i.name)
  }

let [-1, +1, 1] = x

switch science {
| (1.12, -3.13) => true
| [1.12, -3.13] => true
| list{1.12, -3.13} => true
| {x: 1.12, y: -3.13} => true
| Constructor(1.12, -2.45) => true
| #Constuctor(1.12, -2.45) => true
| -4.15 as x => true
| -4.15 | +4.15 => true
| (-3.14 : float) => true
| lazy 5.678 => true
| exception 19.34 => true
| _ => false
}

<div>
  {switch state.uploadProgress {
    | None => React.null
    | Some(1.0) => React.null
    | Some(uploadProgress) =>
      <div className=Styles.fill>
        <StudioUploadProgressIndicator progress=uploadProgress />
      </div>
    }}
</div>
