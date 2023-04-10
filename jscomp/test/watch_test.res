let test = path => {
  open Node.Fs.Watch
  watch(path, ~config=config(~recursive=true, ()), ())
  ->on_(#change((. event, string_buffer) => Js.log((event, string_buffer))))
  ->close
}
