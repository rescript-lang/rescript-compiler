module React = struct
type element = unit
let null = ()
end

module Foo :
  sig
    val make : ?htmlAttributes:float array -> unit -> React.element[@@react.component
                                                                    ]
  end =
  struct let make ?htmlAttributes:_  () = React.null[@@react.component ] end 