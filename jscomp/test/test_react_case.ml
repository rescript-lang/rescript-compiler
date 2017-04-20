(**
var CommentBox = React.createClass({
  render: function() {
    return (
      <div className="commentBox">
        Hello, world! I am a CommentBox.
      </div>
    );
    }
});
*)
type obj_spec 
type react_dom_element
external react_create_class : 
    obj_spec -> react_dom_element = "react_create_class" [@@bs.val (* "createClass" "React" *)]
type react_dom_component
external mk_obj_spec :
        ?display_name:string ->
          render:(unit -> react_dom_component) -> unit -> 
            obj_spec = "" [@@bs.obj ]


external empty_obj : unit -> _ = ""[@@bs.obj]

let v = empty_obj
