type photo = {
  src: string,
  width: int,
  height: int,
};
type selectedPhoto = {
  photo,
  index: int,
  // next: photo,
  // previous: photo,
};

[@bs.module "react-photo-gallery"] [@react.component]
// onClick:
external make:
  (
    ~photos: array(photo),
    ~targetRowHeight: int,
    ~onClick: (ReactEvent.Mouse.t, selectedPhoto) => unit
  ) =>
  React.element =
  "default";
