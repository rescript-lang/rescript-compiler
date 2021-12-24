let confirm = (~onCancel=?, message, f) =>
  if (Webapi.Dom.(window |> Window.confirm(message))) {
    f();
  } else {
    onCancel |> OptionUtils.mapWithDefault(onCancel => onCancel(), ());
  };
