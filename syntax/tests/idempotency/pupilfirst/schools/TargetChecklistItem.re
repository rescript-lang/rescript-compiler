type choices = array(string);

type kind =
  | Files
  | Link
  | ShortText
  | LongText
  | MultiChoice(choices);

type t = {
  title: string,
  kind,
  optional: bool,
};

let title = t => t.title;
let kind = t => t.kind;
let optional = t => t.optional;

let actionStringForKind = kind => {
  switch (kind) {
  | Files => "Upload Files"
  | Link => "Attach a Link"
  | ShortText => "Write Short Text"
  | LongText => "Write Long Text"
  | MultiChoice(_choices) => "Choose from a list"
  };
};

let kindAsString = kind => {
  switch (kind) {
  | Files => "files"
  | Link => "link"
  | ShortText => "shortText"
  | LongText => "longText"
  | MultiChoice(_choices) => "multiChoice"
  };
};

let make = (~title, ~kind, ~optional) => {
  {title, kind, optional};
};

let updateTitle = (title, t) => {
  {...t, title};
};

let updateKind = (kind, t) => {
  {...t, kind};
};

let updateOptional = (optional, t) => {
  {...t, optional};
};

let removeItem = (index, list) => {
  list |> Js.Array.filteri((_item, i) => i != index);
};

let moveUp = (index, list) => {
  list |> ArrayUtils.swapUp(index);
};

let moveDown = (index, list) => {
  list |> ArrayUtils.swapDown(index);
};

let copy = (i, list) => {
  list
  |> Array.mapi((index, item) => i == index ? [item, item] : [item])
  |> ArrayUtils.flatten;
};

let removeMultichoiceOption = (choiceIndex, t) => {
  switch (t.kind) {
  | MultiChoice(choices) =>
    let updatedChoices =
      choices
      |> Array.mapi((i, choice) => i == choiceIndex ? [] : [choice])
      |> ArrayUtils.flatten;
    t |> updateKind(MultiChoice(updatedChoices));
  | Files
  | Link
  | ShortText
  | LongText => t
  };
};

let addMultichoiceOption = t => {
  switch (t.kind) {
  | MultiChoice(choices) =>
    let updatedChoices = [|""|] |> Array.append(choices);
    t |> updateKind(MultiChoice(updatedChoices));
  | Files
  | Link
  | ShortText
  | LongText => t
  };
};

let updateMultichoiceOption = (choiceIndex, newOption, t) => {
  switch (t.kind) {
  | MultiChoice(choices) =>
    let updatedChoices =
      choices
      |> Array.mapi((i, choice) => i == choiceIndex ? newOption : choice);
    t |> updateKind(MultiChoice(updatedChoices));
  | Files
  | Link
  | ShortText
  | LongText => t
  };
};

let longText = {title: "", kind: LongText, optional: false};

let isFilesKind = t => {
  switch (t.kind) {
  | Files => true
  | MultiChoice(_choices) => false
  | Link
  | ShortText
  | LongText => false
  };
};

let isValidChecklistItem = t => {
  switch (t.kind) {
  | MultiChoice(choices) =>
    choices
    |> Js.Array.filter(choice => choice |> String.trim == "")
    |> ArrayUtils.isEmpty
    && t.title
    |> String.trim
    |> String.length >= 1
  | Files
  | Link
  | ShortText
  | LongText => t.title |> String.trim |> String.length >= 1
  };
};

let decodeMetadata = (kind, json) => {
  switch (kind) {
  | `MultiChoice => json |> Json.Decode.(field("choices", array(string)))
  };
};

let decode = json => {
  Json.Decode.{
    kind:
      switch (json |> field("kind", string)) {
      | "files" => Files
      | "link" => Link
      | "shortText" => ShortText
      | "longText" => LongText
      | "multiChoice" =>
        MultiChoice(json |> field("metadata", decodeMetadata(`MultiChoice)))
      | otherKind =>
        Rollbar.error(
          "Unkown kind: "
          ++ otherKind
          ++ "received in CurriculumEditor__TargetChecklistItem",
        );
        LongText;
      },
    optional: json |> field("optional", bool),
    title: json |> field("title", string),
  };
};

let encodeMetadata = kind => {
  switch (kind) {
  | MultiChoice(choices) =>
    Json.Encode.(object_([("choices", choices |> stringArray)]))
  | Files
  | Link
  | ShortText
  | LongText => Json.Encode.(object_([]))
  };
};

let encode = t =>
  Json.Encode.(
    object_([
      ("kind", t.kind |> kindAsString |> string),
      ("title", t.title |> string),
      ("optional", t.optional |> bool),
      ("metadata", t.kind |> encodeMetadata),
    ])
  );

let encodeChecklist = checklist => checklist |> Json.Encode.(array(encode));
