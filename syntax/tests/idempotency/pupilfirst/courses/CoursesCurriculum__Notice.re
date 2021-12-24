type t =
  | Preview
  | CourseEnded
  | CourseComplete
  | AccessEnded
  | LevelUp
  | LevelUpBlocked(int)
  | Nothing;

[@bs.module "../images/course-ended.svg"]
external courseEndedImage: string = "default";
[@bs.module "../images/course-complete.svg"]
external courseCompleteImage: string = "default";
[@bs.module "../images/access-ended.svg"]
external accessEndedImage: string = "default";
[@bs.module "../images/level-up.svg"]
external levelUpImage: string = "default";
[@bs.module "../images/preview-mode.svg"]
external previewModeImage: string = "default";
[@bs.module "../images/level-up-blocked.svg"]
external levelUpBlockedImage: string = "default";

let icon = t =>
  switch (t) {
  | Preview => previewModeImage
  | CourseEnded => courseEndedImage
  | CourseComplete => courseCompleteImage
  | AccessEnded => accessEndedImage
  | LevelUp => levelUpImage
  | LevelUpBlocked(_) => levelUpBlockedImage
  | Nothing => ""
  };
