type t

type options = {
  baseName?: string,
  calendar?: Core__Intl__Common.calendar,
  collation?: Core__Intl__Common.collation,
  hourCycle?: [#h11 | #h12 | #h23 | #h24],
  caseFirst?: [#upper | #lower | #"false"],
  numberingSystem?: Core__Intl__Common.numberingSystem,
  numeric?: bool,
  language?: string,
  script?: string,
  region?: string,
}

@new external make: (string, ~options: options=?) => t = "Intl.Locale"

@get external baseName: t => string = "baseName"
@get external calendar: t => option<string> = "calendar"
@get external caseFirst: t => option<string> = "caseFirst"
@get external collation: t => option<string> = "collation"
@get external hourCycle: t => option<string> = "hourCycle"
@get external language: t => string = "language"
@get external numberingSystem: t => option<string> = "numberingSystem"
@get external numeric: t => bool = "numeric"
@get external region: t => option<string> = "region"
@get external script: t => option<string> = "script"

@send external maximize: t => t = "maximize"
@send external minimize: t => t = "minimize"
