type t

type localeType = [#cardinal | #ordinal]

type options = {
  localeMatcher?: Intl_Common.localeMatcher,
  \"type"?: localeType,
  // use either this group
  minimumIntegerDigits?: Intl_Common.oneTo21,
  minimumFractionDigits?: Intl_Common.zeroTo20,
  maximumFractionDigits?: Intl_Common.zeroTo20,
  // OR this group
  minimumSignificantDigits?: Intl_Common.oneTo21,
  maximumSignificantDigits?: Intl_Common.oneTo21,
}

type pluralCategories = [
  | #zero
  | #one
  | #two
  | #few
  | #many
  | #other
]

type resolvedOptions = {
  locale: string,
  pluralCategories: array<pluralCategories>,
  \"type": localeType,
  // either this group
  minimumIntegerDigits?: Intl_Common.oneTo21,
  minimumFractionDigits?: Intl_Common.zeroTo20,
  maximumFractionDigits?: Intl_Common.zeroTo20,
  // OR this group
  minimumSignificantDigits?: Intl_Common.oneTo21,
  maximumSignificantDigits?: Intl_Common.oneTo21,
}

type supportedLocalesOptions = {localeMatcher: Intl_Common.localeMatcher}

@new external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.PluralRules"

@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => t =
  "Intl.PluralRules.supportedLocalesOf"

@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

type rule = [#zero | #one | #two | #few | #many | #other]

@send external select: (t, float) => rule = "select"
@send external selectInt: (t, int) => rule = "select"
@send external selectBigInt: (t, bigint) => rule = "select"

@send
external selectRange: (t, ~start: float, ~end: float) => rule = "selectRange"

@send
external selectRangeInt: (t, ~start: int, ~end: int) => rule = "selectRange"

@send
external selectRangeBigInt: (t, ~start: bigint, ~end: bigint) => rule = "selectRange"
