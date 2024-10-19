module Common = Core__Intl__Common
module Collator = Core__Intl__Collator
module DateTimeFormat = Core__Intl__DateTimeFormat
module ListFormat = Core__Intl__ListFormat
module Locale = Core__Intl__Locale
module NumberFormat = Core__Intl__NumberFormat
module PluralRules = Core__Intl__PluralRules
module RelativeTimeFormat = Core__Intl__RelativeTimeFormat
module Segmenter = Core__Intl__Segmenter
module Segments = Core__Intl__Segments

/**
@throws RangeError
*/
external getCanonicalLocalesExn: string => array<string> = "Intl.getCanonicalLocales"

/**
@throws RangeError
*/
external getCanonicalLocalesManyExn: array<string> => array<string> = "Intl.getCanonicalLocales"

/**
@throws RangeError
*/
external supportedValuesOfExn: string => array<string> = "Intl.supportedValuesOf"
