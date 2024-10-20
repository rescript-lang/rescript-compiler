module Common = Intl__Common
module Collator = Intl__Collator
module DateTimeFormat = Intl__DateTimeFormat
module ListFormat = Intl__ListFormat
module Locale = Intl__Locale
module NumberFormat = Intl__NumberFormat
module PluralRules = Intl__PluralRules
module RelativeTimeFormat = Intl__RelativeTimeFormat
module Segmenter = Intl__Segmenter
module Segments = Intl__Segments

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
