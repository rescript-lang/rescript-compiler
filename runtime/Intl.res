module Common = Intl_Common
module Collator = Intl_Collator
module DateTimeFormat = Intl_DateTimeFormat
module ListFormat = Intl_ListFormat
module Locale = Intl_Locale
module NumberFormat = Intl_NumberFormat
module PluralRules = Intl_PluralRules
module RelativeTimeFormat = Intl_RelativeTimeFormat
module Segmenter = Intl_Segmenter
module Segments = Intl_Segments

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
