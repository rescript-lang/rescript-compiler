[@bs.deriving jsConverter]
type language = [
  | [@bs.as "de"] `German
  | [@bs.as "es-eu"] `SpanishEurope
  | [@bs.as "es-us"] `SpanishAmerica
  | [@bs.as "fr-eu"] `FrenchEurope
  | [@bs.as "fr-us"] `FrenchAmerica
  | [@bs.as "it"] `Italian
  | [@bs.as "ja"] `Japanese
  | [@bs.as "ko"] `Korean
  | [@bs.as "nl"] `Dutch
  | [@bs.as "ru"] `Russian
  | [@bs.as "zh-cn"] `ChineseSimplified
  | [@bs.as "zh-tw"] `ChineseTraditional
  | [@bs.as "en"] `English
  | [@bs.as "en-gb"] `EnglishEurope
];

let languages: array(language) = [|
  `English,
  `EnglishEurope,
  `SpanishAmerica,
  `SpanishEurope,
  `FrenchAmerica,
  `FrenchEurope,
  `German,
  `Italian,
  `Dutch,
  `Russian,
  `Japanese,
  `Korean,
  `ChineseTraditional,
  `ChineseSimplified,
|];

let languageToString = (language: language) => {
  switch (language) {
  | `German => {j|Deutsch|j}
  | `SpanishEurope => {j|Español (Europe)|j}
  | `SpanishAmerica => {j|Español (America)|j}
  | `FrenchEurope => {j|Français (Europe)|j}
  | `FrenchAmerica => {j|Français (America)|j}
  | `Italian => {j|Italiano|j}
  | `Japanese => {j|日本語|j}
  | `Korean => {j|한국어|j}
  | `Dutch => {j|Nederlands|j}
  | `Russian => {j|Русский|j}
  | `ChineseSimplified => {j|中文|j}
  | `ChineseTraditional => {j|繁體中文|j}
  | `English => {j|English|j}
  | `EnglishEurope => {j|English (UK)|j}
  };
};

[@bs.val] [@bs.scope "navigator"]
external browserLanguage: string = "language";

let browserLanguage =
  if (browserLanguage == "de") {
    `German;
  } else if (Js.String.toLowerCase(browserLanguage) == "es-es") {
    `SpanishEurope;
  } else if (browserLanguage |> Js.String.startsWith("es")) {
    `SpanishAmerica;
  } else if (Js.String.toLowerCase(browserLanguage) == "fr-ca") {
    `FrenchAmerica;
  } else if (browserLanguage |> Js.String.startsWith("fr")) {
    `FrenchEurope;
  } else if (browserLanguage |> Js.String.startsWith("it")) {
    `Italian;
  } else if (browserLanguage == "ja") {
    `Japanese;
  } else if (browserLanguage == "ko") {
    `Korean;
  } else if (browserLanguage == "nl") {
    `Dutch;
  } else if (browserLanguage |> Js.String.startsWith("ru")) {
    `Russian;
  } else if (Js.String.toLowerCase(browserLanguage) == "zh-cn") {
    `ChineseSimplified;
  } else if (browserLanguage |> Js.String.startsWith("zh")) {
    `ChineseTraditional;
  } else if (Js.String.toLowerCase(browserLanguage) == "en-gb") {
    `EnglishEurope;
  } else {
    `English;
  };

type state = {language};
type action =
  | SetLanguage(language);

let localStorageKey = "language";
let localStorageLanguage =
  Dom.Storage.localStorage |> Dom.Storage.getItem(localStorageKey);

let api =
  Restorative.createStore(
    {
      language:
        localStorageLanguage
        ->Belt.Option.flatMap(languageFromJs)
        ->Belt.Option.getWithDefault(browserLanguage),
    },
    (state, action) => {
    switch (action) {
    | SetLanguage(language) => {language: language}
    }
  });

let useLanguage = () => {
  api.useStoreWithSelector(state => state.language, ());
};
let getLanguage = () => api.getState().language;

let setLanguage = (~language) => {
  api.dispatch(SetLanguage(language));
  if (language == browserLanguage) {
    Dom.Storage.localStorage |> Dom.Storage.removeItem(localStorageKey);
  } else {
    Dom.Storage.localStorage
    |> Dom.Storage.setItem(localStorageKey, languageToJs(language));
  };
  Analytics.Amplitude.logEventWithProperties(
    ~eventName="Language Changed",
    ~eventProperties={"language": languageToJs(language)},
  );
};