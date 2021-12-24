%raw
{|
let hljs = require('highlight.js/lib/highlight');
let javascriptHighlightJs = require('highlight.js/lib/languages/javascript');
let ocamlHighlightJs = require('highlight.js/lib/languages/ocaml');
let reasonHighlightJs = require('reason-highlightjs');
let bashHighlightJs = require('highlight.js/lib/languages/bash');
let jsonHighlightJs = require('highlight.js/lib/languages/json');
hljs.registerLanguage('reason', reasonHighlightJs);
hljs.registerLanguage('javascript', javascriptHighlightJs);
hljs.registerLanguage('ocaml', ocamlHighlightJs);
hljs.registerLanguage('sh', bashHighlightJs);
hljs.registerLanguage('json', jsonHighlightJs);
|};

module Link = Next.Link;

// Structure defined by `scripts/extract-tocs.js`
let tocData:
  Js.Dict.t({
    .
    "title": string,
    "headers":
      array({
        .
        "name": string,
        "href": string,
      }),
  }) = [%raw
  "require('../index_data/gentype_toc.json')"
];

module UrlPath = DocsLayout.UrlPath;
module NavItem = DocsLayout.NavItem;
module Category = DocsLayout.Category;
module Toc = DocsLayout.Toc;

let overviewNavs = [|
  NavItem.{name: "Introduction", href: "/docs/gentype/latest/introduction"},
  NavItem.{
    name: "Getting Started",
    href: "/docs/gentype/latest/getting-started",
  },
  NavItem.{name: "Usage", href: "/docs/gentype/latest/usage"},
|];

let advancedNavs = [|
  NavItem.{
    name: "Supported Types",
    href: "/docs/gentype/latest/supported-types",
  },
|];

let categories = [|
  Category.{name: "Overview", items: overviewNavs},
  {name: "Advanced", items: advancedNavs},
|];

[@react.component]
let make = (~components=Markdown.default, ~children) => {
  let router = Next.Router.useRouter();
  let route = router.route;

  let activeToc: option(Toc.t) =
    Belt.Option.(
      Js.Dict.get(tocData, route)
      ->map(data => {
          let title = data##title;
          let entries =
            Belt.Array.map(data##headers, header =>
              {Toc.header: header##name, href: "#" ++ header##href}
            );
          {Toc.title, entries};
        })
    );

  let urlPath = UrlPath.parse(~base="/docs/gentype", route);

  let breadcrumbs =
    Belt.Option.map(
      urlPath,
      v => {
        let {UrlPath.version} = v;
        let prefix =
          UrlPath.[
            {name: "Docs", href: "/docs"},
            {
              name: "GenType",
              href: "/docs/gentype/" ++ version ++ "/introduction",
            },
          ];
        UrlPath.toBreadCrumbs(~prefix, v);
      },
    );

  let title = "GenType";
  let version = "v3";

  <DocsLayout
    theme=`Js components categories version title ?activeToc ?breadcrumbs>
    children
  </DocsLayout>;
};
