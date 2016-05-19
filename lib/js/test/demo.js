// GENERATED CODE BY BUCKLESCRIPT VERSION 0.4.2 , PLEASE EDIT WITH CARE
'use strict';

var BUI     = require("@blp/ui");
var UI      = require("@ui");
var Runtime = require("@runtime");
var Curry   = require("../curry");

var data = /* array */[
  /* record */[
    /* ticker */"GOOG",
    /* price */700.0
  ],
  /* record */[
    /* ticker */"AAPL",
    /* price */500.0
  ],
  /* record */[
    /* ticker */"MSFT",
    /* price */300.0
  ]
];

function ui_layout(compile, lookup, appContext) {
  var init = Curry._1(compile, "bid  - ask");
  var computeFunction = [function (env) {
      return Curry._1(init, function (key) {
                  return Curry._2(lookup, env, key);
                });
    }];
  var hw1 = new BUI.HostedWindow();
  var hc = new BUI.HostedContent();
  var stackPanel = new UI.StackPanel();
  var inputCode = new UI.TextArea();
  var button = new UI.Button();
  var grid = new UI.Grid();
  hw1.appContext = appContext;
  hw1.title = "Test Application From OCaml";
  hw1.content = hc;
  hc.contentWidth = 700;
  hc.content = stackPanel;
  stackPanel.orientation = "vertical";
  stackPanel.minHeight = 10000;
  stackPanel.minWidth = 4000;
  stackPanel.addChild(grid);
  stackPanel.addChild(inputCode);
  stackPanel.addChild(button);
  var u = {
    "width": 200
  };
  grid.minHeight = 300;
  grid.titleRows = /* array */[
    {
      "label": {
        "text": "Ticker"
      }
    },
    {
      "label": {
        "text": "Bid"
      }
    },
    {
      "label": {
        "text": "Ask"
      }
    },
    {
      "label": {
        "text": "Result"
      }
    }
  ];
  grid.columns = /* array */[
    u,
    u,
    u,
    u
  ];
  inputCode.text = " bid - ask";
  inputCode.minHeight = 100;
  button.text = "update formula";
  button.minHeight = 20;
  button.on("click", function () {
        try {
          var hot_function = Curry._1(compile, inputCode.text);
          computeFunction[0] = function (env) {
            return Curry._1(hot_function, function (key) {
                        return Curry._2(lookup, env, key);
                      });
          };
          return /* () */0;
        }
        catch (e){
          return /* () */0;
        }
      });
  Runtime.setInterval(function () {
        return grid.dataSource = Array.prototype.map.call(data, function (param) {
                    var price = param[/* price */1];
                    var bid = price + 20 * Math.random();
                    var ask = price + 20 * Math.random();
                    var result = Curry._1(computeFunction[0], {
                          "bid": bid,
                          "ask": ask
                        });
                    var text = bid.toFixed(2);
                    var text$1 = ask.toFixed(2);
                    var text$2 = result.toFixed(2);
                    return /* array */[
                            {
                              "label": {
                                "text": param[/* ticker */0]
                              }
                            },
                            {
                              "label": {
                                "text": text
                              }
                            },
                            {
                              "label": {
                                "text": text$1
                              }
                            },
                            {
                              "label": {
                                "text": text$2
                              }
                            }
                          ];
                  });
      }, 100);
  return hw1;
}

exports.data      = data;
exports.ui_layout = ui_layout;
/* @blp/ui Not a pure module */
