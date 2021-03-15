%bs.raw
"define(x.y, 'userAgent', {value: 'USER_AGENT_STRING'})";

%raw
"define(x.y, 'userAgent', {value: 'USER_AGENT_STRING'})";

let x = {js|This is a long string with a slash and line break \
carriage return|js};

let x = "\"";
let y = "\n";

<> {"\n"->React.string} </>;

// The `//` should not result into an extra comment
let x = {j|https://www.apple.com|j};
let x = {|https://www.apple.com|};
let x = {js|https://www.apple.com|js};
let x = {|https://www.apple.com|};
let x = {sql|https://www.apple.com|sql};

// /* */ should not result in an extra comments
let x = {j|/* https://www.apple.com */|j};
let x = {|/* https://www.apple.com*/|};
let x = {js|/*https://www.apple.com*/|js};
let x = {|/*https://www.apple.com*/|};
let x = {sql|/*https://www.apple.com*/|sql};

let x = {js|`https://${appleWebsite}`|js};

let var1 = "three"
let var2 = "a string";

switch (var1, var2) {
| ({|3|}, {|a string|}) => Js.log("worked")
| ({| test with ` ${here} `|}, _) => Js.log("escapes ` and ${")
| _ => Js.log("didn't match")
};
