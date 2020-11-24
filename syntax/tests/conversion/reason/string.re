%bs.raw
"define(x.y, 'userAgent', {value: 'USER_AGENT_STRING'})";

let x = {js|This is a long string with a slash and line break \
carriage return|js};

let x = "\"";
let y = "\n";

<> {"\n"->React.string} </>;
