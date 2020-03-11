var a = 9223372036854575807n;

console.time("Int64.to_string");
for (var i = 0; i <= 1_00_000; ++i) {
  a.toString();
}
console.log(a.toString());
console.timeEnd("Int64.to_string");

var a = 30000000n;

console.time("Int64.to_string");
for (var i = 0; i <= 1_00_000; ++i) {
  a.toString();
}
console.log(a.toString());
console.timeEnd("Int64.to_string");

var a = -9223372036854775708n;

console.time("Int64.to_string");
for (var i = 0; i <= 1_00_000; ++i) {
  a.toString();
}
console.log(a.toString());
console.timeEnd("Int64.to_string");
