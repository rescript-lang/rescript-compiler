'use strict';


function get_uint8(str, off) {
  return 33;
}

var BigEndian = {
  get_uint8: get_uint8
};

var ExtUnixAll = {
  BigEndian: BigEndian
};

var ExtUnix = {
  All: undefined
};

function test_endian_string(x) {
  return 33;
}

var v = 33;

var Test = {
  test_endian_string: test_endian_string,
  v: v
};

exports.ExtUnixAll = ExtUnixAll;
exports.ExtUnix = ExtUnix;
exports.Test = Test;
/* No side effect */
