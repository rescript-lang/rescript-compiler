// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function myField(param) {
  return param.myField;
}

function num(param_0) {
  return {
          TAG: "Num",
          _0: param_0
        };
}

function doubleNum(param_0, param_1) {
  return {
          TAG: "DoubleNum",
          _0: param_0,
          _1: param_1
        };
}

function compose(a, accessor) {
  return accessor(a);
}

var _composedMyField = 1;

var _composedNum = {
  TAG: "Num",
  _0: 1
};

var noParam = "NoParam";

var _myFieldAlias = myField;

var _noParamAlias = "NoParam";

var _numAlias = num;

var _doubleNumAlias = doubleNum;

exports.myField = myField;
exports.noParam = noParam;
exports.num = num;
exports.doubleNum = doubleNum;
exports._myFieldAlias = _myFieldAlias;
exports._noParamAlias = _noParamAlias;
exports._numAlias = _numAlias;
exports._doubleNumAlias = _doubleNumAlias;
exports.compose = compose;
exports._composedMyField = _composedMyField;
exports._composedNum = _composedNum;
/* No side effect */
