// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";

function for_(x) {
  for (let i = 0, i_finish = (console.log("hi"), x.length); i <= i_finish; ++i) {
    console.log(x[i]);
  }
}

function for_2(x) {
  for (let i = 0, i_finish = x.length; i <= i_finish; ++i) {
    console.log(x[i]);
  }
}

function for_3(x) {
  let v = {
    contents: 0
  };
  let arr = Belt_Array.map(x, param => (() => {}));
  for (let i = 0, i_finish = x.length; i <= i_finish; ++i) {
    let j = (i << 1);
    arr[i] = () => {
      v.contents = v.contents + j | 0;
    };
  }
  Belt_Array.forEach(arr, x => x());
  return v.contents;
}

function for_4(x) {
  let v = {
    contents: 0
  };
  let arr = Belt_Array.map(x, param => (() => {}));
  for (let i = 0, i_finish = x.length; i <= i_finish; ++i) {
    let j = (i << 1);
    let k = (j << 1);
    arr[i] = () => {
      v.contents = v.contents + k | 0;
    };
  }
  Belt_Array.forEach(arr, x => x());
  return v.contents;
}

function for_5(x, u) {
  let v = {
    contents: 0
  };
  let arr = Belt_Array.map(x, param => (() => {}));
  for (let i = 0, i_finish = x.length; i <= i_finish; ++i) {
    let k = Math.imul((u << 1), u);
    arr[i] = () => {
      v.contents = v.contents + k | 0;
    };
  }
  Belt_Array.forEach(arr, x => x());
  return v.contents;
}

function for_6(x, u) {
  let v = {
    contents: 0
  };
  let arr = Belt_Array.map(x, param => (() => {}));
  let v4 = {
    contents: 0
  };
  let v5 = {
    contents: 0
  };
  v4.contents = v4.contents + 1 | 0;
  for (let j = 0; j <= 1; ++j) {
    v5.contents = v5.contents + 1 | 0;
    let v2 = {
      contents: 0
    };
    for (let i = 0, i_finish = x.length; i <= i_finish; ++i) {
      let k = Math.imul((u << 1), u);
      let h = (v5.contents << 1);
      v2.contents = v2.contents + 1 | 0;
      arr[i] = () => {
        v.contents = (((((v.contents + k | 0) + v2.contents | 0) + u | 0) + v4.contents | 0) + v5.contents | 0) + h | 0;
      };
    }
  }
  Belt_Array.forEach(arr, x => x());
  return v.contents;
}

export {
  for_,
  for_2,
  for_3,
  for_4,
  for_5,
  for_6,
}
/* No side effect */
