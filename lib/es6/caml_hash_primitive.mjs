


function rotl32(x, n) {
  return (x << n) | (x >>> (32 - n | 0)) | 0;
}

function caml_hash_mix_int(h, d) {
  var d$1 = d;
  d$1 = Math.imul(d$1, -862048943);
  d$1 = rotl32(d$1, 15);
  d$1 = Math.imul(d$1, 461845907);
  var h$1 = h ^ d$1;
  h$1 = rotl32(h$1, 13);
  return (h$1 + (h$1 << 2) | 0) - 430675100 | 0;
}

function caml_hash_final_mix(h) {
  var h$1 = h ^ (h >>> 16);
  h$1 = Math.imul(h$1, -2048144789);
  h$1 = h$1 ^ (h$1 >>> 13);
  h$1 = Math.imul(h$1, -1028477387);
  return h$1 ^ (h$1 >>> 16);
}

function caml_hash_mix_string(h, s) {
  var len = s.length;
  var block = (len / 4 | 0) - 1 | 0;
  var hash = h;
  for(var i = 0; i <= block; ++i){
    var j = (i << 2);
    var w = s.charCodeAt(j) | (s.charCodeAt(j + 1 | 0) << 8) | (s.charCodeAt(j + 2 | 0) << 16) | (s.charCodeAt(j + 3 | 0) << 24);
    hash = caml_hash_mix_int(hash, w);
  }
  var modulo = len & 3;
  if (modulo !== 0) {
    var w$1 = modulo === 3 ? (s.charCodeAt(len - 1 | 0) << 16) | (s.charCodeAt(len - 2 | 0) << 8) | s.charCodeAt(len - 3 | 0) : (
        modulo === 2 ? (s.charCodeAt(len - 1 | 0) << 8) | s.charCodeAt(len - 2 | 0) : s.charCodeAt(len - 1 | 0)
      );
    hash = caml_hash_mix_int(hash, w$1);
  }
  hash = hash ^ len;
  return hash;
}

export {
  caml_hash_mix_int ,
  caml_hash_mix_string ,
  caml_hash_final_mix ,
  
}
/* No side effect */
