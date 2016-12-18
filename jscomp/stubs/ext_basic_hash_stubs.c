#include <caml/hash.h>

#define FINAL_MIX(h) \
  h ^= h >> 16; \
  h *= 0x85ebca6b; \
  h ^= h >> 13; \
  h *= 0xc2b2ae35; \
  h ^= h >> 16;

#define ROTL32(x,n) ((x) << n | (x) >> (32-n))

#define MIX(h,d) \
  d *= 0xcc9e2d51; \
  d = ROTL32(d, 15); \
  d *= 0x1b873593; \
  h ^= d; \
  h = ROTL32(h, 13); \
  h = h * 5 + 0xe6546b64;

CAMLprim value caml_bs_hash_string (value obj){

  uint32 h = 0;
  h = caml_hash_mix_string(h,obj);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}

CAMLprim value caml_bs_hash_int  ( value d){
  uint32 h = 0; 
  h = caml_hash_mix_intnat(h,d);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}

CAMLprim value caml_bs_hash_string_and_int  (value obj, value d){
  uint32 h = 0; 
  h = caml_hash_mix_string(h,obj);
  h = caml_hash_mix_intnat(h,d);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}

CAMLprim value caml_bs_hash_string_and_small_int(value obj, value d){
  uint32 h = 0;
  h = caml_hash_mix_string(h,obj);
  MIX(h,d);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}

CAMLprim value caml_bs_hash_small_int(value d){
  uint32 h = 0; 
  intnat stamp = Long_val(d);
  MIX(h,d);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}
/*
 * We gave up the idea to  hash Ident.t (take only one argument)
 * customized hash function for Ident.t, first 
 * argument is stamp, second argument is string 
 * It's not just introducing c stubs, we need make a clear line
 * which part of our libraries depends on Ident.t
 */
CAMLprim value caml_bs_hash_stamp_and_name(value d, value obj ){
  uint32 h = 0;
  intnat stamp = Long_val(d); 
  if (stamp){
    MIX(h,d);
  } else {
    h = caml_hash_mix_string(h,obj);
  }
  
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}



/* local variables: */
/* compile-command: "ocamlopt.opt -c hash_runtime.c" */
/* end: */
