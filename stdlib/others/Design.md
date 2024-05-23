



hierachy

# set 
## bs_internalAVLset (basic module with rotation)

  methods in this moudle could be shared  by (at least 2 of them)

  fuctional set, functional specialized set
  mutable set, mutable specialized set.

  for example, [mem0] could be shared by functional/mutable poly set


## bs_Set
  functional poly set (depends on bs_internalAVLset)

## intenral_set.cppo.ml
## bs_intenralSetInt
## bs_internalSetString

  methods could be shared by funcitional/imperative specialized set.
  This intermediate module is created since we want to share methods
  like [findOpt], [cmp]

## set.cppo.ml
## bs_SetInt
## bs_SetString



## setm.cpp.ml
## bs_SetIntM
## bs_SetStringM

# map 
## bs_internalAVLtree (basic module with rotation)

## bs_Map

## internal_map.cpp.ml
## bs_internalMapInt
## bs_internalMapString

## map.cppo.ml
## bs_MapInt
## bs_MapString

## mapm.cppo.ml
## bs_MapIntM
## bs_MapStringM