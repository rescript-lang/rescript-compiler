// @ts-check

const { execFileSync } = require("child_process");

[
  ["belt_HashSetString.res", "hashset.cppo.res", "TYPE_STRING"],
  ["belt_HashSetString.resi", "hashset.cppo.resi", "TYPE_STRING"],
  ["belt_HashSetInt.res", "hashset.cppo.res", "TYPE_INT"],
  ["belt_HashSetInt.resi", "hashset.cppo.resi", "TYPE_INT"],
  ["belt_HashMapString.res", "hashmap.cppo.res", "TYPE_STRING"],
  ["belt_HashMapString.resi", "hashmap.cppo.resi", "TYPE_STRING"],
  ["belt_HashMapInt.res", "hashmap.cppo.res", "TYPE_INT"],
  ["belt_HashMapInt.resi", "hashmap.cppo.resi", "TYPE_INT"],
  ["belt_MapString.res", "map.cppo.res", "TYPE_STRING"],
  ["belt_MapString.resi", "map.cppo.resi", "TYPE_STRING"],
  ["belt_MapInt.res", "map.cppo.res", "TYPE_INT"],
  ["belt_MapInt.resi", "map.cppo.resi", "TYPE_INT"],
  ["belt_SetString.res", "belt_Set.cppo.res", "TYPE_STRING"],
  ["belt_SetString.resi", "belt_Set.cppo.resi", "TYPE_STRING"],
  ["belt_SetInt.res", "belt_Set.cppo.res", "TYPE_INT"],
  ["belt_SetInt.resi", "belt_Set.cppo.resi", "TYPE_INT"],
  ["belt_MutableMapString.res", "mapm.cppo.res", "TYPE_STRING"],
  ["belt_MutableMapString.resi", "mapm.cppo.resi", "TYPE_STRING"],
  ["belt_MutableMapInt.res", "mapm.cppo.res", "TYPE_INT"],
  ["belt_MutableMapInt.resi", "mapm.cppo.resi", "TYPE_INT"],
  ["belt_MutableSetString.res", "setm.cppo.res", "TYPE_STRING"],
  ["belt_MutableSetString.resi", "setm.cppo.resi", "TYPE_STRING"],
  ["belt_MutableSetInt.res", "setm.cppo.res", "TYPE_INT"],
  ["belt_MutableSetInt.resi", "setm.cppo.resi", "TYPE_INT"],
  ["belt_SortArrayString.res", "sort.cppo.res", "TYPE_STRING"],
  ["belt_SortArrayString.resi", "sort.cppo.resi", "TYPE_STRING"],
  ["belt_SortArrayInt.res", "sort.cppo.res", "TYPE_INT"],
  ["belt_SortArrayInt.resi", "sort.cppo.resi", "TYPE_INT"],
  ["belt_internalMapString.res", "internal_map.cppo.res", "TYPE_STRING"],
  ["belt_internalMapInt.res", "internal_map.cppo.res", "TYPE_INT"],
  ["belt_internalSetString.res", "internal_set.cppo.res", "TYPE_STRING"],
  ["belt_internalSetInt.res", "internal_set.cppo.res", "TYPE_INT"],
].forEach(([output, input, type]) => {
  execFileSync(
    "cppo",
    [
      "-n",
      "-D",
      type,
      `jscomp/others_cppo/${input}`,
      "-o",
      `jscomp/others/${output}`,
    ],
    { stdio: "inherit" },
  );
});
