//@ts-check

// -inline 1000 makes size too large
// # TODO: make sure it can be bootstrapped, at least is a very good
// # test case of our optimizations
// # build bsdep.exe: cc bsdep.mli bsdep.ml
// ../native/4.06.1/bin/ocamlopt.opt -c -O2 -nodynlink -I 4.06.1 -g -w a+32  4.06.1/whole_compiler.mli 4.06.1/whole_compiler.ml  &> warning.log

var targetDir = require("./bin_path").dirName;

/**
 *
 * @param {{ocamlopt : string ;  INCL : string, isWin : boolean}} config
 *
 */
function libNinja(config) {
  return `
ocamlopt = ${config.ocamlopt}${config.isWin ? ".exe" : ""}
ext = .exe
INCL = ${config.INCL}
flags = -nodynlink -I $INCL -g -w -a ../jscomp/stubs/ext_basic_hash_stubs.c
rule cc
    command = $ocamlopt -O2  $flags $in -o $out ${
      config.isWin ? "" : "&& strip $out"
    }
    description = Making $out
build ../${targetDir}/rescript$ext:  cc $INCL/rescript.mli $INCL/rescript.ml
    flags = $flags -unboxed-types unix.cmxa str.cmxa    
build ../${targetDir}/bsb_helper$ext:  cc $INCL/bsb_helper.mli $INCL/bsb_helper.ml
    flags = $flags  -unboxed-types -w -a
build ../${targetDir}/bsc$ext: cc $INCL/whole_compiler.mli $INCL/whole_compiler.ml
    flags = $flags -w A-4-9-48-40-45-41-44-50-21-30-32-34-37-27-60-42 str.cmxa
`;
}

exports.libNinja = libNinja;
