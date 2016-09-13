
// For Windows, we distribute a prebuilt bsc.exe
// To build on windows, we still need figure out constructing config.ml
// from existing  compiler

// For other OSes, we detect 
// if there is other compiler installed and the version matches, 
// we get the config.ml from existing OCaml compiler and build `whole_compiler`

// Otherwise, we build the compiler shipped with Buckle and use the 
// old compiler.ml

var child_process = require('child_process')
var fs = require('fs')
var process = require('process')
var path = require('path')

// set up environment
process.env.BS_RELEASE_BUILD = 1 
process.env.OCAMLPARAM = '_,bin-annot=1' 
process.env.OCAMLRUNPARAM = 'b'
var bs_travis_ci = process.env.BS_TRAVIS_CI !==  undefined

function push_front(p){
    process.env.PATH= p + path.delimiter + process.env.PATH
}

if (bs_travis_ci){
    child_process.execSync('git submodule update --init --recursive')

}else {
}
var content = fs.readFileSync('../ocaml/utils/config.mlp','utf8')

var config_output;


try
{
  config_output = child_process.execSync('ocamlc.opt -config', {encoding: 'utf8'})
}
catch(e){
    // No ocamlc.opt found
}

var output = config_output.split('\n').filter(function(x){return x}).map(function(x){return x.split(":",2).map(function(x){return x.trim()})})

var config_map = {}

for (var k = 0 ; k < output.length ; ++k){
    config_map[output[k][0]] = output[k][1]
}

// need check which variables exist when we update compiler 
var map = {
    LIBDIR : "standard_library_default",
    BYTERUN : "standard_runtime", 
    CCOMPTYPE : "ccomp_type",
    BYTECC : "bytecomp_c_compiler",
    BYTECCLIBS : "bytecomp_c_libraries",
    NATIVECC : "native_c_compiler",
    NATIVECCLIBS : "native_c_libraries",
    PACKLD : "native_pack_linker",
    RANLIBCMD : "ranlib",
    ARCMD : "ar",
    CC_PROFILE : "cc_profile",

    MKDLL : "mkdll", // undefined
    MKEXE : "mkexe", // undefined
    MKMAINDLL : "mkmaindll", // undefined TODO: upstream to print it too
    
    ARCH : "architecture",
    MODEL : "model",
    SYSTEM : "system",
    ASM : "asm",
    ASM_CFI_SUPPORTED : "asm_cfi_supported", // boolean
    WITH_FRAME_POINTERS : "with_frame_pointers", // boolean
    EXT_OBJ : "ext_obj", 
    EXT_ASM : "ext_asm",
    EXT_LIB : "ext_lib",
    EXT_DLL : "ext_lib",
    HOST : "host",
    TARGET : "target",
    SYSTHREAD_SUPPORT : "systhread_supported" // boolean
}

fs.writeFileSync('config.ml', content.replace(/%%(\w+)%%/g, 
    function(_whole,p0){
        return config_map[map[p0]]
    }), 'utf8')