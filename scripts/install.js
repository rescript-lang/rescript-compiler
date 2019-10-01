//@ts-check
// For Windows, we distribute a prebuilt bsc.exe
// To build on windows, we still need figure out constructing config.ml
// from existing  compiler

// For other OSes, we detect
// if there is other compiler installed and the version matches,
// we get the config.ml from existing OCaml compiler and build whole_compiler

// Otherwise, we build the compiler shipped with Buckle and use the
// old compiler.ml
// This will be run in npm postinstall, don't use too fancy features here

var cp = require("child_process");
var fs = require("fs");
var path = require("path");
var root_dir = path.join(__dirname, "..");
var lib_dir = path.join(root_dir, "lib");
var jscomp_dir = path.join(root_dir, "jscomp");
var runtime_dir = path.join(jscomp_dir, "runtime");
var others_dir = path.join(jscomp_dir, "others");

var ocaml_dir = path.join(lib_dir, "ocaml");
var config = require("./config.js");

var is_windows = config.is_windows;
var sys_extension = config.sys_extension;

process.env.BS_RELEASE_BUILD = "true";

var ninja_bin_output = path.join(root_dir, "lib", "ninja.exe");
var preBuiltCompilerArtifacts = ["bsc", "bsb", "bsb_helper", "bsppx", "refmt"];

/**
 * Make sure `ninja_bin_output` exists
 * The installation of `ninja.exe` is re-entrant, since we always pre-check if it is already installed
 * This is less problematic since `ninja.exe` is very stable
 */
function provideNinja() {
    var vendor_ninja_version = "1.9.0.git";
    var ninja_source_dir = path.join(root_dir, "vendor", "ninja");
    function build_ninja() {
        console.log(`building ninja`);
        cp.execSync(`tar xzvf ../ninja.tar.gz`, {
            cwd: ninja_source_dir,
            stdio: [0, 1, 2]
        });
        console.log("No prebuilt Ninja, building Ninja now");
        var build_ninja_command = "./configure.py --bootstrap";
        cp.execSync(build_ninja_command, {
            cwd: ninja_source_dir,
            stdio: [0, 1, 2]
        });
        fs.renameSync(path.join(ninja_source_dir, "ninja"), ninja_bin_output);
        console.log("ninja binary is ready: ", ninja_bin_output);
    }

    // sanity check to make sure the binary actually runs. Used for Linux. Too many variants
    /**
     *
     * @param {string} binary_path
     */
    function test_ninja_compatible(binary_path) {
        var version;
        try {
            version = cp
                .execSync(JSON.stringify(binary_path) + " --version", {
                    encoding: "utf8",
                    stdio: ["pipe", "pipe", "ignore"] // execSync outputs to stdout even if we catch the error. Silent it here
                })
                .trim();
        } catch (e) {
            console.log("ninja not compatible?", String(e));
            return false;
        }
        return version === vendor_ninja_version;
    }

    var ninja_os_path = path.join(
        ninja_source_dir,
        "snapshot",
        "ninja" + sys_extension
    );
    if (
        fs.existsSync(ninja_bin_output) &&
        test_ninja_compatible(ninja_bin_output)
    ) {
        console.log(
            "ninja binary is already cached and installed: ",
            ninja_bin_output
        );
        return;
    }
    if (fs.existsSync(ninja_os_path)) {
        if (fs.copyFileSync) {
            // ninja binary size is small
            fs.copyFileSync(ninja_os_path, ninja_bin_output);
        } else {
            fs.renameSync(ninja_os_path, ninja_bin_output);
        }
        if (test_ninja_compatible(ninja_bin_output)) {
            console.log("ninja binary is copied from pre-distribution");
            return;
        }
    }
    build_ninja();
}
/**
 *
 * @param {NodeJS.ErrnoException} err
 */
function throwWhenError(err) {
    if (err !== null) {
        throw err;
    }
}
/**
 *
 * @param {string} file
 * @param {string} target
 */
function poorCopyFile(file, target) {
    var stat = fs.statSync(file);
    fs.createReadStream(file).pipe(
        fs.createWriteStream(target, { mode: stat.mode })
    );
}
/**
 * @type {(x:string,y:string)=>void}
 *
 */
var installTrytoCopy;
if (fs.copyFile !== undefined) {
    installTrytoCopy = function (x, y) {
        fs.copyFile(x, y, throwWhenError);
    };
} else if (is_windows) {
    installTrytoCopy = function (x, y) {
        fs.rename(x, y, throwWhenError);
    };
} else {
    installTrytoCopy = function (x, y) {
        poorCopyFile(x, y);
    };
}

/**
 *
 * @param {string} src
 * @param {(file:string)=>boolean} filter
 * @param {string} dest
 */
function installDirBy(src, dest, filter) {
    fs.readdir(src, function (err, files) {
        if (err === null) {
            files.forEach(function (file) {
                if (filter(file)) {
                    var x = path.join(src, file);
                    var y = path.join(dest, file);
                    // console.log(x, '----->', y )
                    installTrytoCopy(x, y);
                }
            });
        } else {
            throw err;
        }
    });
}

/**
 *
 * @param {string} dir
 * TODO: `mkdirSync` may fail
 */
function ensureExists(dir) {
    if (!fs.existsSync(dir)) {
        fs.mkdirSync(dir);
    }
}

/**
 * @param {string} stdlib
 */
function install(stdlib) {
    installDirBy(runtime_dir, ocaml_dir, function (file) {
        var y = path.parse(file);
        return y.name === "js" || y.ext.includes("cm");
    });
    installDirBy(others_dir, ocaml_dir, function (file) {
        var y = path.parse(file);
        return y.ext === ".ml" || y.ext === ".mli" || y.ext.includes("cm");
    });
    var stdlib_dir = path.join(jscomp_dir, stdlib);
    installDirBy(stdlib_dir, ocaml_dir, function (file) {
        var y = path.parse(file);
        return y.ext === ".ml" || y.ext === ".mli" || y.ext.includes("cm");
    });
}

/**
 *
 * @param {string} sys_extension
 *
 */
function copyPrebuiltCompilersForUnix(sys_extension) {
    var output = `
rule cp 
    command = cp $in $out
`;
    output += preBuiltCompilerArtifacts
        .map(function (x) {
            return `build ${x}.exe: cp ${x}${sys_extension}`;
        })
        .join("\n");
    output += "\n";

    var filePath = path.join(lib_dir, "copy.ninja");
    fs.writeFileSync(filePath, output, "ascii");
    cp.execFileSync(ninja_bin_output, ["-f", "copy.ninja"], {
        cwd: lib_dir,
        stdio: [0, 1, 2]
    });
    fs.unlinkSync(filePath);
}

/**
 *
 * @param {string} sys_extension
 *
 */
function copyPrebuiltCompilersForWindows(sys_extension) {
    preBuiltCompilerArtifacts
        .forEach(function (x) {
            fs.copyFileSync(path.join(lib_dir, `${x}${sys_extension}`), path.join((lib_dir), `${x}.exe`));
        });
}

function copyPrebuiltCompilers() {
    switch (sys_extension) {
        case ".win32":
            copyPrebuiltCompilersForWindows(sys_extension);
            break;

        default:
            copyPrebuiltCompilersForUnix(sys_extension);
            break;
    }
}

/**
 * @returns {string|undefined}
 */
function checkPrebuiltBscCompiler() {
    if (process.env.BS_TRAVIS_CI) {
        return;
    }
    try {
        var version = String(
            cp.execFileSync(path.join(lib_dir, "bsc" + sys_extension), ["-v"])
        );

        var myOCamlVersion = version.substr(
            version.indexOf(":") + 1,
            version.lastIndexOf(" ") - version.indexOf(":") - 1
        );
        console.log("checkoutput:", version, "ocaml version", myOCamlVersion);
        console.log("Prebuilt compiler works good");

        return myOCamlVersion;
    } catch (e) {
        console.log("No working prebuilt buckleScript compiler");
        if (is_windows) {
            throw new Error("no prebuilt bsc compiler on windows");
        }
        return;
    }
}
/**
 *
 * @param {string} stdlib
 */
function buildLibs(stdlib) {
    ensureExists(lib_dir);
    ensureExists(ocaml_dir);
    ensureExists(path.join(lib_dir, "js"));
    ensureExists(path.join(lib_dir, "es6"));
    process.env.NINJA_IGNORE_GENERATOR = "true";
    var releaseNinja = `
stdlib = ${stdlib}
subninja runtime/release.ninja
subninja others/release.ninja
subninja $stdlib/release.ninja
${process.env.BS_TRAVIS_CI ? "subninja test/build.ninja\n" : "\n"}
build all: phony runtime others $stdlib
`;
    var filePath = path.join(jscomp_dir, "release.ninja");
    fs.writeFileSync(filePath, releaseNinja, "ascii");
    var cleanArgs = ["-f", "release.ninja", "-t", "clean"];
    cp.execFileSync(ninja_bin_output, cleanArgs, {
        cwd: jscomp_dir,
        stdio: [0, 1, 2],
        shell: false
    });
    var buildArgs = ["-f", "release.ninja"];
    if (process.env.BS_TRAVIS_CI) {
        buildArgs.push("--verbose");
    }
    cp.execFileSync(ninja_bin_output, buildArgs, {
        cwd: jscomp_dir,
        stdio: [0, 1, 2],
        shell: false
    });
    fs.unlinkSync(filePath);
    console.log("Build finished");
}

/**
 * @returns {string}
 */
function provideCompiler() {
    var myVersion = checkPrebuiltBscCompiler();
    if (myVersion !== undefined) {
        copyPrebuiltCompilers();
        return myVersion;
    } else {
        myVersion = require("./buildocaml.js").getVersionPrefix();
        var ocamlopt =
            process.env.ESY === "true"
                ? "ocamlopt.opt"
                : path.join(
                    __dirname,
                    "..",
                    "native",
                    myVersion,
                    "bin",
                    "ocamlopt.opt"
                );
        if (!fs.existsSync(ocamlopt)) {
            require("./buildocaml.js").build(true);
        } else {
            console.log(ocamlopt, "is already there");
        }
        // Note this ninja file only works under *nix due to the suffix
        // under windows require '.exe'
        var releaseNinja = require("./ninjaFactory.js").libNinja({
            ocamlopt: ocamlopt,
            ext: ".exe",
            INCL: myVersion,
            isWin: is_windows
        });

        var filePath = path.join(lib_dir, "release.ninja");
        fs.writeFileSync(filePath, releaseNinja, "ascii");
        cp.execFileSync(ninja_bin_output, ["-f", "release.ninja"], {
            cwd: lib_dir,
            stdio: [0, 1, 2]
        });
        fs.unlinkSync(filePath);
        return myVersion;
    }
}

provideNinja();

var ocamlVersion = provideCompiler();

var stdlib = ocamlVersion.includes("4.02") ? "stdlib-402" : "stdlib-406";

buildLibs(stdlib);
install(stdlib);
