let path = require("path");
let fs = require("fs");
let cp = require("child_process");

let parser = path.join(process.cwd(), "./lib/napkinscript.exe");

let refmt = path.join(process.cwd(), "lib", "refmt.exe");

function parseBinaryReason(filename) {
  let intf = isInterface(filename);
  let args = ["--print", "binary"];

  if (intf) {
    args.push("--interface");
    args.push("true");
  }

  args.push(filename);

  return cp.spawnSync(refmt, args).stdout;
}

function classifyLang(filename) {
  if (filename.length > 3) {
    let suffix = filename.substring(filename.length - 3);
    if (suffix === ".re" || suffix === "rei") {
      return "reason";
    } else if (suffix === ".ml" || suffix === "mli") {
      return "ocaml";
    }
  }
  return "napkinscript";
}

function isInterface(filename) {
  if (filename.length === 0) {
    return false;
  } else {
    return filename[filename.length - 1] === "i";
  }
}

function parseFile(filename, recover, env) {
  let args = ["-print", "ml"];
  if (recover) args.push("-recover");
  args.push(filename);
  return env ? cp.spawnSync(parser, args, { env }) : cp.spawnSync(parser, args);
}

function parseOcamlFileToNapkin(filename) {
  let { stdout } = cp.spawnSync(parser, [
    "-parse",
    "ml",
    "-print",
    "ns",
    filename,
  ]);
  return stdout.toString();
}

function parseOcamlFileToSexp(filename) {
  return cp
    .spawnSync(parser, ["-parse", "ml", "-print", "sexp", filename])
    .stdout.toString();
}

function parseReasonFileToSexp(filename) {
  let reasonBinary = parseBinaryReason(filename);

  let intf = isInterface(filename);

  let args = ["-parse", "reasonBinary", "-print", "sexp"];
  if (intf) {
    args.push("-interface");
  }
  return cp
    .spawnSync(parser, args, {
      input: reasonBinary,
      encoding: null,
    })
    .stdout.toString();
}

function parseNapkinFileToSexp(filename) {
  let { stdout } = cp.spawnSync(parser, [
    "-parse",
    "ns",
    "-print",
    "sexp",
    filename,
  ]);
  return stdout.toString();
}

function parseFileToSexp(filename) {
  let lang = classifyLang(filename);

  switch (lang) {
    case "reason":
      return parseReasonFileToSexp(filename);

    case "ocaml":
      return parseOcamlFileToSexp(filename);

    default:
      return parseNapkinFileToSexp(filename);
  }
}

function parseReasonFileToNapkin(filename, width = 100) {
  let intf = isInterface(filename);
  let reasonBinary = parseBinaryReason(filename);
  let args = ["-parse", "reasonBinary", "-print", "ns", "-width", width.toString()];

  if (intf) {
    args.push("-interface");
  }

  return cp
    .spawnSync(parser, args, { input: reasonBinary })
    .stdout.toString("utf8");
}

function parseNapkinStdinToSexp(src, isInterface) {
  let args = ["-parse", "ns", "-print", "sexp"];
  if (isInterface) {
    args.push("-interface");
  }
  return cp
    .spawnSync(parser, args, {
      input: src,
    })
    .stdout.toString("utf8");
}

function parseNapkinStdinToNapkin(src, isInterface, width = 100) {
  let args = ["-parse", "ns", "-print", "ns", "-width", width];
  if (isInterface) {
    args.push("-interface");
  }
  return cp
    .spawnSync(parser, args, {
      input: src,
    })
    .stdout.toString("utf8");
}

function printFile(filename) {
  let parserSrc;
  switch (classifyLang(filename)) {
    case "ocaml":
      parserSrc = "ml";
      break;

    case "reason":
      parserSrc = "re";
      return parseReasonFileToNapkin(filename, 80);
      break;

    case "napkinscript":
    default:
      parserSrc = "ns";
      break;
  }

  let intf = isInterface(filename);

  let args = ["-parse", parserSrc, "-print", "ns", "-width", "80"];

  if (intf) {
    args.push("-interface");
  }

  args.push(filename);

  return cp.spawnSync(parser, args).stdout.toString("utf8");
}

// File "/home/travis/build/IwanKaramazow/syntax/tests/parsing/errors/scanner/oldDerefOp.js", line: 1, characters 4-5:
// test output contains the full path of the file
// this differs between multiple machines
// Just drop "/home/travis/build/IwanKaramazow" to make the file path machine independent
let makeReproducibleFilename = (txt) => {
  // "Parse error: "
  let lines = txt.split("\n");
  for (let i = 0; i < lines.length; i++) {
    let txt = lines[i];
    if (
      txt.indexOf("File") === -1 ||
      txt.indexOf("line") === -1 ||
      txt.indexOf("characters") === -1
    ) {
      continue;
    }
    let prefix = txt.substring(0, 6); // Keep `File "`-prefix
    let suffix = txt.substring(txt.indexOf("/syntax"), txt.length);
    lines[i] = prefix + suffix;
  }
  return lines.join("\n");
};

global.runPrinter = (dirname) => {
  fs.readdirSync(dirname).forEach((base) => {
    let filename = path.join(dirname, base);
    if (!fs.lstatSync(filename).isFile() || base === "render.spec.js") {
      return;
    }

    test(base, () => {
      let napkin = printFile(filename);
      expect(napkin).toMatchSnapshot();

      if (process.env.ROUNDTRIP_TEST) {
        let intf = isInterface(filename);
        let sexpAst = parseFileToSexp(filename);
        let napkin2 = parseNapkinStdinToNapkin(napkin, intf, 80);
        let napkinSexpAst = parseNapkinStdinToSexp(napkin, intf);
        expect(sexpAst).toEqual(napkinSexpAst);
        expect(napkin).toEqual(napkin2);
      }
    });
  });
};

global.runParser = (dirname, recover = false, showError = false, env) => {
  fs.readdirSync(dirname).forEach((base) => {
    let filename = path.join(dirname, base);
    if (!fs.lstatSync(filename).isFile() || base === "parse.spec.js") {
      return;
    }

    test(base, () => {
      let res = parseFile(filename, recover, env);
      let parsetree = res.stdout.toString();
      let output = "";
      if (showError) {
        output += `=====Parsetree==========================================\n`;
        output += `${parsetree}\n`;
        output += `=====Errors=============================================\n`;
        output += `${makeReproducibleFilename(res.stderr.toString())}\n`;
        output += `========================================================`;
      } else {
        output = parsetree;
      }

      expect(output).toMatchSnapshot();
    });
  });
};

global.idemPotency = (dirname) => {
  let describeFn = process.env.ROUNDTRIP_TEST ? describe : describe.skip;

  describeFn(dirname, () => {
    fs.readdirSync(dirname).forEach((base) => {
      let filename = path.join(dirname, base);
      if (!fs.lstatSync(filename).isFile() || base === "parse.spec.js") {
        return;
      }

      let lang = classifyLang(base);

      let parseToNapkin;

      switch (lang) {
        case "reason":
          parseToNapkin = parseReasonFileToNapkin;
          break;

        case "ocaml":
        default:
          parseToNapkin = parseOcamlFileToNapkin;
      }

      test(base, () => {
        let intf = isInterface(filename);
        let napkin = parseToNapkin(filename);
        let sexpAst = parseFileToSexp(filename);
        let napkinSexpAst = parseNapkinStdinToSexp(napkin, intf);
        let napkin2 = parseNapkinStdinToNapkin(napkin, intf);
        expect(sexpAst).toEqual(napkinSexpAst);
        expect(napkin).toEqual(napkin2);
      });
    });
  });
};
