//@ts-check

const fs = require("fs");
const path = require("path");
const { execSync } = require("child_process");
const puppeteer = require("puppeteer");

const jscompDir = path.join(__dirname, "..", "jscomp");
const keywordsFile = path.join(jscompDir, "keywords.list");
const reservedMap = path.join(jscompDir, "ext", "js_reserved_map.ml");

(async function () {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  /**
   * @type string[]
   */
  const result = await page.evaluate(`Object.getOwnPropertyNames(window)`);
  fs.writeFileSync(
    keywordsFile,
    result
      .filter(x => /^[A-Z]/.test(x))
      .sort()
      .join("\n"),
    "utf8"
  );
  await browser.close();

  execSync(`ocaml build_reserved.ml ${keywordsFile} ${reservedMap}`, {
    cwd: __dirname,
  });
})();
