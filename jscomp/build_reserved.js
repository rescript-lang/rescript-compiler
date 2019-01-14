//@ts-check

var fs = require('fs');
var puppeteer = require('puppeteer');
(async function(){
    const browser = await puppeteer.launch();
    const page = await browser.newPage();
    /**
     * @type string[]
     */
    const result = await page.evaluate(`Object.getOwnPropertyNames(window)`)
    fs.writeFileSync('keywords.list',result.filter(x=>/^[A-Z]/.test(x)).join('\n'),'utf8')
    await browser.close()
}())