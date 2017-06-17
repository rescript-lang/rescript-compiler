'use strict';
var fs = require('fs')
var marked = require('marked')

var renderer = new marked.Renderer()
var path = require('path')

var oldrender = new marked.Renderer()
renderer.link = function(href,title,text){
    var ext = path.extname(href)
    if(!title && (ext === ".md" )){
        return `<a href="${path.join(path.dirname(href),path.basename(href,".md"))+'.html'}">${text}</a>` 
    } else{
        return oldrender.link(href,title,text);
    }
}

var glob= require('glob')
var sidebar = "_Sidebar.md"
var HandleBars = require('handlebars')

let template = HandleBars.compile(fs.readFileSync("_template.html.tpl", 'utf8'))
let index = marked(fs.readFileSync(sidebar,'utf8'), {renderer})



glob.sync("*.md").forEach(x=>{
    if (!x.startsWith("_")){
        console.log(`processing ${x}`)

        var content = marked(fs.readFileSync(x,'utf8'),{renderer});
        var github_link = `https://github.com/bucklescript/bucklescript/tree/master/docs/${x}`
        let output = template({content, index, github_link})
        fs.writeFileSync(
            "dist/" + path.basename(x,".md")+".html",
            output,'utf8')
    }
})