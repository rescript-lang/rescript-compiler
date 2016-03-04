var globalEval = eval;

/**
 * assume user input is 
 * @param {string} base  './stdlib/array[.js]'
 * @rel '../runtime/caml_array'
 */
function relativePath(base, rel){
    return base + '/../' + rel
}

/**
 * Used in cache
 * @param {string} path 
 * for example './stdlib/array.js'
 * 
 */
function normalizePath(path){
    if (!path.endsWith(".js")){
        path += ".js"
    }
    var files = path.split('/')
    var real = []
    for (var i = 0 ; i < files.length; ++i){
        if (files[i] === '..'){
            real.pop()
        } 
        else if(files[i] === '.'){
            // nothing
        } 
        else if (files[i] === ''){
            
        }
        else {
            real.push(files[i])            
        }
    }
    return real.join('/')
}



function extractDeps(code){
    var deps = [];
    code.replace(/(?:^|[^\w\$_.])require\s*\(\s*["']([^"']*)["']\s*\)/g, function (_, id) {
                    deps.push(id);
                   });
    //return [deps, "(function (require,exports) {\n" + code +  "\n})"];
    return deps;   
}

/**
 * @url example : './stdlib/array.js'
 */
function getContent(url, cb) {
    url = normalizePath(url);
    var x = new XMLHttpRequest();
    x.open('GET', url, true);
    x.addEventListener('load', function(event){ return cb(url, x.response)});
    x.send()
}

var Modules = {}

function initModule(url, deps, response){
    if(!Modules[url]){
        Modules[url] = {
            deps : deps ,
            exports : undefined,
            response : response,
            url : url
       };
    }
    return Modules[url]
}
function decrDeps(mod){
    -- mod.deps ; 
    if(mod.deps === 0){
        evalResponse(mod.url, mod.response)
    } 
}
function evalResponse(url, response) {
    if (!Modules[url].exports) {
        (globalEval("(function(require, exports){\n" + response + "\n})//# sourceURL=" + document.getElementById('base_ref').href + url))(
            function (link) { return req(url, link); },
            Modules[url].exports = {});
    }
}
function req(currentloc, file) {
    var url = normalizePath(relativePath(currentloc, file));
    if (Modules[url].exports !== undefined) {
        return Modules[url].exports
    } else {
        throw new Error(currentloc + " " + file + "not loaded");
    }
}
function cb(url, response, evalResponse) {
    url = normalizePath(url);
    var deps = extractDeps(response);
    var mod = initModule(url, deps.length, response)
    if (deps.length > 0) {
        /*  async.map(deps, function (value,yet_ignored){
             console.log(url, '..', value);
             getContent(relativePath(url,value),function(url,response){
                 cb(url,response, yet_ignored);
                 // yet_ignored(null,true);
             });
             
          }, function(err, results){
              evalResponse(url,response)
          });
          */
        for (var i = 0; i < deps.length; ++i) {
            // console.log(url,deps[i], relativePath(url,deps[i]));
            getContent(relativePath(url, deps[i]), function (url, response) {
                cb(url, response);
                decrDeps(mod)
            })
        }
        // evalResponse(url, response)
    } else {
        evalResponse(null, url, response)
    }
}
