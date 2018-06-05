/* Copyright (C) 2018 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */



//@ts-check

// @ts-ignore
window.process = { env: { NODE_ENV: 'dev' } }


// local to getPath
var relativeElement = document.createElement("a");
var baseElement = document.createElement("base");
document.head.appendChild(baseElement);

export function BsGetPath(id, parent) {
    var oldPath = baseElement.href
    baseElement.href = parent
    relativeElement.href = id
    var result = relativeElement.href
    baseElement.href = oldPath
    return result
}
/**
 * 
 * Given current link and its parent, return the new link
 * @param {string} id 
 * @param {string} parent 
 * @return {string}
 */
function getPathWithJsSuffix(id, parent) {
    var oldPath = baseElement.href
    baseElement.href = parent
    relativeElement.href = id
    var result = addSuffixJsIfNot(relativeElement.href)
    baseElement.href = oldPath
    return result
}

/**
 * 
 * @param {string} x 
 */
function addSuffixJsIfNot(x) {
    if (x.endsWith('.js')) {
        return x
    } else {
        return x + '.js'
    }
}


var falsePromise = Promise.resolve(false)
var fetchConfig = {'cache' : 'no-cache'}
// package.json semantics
// a string to module object 
// from url -> module object 
// Modules : Map<string, Promise < boolean | string > 
// fetch the link:
// - if it is already fetched before, return the stored promise
//   otherwise create the promise which will be filled with the text if successful
//   or filled with boolean false when failed
var MODULES = new Map()
function cachedFetch(link) {
    // console.info(link)
    var linkResult = MODULES.get(link)
    if (linkResult) {
        return linkResult
    } else {
        var p = fetch(link, fetchConfig)
            .then(resp => {
                if (resp.ok) {
                    return resp.text()
                } else {
                    return falsePromise
                }
            })

        MODULES.set(link, p)
        return p
    }
}

// from location id -> url 
// There are two rounds of caching:
// 1. if location and relative path is hit, no need to run 
// 2. if location and relative path is not hit, but the resolved link is hit, no need 
//     for network request
/**
 * @type {Map<string, Map<string, Promise<any> > > }
 */
var IDLocations = new Map()

/**
 * @type {Map<string, Map<string, any> > }
 */
var SyncedIDLocations = new Map()
// Its value is an object 
// { link : String }
// We will first mark it when visiting (to avoid duplicated computation)
// and populate its link later

/**
 * 
 * @param {string} id 
 * @param {string} location 
 */
function getIdLocation(id, location) {
    var idMap = IDLocations.get(location)
    if (idMap) {
        return idMap.get(id)
    }
}

/**
 * 
 * @param {string} id 
 * @param {string} location 
 */
function getIdLocationSync(id, location) {
    var idMap = SyncedIDLocations.get(location)
    if (idMap) {
        return idMap.get(id)
    }
}

function countIDLocations() {
    var count = 0
    for (let [k, vv] of IDLocations) {
        for (let [kv, v] of vv) {
            count += 1
        }
    }
    console.log(count, 'modules loaded')
}


/**
 * 
 * @param {string} id 
 * @param {string} location 
 * @param {Function} cb 
 * @returns Promise<any>
 */
function visitIdLocation(id, location, cb) {
    var result;
    var idMap = IDLocations.get(location)
    if (idMap && (result = idMap.get(id))) {
        return result
    }
    else {
        result = new Promise(resolve => {
            return (cb()).then(res => {
                var idMap = SyncedIDLocations.get(location)
                if (idMap) {
                    idMap.set(id, res)
                } else {
                    SyncedIDLocations.set(location, new Map([[id, res]]))
                }
                return resolve(res)
            })
        })
        if (idMap) {
            idMap.set(id, result)
        }
        else {
            IDLocations.set(location, new Map([[id, result]]))
        }
        return result
    }
}



/**
 * 
 * @param {string} text 
 * @return {string[]}
 */
function getDeps(text) {
    var deps = []
    text.replace(/(\/\*[\w\W]*?\*\/|\/\/[^\n]*|[.$]r)|\brequire\s*\(\s*["']([^"']*)["']\s*\)/g, function (_, ignore, id) {
        if (!ignore) deps.push(id);
    });
    return deps;
}



// By using a named "eval" most browsers will execute in the global scope.
// http://www.davidflanagan.com/2010/12/global-eval-in.html
var globalEval = eval;

// function parentURL(url) {
//     if (url.endsWith('/')) {
//         return url + '../'
//     } else {
//         return url + '/../'
//     }
// }



// loader.js:23 http://localhost:8080/node_modules/react-dom/cjs/react-dom.development.js/..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//../ fbjs/lib/containsNode Promise {<pending>}
// 23:10:02.884 loader.js:23 http://localhost:8080/node_modules/react-dom/cjs/react-dom.development.js/..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//..//../ fbjs/lib/invariant Promise {<pending>}


/**
 * 
 * @param {string} id 
 * @param {string} parent 
 */
function getParentModulePromise(id, parent) {
    var parentLink = BsGetPath('..', parent)
    if (parentLink === parent) {
        return falsePromise
    }
    return getPackageJsPromise(id, parentLink)
}
// In the beginning
// it is `resolveModule('./main.js', '')
// return the promise of link and text 

/**
 * 
 * @param {string} id 
 */
function getPackageName(id) {
    var index = id.indexOf('/')
    if (index === -1) {
        return id
    }
    return id.substring(0, index)
}
function getPackageJsPromise(id, parent) {
    var idNodeModulesPrefix = './node_modules/' + id
    var link = getPathWithJsSuffix(idNodeModulesPrefix, parent)
    if (parent.endsWith('node_modules/')) {
        // impossible that `node_modules/node_modules/xx/x
        // return falsePromise
        return getParentModulePromise(id, parent)
    }

    var packageJson = BsGetPath(`./node_modules/${getPackageName(id)}/package.json`, parent)

    return cachedFetch(packageJson)
        .then(
            function (text) {
                if (text !== false) {
                    // package indeed exist
                    return cachedFetch(link).then(function (text) {
                        if (text !== false) {
                            return { text, link }
                        } else if (!id.endsWith('.js')) {
                            var linkNew = getPathWithJsSuffix(idNodeModulesPrefix + `/index.js`, parent)
                            return cachedFetch(linkNew)
                                .then(function (text) {
                                    if (text !== false) {
                                        return { text, link: linkNew }
                                    } else {
                                        return getParentModulePromise(id, parent)
                                    }
                                })

                        } else {
                            return getParentModulePromise(id, parent)
                        }
                    })
                }
                else {
                    return getParentModulePromise(id, parent)
                }
            }
        )


}

/**
 * 
 * @param {string} id 
 * @param {string} parent 
 * can return Promise <boolean | object>, false means
 * this module can not be resolved
 */
function getModulePromise(id, parent) {
    var done = getIdLocation(id, parent)
    if (!done) {
        return visitIdLocation(id, parent, function () {
            if (id[0] != '.') { // package path
                return getPackageJsPromise(id, parent)
            } else { // relative path, one shot resolve            
                let link = getPathWithJsSuffix(id, parent)
                return cachedFetch(link).then(
                    function (text) {
                        if (text !== false) {
                            return { text, link }
                        } else {
                            throw new Error(` ${id} : ${parent} could not be resolved`)
                        }
                    }
                )
            }
        })
    } else {
        return done
    }
}


/**
 * 
 * @param {string} id 
 * @param {string} parent 
 * @returns {Promise<any>}
 */
function getAll(id, parent) {
    return getModulePromise(id, parent)
        .then(function (obj) {
            if (obj) {
                var deps = getDeps(obj.text)
                return Promise.all(deps.map(x => getAll(x, obj.link)))
            } else {
                throw new Error(`${id}@${parent} was not resolved successfully`)
            }
        })
};

/**
 * 
 * @param {string} text 
 * @param {string} parent 
 * @returns {Promise<any>}
 */
function getAllFromText(text, parent) {
    var deps = getDeps(text)
    return Promise.all(deps.map(x => getAll(x, parent)))
}

function loadSync(id, parent) {
    var baseOrModule = getIdLocationSync(id, parent)
    if (baseOrModule) {
        if (!baseOrModule.exports) {
            baseOrModule.exports = {}
            globalEval(`(function(require,exports,module){${baseOrModule.text}\n})//# sourceURL=${baseOrModule.link}`)(
                function require(id) {
                    return loadSync(id, baseOrModule.link);
                }, // require
                baseOrModule.exports = {}, // exports
                baseOrModule // module
            );
        }
        return baseOrModule.exports
    } else {
        throw new Error(`${id} : ${parent} could not be resolved`)
    }
}


function genEvalName() {
    return "eval-" + (("" + Math.random()).substr(2, 5))
}
/**
 * 
 * @param {string} text 
 * @param {string} link
 * In this case [text] evaluated result will not be cached
 */
function loadTextSync(text, link) {
    var baseOrModule = { exports: {}, text, link }
    globalEval(`(function(require,exports,module){${baseOrModule.text}\n})//# sourceURL=${baseOrModule.link}/${genEvalName()}.js`)(
        function require(id) {
            return loadSync(id, baseOrModule.link);
        }, // require
        baseOrModule.exports, // exports
        baseOrModule // module
    );
    return baseOrModule.exports
}

/**
 * 
 * @param {string} text 
 */
function BSloadText(text) {
    console.time("Loading")
    var parent = BsGetPath(".", document.baseURI)
    return getAllFromText(text, parent).then(function () {
        var result = loadTextSync(text, parent)
        console.timeEnd("Loading")
        return result
    })
};


function load(id, parent) {
    return getAll(id, parent).then(function () {
        return loadSync(id, parent)
    })

};


export function BSload(id) {
    var parent = BsGetPath(".", document.baseURI)
    return load(id, parent)
}

export var BSLoader = {
    loadText: BSloadText,
    load: BSload,
    SyncedIDLocations: SyncedIDLocations
};

window.BSLoader = BSLoader;

var main = document.querySelector('script[data-main]')
if (main) {
    BSload(main.dataset.main)
}
