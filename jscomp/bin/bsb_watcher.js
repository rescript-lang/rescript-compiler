//@ts-check
var fs = require('fs')
var child_process = require('child_process')
var path = require('path')


/* This script is supposed to be running in project root directory
 * It matters since we need read .sourcedirs(location)
 * and its content are file/directories with regard to project root
 */
// for column one based error message
process.env.BS_VSCODE = 1
var bsb_exe  = "bsb.exe"
var bsb = path.join(__dirname, bsb_exe)
var bsconfig = "bsconfig.json"


var reasons_to_rebuild = [];

/**
 * watchers are held so that we close it later
 */
var watchers = [];


var is_building = false;
function releaseBuild(){
    is_building = false 
}
function acquireBuild(){
    if(is_building){
        return false
    }
    else {
        is_building = true
        return true
    }
}
var sourcedirs = path.join('lib', 'bs', '.sourcedirs')
function watch_build(watch_files) {
    // close and remove all unused watchers
    watchers = watchers.filter(function(watcher){
        if(watcher.dir === bsconfig){
            return true;
        } else if(watch_files.indexOf(watcher.dir) < 0){
            console.log(watcher.dir,'is no longer watched');
            watcher.watcher.close();
            return false
        }  else {
            return true;
        }
    })

    // adding new watchers
    for (var i = 0; i < watch_files.length; ++i) {
        var dir = watch_files[i]
        if (!watchers.find(function (watcher) { return watcher.dir === dir })) {
            console.log('watching dir', dir, 'now')
            var watcher = fs.watch(dir,on_change);
            watchers.push({ dir: dir, watcher: watcher })
        } else {
            // console.log(dir, 'already watched')
        }
    }
};


/**
 * 
 * @param {string} eventType 
 * @param {string} fileName 
 */
function validEvent(eventType,fileName){
    // Return true if filename is nil, filename is only provided on Linux, macOS, Windows, and AIX.
    // On other systems, we just have to assume that any change is valid.
    // This could cause problems if source builds (generating js files in the same directory) are supported. 
    if (!fileName)
        return true;

    return  !(fileName === '.merlin' ||  fileName.endsWith('.js'))
}
/**
 * @return {boolean}
 */
function needRebuild(){
    return reasons_to_rebuild.length != 0
}
function build_finished_callback() {
    console.log(">>>> Finish compiling")
    releaseBuild()
    if (needRebuild()) {
        build()
    } else {
        var files = getWatchFiles(sourcedirs);
        watch_build(files)
    }
}
function build() {
    if (acquireBuild()) {
        console.log(">>>> Start compiling");
        console.log("Rebuilding since", reasons_to_rebuild);
        reasons_to_rebuild = [];
        child_process
          .spawn(bsb, [],{stdio: 'inherit'})
          .on('exit', build_finished_callback);
    }
}
/**
 * 
 * @param {string} event 
 * @param {string} reason 
 */
function on_change(event, reason) {    
    if(validEvent(event,reason)){
        console.log("Event", event);
        reasons_to_rebuild.push([event, reason])
        if(needRebuild()){
            build()
        }    
    }    
    
}
function getWatchFiles(file) {
    if (fs.existsSync(file)){
        return fs.readFileSync(file, 'utf8').split('\n').filter(function(x){return x})
    } else {
        return []
    }

}


// Initialization

watchers.push({watcher : fs.watch(bsconfig,on_change) , dir : bsconfig});
build();
