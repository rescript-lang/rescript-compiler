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

var is_building = false;
var reasons_to_rebuild = [];

/**
 * watchers are held so that we close it later
 */
var watchers = [];

function get_building_status() {
    return is_building;
}

function mark_is_building() {
    is_building = true;
    console.log("Rebuilding since", reasons_to_rebuild);
    reasons_to_rebuild = [];
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
    /*
    watchers = watch_files.map(function (dir) {
        var watcher = fs.watch(dir)
        return  {dir : dir , watcher : watcher}
    })
    watchers.forEach(function (watcher) {
        watcher.watcher.on('change', on_change)
    })
    return watchers;
    */
};

function build_finished_callback() {
    console.log(">>>> Finish compiling")
    is_building = false;
    if (reasons_to_rebuild.length != 0) {

        build()
    } else {
        var files = getWatchFiles(sourcedirs);
        watch_build(files)
    }
}
function build() {
    if (get_building_status()) {

    }
    else {
        console.log(">>>> Start compiling");
        mark_is_building()
        child_process
          .spawn(bsb, {stdio: 'inherit'})
          .on('exit', build_finished_callback);
    }
}
function on_change(event, reason) {
    console.log("Event", event);
    reasons_to_rebuild.push(event + ' : '+ reason)
    build()
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
