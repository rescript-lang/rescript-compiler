// This is our simple, robust watcher. It hooks into the BuckleScript build
// system to listen for build events.
// See package.json's `start` script and `./node_modules/.bin/bsb --help`

// Btw, if you change this file and reload the page, your browser cache
// _might_ not pick up the new version. If you're in Chrome, do Force Reload.

var websocketReloader;
var LAST_SUCCESS_BUILD_STAMP = localStorage.getItem('LAST_SUCCESS_BUILD_STAMP') || 0;
// package.json's `start` script's `bsb -ws _` means it'll pipe build events
// through a websocket connection to a default port of 9999. This is
// configurable, e.g. `-ws 5000`
var webSocketPort = 9999;

function setUpWebSocket() {
  if (websocketReloader == null || websocketReloader.readyState !== 1) {
    try {
      websocketReloader = new WebSocket(`ws://${window.location.hostname}:${webSocketPort}`);
      websocketReloader.onmessage = (message) => {
        var newData = JSON.parse(message.data).LAST_SUCCESS_BUILD_STAMP;
        if (newData > LAST_SUCCESS_BUILD_STAMP) {
          LAST_SUCCESS_BUILD_STAMP = newData;
          localStorage.setItem('LAST_SUCCESS_BUILD_STAMP', LAST_SUCCESS_BUILD_STAMP);
          // Refresh the page! This will naturally re-run everything,
          // including our moduleserve which will re-resolve all the modules.
          // No stable build!
          location.reload(true);
        }

      }
    } catch (exn) {
      console.error("The watcher tried to connect to web socket, but failed. Here's the message:");
      console.error(exn);
    }
  }
};

setUpWebSocket();
setInterval(setUpWebSocket, 2000);
