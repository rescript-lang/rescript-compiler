

var wsReloader;
var LAST_SUCCESS_BUILD_STAMP = (localStorage.getItem('LAST_SUCCESS_BUILD_STAMP') || 0)
var WS_PORT = 9999; // configurable

function setUpWebScoket() {
    if (wsReloader == null || wsReloader.readyState !== 1) {
        try {
            wsReloader = new WebSocket(`ws://localhost:${WS_PORT}`)
            wsReloader.onmessage = (msg) => {
                var newData = JSON.parse(msg.data).LAST_SUCCESS_BUILD_STAMP
                if (newData > LAST_SUCCESS_BUILD_STAMP) {
                    LAST_SUCCESS_BUILD_STAMP = newData
                    localStorage.setItem('LAST_SUCCESS_BUILD_STAMP', LAST_SUCCESS_BUILD_STAMP)
                    location.reload(true)
                }

            }
        } catch (exn) {
            console.error("web socket failed connect")
        }
    }
};

setUpWebScoket();
setInterval(setUpWebScoket, 2000);