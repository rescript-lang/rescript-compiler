
//  Copyright (c) 2015 Bloomberg LP. All rights reserved. 
// Hongbo Zhang (hzhang295@bloomberg.net)              



if(typeof Array.prototype.fill !== 'function'){
    Array.prototype.fill =  function (value){
        var len = this.length;
        for (var i = 0; i < len; i++) {
            this[i] = init;
        }
    }
}


