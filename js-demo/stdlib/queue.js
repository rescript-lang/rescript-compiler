// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
define(["../runtime/caml_exceptions","../runtime/caml_primitive"],
  function(Caml_exceptions,Caml_primitive){
    'use strict';
    var Empty = [
      248,
      "Queue.Empty",
      ++ Caml_exceptions.caml_oo_last_id
    ];
    
    function create() {
      return [
              /* record */0,
              0,
              /* None */0
            ];
    }
    
    function clear(q) {
      q[1] = 0;
      q[2] = /* None */0;
      return /* () */0;
    }
    
    function add(x, q) {
      if (q[1]) {
        var tail = q[2];
        var head = tail[2];
        var cell = [
          /* record */0,
          x,
          head
        ];
        ++ q[1];
        tail[2] = cell;
        q[2] = cell;
        return /* () */0;
      }
      else {
        var cell$1 = [];
        Caml_primitive.caml_update_dummy(cell$1, [
              /* record */0,
              x,
              cell$1
            ]);
        q[1] = 1;
        q[2] = cell$1;
        return /* () */0;
      }
    }
    
    function peek(q) {
      if (q[1]) {
        return q[2][2][1];
      }
      else {
        throw Empty;
      }
    }
    
    function take(q) {
      if (!q[1]) {
        throw Empty;
      }
      -- q[1];
      var tail = q[2];
      var head = tail[2];
      if (head === tail) {
        q[2] = /* None */0;
      }
      else {
        tail[2] = head[2];
      }
      return head[1];
    }
    
    function copy(q) {
      if (q[1]) {
        var tail = q[2];
        var tail$prime = [];
        Caml_primitive.caml_update_dummy(tail$prime, [
              /* record */0,
              tail[1],
              tail$prime
            ]);
        var copy$1 = function (_prev, _cell) {
          while(true) {
            var cell = _cell;
            var prev = _prev;
            if (cell !== tail) {
              var res = [
                /* record */0,
                cell[1],
                tail$prime
              ];
              prev[2] = res;
              _cell = cell[2];
              _prev = res;
            }
            else {
              return 0;
            }
          };
        };
        copy$1(tail$prime, tail[2]);
        return [
                /* record */0,
                q[1],
                tail$prime
              ];
      }
      else {
        return [
                /* record */0,
                0,
                /* None */0
              ];
      }
    }
    
    function is_empty(q) {
      return +(q[1] === 0);
    }
    
    function length(q) {
      return q[1];
    }
    
    function iter(f, q) {
      if (q[1] > 0) {
        var tail = q[2];
        var _cell = tail[2];
        while(true) {
          var cell = _cell;
          f(cell[1]);
          if (cell !== tail) {
            _cell = cell[2];
          }
          else {
            return 0;
          }
        };
      }
      else {
        return 0;
      }
    }
    
    function fold(f, accu, q) {
      if (q[1]) {
        var tail = q[2];
        var _accu = accu;
        var _cell = tail[2];
        while(true) {
          var cell = _cell;
          var accu$1 = _accu;
          var accu$2 = f(accu$1, cell[1]);
          if (cell === tail) {
            return accu$2;
          }
          else {
            _cell = cell[2];
            _accu = accu$2;
          }
        };
      }
      else {
        return accu;
      }
    }
    
    function transfer(q1, q2) {
      var length1 = q1[1];
      if (length1 > 0) {
        var tail1 = q1[2];
        clear(q1);
        if (q2[1] > 0) {
          var tail2 = q2[2];
          var head1 = tail1[2];
          var head2 = tail2[2];
          tail1[2] = head2;
          tail2[2] = head1;
        }
        q2[1] += length1;
        q2[2] = tail1;
        return /* () */0;
      }
      else {
        return 0;
      }
    }
    
    var push = add;
    
    var pop = take;
    
    var top = peek;
    return {
      Empty : Empty, 
      create : create, 
      add : add, 
      push : push, 
      take : take, 
      pop : pop, 
      peek : peek, 
      top : top, 
      clear : clear, 
      copy : copy, 
      is_empty : is_empty, 
      length : length, 
      iter : iter, 
      fold : fold, 
      transfer : transfer
    }
  })
/* No side effect */
