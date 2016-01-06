// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Jérôme Vouillon, Hugo Heuzard, Andy Ray
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

//  Copyright (c) 2015 Bloomberg LP. All rights reserved. 
// Hongbo Zhang (hzhang295@bloomberg.net)              

"use strict";

import {caml_failwith} from './caml_exceptions'

/**
 * caml_lex_array("abcd")
 * [25185, 25699]
 * @param s
 * @returns {any[]}
 */
function caml_lex_array(s : string) {
    var l = s.length / 2;
    var a = new Array(l);
    for (var i = 0; i < l; i++)
        a[i] = (s.charCodeAt(2 * i) | (s.charCodeAt(2 * i + 1) << 8)) << 16 >> 16;
    return a;
}

/**
 * external c_engine  : lex_tables -> int -> lexbuf -> int
 * lexing.ml
 * type lex_tables = {
 *   lex_base : string;
 *   lex_backtrk : string;
 *   lex_default : string;
 *   lex_trans : string;
 *   lex_check : string;
 *   lex_base_code : string;
 *   lex_backtrk_code : string;
 *   lex_default_code : string;
 *   lex_trans_code : string;
 *   lex_check_code : string;
 *   lex_code : string;
 * }
 *
 * type lexbuf = {
 *   refill_buff : lexbuf -> unit ;
 *   mutable lex_buffer : bytes;
 *   mutable lex_buffer_len : int;
 *   mutable lex_abs_pos : int;
 *   mutable lex_start_pos : int;
 *   mutable lex_curr_pos : int;
 *   mutable lex_last_pos : int;
 *   mutable lex_last_action : int;
 *   mutable lex_eof_reached : bool;
 *   mutable lex_mem : int array;
 *   mutable lex_start_p : position;
 *   mutable lex_curr_p;
 * }
 * @param tbl
 * @param start_state
 * @param lexbuf
 * @returns {any}
 */
function caml_lex_engine(tbl, start_state : number, lexbuf) : number {
    var lex_buffer = 2;
    var lex_buffer_len = 3;
    var lex_start_pos = 5;
    var lex_curr_pos = 6;
    var lex_last_pos = 7;
    var lex_last_action = 8;
    var lex_eof_reached = 9;
    var lex_base = 1;
    var lex_backtrk = 2;
    var lex_default = 3;
    var lex_trans = 4;
    var lex_check = 5;

    if (!tbl.lex_default) {
        tbl.lex_base =    caml_lex_array (tbl[lex_base]);
        tbl.lex_backtrk = caml_lex_array (tbl[lex_backtrk]);
        tbl.lex_check =   caml_lex_array (tbl[lex_check]);
        tbl.lex_trans =   caml_lex_array (tbl[lex_trans]);
        tbl.lex_default = caml_lex_array (tbl[lex_default]);
    }

    var c : number;
    var state = start_state;

    //var buffer = bytes_of_string(lexbuf[lex_buffer]);
    var buffer = lexbuf[lex_buffer];
    if (state >= 0) {
        /* First entry */
        lexbuf[lex_last_pos] = lexbuf[lex_start_pos] = lexbuf[lex_curr_pos];
        lexbuf[lex_last_action] = -1;
    } else {
        /* Reentry after refill */
        state = -state - 1;
    }
    for(;;) {
        /* Lookup base address or action number for current state */
        var base = tbl.lex_base[state];
        if (base < 0) return -base-1;
        /* See if it's a backtrack point */
        var backtrk = tbl.lex_backtrk[state];
        if (backtrk >= 0) {
            lexbuf[lex_last_pos] = lexbuf[lex_curr_pos];
            lexbuf[lex_last_action] = backtrk;
        }
        /* See if we need a refill */
        if (lexbuf[lex_curr_pos] >= lexbuf[lex_buffer_len]){
            if (lexbuf[lex_eof_reached] === 0)
                return -state - 1;
            else
                c = 256;
        }else{
            /* Read next input char */
            c = buffer[lexbuf[lex_curr_pos]];
            lexbuf[lex_curr_pos] ++;
        }
        /* Determine next state */
        if (tbl.lex_check[base + c] === state) {
            state = tbl.lex_trans[base + c];
        }
        else {
            state = tbl.lex_default[state];
        }
        /* If no transition on this char, return to last backtrack point */
        if (state < 0) {
            lexbuf[lex_curr_pos] = lexbuf[lex_last_pos];
            if (lexbuf[lex_last_action] == -1)
                caml_failwith("lexing: empty token");
            else
                return lexbuf[lex_last_action];
        }else{
            /* Erase the EOF condition only if the EOF pseudo-character was
             consumed by the automaton (i.e. there was no backtrack above)
             */
            if (c == 256) lexbuf[lex_eof_reached] = 0;
        }
    }
}

/***********************************************/
/* New lexer engine, with memory of positions  */
/***********************************************/


function caml_lex_run_mem(s, i, mem, curr_pos) {
    for (;;) {
        var dst = s.charCodeAt(i); i++;
        if (dst == 0xff) return;
        var src = s.charCodeAt(i); i++;
        if (src == 0xff)
            mem [dst + 1] = curr_pos;
        else
            mem [dst + 1] = mem [src + 1];
    }
}

function caml_lex_run_tag(s, i, mem) {
    for (;;) {
        var dst = s.charCodeAt(i); i++;
        if (dst == 0xff) return ;
        var src = s.charCodeAt(i); i++;
        if (src == 0xff)
            mem [dst + 1] = -1;
        else
            mem [dst + 1] = mem [src + 1];
    }
}
/**
 * external c_new_engine : lex_tables -> int -> lexbuf -> int = "caml_new_lex_engine"
 * @param tbl
 * @param start_state
 * @param lexbuf
 * @returns {any}
 */
function caml_new_lex_engine(tbl, start_state, lexbuf) {
    var lex_buffer = 2;
    var lex_buffer_len = 3;
    var lex_start_pos = 5;
    var lex_curr_pos = 6;
    var lex_last_pos = 7;
    var lex_last_action = 8;
    var lex_eof_reached = 9;
    var lex_mem = 10;
    var lex_base = 1;
    var lex_backtrk = 2;
    var lex_default = 3;
    var lex_trans = 4;
    var lex_check = 5;
    var lex_base_code = 6;
    var lex_backtrk_code = 7;
    var lex_default_code = 8;
    var lex_trans_code = 9;
    var lex_check_code = 10;
    var lex_code = 11;

    if (!tbl.lex_default) {
        tbl.lex_base =    caml_lex_array (tbl[lex_base]);
        tbl.lex_backtrk = caml_lex_array (tbl[lex_backtrk]);
        tbl.lex_check =   caml_lex_array (tbl[lex_check]);
        tbl.lex_trans =   caml_lex_array (tbl[lex_trans]);
        tbl.lex_default = caml_lex_array (tbl[lex_default]);
    }
    if (!tbl.lex_default_code) {
        tbl.lex_base_code =    caml_lex_array (tbl[lex_base_code]);
        tbl.lex_backtrk_code = caml_lex_array (tbl[lex_backtrk_code]);
        tbl.lex_check_code =   caml_lex_array (tbl[lex_check_code]);
        tbl.lex_trans_code =   caml_lex_array (tbl[lex_trans_code]);
        tbl.lex_default_code = caml_lex_array (tbl[lex_default_code]);
    }
    if (tbl.lex_code == null) {
        //tbl.lex_code = caml_bytes_of_string(tbl[lex_code]);
        tbl.lex_code = (tbl[lex_code]);
    }

    var c, state = start_state;

    //var buffer = caml_bytes_of_string(lexbuf[lex_buffer]);
    var buffer = lexbuf[lex_buffer];

    if (state >= 0) {
        /* First entry */
        lexbuf[lex_last_pos] = lexbuf[lex_start_pos] = lexbuf[lex_curr_pos];
        lexbuf[lex_last_action] = -1;
    } else {
        /* Reentry after refill */
        state = -state - 1;
    }
    for(;;) {
        /* Lookup base address or action number for current state */
        var base = tbl.lex_base[state];
        if (base < 0) {
            var pc_off = tbl.lex_base_code[state];
            caml_lex_run_tag(tbl.lex_code, pc_off, lexbuf[lex_mem]);
            return -base-1;
        }
        /* See if it's a backtrack point */
        var backtrk = tbl.lex_backtrk[state];
        if (backtrk >= 0) {
            var pc_off = tbl.lex_backtrk_code[state];
            caml_lex_run_tag(tbl.lex_code, pc_off, lexbuf[lex_mem]);
            lexbuf[lex_last_pos] = lexbuf[lex_curr_pos];
            lexbuf[lex_last_action] = backtrk;
        }
        /* See if we need a refill */
        if (lexbuf[lex_curr_pos] >= lexbuf[lex_buffer_len]){
            if (lexbuf[lex_eof_reached] == 0)
                return -state - 1;
            else
                c = 256;
        }else{
            /* Read next input char */
            c = buffer[lexbuf[lex_curr_pos]];
            lexbuf[lex_curr_pos] ++;
        }
        /* Determine next state */
        var pstate = state ;
        if (tbl.lex_check[base + c] == state)
            state = tbl.lex_trans[base + c];
        else
            state = tbl.lex_default[state];
        /* If no transition on this char, return to last backtrack point */
        if (state < 0) {
            lexbuf[lex_curr_pos] = lexbuf[lex_last_pos];
            if (lexbuf[lex_last_action] == -1)
                caml_failwith("lexing: empty token");
            else
                return lexbuf[lex_last_action];
        }else{
            /* If some transition, get and perform memory moves */
            var base_code = tbl.lex_base_code[pstate], pc_off;
            if (tbl.lex_check_code[base_code + c] == pstate)
                pc_off = tbl.lex_trans_code[base_code + c];
            else
                pc_off = tbl.lex_default_code[pstate];
            if (pc_off > 0)
                caml_lex_run_mem
                (tbl.lex_code, pc_off, lexbuf[lex_mem], lexbuf[lex_curr_pos]);
            /* Erase the EOF condition only if the EOF pseudo-character was
             consumed by the automaton (i.e. there was no backtrack above)
             */
            if (c == 256) lexbuf[lex_eof_reached] = 0;
        }
    }
}

/**
 * Note that TS enum is not friendly to Closure compiler
 * @enum{number}
 */
var Automata = {
    START : 0,
    LOOP : 6,
    TOKEN_READ : 1,
    TEST_SHIFT : 7,
    ERROR_DETECTED : 5,
    SHIFT : 8,
    SHIFT_RECOVER : 9,
    STACK_GROWN_1 : 2,
    REDUCE : 10,
    STACK_GROWN_2 : 3,
    SEMANTIC_ACTION_COMPUTED : 4
}
/**
 * @enum{number}
 */
var Result = {
    READ_TOKEN : 0,
    RAISE_PARSE_ERROR : 1,
    GROW_STACKS_1 : 2,
    GROW_STACKS_2 : 3,
    COMPUTE_SEMANTIC_ACTION : 4,
    CALL_ERROR_FUNCTION : 5
}

var PARSER_TRACE  = false;

/**
 * external parse_engine : parse_tables -> parser_env -> parser_input -> Obj.t -> parser_output
 * parsing.ml
 *
 * type parse_tables = {
 *   actions : (parser_env -> Obj.t) array
 *   transl_const : int array;
 *   transl_block : int array;
 *   lhs : string;
 *   len : string;
 *   defred : string;
 *   dgoto : string;
 *   sindex : string;
 *   rindex : string;
 *   gindex : string;
 *   tablesize : int;
 *   table : string;
 *   check : string;
 *   error_function : string -> unit;
 *   names_const : string;
 *   names_block : string
 * }
 *
 * type parser_env =
 * { mutable s_stack : int array;        (* States *)
 *  mutable v_stack : Obj.t array;      (* Semantic attributes *)
 *  mutable symb_start_stack : position array; (* Start positions *)
 *  mutable symb_end_stack : position array;   (* End positions *)
 *  mutable stacksize : int;            (* Size of the stacks *)
 *  mutable stackbase : int;            (* Base sp for current parse *)
 *  mutable curr_char : int;            (* Last token read *)
 *  mutable lval : Obj.t;               (* Its semantic attribute *)
 *  mutable symb_start : position;      (* Start pos. of the current symbol*)
 *  mutable symb_end : position;        (* End pos. of the current symbol *)
 *  mutable asp : int;                  (* The stack pointer for attributes *)
 *  mutable rule_len : int;             (* Number of rhs items in the rule *)
 *  mutable rule_number : int;          (* Rule number to reduce by *)
 *  mutable sp : int;                   (* Saved sp for parse_engine *)
 *  mutable state : int;                (* Saved state for parse_engine *)
 *  mutable errflag : int }             (* Saved error flag for parse_engine *)
 *
 *  type parser_input =
 *   | Start
 *   | Token_read
 *   | Stacks_grown_1
 *   | Stacks_grown_2
 *   | Semantic_action_computed
 *   | Error_detected

 * @param tables
 * @param env
 * @param cmd
 * @param arg
 * @returns {number}
 */
function caml_parse_engine(tables, env, cmd, arg)
{
    var ERRCODE = 256;

    //var START = 0;
    //var TOKEN_READ = 1;
    //var STACKS_GROWN_1 = 2;
    //var STACKS_GROWN_2 = 3;
    //var SEMANTIC_ACTION_COMPUTED = 4;
    //var ERROR_DETECTED = 5;
    //var loop = 6;
    //var testshift = 7;
    //var shift = 8;
    //var shift_recover = 9;
    //var reduce = 10;



    var env_s_stack = 1; // array
    var env_v_stack = 2; // array
    var env_symb_start_stack = 3; // array
    var env_symb_end_stack = 4; // array
    var env_stacksize = 5;
    var env_stackbase = 6;
    var env_curr_char = 7;
    var env_lval = 8;
    var env_symb_start = 9;
    var env_symb_end = 10;
    var env_asp = 11;
    var env_rule_len = 12;
    var env_rule_number = 13;
    var env_sp = 14;
    var env_state = 15;
    var env_errflag = 16;

    // var _tbl_actions = 1;
    var tbl_transl_const = 2; // array
    var tbl_transl_block = 3; // array
    var tbl_lhs = 4;
    var tbl_len = 5;
    var tbl_defred = 6;
    var tbl_dgoto = 7;
    var tbl_sindex = 8;
    var tbl_rindex = 9;
    var tbl_gindex = 10;
    var tbl_tablesize = 11;
    var tbl_table = 12;
    var tbl_check = 13;
    // var _tbl_error_function = 14;
    // var _tbl_names_const = 15;
    // var _tbl_names_block = 16;

    if (!tables.dgoto) {
        tables.defred = caml_lex_array (tables[tbl_defred]);
        tables.sindex = caml_lex_array (tables[tbl_sindex]);
        tables.check  = caml_lex_array (tables[tbl_check]);
        tables.rindex = caml_lex_array (tables[tbl_rindex]);
        tables.table  = caml_lex_array (tables[tbl_table]);
        tables.len    = caml_lex_array (tables[tbl_len]);
        tables.lhs    = caml_lex_array (tables[tbl_lhs]);
        tables.gindex = caml_lex_array (tables[tbl_gindex]);
        tables.dgoto  = caml_lex_array (tables[tbl_dgoto]);
    }

    var res;
    var n, n1, n2, state1;

    // RESTORE
    var sp = env[env_sp];
    var state = env[env_state];
    var errflag = env[env_errflag];

    exit:for (;;) {
        //console.error("State", Automata[cmd]);

        switch(cmd) {
            case Automata.START://START:
                state = 0;
                errflag = 0;
            // Fall through

            case Automata.LOOP://loop:
                n = tables.defred[state];
                if (n != 0) { cmd = Automata.REDUCE; break; }
                if (env[env_curr_char] >= 0) { cmd = Automata.TEST_SHIFT; break; }
                res = Result.READ_TOKEN;
                break exit;
            /* The ML code calls the lexer and updates */
            /* symb_start and symb_end */
            case Automata.TOKEN_READ://TOKEN_READ:
                if (arg instanceof Array) {
                    env[env_curr_char] = tables[tbl_transl_block][arg[0] /* + 1 */];
                    env[env_lval] = arg[1];
                } else {
                    env[env_curr_char] = tables[tbl_transl_const][arg /* + 1 */];
                    env[env_lval] = 0;
                }
                if(PARSER_TRACE){
                    console.error("State %d, read token", state, arg)
                }
            // Fall through

            case Automata.TEST_SHIFT://testshift:
                n1 = tables.sindex[state];
                n2 = n1 + env[env_curr_char];
                if (n1 != 0 && n2 >= 0 && n2 <= tables[tbl_tablesize] &&
                    tables.check[n2] == env[env_curr_char]) {
                    cmd = Automata.SHIFT; break;
                }
                n1 = tables.rindex[state];
                n2 = n1 + env[env_curr_char];
                if (n1 != 0 && n2 >= 0 && n2 <= tables[tbl_tablesize] &&
                    tables.check[n2] == env[env_curr_char]) {
                    n = tables.table[n2];
                    cmd = Automata.REDUCE; break;
                }
                if (errflag <= 0) {

                    res = Result.CALL_ERROR_FUNCTION;
                    break exit;
                }
            // Fall through
            /* The ML code calls the error function */
            case Automata.ERROR_DETECTED://ERROR_DETECTED:
                if (errflag < 3) {
                    errflag = 3;
                    for (;;) {
                        state1 = env[env_s_stack][sp /* + 1*/];
                        n1 = tables.sindex[state1];
                        n2 = n1 + ERRCODE;
                        if (n1 != 0 && n2 >= 0 && n2 <= tables[tbl_tablesize] &&
                            tables.check[n2] == ERRCODE) {
                            cmd = Automata.SHIFT_RECOVER; break;
                        } else {
                            if (sp <= env[env_stackbase]) return Result.RAISE_PARSE_ERROR;
                            /* The ML code raises Parse_error */
                            sp--;
                        }
                    }
                } else {
                    if (env[env_curr_char] == 0) return Result.RAISE_PARSE_ERROR;
                    /* The ML code raises Parse_error */
                    env[env_curr_char] = -1;
                    cmd = Automata.LOOP; break;
                }
            // Fall through
            case Automata.SHIFT://shift:
                env[env_curr_char] = -1;
                if (errflag > 0) errflag--;
            // Fall through
            case Automata.SHIFT_RECOVER://shift_recover:
                if(PARSER_TRACE) {
                    console.error("State %d: shift to state %d", state, tables.table[n2])
                }
                state = tables.table[n2];
                sp++;
                if (sp >= env[env_stacksize]) {
                    res = Result.GROW_STACKS_1;
                    break exit;
                }
            // Fall through
            /* The ML code resizes the stacks */
            case Automata.STACK_GROWN_1:
                env[env_s_stack][sp /* + 1 */] = state;
                env[env_v_stack][sp /* + 1 */] = env[env_lval];
                env[env_symb_start_stack][sp /* + 1 */] = env[env_symb_start];
                env[env_symb_end_stack][sp /* + 1 */] = env[env_symb_end];
                cmd = Automata.LOOP;
                break;

            case Automata.REDUCE:
                if(PARSER_TRACE) {
                    console.error("State %d : reduce by rule %d", state, n)
                }
                var m = tables.len[n];
                env[env_asp] = sp;
                env[env_rule_number] = n;
                env[env_rule_len] = m;
                sp = sp - m + 1;
                m = tables.lhs[n];
                state1 = env[env_s_stack][sp - 1]; //

                n1 = tables.gindex[m];
                n2 = n1 + state1;
                if (n1 != 0 && n2 >= 0 && n2 <= tables[tbl_tablesize] &&
                    tables.check[n2] == state1)
                    state = tables.table[n2];
                else
                    state = tables.dgoto[m];
                if (sp >= env[env_stacksize]) {
                    res = Result.GROW_STACKS_2;
                    break exit;
                }
            // Fall through
            /* The ML code resizes the stacks */
            case Automata.STACK_GROWN_2://STACKS_GROWN_2:
                res = Result.COMPUTE_SEMANTIC_ACTION;
                break exit;
            /* The ML code calls the semantic action */
            case Automata.SEMANTIC_ACTION_COMPUTED://SEMANTIC_ACTION_COMPUTED:
                env[env_s_stack][sp /* + 1 */] = state;
                env[env_v_stack][sp /* + 1*/] = arg;
                var asp = env[env_asp];
                env[env_symb_end_stack][sp /* + 1*/] = env[env_symb_end_stack][asp /* + 1*/];
                if (sp > asp) {
                    /* This is an epsilon production. Take symb_start equal to symb_end. */
                    env[env_symb_start_stack][sp /* + 1*/] = env[env_symb_end_stack][asp /*+ 1*/];
                }
                cmd = Automata.LOOP; break;
            /* Should not happen */
            default:
                return Result.RAISE_PARSE_ERROR;
        }
    }
    // SAVE
    env[env_sp] = sp;
    env[env_state] = state;
    env[env_errflag] = errflag;

    return res;
}



/**
 * external set_trace: bool -> bool = "caml_set_parser_trace"
 * parsing.ml
 * @param {boolean}
 * @returns {boolean}
 */
function caml_set_parser_trace(v : boolean) {
    var old = PARSER_TRACE;
    PARSER_TRACE = v;
    return old;
}

export {
  caml_lex_engine,
  caml_new_lex_engine,
  caml_parse_engine,
  caml_set_parser_trace
}