"use strict";
/// <reference path="./mocha.d.ts"/>
/// <reference path="./node.d.ts"/>


import * as List from "../../stdlib/list"
import * as A from "../../stdlib/array"
import * as assert from "assert"
import * as OCaml_array from "../ocaml_array"

describe("Array", () => {
    it("init", () => {
            assert.deepEqual(A.init(5, x=>x),[0,1,2,3,4])
        })
        }
)

describe("List",()=>{
    it("init",()=>{
        assert.deepEqual( 5, List.length(A.to_list([0,1,2,3,4])))
    })
})


describe("OCaml_array",()=>{
    it("caml_array_concat", ()=>{
        assert.deepEqual([0,1,2,3,4,5],
            OCaml_array.caml_array_concat(A.to_list([[0,1,2],[3,4],[],[5]])))
    })
    it("caml_array_sub",()=>{
        assert.deepEqual([2,3],OCaml_array.caml_array_sub([0,1,2,3,4],2,2))
    })
    it("caml_make_vect",()=>{
            assert.deepEqual([0,0], OCaml_array.caml_make_vect(2,0))
        })
    it("caml_array_blit",()=>{
        let u = [100,0,0]

        let v = A.init(3,x=>x*2) // 0, 1, 2
        OCaml_array.caml_array_blit(v,1,u,1,2)
        assert.deepEqual([0,2,4], v)
        assert.deepEqual([100,2,4],u)
    })
})

