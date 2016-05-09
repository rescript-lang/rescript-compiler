var assert = require("assert")
var printf = require("./../../printf")
var curry = require("../../curry")
var test_formatter = require("../test_formatter")

// Fmt can be defined other ocaml module : )



describe("curry", function(){
    it("printf", function(){
        assert.deepEqual(
            curry._3(printf.sprintf,
                test_formatter.f(),
                32,
                "ss"
            ),
            "32ss"
        )
    }
    );
    it("printf2",function(){
                assert.deepEqual(
            curry._1(curry._2(printf.sprintf,
                test_formatter.f(),
                32),
                "ss"
            ),
            "32ss"
        )
    });
    it("printf3",function(){
                assert.deepEqual(
            curry._1(curry._1(curry._1(printf.sprintf,
                test_formatter.f()),
                32),
                "ss"
            ),
            "32ss"
        )
    });
    it("printf4",function(){
        assert.deepEqual(
            curry.app(printf.sprintf,
                [test_formatter.f(),
                    32,
                    "ss",
                    ]
        ),"32ss"
    )}
 )})