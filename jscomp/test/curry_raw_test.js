var assert = require("assert")
var printf = require("./../stdlib/printf")
var curry = require("../runtime/caml_curry")
var test_formatter = require("./test_formatter")

// Fmt can be defined other ocaml module : )



describe("curry", function(){
    it("printf", function(){
        assert.deepEqual(
            curry.app3(printf.sprintf,
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
            curry.app1(curry.app2(printf.sprintf,
                test_formatter.f(),
                32),
                "ss"
            ),
            "32ss"
        )
    });
    it("printf3",function(){
                assert.deepEqual(
            curry.app1(curry.app1(curry.app1(printf.sprintf,
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