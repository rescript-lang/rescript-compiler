#!/bin/sh
set -e
CLOSURE=./compiler.jar
# CLOSURE=../closure-compiler/target/closure-compiler-1.0-SNAPSHOT.jar
# LANGUAGE_IN=ES5_STRICT
LANGUAGE_IN=ES5
# LANGUAGE_IN=ES6
COMPILATION_LEVEL=ADVANCED
# COMPILATION_LEVEL=SIMPLE
FORMATTING=PRETTY_PRINT
FORMATTING=PRINT_INPUT_DELIMITER
# java -jar $CLOSURE   --compilation_level ADVANCED --language_in $LANGUAGE_IN  --language_out ES5_STRICT --process_common_js_modules --common_js_entry_module node_3.js *.js > c.js
# --formatting PRETTY_PRINT --formatting $FORMATTING

# /opt/bb/bin/java -server -XX:+TieredCompilation -jar /bb/mbigc/mbig2899/bgit/jars/compiler.jar   --compilation_level $COMPILATION_LEVEL --language_in $LANGUAGE_IN  --language_out ES5_STRICT --process_common_js_modules --common_js_entry_module test_map_find.js test_map_find.js list.js caml_primitive.js caml_exceptions.js map.js caml_array.js caml_io.js caml_utils.js pervasives.js camlinternalFormatBasics.js > c.js

# /opt/bb/bin/java -server -XX:+TieredCompilation -jar /bb/mbigc/mbig2899/bgit/jars/compiler.jar   --compilation_level $COMPILATION_LEVEL --language_in $LANGUAGE_IN  --language_out ES5_STRICT --process_common_js_modules --common_js_entry_module test_inline_map.js test_inline_map.js list.js caml_primitive.js caml_exceptions.js map.js caml_array.js caml_io.js caml_utils.js pervasives.js camlinternalFormatBasics.js > c.compile.js

# /opt/bb/bin/java -server -XX:+TieredCompilation -jar /bb/mbigc/mbig2899/bgit/jars/compiler.jar   --compilation_level $COMPILATION_LEVEL --language_in $LANGUAGE_IN  --language_out ES5_STRICT --process_common_js_modules --common_js_entry_module test_inline_map2.js test_inline_map2.js list.js caml_primitive.js caml_exceptions.js map.js caml_array.js caml_io.js caml_utils.js pervasives.js camlinternalFormatBasics.js > d.compile.js

/opt/bb/bin/java -server -XX:+TieredCompilation -jar /bb/mbigc/mbig2899/bgit/jars/compiler.jar   --compilation_level $COMPILATION_LEVEL --language_in $LANGUAGE_IN  --language_out ES5_STRICT --process_common_js_modules --common_js_entry_module test_simple_lexer.js test_simple_lexer.js list.js caml_*.js lexing.js string.js bytes.js sys.js char.js pervasives.js camlinternalFormatBasics.js > c.compile.js
# node c.js
# cat c.compile.js
# java -jar --process_common_js 