


## Difference from Lambda

`Levent` is removed, it hurts pattern match peep-hole

Based on `ocaml/bytecomp/bytegen.ml`

```ocaml
| Lifused (_, exp) ->
    comp_expr env exp sz cont
```

`ocaml/bytecomp/simplif.ml`
```
  | Lifused(v, l) ->
      if count_var v > 0 then count bv l
```      

`ocaml/bytecomp/simplif.ml`
```
  | Lifused(v, l) ->
      if count_var v > 0 then simplif l else lambda_unit
```

`ocaml/bytecomp/translclass.ml`
```
List.fold_right
(fun (id, expr) rem ->
 lsequence (Lifused (id, set_inst_var obj id expr)) rem)
```

If we don't shake away `Lifused`, the diff would be simliar to 
```
diff --git a/jscomp/test/class3_test.js b/jscomp/test/class3_test.js
index c5fc8ac..450fcb5 100644
--- a/jscomp/test/class3_test.js
+++ b/jscomp/test/class3_test.js
@@ -83,6 +83,7 @@ function eq(loc, x, y) {
 }
 
 function point_init($$class) {
+  var x_init = CamlinternalOO.new_variable($$class, "");
   var ids = CamlinternalOO.new_methods_variables($$class, shared$1, shared$5);
   var move = ids[0];
   var get_x = ids[1];
@@ -98,9 +99,10 @@ function point_init($$class) {
           return /* () */0;
         }
       ]);
-  return function (_, self, x_init) {
+  return function (_, self, x_init$1) {
     var self$1 = CamlinternalOO.create_object_opt(self, $$class);
-    self$1[x] = x_init;
+    self$1[x_init] = x_init$1;
+    self$1[x] = x_init$1;
     return self$1;
   };
 }
@@ -112,6 +114,7 @@ var p = Curry._2(point[0], 0, 7);
 eq("File \"class3_test.ml\", line 17, characters 12-19", Caml_oo_curry.js1(291546447, 1, p), 7);
 
 function adjusted_point_init($$class) {
+  var x_init = CamlinternalOO.new_variable($$class, "");
   var origin = CamlinternalOO.new_variable($$class, "");
   var ids = CamlinternalOO.new_methods_variables($$class, shared$7, shared$5);
   var move = ids[0];
@@ -133,10 +136,11 @@ function adjusted_point_init($$class) {
           return /* () */0;
         }
       ]);
-  return function (_, self, x_init) {
-    var origin$1 = Caml_int32.imul(x_init / 10 | 0, 10);
+  return function (_, self, x_init$1) {
+    var origin$1 = Caml_int32.imul(x_init$1 / 10 | 0, 10);
     var self$1 = CamlinternalOO.create_object_opt(self, $$class);
     self$1[origin] = origin$1;
+    self$1[x_init] = x_init$1;
     self$1[x] = origin$1;
     return self$1;
   };
@@ -184,6 +188,7 @@ var tmp$1 = Curry._2(adjusted_point2_000, 0, 31);
 eq("File \"class3_test.ml\", line 33, characters 12-19", Caml_oo_curry.js1(291546447, 3, tmp$1), 30);
 
 function printable_point_init($$class) {
+  var x_init = CamlinternalOO.new_variable($$class, "");
   var ids = CamlinternalOO.new_methods_variables($$class, shared$8, shared$5);
   var print = ids[0];
   var move = ids[1];
@@ -204,9 +209,10 @@ function printable_point_init($$class) {
           return Curry._1(self$neg4[0][get_x], self$neg4);
         }
       ]);
-  return function (_, self, x_init) {
+  return function (_, self, x_init$1) {
     var self$1 = CamlinternalOO.create_object_opt(self, $$class);
-    self$1[x] = x_init;
+    self$1[x_init] = x_init$1;
+    self$1[x] = x_init$1;
     return self$1;
   };
 }
@@ -264,6 +270,8 @@ var v = /* int array */[
 ];
 
 function printable_point2_init($$class) {
+  var x_init = CamlinternalOO.new_variable($$class, "");
+  var origin = CamlinternalOO.new_variable($$class, "");
   var ids = CamlinternalOO.new_methods_variables($$class, shared$8, shared$5);
   var print = ids[0];
   var move = ids[1];
@@ -288,10 +296,12 @@ function printable_point2_init($$class) {
         console.log("initializingFile \"class3_test.ml\", line 76, characters 50-57");
         return Caml_array.caml_array_set(v, 0, self$neg6[x]);
       });
-  return function (_, self, x_init) {
-    var origin = Caml_int32.imul(x_init / 10 | 0, 10);
+  return function (_, self, x_init$1) {
+    var origin$1 = Caml_int32.imul(x_init$1 / 10 | 0, 10);
     var self$1 = CamlinternalOO.create_object_opt(self, $$class);
-    self$1[x] = origin;
+    self$1[origin] = origin$1;
+    self$1[x_init] = x_init$1;
+    self$1[x] = origin$1;
     return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
   };
 }
@@ -328,6 +338,7 @@ var abstract_point = [
 ];
 
 function vpoint_init($$class) {
+  var x_init = CamlinternalOO.new_variable($$class, "");
   var ids = CamlinternalOO.new_methods_variables($$class, shared$1, shared$5);
   var move = ids[0];
   var get_x = ids[1];
@@ -345,10 +356,11 @@ function vpoint_init($$class) {
           return /* () */0;
         }
       ]);
-  return function (_, self, x_init) {
+  return function (_, self, x_init$1) {
     var self$1 = CamlinternalOO.create_object_opt(self, $$class);
-    Curry._2(obj_init, self$1, x_init);
-    self$1[x] = x_init;
+    self$1[x_init] = x_init$1;
+    Curry._2(obj_init, self$1, x_init$1);
+    self$1[x] = x_init$1;
     return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
   };
 }
@@ -415,6 +427,7 @@ var vv = Caml_oo_curry.js1(-792262820, 11, h$1);
 eq("File \"class3_test.ml\", line 128, characters 12-19", vv, 32);
 
 function restricted_point_init($$class) {
+  var x_init = CamlinternalOO.new_variable($$class, "");
   var ids = CamlinternalOO.new_methods_variables($$class, [
         "move",
         "get_x",
@@ -439,9 +452,10 @@ function restricted_point_init($$class) {
           return Curry._2(self$neg11[0][move], self$neg11, 1);
         }
       ]);
-  return function (_, self, x_init) {
+  return function (_, self, x_init$1) {
     var self$1 = CamlinternalOO.create_object_opt(self, $$class);
-    self$1[x] = x_init;
+    self$1[x_init] = x_init$1;
+    self$1[x] = x_init$1;
     return self$1;
   };
 }
@@ -460,12 +474,14 @@ var h$2 = Caml_oo_curry.js1(291546447, 13, p$2);
 eq("File \"class3_test.ml\", line 144, characters 12-19", h$2, 1);
 
 function point_again_init($$class) {
+  var x = CamlinternalOO.new_variable($$class, "");
   CamlinternalOO.get_method_label($$class, "move");
   var inh = CamlinternalOO.inherits($$class, shared$5, 0, shared, restricted_point, 1);
   var obj_init = inh[0];
-  return function (_, self, x) {
+  return function (_, self, x$1) {
     var self$1 = CamlinternalOO.create_object_opt(self, $$class);
-    Curry._2(obj_init, self$1, x);
+    self$1[x] = x$1;
+    Curry._2(obj_init, self$1, x$1);
     return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
   };
 }
@@ -485,11 +501,13 @@ var hh = Caml_oo_curry.js1(291546447, 17, p$3);
 eq("File \"class3_test.ml\", line 161, characters 12-19", hh, 8);
 
 function point_again2_init($$class) {
+  var x = CamlinternalOO.new_variable($$class, "");
   var inh = CamlinternalOO.inherits($$class, shared$5, 0, shared, restricted_point, 1);
   var obj_init = inh[0];
-  return function (_, self, x) {
+  return function (_, self, x$1) {
     var self$1 = CamlinternalOO.create_object_opt(self, $$class);
-    Curry._2(obj_init, self$1, x);
+    self$1[x] = x$1;
+    Curry._2(obj_init, self$1, x$1);
     return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
   };
 }
@@ -509,14 +527,16 @@ var hhh = Caml_oo_curry.js1(291546447, 21, p$4);
 eq("File \"class3_test.ml\", line 177, characters 12-19", hhh, 35);
 
 function point_again3_init($$class) {
+  var x = CamlinternalOO.new_variable($$class, "");
   var move = CamlinternalOO.get_method_label($$class, "move");
   var inh = CamlinternalOO.inherits($$class, shared$5, 0, shared, restricted_point, 1);
   var obj_init = inh[0];
   var move$1 = inh[4];
   CamlinternalOO.set_method($$class, move, Curry.__1(move$1));
-  return function (_, self, x) {
+  return function (_, self, x$1) {
     var self$1 = CamlinternalOO.create_object_opt(self, $$class);
-    Curry._2(obj_init, self$1, x);
+    self$1[x] = x$1;
+    Curry._2(obj_init, self$1, x$1);
     return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
   };
 }

```
## Pattern match reverse-engineering
