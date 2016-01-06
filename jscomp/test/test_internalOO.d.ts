export var copy: (o : any) => any ;
export var params: any ;
export var step: any ;
export var initial_object_size: any ;
export var dummy_item: any ;
export var public_method_label: (s : any) => any ;
export var Vars: any ;
export var Meths: any ;
export var Labs: any ;
export var dummy_table: any ;
export var table_count: any ;
export var dummy_met: any ;
export var fit_size: (n : any) => any ;
export var new_table: (pub_labels : any) => any ;
export var resize: (array : any, new_size : any) => any ;
export var put: (array : any, label : any, element : any) => any ;
export var method_count: any ;
export var inst_var_count: any ;
export var new_method: (table : any) => any ;
export var get_method_label: (table : any, name : any) => any ;
export var get_method_labels: (table : any, names : any) => any ;
export var set_method: (table : any, label : any, element : any) => any ;
export var get_method: (table : any, label : any) => any ;
export var to_list: (arr : any) => any ;
export var narrow:
  (table : any, vars : any, virt_meths : any, concr_meths : any) => any ;
export var widen: (table : any) => any ;
export var new_slot: (table : any) => any ;
export var new_variable: (table : any, name : any) => any ;
export var to_array: (arr : any) => any ;
export var new_methods_variables:
  (table : any, meths : any, vals : any) => any ;
export var get_variable: (table : any, name : any) => any ;
export var get_variables: (table : any, names : any) => any ;
export var add_initializer: (table : any, f : any) => any ;
export var create_table: (public_methods : any) => any ;
export var init_class: (table : any) => any ;
export var inherits:
  (cla : any, vals : any, virt_meths : any, concr_meths : any, param : any,
  top : any) => any ;
export var make_class: (pub_meths : any, class_init : any) => any ;
export var make_class_store:
  (pub_meths : any, class_init : any, init_table : any) => any ;
export var dummy_class: (loc : any) => any ;
export var create_object: (table : any) => any ;
export var create_object_opt: (obj_0 : any, table : any) => any ;
export var iter_f: (obj : any, param : any) => any ;
export var run_initializers: (obj : any, table : any) => any ;
export var run_initializers_opt: (obj_0 : any, obj : any, table : any) => any
  ;
export var create_object_and_run_initializers:
  (obj_0 : any, table : any) => any ;
export var build_path: (n : any, keys : any, tables : any) => any ;
export var lookup_keys: (i : any, keys : any, tables : any) => any ;
export var lookup_tables: (root : any, keys : any) => any ;
export var get_const: (x : any) => any ;
export var get_var: (n : any) => any ;
export var get_env: (e : any, n : any) => any ;
export var get_meth: (n : any) => any ;
export var set_var: (n : any) => any ;
export var app_const: (f : any, x : any) => any ;
export var app_var: (f : any, n : any) => any ;
export var app_env: (f : any, e : any, n : any) => any ;
export var app_meth: (f : any, n : any) => any ;
export var app_const_const: (f : any, x : any, y : any) => any ;
export var app_const_var: (f : any, x : any, n : any) => any ;
export var app_const_meth: (f : any, x : any, n : any) => any ;
export var app_var_const: (f : any, n : any, x : any) => any ;
export var app_meth_const: (f : any, n : any, x : any) => any ;
export var app_const_env: (f : any, x : any, e : any, n : any) => any ;
export var app_env_const: (f : any, e : any, n : any, x : any) => any ;
export var meth_app_const: (n : any, x : any) => any ;
export var meth_app_var: (n : any, m : any) => any ;
export var meth_app_env: (n : any, e : any, m : any) => any ;
export var meth_app_meth: (n : any, m : any) => any ;
export var send_const: (m : any, x : any, c : any) => any ;
export var send_var: (m : any, n : any, c : any) => any ;
export var send_env: (m : any, e : any, n : any, c : any) => any ;
export var send_meth: (m : any, n : any, c : any) => any ;
export var new_cache: (table : any) => any ;
export var method_impl: (table : any, i : any, arr : any) => any ;
export var set_methods: (table : any, methods : any) => any ;
export var stats: (param : any) => any ;

