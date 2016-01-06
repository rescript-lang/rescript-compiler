export var public_method_label: (s : any) => any ;
export var new_method: (table : any) => any ;
export var new_variable: (table : any, name : any) => any ;
export var new_methods_variables:
  (table : any, meths : any, vals : any) => any ;
export var get_variable: (table : any, name : any) => any ;
export var get_variables: (table : any, names : any) => any ;
export var get_method_label: (table : any, name : any) => any ;
export var get_method_labels: (table : any, names : any) => any ;
export var get_method: (table : any, label : any) => any ;
export var set_method: (table : any, label : any, element : any) => any ;
export var set_methods: (table : any, methods : any) => any ;
export var narrow:
  (table : any, vars : any, virt_meths : any, concr_meths : any) => any ;
export var widen: (table : any) => any ;
export var add_initializer: (table : any, f : any) => any ;
export var dummy_table: any ;
export var create_table: (public_methods : any) => any ;
export var init_class: (table : any) => any ;
export var inherits:
  (cla : any, vals : any, virt_meths : any, concr_meths : any, param : any,
  top : any) => any ;
export var make_class: (pub_meths : any, class_init : any) => any ;
export var make_class_store:
  (pub_meths : any, class_init : any, init_table : any) => any ;
export var dummy_class: (loc : any) => any ;
export var copy: (o : any) => any ;
export var create_object: (table : any) => any ;
export var create_object_opt: (obj_0 : any, table : any) => any ;
export var run_initializers: (obj : any, table : any) => any ;
export var run_initializers_opt: (obj_0 : any, obj : any, table : any) => any
  ;
export var create_object_and_run_initializers:
  (obj_0 : any, table : any) => any ;
export var lookup_tables: (root : any, keys : any) => any ;
export var params: any ;
export var stats: (param : any) => any ;

