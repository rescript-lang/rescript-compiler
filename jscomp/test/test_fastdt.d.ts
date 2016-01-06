export var H: any ;
export var A: any ;
export var mkfp: (a : any, b : any) => any ;
export var array_elem: (a : any, i : any) => any ;
export var split_white_re: any ;
export var split_white: any ;
export var dict: any ;
export var dictN: any ;
export var get_fid: (str : any) => any ;
export var clean_up_dict: (fcounts : any, minfc : any) => any ;
export var map_filter: (f : any, param : any) => any ;
export var $slash$dot$dot: (a : any, b : any) => any ;
export var predict: (dt : any, x : any) => any ;
export var compute_tree_error: (tree : any) => any ;
export var predict_committee: (dts : any, x : any) => any ;
export var is_real_value: (f : any) => any ;
export var find_split_feature:
  (c_t : any, c_f : any, _F : any, _Y : any, _W : any, used : any,
  validEx : any) => any ;
export var trim_tree_same: (dt : any) => any ;
export var build_dt:
  (max_depth : any, leaf_acc : any, smooth : any, validExO : any, _F : any,
  _Y : any, _W : any) => any ;
export var build_bagged_dt:
  (size : any, max_depth : any, leaf_acc : any, smooth : any, param : any,
  _F : any, _Y : any, _W : any) => any ;
export var build_boosted_dt:
  (size : any, max_depth : any, leaf_acc : any, smooth : any, input_f : any,
  _F : any, _Y : any, _W : any) => any ;
export var build_single_dt:
  (max_depth : any, leaf_acc : any, smooth : any, param : any, _F : any,
  _Y : any, _W : any) => any ;
export var uniq: (l : any) => any ;
export var of_list_rev: (list : any) => any ;
export var load_data: (minfc : any, fp : any) => any ;
export var predict_file: (model : any, fp : any) => any ;
export var print_tree: (out : any, dt : any) => any ;
export var read_tree: (h : any) => any ;
export var load_model: (fp : any) => any ;

