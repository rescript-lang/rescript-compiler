include ../Makefile.shared

CAMLC=../../lib/bsc.exe
BSPACK=../bin/bspack.exe
INCLUDES= -I ../stdlib  -I ../runtime -I ../others/

OTHERS := test_literals a test_ari test_export2 test_internalOO test_obj_simple_ffi test_scope \
	test_switch arith_lexer test_array test_external test_is_js test_order \
	test_trywith arith_parser test_array_primitive test_js_ffi test_pack \
	test_seq test_u arith_syntax test_bug test_ffi test_let test_per\
	test_set unitest_string b test_bytes test_fib test_list test_pervasive simple_lexer_test\
	demo_binding demo test_char test_generative_module test_literal test_primitive test_simple_obj\
	empty_obj fib test_common test_include map_find_test test_react test_small\
	number_lexer test_demo inline_map_test test_nat64 test_react_case test_string\
	qcc test_exception inline_map2_test test_obj test_runtime_encoding test_string_case\
	buffer_test test_format test_simple_include test_exception_escape test_simple_pattern_match\
	test_nested_let test_tuple_destructring test_string_switch test_pervasives2\
	test_pervasives3 test_non_export test_const_elim test_int_map_find test_for_map\
	test_string_map bdd test_simple_tailcall test_bool_equal test_match_exception\
	test_sprintf test_functor_dead_code test_global_print test_side_effect_functor\
	test_static_catch_ident test_order_tailcall test_google_closure test_dup_param\
	test_simple_ref test_incr_ref ocaml_array ticker test_filename test_alias mt\
	mock_mt array_test test_nullary test_for_map2 int_map test_tuple\
	list_test map_test lexer_test google_closure_test boolean_test equal_exception_test\
	lazy_test ext_string_test\
	ext_bytes_test a_string_test \
	ext_filename_test a_filename_test test_nested_print\
	a_scope_bug a_recursive_type test_array_append ext_list_test\
	a_list_test test_type_based_arity\
	abstract_type curry test_formatter test_cps test_closure test_for_loop test_while_closure\
	test_while_side_effect for_side_effect_test cps_test for_loop_test queue_test libqueue_test\
	libarg_test loop_suites_test const_block_test stack_test test_stack\
	complex_while_loop hashtbl_test int_hashtbl_test cont_int_fold_test test_incomplete\
	test_ramification string_test rec_value_test test_string_const string_runtime_test\
	of_string_test tailcall_inline_test obj_test demo_page complex_test inline_edge_cases\
	inline_regression_test primitive_reg_test test_eq equal_test typeof_test lib_js_test\
	small_inline_test ari_regress_test record_with_test complex_if_test module_parameter_test\
	regression_print global_exception_regression_test printf_test format_regression loop_regression_test\
	and_or_tailcall_test const_defs const_defs_test genlex_test cross_module_inline_test runtime_encoding_test\
	obj_magic_test caml_compare_test rec_module_test guide_for_ext local_exception_test js_obj_test extensible_variant_test\
	ext_log_test lam_current_unit test_zero_nullable int64_test string_literal_print_test js_bool_test class_type_ffi_test unsafe_ppx_test\
	js_date js_date_test es6_module_test int_overflow_test float_test bigarray_test float_of_bits_test swap_test int64_mul_div_test\
	ffi_test shift_test debugger_test caml_format_test js_val epsilon_test io_test random_test ffi_js_test\
	stringmatch_test printf_sim test_external_unit \
	ext_array_test int32_test hamming_test pr_regression_test\
	to_string_test print_alpha_test ffi_arity_test digest_test string_get_set_test exception_raise_test\
	class_test debug_keep_test demo_int_map const_test class3_test class4_test class5_test class6_test\
	class7_test class8_test class_repr pq_test mt_global event_ffi hash_test exception_value_test sprintf_reg_test\
	scanf_test testing tscanf_test scanf_reference_error_regression_test sprintf_reg_test scanf_io fail_comp\
	stack_comp_test float_array float_record tfloat_record_test basic_module_test offset\
	pr6726 undef_regression_test tuple_alloc test_unsupported_primitive module_alias ext_pervasives_test\
	test_const_propogate limits_test ext_sys_test test_unsafe_obj_ffi module_missing_conversion\
	ocaml_proto_test test_unsafe_obj_ffi_ppx ppx_apply_test test_http_server\
	http_types ignore_test test_index obj_literal_ppx_test obj_literal_ppx\
	gpr_405_test attr_test uncurry_glob_test nested_obj_test nested_obj_literal\
	method_name_test format_test config1_test config2_test largest_int_flow\
	noassert test_unsafe_cmp gpr_441 gpr_459_test promise chain_code_test\
	method_chain recursive_module recursive_module_test simplify_lambda_632o\
	div_by_zero_test unsafe_this obj_type_test polymorphism_test class_error_message\
	test_bs_this class_setter_getter gpr496_test local_class_type test_regex ffi_array_test\
	nested_include \
	mutual_non_recursive_type external_ppx \
	optional_ffi_test poly_variant_test \
	bs_rest_test infer_type_test fs_test module_as_function\
	test_case_set test_mutliple string_bound_get_test inline_string_test\
	ppx_this_obj_test unsafe_obj_external gpr_627_test jsoo_485_test jsoo_400_test	\
	test_require more_uncurry earger_curry_test poly_type bench mutable_obj_test\
	gpr_658 module_as_class_ffi pipe_send_readline demo_pipe bs_array_test bs_string_test\
	watch_test bs_node_string_buffer_test \
	bs_ignore_test bs_ignore_effect \
	ppx_this_obj_field single_module_alias \
	sexpm sexpm_test sexp js_json_test array_subtle_test  \
	bytes_split_gpr_743_test module_splice_test hello.foo \
	ffi_splice_test node_path_test js_string_test js_array_test string_unicode_test node_fs_test\
	flow_parser_reg_test	utf8_decode_test stream_parser_test condition_compilation_test semver_test update_record_test installation_test app_root_finder derive_projector_test\
	gpr_904_test gpr_858_unit2_test inner_unused \
	set_gen bal_tree string_set string_set_test \
	js_math_test bal_set_mini gpr_974_test test_cpp\
	global_module_alias_test class_fib_open_recursion_test\
	topsort_test flexible_array_test\
	gray_code_test alias_test\
	flattern_order_test\
	minimal_test\
	gpr_1063_test\
	gpr_977_test\
	gpr_1072\
	submodule\
	submodule_call\
	side_effect\
	side_effect_free\
	include_side_effect_free\
	include_side_effect\
	exception_alias\
	prepend_data_ffi\
	gbk\
	gpr_1072_reg\
	gpr_1150\
	gpr_1154_test\
	obj_curry_test\
	gpr_1170\
	js_re_test\
	js_dict_test\
	name_mangle_test\
	oo_js_test_date\
	functor_ffi\
	js_typed_array_test\
	gpr_1240_missing_unbox\
	js_null_undefined_test js_null_test js_undefined_test\
	gpr_1268\
	bs_auto_uncurry\
	bs_auto_uncurry_test\
	chn_test\
	return_check\
	bs_splice_partial\
	gpr_return_type_unused_attribute\
	test\
	undef_regression2_test\
	js_global_test\
	js_float_test\
	js_int_test\
	bang_primitive\
	compare_test\
	js_cast_test\
	gpr_1423_nav\
	gpr_1423_app_test\
	export_keyword\
	key_word_property\
	key_word_property2\
	key_word_property_plus_test\
	global_mangles\
	gpr_1438\
	unicode_type_error\
	arity_deopt\
	switch_case_test\
	bb\
	ocaml_parsetree_test\
	ocaml_typedtree_test\
	gpr_1484\
	gpr_1481\
	custom_error_test\
	exception_rebound_err_test\
	js_exception_catch_test\
	gpr_1503_test\
	gpr_1501_test\
	exception_repr_test\
	nested_pattern_match_test\
	caml_sys_poly_fill_test\
	argv_test\
	promise_catch_test\
	exception_rebind_test\
	exception_def\
	test_other_exn\
	rebind_module\
	rebind_module_test\
	js_promise_basic_test\
	exn_error_pattern\
	string_interp_test\
	gpr_1539_test\
	webpack_config\
	bs_qualified\
	mario_game\
	gpr_1600_test\
	js_list_test\
	js_option_test\
	gpr_1658_test\
	gpr_1667_test\
	gpr_1692_test\
	gpr_1701_test\
	gpr_1698_test\
	gpr_1717_test\
	gpr_1716_test\
	gpr_1728_test\
	gpr_1409_test\
	rec_fun_test\
	gpr_1749_test\
	ocaml_re_test\
	gpr_1762_test\
	gpr_1760_test\
	gpr_1759_test\
	inner_define\
	inner_call\
	default_export_test\
	gpr_1817_test\
	gpr_1822_test\
	bs_unwrap_test\
	dollar_escape_test\
	js_nullable_test\
	gpr_1891_test\
	gpr_1943_test\
	gpr_1946_test\
	adt_optimize_test\
	mpr_6033_test\
	polyvar_test\
	functor_def\
	functor_inst\
	functor_app_test\
	module_alias_test\
	escape_esmodule\
	esmodule_ref\
	miss_colon_test\
	gpr_1245_test\
	gpr_2126_test\
	big_enum\
	ast_js_mapper_test\
	ast_js_mapper_poly_test\
	big_polyvar_test\
	ast_abstract_test\
	ast_mapper_defensive_test\
	ast_mapper_unused_warning_test\
	bs_MapInt_test\
	gpr_2316_test\
	gpr_2250_test\
	gpr_1285_test\
	gpr_2474\
	defunctor_make_test\
	bs_hashtbl_string_test\
	console_log_test\
	bs_set_bench\
	bs_set_int_test\
	rbset\
	bs_rbset_int_bench\
	bs_array_test\
	bs_poly_mutable_set_test\
	bs_mutable_set_test\
	bs_poly_map_test\
	bs_map_test\
	bs_map_set_dict_test\
	bs_abstract_test\
	bs_queue_test\
	bs_hashmap_test\
	bs_hashset_int_test\
	array_data_util\
	bs_list_test\
	bs_min_max_test\
	bs_sort_test\
	equal_box_test\
	bs_poly_set_test\
	bs_stack_test\
	bs_poly_mutable_map_test\
	imm_map_bench\
	gpr_2487\
	gpr_2503_test\
	block_alias_test\
	gpr_2608_test\
	pipe_syntax\
	gpr_2352_test\
	gpr_2633_test\
	gpr_2413_test\
# bs_uncurry_test
# needs Lam to get rid of Uncurry arity first
# simple_derive_test
# simple_derive_use
# derive_type_test
# derive_dyntype


SOURCE_LIST :=  $(OTHERS)
TESTS := $(addsuffix .cmj, $(SOURCE_LIST) )
## since we use ppx
$(addsuffix .cmi, $(OTHERS)): ../runtime/js.cmi
$(addsuffix .cmj, $(OTHERS)): ../runtime/js.cmj

# It is okay to test deprecated API
COMPFLAGS+=  $(MODULE_FLAGS) -w -40 -warn-error A+8-3-30-26+101-102-103-104
COMPFLAGS+=  -bs-no-version-header -absname -bs-diagnose -bs-cross-module-opt -bs-package-name  bs-platform -bs-package-output commonjs:jscomp/test


$(TESTS): $(CAMLC)

all: $(TESTS)
.PHONY: all
clean::
	rm -f *.cm*
	rm -f *~
	rm -f  *.annot
	rm -f $(TESTS)
	rm -f *.rawlambda *.lam *.lambda *.map
	rm -f arith_lexer.ml number_lexer.ml simple_lexer_test.ml

.mli.cmi:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(INCLUDES) $(COMPFLAGS) -c $<

.ml.cmj:
	$(CAMLC) $(INCLUDES)  $(COMPFLAGS) -c $<

OCAML_SRC_UTILS=../../vendor/ocaml/utils/
OCAML_SRC_PARSING=../../vendor/ocaml/parsing/
OCAML_SRC_TYPING=../../vendor/ocaml/typing/

arith_lexer.ml: arith_lexer.mll
	$(CAMLLEX) $<
number_lexer.ml: number_lexer.mll
	$(CAMLLEX) $<
simple_lexer_test.ml: simple_lexer_test.mll
	$(CAMLLEX) $<
ocaml_parsetree_test.ml: ocaml_parsetree_main.ml
	$(BSPACK) -bs-main Ocaml_parsetree_main -bs-MD  -I $(OCAML_SRC_PARSING) -I $(OCAML_SRC_UTILS) -o $@

ocaml_typedtree_test.ml: ocaml_typed_tree_main.ml
	$(BSPACK) -bs-main Ocaml_typed_tree_main -bs-MD -I $(OCAML_SRC_PARSING) -I $(OCAML_SRC_UTILS) -I $(OCAML_SRC_TYPING) -o $@

-include .depend

depend:
	$(CAMLDEP) -native $(INCLUDES) *.mli *.ml | sed -e 's/\.cmx/.cmj/g' > .depend
