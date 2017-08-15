(* This is used by js_main.ml *)
let setup () =
  Oprint.out_value := Tweaked_reason_oprint.print_out_value;
  Oprint.out_type := Tweaked_reason_oprint.print_out_type;
  Oprint.out_class_type := Tweaked_reason_oprint.print_out_class_type;
  Oprint.out_module_type := Tweaked_reason_oprint.print_out_module_type;
  Oprint.out_sig_item := Tweaked_reason_oprint.print_out_sig_item;
  Oprint.out_signature := Tweaked_reason_oprint.print_out_signature;
  Oprint.out_type_extension := Tweaked_reason_oprint.print_out_type_extension;
  Oprint.out_phrase := Tweaked_reason_oprint.print_out_phrase
