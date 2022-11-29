type t = { module_names : string array; module_data : bytes array }
type cmi_data = Cmi_format.cmi_infos
type cmj_data = { values : Js_cmj_format.keyed_cmj_value array; pure : bool }

val marshal_cmi_data : cmi_data -> bytes
val marshal_cmj_data : cmj_data -> bytes
val unmarshal_cmi_data : bytes -> cmi_data
val unmarshal_cmj_data : bytes -> cmj_data
