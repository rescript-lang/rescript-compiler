type t = { module_names : string array; module_data : bytes array }
type cmi_data = Cmi_format.cmi_infos
type cmj_data = { values : Js_cmj_format.keyed_cmj_value array; pure : bool }

let marshal_cmi_data (cmi_data : cmi_data) = Marshal.to_bytes cmi_data []
let marshal_cmj_data (cmj_data : cmj_data) = Marshal.to_bytes cmj_data []
let unmarshal_cmi_data bytes : cmi_data = Marshal.from_bytes bytes 0
let unmarshal_cmj_data bytes : cmj_data = Marshal.from_bytes bytes 0
