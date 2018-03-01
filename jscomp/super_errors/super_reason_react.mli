val type_is_component_spec: Types.type_expr -> bool
(** Used by super_typemod when we detect the message "... contains type variables that cannot be generalized" *)

val module_type_is_component_spec: Types.module_type -> bool
(** Used by super_typemod when we detect the message "... contains type variables that cannot be generalized" *)

val state_escape_scope: (Types.type_expr * Types.type_expr) list -> bool
(** Used by super_typecore when we detect the message "The type constructor state would escape its scope" *)

val is_array_wanted_reactElement: (Types.type_expr * Types.type_expr) list -> bool
(** Used by super_typecore when we detect the message "This has type array but expected reactElement" *)

val is_componentSpec_wanted_reactElement: (Types.type_expr * Types.type_expr) list -> bool
(** Used by super_typecore when we detect the message "This has type componentSpec but expected reactElement" *)
