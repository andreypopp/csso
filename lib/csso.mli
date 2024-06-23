type style

val empty : style
val make_static : string -> class_name:string -> style
val make_dynamic : string -> class_name:string -> value:string -> style
val merge : style list -> style
val to_class_name : style -> string option
val to_inline_style : style -> ReactDOM.style

include module type of Csso_value
