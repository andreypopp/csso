type style

val empty : style

external make :
  < classes : < .. > Js.t ; style : < .. > Js.t ; className : string option >
  Js.t ->
  style = "%identity"

external style : style -> ReactDOM.style = "style" [@@mel.get]
external className : style -> string option = "className" [@@mel.get]
val merge : style array -> style

include module type of Csso_value
