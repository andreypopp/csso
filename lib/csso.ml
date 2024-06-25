[@@@ocaml.warning "-69"]

type style = { classes : < > Js.t; style : < > Js.t; className : string option }

let no_style = Js.Obj.empty ()
let no_classes = Js.Obj.empty ()
let empty = { classes = no_classes; style = no_style; className = None }

external make :
  < classes : < .. > Js.t ; style : < .. > Js.t ; className : string option >
  Js.t ->
  style = "%identity"

external style : style -> ReactDOM.style = "style" [@@mel.get]
external className : style -> string option = "className" [@@mel.get]

let merge xs =
  let len = Js.Array.length xs in
  if len = 0 then empty
  else if len = 1 then Js.Array.unsafe_get xs 0
  else
    let classes = Js.Obj.empty () in
    let style = Js.Obj.empty () in
    for i = 0 to len - 1 do
      let x = Js.Array.unsafe_get xs i in
      ignore (Js.Obj.assign classes x.classes);
      ignore (Js.Obj.assign style x.style)
    done;
    let className =
      let values = Js.Dict.values (Obj.magic classes) in
      Some (Js.Array.join ~sep:" " values)
    in
    { classes; style; className }
