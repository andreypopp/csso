type style = {
  classes : string Js.Dict.t;
  styles : string Js.undefined Js.Dict.t;
}

let no_styles = Js.Dict.empty ()
let no_classes = Js.Dict.empty ()
let empty = { classes = no_classes; styles = no_styles }
let var_name name = "--" ^ name

let make_static name ~class_name =
  let classes = Js.Dict.empty () in
  let styles = Js.Dict.empty () in
  Js.Dict.set classes name class_name;
  Js.Dict.set styles (var_name name) Js.Undefined.empty;
  { classes; styles }

let make_dynamic name ~class_name ~value =
  let classes = Js.Dict.empty () in
  let styles = Js.Dict.empty () in
  Js.Dict.set classes name class_name;
  Js.Dict.set styles (var_name name) (Js.Undefined.return value);
  { classes; styles }

let merge xs =
  let classes = Js.Dict.empty () in
  let styles = Js.Dict.empty () in
  List.iter
    (fun x ->
      ignore (Js.Obj.assign (Obj.magic classes) (Obj.magic x.classes));
      ignore (Js.Obj.assign (Obj.magic styles) (Obj.magic x.styles)))
    xs;
  { classes; styles }

let to_class_name { classes; _ } =
  let values = Js.Dict.values classes in
  match Js.Array.length values with
  | 0 -> None
  | _ -> Some (Js.Array.join ~sep:" " values)

let to_inline_style { styles; _ } =
  ReactDOM.Style._dictToStyle (Obj.magic styles : string Js.Dict.t)

include Csso_value
