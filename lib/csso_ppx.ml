open Ppxlib
open Printf

module Spec : sig
  type t

  val static : string -> string -> t
  val dynamic : string -> expression -> t
  val to_class_name : t -> string
  val to_css : t -> string
  val to_expression : loc:Location.t -> t list -> expression
end = struct
  type value = S of string | D of expression
  type t = { name : string; value : value }

  let static name value = { name; value = S value }
  let dynamic name value = { name; value = D value }

  let to_class_name { name; value } =
    let value = match value with S v -> v | D _ -> "dynamic" in
    sprintf "%s-%s" name value

  let to_css spec =
    let class_name = to_class_name spec in
    sprintf ".%s { %s: %s; }" class_name spec.name
      (match spec.value with S v -> v | D _ -> "var(--" ^ spec.name ^ ")")

  let to_expression ~loc = function
    | [] -> [%expr Csso.empty]
    | specs ->
        let open Ast_builder.Default in
        let classes, styles =
          List.map
            (fun spec ->
              let class_name = to_class_name spec in
              let classes =
                ({ txt = Lident spec.name; loc }, estring ~loc class_name)
              in
              let styles =
                let v =
                  match spec.value with
                  | S _ -> [%expr Js.Undefined.empty]
                  | D v -> [%expr Js.Undefined.return [%e v]]
                in
                ({ txt = Lident ("--" ^ spec.name); loc }, [%expr [%e v]])
              in
              ((class_name, classes), styles))
            specs
          |> List.split
        in
        let class_names, classes = List.split classes in
        let classes = pexp_record ~loc classes None in
        let styles = pexp_record ~loc styles None in
        let className = estring ~loc (String.concat " " class_names) in
        [%expr
          Csso.make
            [%mel.obj
              {
                classes = [%mel.obj [%e classes]];
                style = [%mel.obj [%e styles]];
                className = Some [%e className];
              }]]
end

module Specs : sig
  val of_expression : loc:location -> expression -> Spec.t option
  val all : unit -> Spec.t list
end = struct
  let found_specs : Spec.t list ref = ref []
  let all () = !found_specs

  let match_int e =
    match e.Ppxlib.pexp_desc with
    | Pexp_constant (Pconst_integer (v, _)) -> Some (int_of_string v)
    | _ -> None

  let of_expression ~loc e =
    let exception Not_a_prop in
    let spec =
      try
        let spec =
          match e with
          | [%expr flex_basis `auto] ->
              Spec.static "flex-basis" (Csso_value.flex_basis `auto)
          | [%expr flex_basis (`px [%e? v])] -> (
              match match_int v with
              | Some v ->
                  Spec.static "flex-basis" (Csso_value.flex_basis (`px v))
              | None ->
                  Spec.dynamic "flex-basis"
                    [%expr Csso_value.flex_basis (`px [%e v])])
          | [%expr flex_basis [%e? v]] ->
              Spec.dynamic "flex-basis" [%expr Csso_value.flex_basis [%e v]]
          | [%expr width (`px [%e? v])] -> (
              match match_int v with
              | Some v -> Spec.static "width" (Csso_value.width (`px v))
              | None ->
                  Spec.dynamic "width" [%expr Csso_value.width (`px [%e v])])
          | [%expr width [%e? v]] ->
              Spec.dynamic "width" [%expr Csso_value.width [%e v]]
          | [%expr height (`px [%e? v])] -> (
              match match_int v with
              | Some v -> Spec.static "height" (Csso_value.height (`px v))
              | None ->
                  Spec.dynamic "height" [%expr Csso_value.height (`px [%e v])])
          | [%expr height [%e? v]] ->
              Spec.dynamic "height" [%expr Csso_value.height [%e v]]
          | _ -> raise Not_a_prop
        in
        Some spec
      with Not_a_prop -> None
    in
    match spec with
    | None -> None
    | Some spec ->
        found_specs := spec :: !found_specs;
        Some spec
end

let compile_props ~loc es =
  let open Ast_builder.Default in
  let rec go specs acc = function
    | [] -> List.rev (compile_specs specs acc)
    | e :: es -> (
        match Specs.of_expression ~loc e with
        | Some spec -> go (spec :: specs) acc es
        | None -> go [] (e :: compile_specs specs acc) es)
  and compile_specs specs acc =
    match specs with [] -> acc | specs -> Spec.to_expression ~loc specs :: acc
  in
  let es = go [] [] es in
  [%expr
    let open Csso_value in
    let _ = height in
    [%e
      match es with
      | [] -> [%expr Csso.empty]
      | [ e ] -> e
      | _ -> [%expr Csso.merge [%e pexp_array ~loc es]]]]

let extension_stri =
  let pattern =
    let open Ast_pattern in
    let extractor_in_let =
      pstr_value drop (value_binding ~pat:__ ~expr:(esequence __) ^:: nil)
    in
    pstr @@ extractor_in_let ^:: nil
  in
  let expand ~ctxt p es =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let e = compile_props ~loc es in
    [%stri let [%p p] = [%e e]]
  in
  Context_free.Rule.extension
    (Extension.V3.declare "csso" Extension.Context.structure_item pattern expand)

let extension_expr =
  let pattern =
    let open Ast_pattern in
    single_expr_payload (esequence __)
  in
  let expand ~ctxt es =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    compile_props ~loc es
  in
  Context_free.Rule.extension
    (Extension.V3.declare "csso" Extension.Context.expression pattern expand)

let is_jsx expr =
  List.exists
    (function { attr_name = { txt = "JSX"; _ }; _ } -> true | _ -> false)
    expr.pexp_attributes

let is_html_element = function
  | "div" | "span" | "a" | "button" | "input" | "label" | "img" | "ul" | "li"
  | "ol" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "p" | "form" | "textarea"
  | "select" | "option" | "table" | "tr" | "td" | "th" | "thead" | "tbody"
  | "tfoot" | "nav" | "header" | "footer" | "section" | "article" | "aside"
  | "main" | "figure" | "figcaption" | "blockquote" | "cite" | "pre" | "code"
  | "abbr" | "acronym" | "address" | "b" | "strong" | "i" | "em" | "mark"
  | "small" | "del" | "ins" | "sub" | "sup" | "s" | "u" | "var" | "kbd" | "samp"
  | "q" | "dfn" | "ruby" | "rt" | "rp" | "bdo" | "br" | "wbr" | "hr" | "meter"
  | "progress" | "time" | "audio" | "video" | "source" | "track" | "embed"
  | "object" | "param" | "canvas" | "map" | "area" | "svg" | "math" | "iframe"
  | "frame" | "frameset" | "noframes" | "details" | "summary" | "dialog"
  | "menu" | "menuitem" | "legend" | "fieldset" | "datalist" | "keygen"
  | "output" | "slot" | "template" | "caption" | "col" | "colgroup" ->
      true
  | _ -> false

let jsx_rewrite =
  object
    inherit Ast_traverse.map as super

    method! expression : expression -> expression =
      fun expr ->
        let loc = expr.pexp_loc in
        let make s tag args =
          [%expr
            let s = [%e s] in
            let className = Csso.className s in
            let style = Csso.style s in
            [%e { expr with pexp_desc = Pexp_apply (tag, args) }]]
        in
        let extract_spec expr tag args =
          let exception Nope in
          try
            let loc = expr.pexp_loc in
            let () =
              match tag.pexp_desc with
              | Pexp_ident { txt = Lident name; _ } when is_html_element name ->
                  ()
              | _ -> raise Nope
            in
            let found = ref None in
            let args =
              List.concat_map
                (function
                  | Labelled "csso", arg ->
                      found := Some arg;
                      [
                        (Optional "className", [%expr className]);
                        (Labelled "style", [%expr style]);
                      ]
                  | arg_label, arg -> [ (arg_label, super#expression arg) ])
                args
            in
            match !found with
            | None -> raise Nope
            | Some arg ->
                let pat = Ast_pattern.(elist __) in
                Some
                  (match Ast_pattern.parse_res pat loc arg Fun.id with
                  | Error _ -> `Expr (arg, args)
                  | Ok es -> `Specs (es, args))
          with Nope -> None
        in
        match expr.pexp_desc with
        | Pexp_apply (tag, args) when is_jsx expr -> (
            match extract_spec expr tag args with
            | None -> expr
            | Some (`Expr (s, args)) -> make s tag args
            | Some (`Specs (s, args)) ->
                let s = compile_props ~loc s in
                make s tag args)
        | _ -> super#expression expr
  end

let impl (str : structure) =
  let open Ast_builder.Default in
  let loc = Location.none in
  List.fold_left
    (fun str spec ->
      let name = Spec.to_class_name spec in
      let css = Spec.to_css spec in
      [%stri [@@@CSS [%e estring ~loc name], [%e estring ~loc css]]] :: str)
    str (Specs.all ())

let () =
  Driver.register_transformation "csso"
    ~rules:[ extension_stri; extension_expr ]
    ~preprocess_impl:jsx_rewrite#structure ~impl

module Stylesheet = struct
  module String_map = Map.Make (String)

  type t = string String_map.t

  let empty = String_map.empty

  let of_structure s css =
    let get_string e =
      match e.pexp_desc with
      | Pexp_constant (Pconst_string (v, _, None)) -> v
      | _ -> assert false
    in
    List.fold_left
      (fun css item ->
        match item with
        | [%stri [@@@CSS [%e? k], [%e? v]]] ->
            let k = get_string k in
            let v = get_string v in
            String_map.add k v css
        | _ -> css)
      css s

  let of_ml filename css =
    match Ppxlib.Ast_io.read_binary filename with
    | Error _ -> css
    | Ok t -> (
        match Ppxlib.Ast_io.get_ast t with
        | Impl s -> of_structure s css
        | Intf _ -> (* no CSS in interfaces *) css)

  let of_css filename css =
    In_channel.with_open_bin filename @@ fun ic ->
    let rec loop css =
      match In_channel.input_line ic with
      | None -> css
      | Some v -> (
          match String.index v ' ' with
          | exception Not_found -> loop css
          | i ->
              let k = String.sub v 0 i in
              loop (String_map.add k v css))
    in
    loop css

  let output_css oc =
    String_map.iter (fun _ v ->
        output_string oc v;
        output_char oc '\n')
end
