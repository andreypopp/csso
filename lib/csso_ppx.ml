open Ppxlib
open Printf
module String_map = Map.Make (String)

module Spec : sig
  type t

  val css : label array -> string array -> t
  (** statically known stylesheet *)

  val dyn : label array -> expression -> t
  (** dynamically computed stylesheet *)

  val merge : t list -> t
  (** merge multiple stylesheets *)

  val to_class_name : t -> string list
  val to_css : t -> string list
  val to_expression : loc:Location.t -> t -> expression
end = struct
  type t = value String_map.t

  and value =
    | Css of string (* a statically known css value *)
    | Dyn of expression * label * int
  (* a dynamic css value, as an expression which evaluates to an array and an
     index into the array *)

  let css keys vals : t =
    String_map.of_seq
      (Array.to_seq keys |> Seq.mapi (fun i k -> (k, Css vals.(i))))

  let dyn keys expr : t =
    let name = gen_symbol ~prefix:"css" () in
    String_map.of_seq
      (Array.to_seq keys |> Seq.mapi (fun i k -> (k, Dyn (expr, name, i))))

  let merge a b =
    String_map.merge
      (fun _ a b ->
        match (a, b) with
        | Some _, Some v | Some v, None | None, Some v -> Some v
        | None, None -> None)
      a b

  let merge xs = List.fold_left merge String_map.empty xs
  let css_to_class_name = sprintf "%s-%s"
  let dyn_to_class_name = sprintf "%s-dyn"

  let to_class_name v =
    String_map.to_seq v
    |> Seq.map (fun (n, v) ->
           match v with
           | Css v -> css_to_class_name n v
           | Dyn _ -> dyn_to_class_name n)
    |> List.of_seq

  let to_css v =
    String_map.to_seq v
    |> Seq.map (fun (n, v) ->
           match v with
           | Css v -> sprintf ".%s { %s: %s; }" (css_to_class_name n v) n v
           | Dyn _ -> sprintf ".%s { %s: var(--%s); }" (dyn_to_class_name n) n n)
    |> List.of_seq

  let to_expression ~loc v =
    let open Ast_builder.Default in
    if String_map.is_empty v then [%expr Csso.empty]
    else
      let used = ref String_map.empty in
      let classes, styles =
        let style_name n = { txt = Lident (sprintf "--%s" n); loc } in
        let class_info n class_name =
          let classes = ({ txt = Lident n; loc }, estring ~loc class_name) in
          (class_name, classes)
        in
        String_map.to_seq v
        |> Seq.map (fun (n, v) ->
               match v with
               | Css v ->
                   let class_name = css_to_class_name n v in
                   let styles = (style_name n, [%expr Js.Undefined.empty]) in
                   (class_info n class_name, styles)
               | Dyn (e, en, idx) ->
                   let class_name = dyn_to_class_name n in
                   used := String_map.add en e !used;
                   let styles =
                     ( style_name n,
                       [%expr
                         Js.Undefined.return
                           (Array.get [%e evar ~loc:e.pexp_loc en]
                              [%e eint ~loc idx])] )
                   in
                   (class_info n class_name, styles))
        |> List.of_seq |> List.split
      in
      let class_names, classes = List.split classes in
      let classes = pexp_record ~loc classes None in
      let styles = pexp_record ~loc styles None in
      let className = estring ~loc (String.concat " " class_names) in
      let expr =
        [%expr
          Csso.make
            [%mel.obj
              {
                classes = [%mel.obj [%e classes]];
                style = [%mel.obj [%e styles]];
                className = Some [%e className];
              }]]
      in
      String_map.fold
        (fun k v expr ->
          [%expr
            let [%p pvar ~loc k] = [%e v] in
            [%e expr]])
        !used expr
end

module Arg_spec : sig
  [@@@ocaml.warning "-32"]

  type _ t

  val bool : bool t
  val int : int t
  val float : float t
  val string : string t

  type _ tuple

  val variant : (string * 'x tuple) list -> 'x t
  val case0 : string -> string * unit tuple
  val case1 : string -> 'x t -> string * 'x tuple
  val case2 : string -> 'x t -> 'y t -> string * ('x * 'y) tuple
  val ( --> ) : string * 'x tuple -> ('x -> 'y) -> string * 'y tuple

  type _ a

  val return : 'x -> 'x a
  val ( $ ) : ('x -> 'y) a -> 'x t -> 'y a

  type args = (arg_label * expression) list

  val eval : 'x a -> args -> 'x option
end = struct
  type _ t =
    | I : int t
    | F : float t
    | S : string t
    | B : bool t
    | V : (string * 'x tuple) list -> 'x t

  and _ tuple =
    | T0 : unit tuple
    | T1 : 'x t -> 'x tuple
    | T2 : 'x t * 'y t -> ('x * 'y) tuple
    | TF : 'x tuple * ('x -> 'y) -> 'y tuple

  let int = I
  let float = F
  let string = S
  let bool = B
  let variant cases = V cases
  let case0 l = (l, T0)
  let case1 l t = (l, T1 t)
  let case2 l x y = (l, T2 (x, y))
  let ( --> ) (l, x) y = (l, TF (x, y))

  (** argument specification *)
  type _ a = R : 'a -> 'a a | A : ('a -> 'b) a * 'a t -> 'b a

  let return v = R v
  let ( $ ) f t = A (f, t)

  type args = (arg_label * expression) list

  let ( let* ) = Option.bind

  let rec parse_expression : type x. x t -> expression -> x option =
   fun t e ->
    match (t, e) with
    | I, { pexp_desc = Pexp_constant (Pconst_integer (v, _)); _ } ->
        Some (int_of_string v)
    | F, { pexp_desc = Pexp_constant (Pconst_float (v, _)); _ } ->
        Some (float_of_string v)
    | S, { pexp_desc = Pexp_constant (Pconst_string (v, _, _)); _ } -> Some v
    | B, { pexp_desc = Pexp_construct ({ txt = Lident "true"; _ }, None); _ } ->
        Some true
    | B, { pexp_desc = Pexp_construct ({ txt = Lident "false"; _ }, None); _ }
      ->
        Some true
    | V cases, { pexp_desc = Pexp_variant (l', payload); _ } -> (
        let rec eval_tuple : type x. x tuple -> expression option -> x option =
         fun t e ->
          match (t, e) with
          | T0, None -> Some ()
          | T1 t, Some x' ->
              let* x = parse_expression t x' in
              Some x
          | T2 (x, y), Some { pexp_desc = Pexp_tuple [ x'; y' ]; _ } ->
              let* x = parse_expression x x' in
              let* y = parse_expression y y' in
              Some (x, y)
          | TF (t, f), x ->
              let* x = eval_tuple t x in
              Some (f x)
          | _, _ -> None
        in
        match List.assoc_opt l' cases with
        | None -> None
        | Some t -> eval_tuple t payload)
    | _, _ -> None

  let parse_arg args label : args * expression option =
    let rec go seen args =
      match (args, label) with
      | [], _ -> (List.rev seen, None)
      | (Nolabel, e) :: args, None -> (List.rev_append seen args, Some e)
      | ((Labelled l' | Optional l'), e) :: args, Some l when String.equal l l'
        ->
          (List.rev_append seen args, Some e)
      | arg :: args, _ -> go (arg :: seen) args
    in
    go [] args

  let rec eval : type x. x a -> args -> x option =
   fun spec args ->
    match spec with
    | R x -> Some x
    | A (f, t) ->
        let args, e = parse_arg args None in
        let* e = e in
        let* x = parse_expression t e in
        let* f = eval f args in
        Some (f x)
end

module Specs : sig
  val of_expression :
    loc:location -> expression -> (Spec.t, expression) Either.t

  val all : unit -> Spec.t list
end = struct
  let found_specs : Spec.t list ref = ref []
  let all () = !found_specs

  let css_func (keys, f) =
    let f x = Spec.css keys (f x) in
    Arg_spec.return f

  let specs : (expression * string array * Spec.t Arg_spec.a) list =
    let px () = Arg_spec.(case1 "px" int --> fun x -> `px x) in
    let auto () = Arg_spec.(case0 "auto" --> fun () -> `auto) in
    let loc = Location.none in
    [
      ( [%expr Csso_lang.flex_basis],
        fst Csso_lang.flex_basis,
        Arg_spec.(css_func Csso_lang.flex_basis $ variant [ auto (); px () ]) );
      ( [%expr Csso_lang.width],
        fst Csso_lang.width,
        Arg_spec.(css_func Csso_lang.width $ variant [ px () ]) );
      ( [%expr Csso_lang.height],
        fst Csso_lang.height,
        Arg_spec.(css_func Csso_lang.height $ variant [ px () ]) );
    ]

  let specs =
    List.fold_left
      (fun acc (e, p, v) ->
        let k =
          match e with
          | {
           pexp_desc = Pexp_ident { txt = Ldot (Lident "Csso_lang", k); _ };
           _;
          } ->
              k
          | _ -> assert false
        in
        String_map.add k (e, p, v) acc)
      String_map.empty specs

  let of_expression ~loc e =
    let open Ast_builder.Default in
    let spec =
      match e with
      | [%expr use [%e? v]] -> Either.Right v
      | e -> (
          let v =
            let ( let* ) = Option.bind in
            let* k, args =
              match e with
              | {
               pexp_desc =
                 Pexp_apply
                   ({ pexp_desc = Pexp_ident { txt = Lident k; _ }; _ }, args);
               _;
              } ->
                  Some (k, args)
              | _ -> None
            in
            let* f, props, spec = String_map.find_opt k specs in
            let e = pexp_apply ~loc [%expr snd [%e f]] args in
            match Arg_spec.eval spec args with
            | None -> Some (Spec.dyn props e)
            | Some spec -> Some spec
          in
          match v with
          | Some v -> Left v
          | None ->
              Location.raise_errorf ~loc:e.pexp_loc
                "invalid style declaration, did you forget to use `use`?")
    in
    let () =
      match spec with
      | Left spec -> found_specs := spec :: !found_specs
      | Right _ -> ()
    in
    spec
end

let compile_props ~loc es =
  let open Ast_builder.Default in
  let rec go specs acc = function
    | [] -> List.rev (compile_specs specs acc)
    | e :: es -> (
        match Specs.of_expression ~loc e with
        | Either.Left spec -> go (spec :: specs) acc es
        | Right e -> go [] (e :: compile_specs specs acc) es)
  and compile_specs specs acc =
    match specs with
    | [] -> acc
    (*| specs -> Spec.to_expression ~loc (Spec.merge specs) :: acc*)
    | specs -> Spec.to_expression ~loc (Spec.merge specs) :: acc
  in
  let es = go [] [] es in
  [%expr
    let open Csso_lang in
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
      let name = String.concat " " name in
      let css = Spec.to_css spec in
      let css = String.concat "\n" css in
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
