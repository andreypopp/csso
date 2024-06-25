type 'a cssgen = string array * ('a -> string array)

let flex_basis : [ `auto | `px of int ] cssgen =
  let f = function
    | `auto -> [| "auto" |]
    | `px n -> [| string_of_int n ^ "px" |]
  in
  ([| "flex-basis" |], f)

let width : [ `px of int ] cssgen =
  let f = function `px n -> [| string_of_int n ^ "px" |] in
  ([| "width" |], f)

let height : [ `px of int ] cssgen =
  let f = function `px n -> [| string_of_int n ^ "px" |] in
  ([| "height" |], f)
