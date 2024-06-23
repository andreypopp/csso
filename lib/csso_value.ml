let flex_basis = function `auto -> "auto" | `px n -> string_of_int n ^ "px"
let width = function `px n -> string_of_int n ^ "px"
let height = function `px n -> string_of_int n ^ "px"
