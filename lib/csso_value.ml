let flex_basis = function `auto -> "auto" | `px n -> string_of_int n ^ "px"
let width = function `px n -> string_of_int n ^ "px"
let height = function `px n -> string_of_int n ^ "px"

let aspect_ratio = function
  | `auto -> "auto"
  | `square -> "1 / 1"
  | `video -> "16 / 9"
  | `ratio (n, m) -> string_of_int n ^ " / " ^ string_of_int m

let break_after = function
  | `auto -> "auto"
  | `avoid -> "avoid"
  | `all -> "all"
  | `avoid_page -> "avoid-page"
  | `page -> "page"
  | `left -> "left"
  | `right -> "right"
  | `column -> "column"

let break_before = break_after

let break_inside = function
  | `auto -> "auto"
  | `avoid -> "avoid"
  | `avoid_page -> "avoid-page"
  | `avoid_column -> "avoid-column"

let box_decoration_break = function `slice -> "slice" | `clone -> "clone"

let box_sizing = function
  | `content_box -> "content-box"
  | `border_box -> "border-box"

let display = function
  | `block -> "block"
  | `inline_block -> "inline-block"
  | `inline -> "inline"
  | `flex -> "flex"
  | `inline_flex -> "inline-flex"
  | `table -> "table"
  | `inline_table -> "inline-table"
  | `table_caption -> "table-caption"
  | `table_cell -> "table-cell"
  | `table_column -> "table-column"
  | `table_column_group -> "table-column-group"
  | `table_footer_group -> "table-footer-group"
  | `table_header_group -> "table-header-group"
  | `table_row_group -> "table-row-group"
  | `table_row -> "table-row"
  | `flow_root -> "flow-root"
  | `grid -> "grid"
  | `inline_grid -> "inline-grid"
  | `contents -> "contents"
  | `list_item -> "list-item"
  | `hidden -> "none"

let float = function
  | `inline_start -> "inline-start"
  | `inline_end_ -> "inline-end"
  | `right -> "right"
  | `left -> "left"
  | `none -> "none"

let clear = function
  | `inline_start -> "inline-start"
  | `inline_end -> "inline-end"
  | `left -> "left"
  | `right -> "right"
  | `both -> "both"
  | `none -> "none"

let isolation = function `isolate -> "isolate" | `auto -> "auto"

let object_fit = function
  | `contain -> "contain"
  | `cover -> "cover"
  | `fill -> "fill"
  | `none -> "none"
  | `scale_down -> "scale-down"

let object_position = function
  | `bottom -> "bottom"
  | `center -> "center"
  | `left -> "left"
  | `left_bottom -> "left bottom"
  | `left_top -> "left top"
  | `right -> "right"
  | `right_bottom -> "right bottom"
  | `right_top -> "right top"
  | `top -> "top"

let overflow = function
  | `auto -> "auto"
  | `hidden -> "hidden"
  | `clip -> "clip"
  | `visible -> "visible"
  | `scroll -> "scroll"

let overflow_x = overflow
let overflow_y = overflow

let overscoll_behavior = function
  | `auto -> "auto"
  | `contain -> "contain"
  | `none -> "none"

let overscroll_behavior_x = overscoll_behavior
let overscroll_behavior_y = overscoll_behavior

let position = function
  | `static -> "static"
  | `fixed -> "fixed"
  | `absolute -> "absolute"
  | `relative -> "relative"
  | `sticky -> "sticky"

let visibility = function
  | `visible -> "visible"
  | `hidden -> "hidden"
  | `collapse -> "collapse"

let z_index = function `auto -> "auto" | `z n -> string_of_int n
