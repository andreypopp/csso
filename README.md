# csso

Atomic CSS for OCaml (and ReasonML).

## Usage

Specify CSS styles using OCaml syntax, either as let-bindings on the top level:
```ocaml
let%csso box =
  flex_basis `auto;
  width (`px 100)
```

or as expression in the `[%csso ...]` extension:
```ocaml
let square n = [%csso height (`px n); width (`px n)]
```

or directly in the `csso` attribute:
```ocaml
module Page = struct
  let make () =
    <div csso=[ box; square 100 ]>
      (React.string "hello!")
    </div>
    [@@react.component]
end
```

Configure `csso.ppx` preprocessor in `dune` file:
```lisp
(melange.emit
 (preprocess
  (pps csso.ppx reason-react-ppx))
 (libraries reason-react)
 (target commonjs))
```

Setup a dune rule to generate extracted CSS:
```lisp
(rule
 (target main.css)
 (mode promote)
 (deps
  (glob_files *.pp.ml))
 (action
  (with-stdout-to
   %{target}
   (run csso %{deps}))))
```

The extracted CSS will look like:
```css
.flex-basis-auto { flex-basis: auto; }
.height-dynamic { height: var(--height); }
.width-100px { width: 100px; }
.width-dynamic { width: var(--width); }
```

and the generated HTML for the React component:
```html
<div style="--width:100px;--height:100px"
     class="flex-basis-auto width-dynamic height-dynamic">
  hello!
</div>
```
