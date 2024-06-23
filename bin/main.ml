let extract_stylesheet s filename =
  if String.ends_with filename ~suffix:".css" then
    Csso_ppx.Stylesheet.of_css filename s
  else if String.ends_with filename ~suffix:".pp.ml" then
    Csso_ppx.Stylesheet.of_ml filename s
  else failwith "Expected .css or .pp.ml file"

let () =
  let stylesheet =
    Array.to_seq Sys.argv |> Seq.drop (* prog name *) 1
    |> Seq.fold_left extract_stylesheet Csso_ppx.Stylesheet.empty
  in
  Csso_ppx.Stylesheet.output_css stdout stylesheet
