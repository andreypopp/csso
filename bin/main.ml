let extract_css css filename =
  match Ppxlib.Ast_io.read_binary filename with
  | Error _ -> css
  | Ok t -> (
      match Ppxlib.Ast_io.get_ast t with
      | Impl s -> Csso_ppx.find_css s css
      | Intf _ -> css)

let () =
  let css =
    Array.to_seq Sys.argv |> Seq.drop (* prog name *) 1
    |> Seq.fold_left extract_css Csso_ppx.String_map.empty
  in
  Csso_ppx.String_map.iter (fun _ -> print_endline) css
