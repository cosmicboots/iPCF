let run () =
  print_endline
    {|
=========================
Welcome to the iPCF REPL!
=========================

You can exit the REPL with either [exit] or [CTRL+D]
|};
  let rec loop () =
    let cmd =
      try Ocamline.read ~prompt:"iPCF>" ~brackets:[ '(', ')' ] () with
      | End_of_file -> "quit"
      | Stdlib.Sys.Break -> ""
    in
    match cmd with
    | "quit" -> ()
    | "" -> loop ()
    | _ ->
      let ast = cmd |> Lexer.lex |> Parser.parse in
      let eval_res = ast |> Evaluator.reduce in
      let eval_str = [%derive.show: string Parser.terms] eval_res in
      Printf.printf "%s : " eval_str;
      let t = ast |> Typing.infer_type Typing.init_context in
      Printf.printf "%s\n%!" @@ Typing.Type.show t;
      loop ()
  in
  loop ()
;;
