let run debug =
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
      (* Lexing *)
      let tokens = Lexer.lex cmd in
      (* Parsing *)
      let ast = Parser.parse tokens in
      (* Type inference *)
      let t = ast |> Typing.infer_type Typing.init_context in
      (* Evaluation *)
      let eval_res = ast |> Evaluator.reduce in
      let eval_str =
        if not debug
        then Parser.show_terms eval_res
        else [%show: string Parser.terms] eval_res
      in
      (* Print results *)
      ANSITerminal.(
        printf [ magenta ] "%s" eval_str;
        printf [ blue ] " : ";
        printf [ green ] "%s" (Typing.Type.show t));
      Printf.printf "\n%!";
      loop ()
  in
  loop ()
;;
