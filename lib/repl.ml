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
      let result = cmd |> Lexer.lex |> Parser.parse |> Evaluator.reduce in
      print_endline @@ [%derive.show: string Parser.terms] result;
      loop ()
  in
  loop ()
;;
