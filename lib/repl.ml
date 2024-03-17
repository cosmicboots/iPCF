let run () =
  Printf.printf {|
=========================
Welcome to the iPCF REPL!
=========================

You can exit the REPL with either [exit] or [CTRL+D]
|};
  let rec loop () =
    Printf.printf "> ";
    let cmd = read_line () in
    match cmd with
    | "quit" -> ()
    | _ ->
      let result = cmd |> Lexer.lex |> Parser.parse |> Evaluator.reduce in
      Printf.printf " %s\n" @@ [%derive.show: string Parser.terms] result;
      loop ()
  in
  loop ()
;;
