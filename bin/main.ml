open Cmdliner
open Ipcf
open Printf

let demo () =
  print_endline " === Demo ===";
  let prog = {|( \ x . ( \y . x y ) ) z z|} in
  printf "Original program:\n %s\n\n" prog;
  let tokens = Lexer.lex prog in
  printf "Tokens:\n %s\n\n" @@ [%derive.show: Lexer.t list] tokens;
  let parsed = Parser.parse tokens in
  printf "Parsed expression:\n %s\n\n"
  @@ [%derive.show: string Parser.terms] parsed;
  let evald = Evaluator.reduce parsed in
  printf "Evaluated expression:\n %s\n\n"
  @@ [%derive.show: string Parser.terms] evald
;;

let debug =
  Arg.(
    value
    & flag
    & info [ "debug" ] ~doc:"Print internal representation of terms")
;;

let repl_t = Term.(const Repl.run $ debug)
let repl_cmd = Cmd.v (Cmd.info "repl" ~doc:"Start the interactive REPL") repl_t

let demo_cmd =
  Cmd.v
    (Cmd.info "demo" ~doc:"Run some demo expressions")
    Term.(const demo $ const ())
;;

let cmd =
  Cmd.group ?default:(Some repl_t) (Cmd.info "ipcf") [ repl_cmd; demo_cmd ]
;;

let () = exit @@ Cmd.eval cmd
