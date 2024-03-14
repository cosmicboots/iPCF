open Ipcf
open Printf

let () = print_endline " === Demo ==="
let prog = {|( \ x . ( \y . x y ) ) z z|}
let () = printf "Original program:\n %s\n\n" prog
let tokens = Lexer.lex prog
let () = printf "Tokens:\n %s\n\n" @@ [%derive.show: Lexer.t list] tokens
let parsed = Parser.parse tokens

let () =
  printf "Parsed expression:\n %s\n\n"
  @@ [%derive.show: string Parser.terms] parsed
;;

let evald = Evaluator.reduce parsed

let () =
  printf "Evaluated expression:\n %s\n\n"
  @@ [%derive.show: string Parser.terms] evald
;;
