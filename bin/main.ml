open Ipcf

let tokens = Lexer.lex "\\ x y z ()"

let () = print_endline @@ [%derive.show: Lexer.t list] tokens
