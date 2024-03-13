(** [subst s t] substitutes the the term [t] in the term [s].

    This is the shown as [[s/t]] in written notation.*)
let subst s t =
  Parser.bind_terms
    (function
      | Some x -> Var x
      | None -> t)
    s
;;

let%expect_test "subst" =
  let s = Parser.parse @@ Lexer.lex {|(\ x. x x) y|} in
  (match s with
   | App ((Abs s), t) ->
     let result = subst s t in
     print_endline @@ [%derive.show: string Parser.terms] result
   | _ -> raise @@ Invalid_argument "");
  [%expect {| (Parser.App ((Parser.Var "y"), (Parser.Var "y"))) |}]
;;
