(** [subst s t] substitutes the the term [t] in the term [s].

    This is the shown as [s[t/x]] in written notation where [x] is a variable
    identifier.*)
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
   | App (Abs s, t) ->
     let result = subst s t in
     print_endline @@ [%derive.show: string Parser.terms] result
   | _ -> raise @@ Invalid_argument "This should never happend");
  [%expect {| (Parser.App ((Parser.Var "y"), (Parser.Var "y"))) |}]
;;

(** [root_reduction t] applies a single root reduction rule to [t].*)
let root_reduction =
  Parser.(
    function
    | App (Abs s, t) -> subst s t (* Beta reduction *)
    | IfThenElse (Const (Bool True), t, _) -> t
    | IfThenElse (Const (Bool False), _, f) -> f
    | Let (m, n) -> subst n m
    | Fix m -> subst m (Box (Fix m))
    (* TODO: Add natural number rules *)
    | _ -> raise @@ Invalid_argument "No root reduction rule applies")
;;

(* TODO: Reduction steps *)

(* TODO: Top level reduction function.
   This function might require deriving [eq] for abstract types, which isn't
   possible in OCaml.
   This might end up being a problem, and could require a custom equality
   function that can operate on ['a terms] types.
*)
