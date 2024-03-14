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
    | t -> t)
;;

(* TODO: Reduction steps *)
let rec redstep : 'a. 'a Parser.terms -> 'a Parser.terms =
  fun t ->
  let red = root_reduction t in
  match red with
  | App (x, y) -> App (redstep x, redstep y)
  | IfThenElse (c, t, e) -> IfThenElse (redstep c, t, e)
  | Var _ as v -> v
  | Const _ as c -> c
  | Abs s -> Abs (redstep s)
  | Box m -> Box (redstep m)
  | Let (m, n) -> Let (redstep m, n)
  | Fix m -> Fix (redstep m)
  | Succ x -> Succ (redstep x)
;;

let%expect_test "single reduction step" =
  let t = Parser.parse @@ Lexer.lex {|(\ x . (\ y . x y)) z z|} in
  let result = redstep t in
  print_endline @@ [%derive.show: string Parser.terms] result;
  [%expect {|
    (Parser.App (
       (Parser.Abs (Parser.App ((Parser.Var (Some "z")), (Parser.Var None)))),
       (Parser.Var "z"))) |}]
;;

(* TODO: Top level reduction function.
   This function might require deriving [eq] for abstract types, which isn't
   possible in OCaml.
   This might end up being a problem, and could require a custom equality
   function that can operate on ['a terms] types.
*)
