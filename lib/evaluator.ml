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

let%test "subst" =
  let s = Parser.parse @@ Lexer.lex {|(\ x. x x) y|} in
  match s with
  | App (Abs s, t) ->
    let result = subst s t in
    result = App (Var "y", Var "y")
  | _ -> false
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

(** [redstep t] maps the root reduction steps over the term [t] once. *)
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

let%test "single reduction step" =
  let t = Parser.parse @@ Lexer.lex {|(\ x . (\ y . x y)) z z|} in
  let result = redstep t in
  result = App (Abs (App (Var (Some "z"), Var None)), Var "z")
;;

(** [reduce t] fully reduces the term [t]. *)
let reduce t =
  let rec f t =
    let red = redstep t in
    (* ppx_deriving eq is used rather than Stdlib.(=) because ppx_deriving eq
       is a short-circuiting function, which is faster in theory and it's
       guaranteed not to raise at runtime. *)
    if Parser.equal_terms String.equal red t then t else f red
  in
  f t
;;

let%test "full reduction" =
  let t = Parser.parse @@ Lexer.lex {|(\ x . (\ y . x y)) z z|} in
  let result = reduce t in
  result = App (Var "z", Var "z")
;;
