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
  let s = Parser.(App (Abs (App (Var None, Var None)), Var "y")) in
  match s with
  | App (Abs s, t) ->
    let result = subst s t in
    result = App (Var "y", Var "y")
  | _ -> false
;;

(** [root_reduction t] applies a single root reduction rule to [t].*)
let root_reduction
  : 'a.
  ('a -> ('a Parser.terms -> 'a Parser.terms) option)
  -> 'a Parser.terms
  -> 'a Parser.terms
  =
  fun env ->
  Parser.(
    function
    | App (Abs s, t) -> subst s t (* Beta reduction *)
    | App (Var x, t) when Option.is_some (env x) ->
      (* Intensional operation *)
      Option.get (env x) @@ t
    | Unbox (Box m, n) -> subst n m
    | Fix m -> subst m (Box (Fix m))
    | Succ (Const (Nat n)) -> Const (Nat (n + 1))
    | Pred (Const (Nat n)) -> Const (Nat (n - 1))
    | IsZero (Const (Nat 0)) -> Const (Bool true)
    | IsZero (Const (Nat _)) -> Const (Bool false)
    | IfThenElse (Const (Bool true), t, _) -> t
    | IfThenElse (Const (Bool false), _, f) -> f
    | t -> t)
;;

(** [redstep t] maps the root reduction steps over the term [t] once. *)
let rec redstep
  : 'a.
  ('a -> ('a Parser.terms -> 'a Parser.terms) option)
  -> 'a Parser.terms
  -> 'a Parser.terms
  =
  fun env t ->
  let red = root_reduction env t in
  (* TODO: [env'] should be able to expand intensional operations under bound terms as well..? *)
  let env' _ = None in
  match red with
  | App (x, y) -> App (redstep env x, redstep env y)
  | IfThenElse (c, t, e) -> IfThenElse (redstep env c, t, e)
  | Var _ as v -> v
  | Const _ as c -> c
  | Abs s -> Abs (redstep env' s)
  | Box m -> Box (redstep env m)
  | Unbox (m, n) -> Unbox (redstep env m, redstep env' n)
  | Fix m -> Fix (redstep env' m)
  | Succ x -> Succ (redstep env x)
  | Pred x -> Pred (redstep env x)
  | IsZero x -> IsZero (redstep env x)
;;

let%test "single reduction step" =
  let t =
    Parser.(
      App (App (Abs (Abs (App (Var (Some None), Var None))), Var "z"), Var "z"))
  in
  let result = redstep (fun _ -> None) t in
  result = App (Abs (App (Var (Some "z"), Var None)), Var "z")
;;

(** [reduce t] fully reduces the term [t]. *)
let reduce env t =
  let rec f t =
    let red = redstep env t in
    if red = t then t else f red
  in
  f t
;;

let%test "full reduction" =
  let t =
    Parser.(
      App (App (Abs (Abs (App (Var (Some None), Var None))), Var "z"), Var "z"))
  in
  let result = reduce (fun _ -> None) t in
  result = App (Var "z", Var "z")
;;
