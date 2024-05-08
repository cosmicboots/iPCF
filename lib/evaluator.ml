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

module Reduction (Ops : Moduletypes.Ops) = struct
  (** [redstep t] applies a single root reduction rule to [t].*)
  let rec redstep : 'a. 'a Parser.terms -> 'a Parser.terms =
    Parser.(
      function
      | App (Abs s, t) -> subst s t (* Beta reduction *)
      | App (IntOp op, (Box _ as t)) -> Ops.exec op t
      | Unbox (Box m, n) -> subst n m
      | Fix m -> subst m (Box (Fix m))
      | Succ (Const (Nat n)) -> Const (Nat (n + 1))
      | Pred (Const (Nat 0)) -> Const (Nat 0)
      | Pred (Const (Nat n)) -> Const (Nat (n - 1))
      | Pred (Succ t) -> t
      | IsZero (Succ _) -> Const (Bool false)
      | IsZero (Const (Nat 0)) -> Const (Bool true)
      | IsZero (Const (Nat _)) -> Const (Bool false)
      | IfThenElse (Const (Bool true), t, _) -> t
      | IfThenElse (Const (Bool false), _, f) -> f
      | t ->
        (match t with
         | App (x, y) -> App (redstep x, redstep y)
         | IfThenElse (c, t, e) -> IfThenElse (redstep c, t, e)
         | Var _ as v -> v
         | Const _ as c -> c
         | Abs s -> Abs s
         | Box _ as m -> m
         | Unbox (m, n) -> Unbox (redstep m, redstep n)
         | Fix m -> Fix m
         | Succ x -> Succ (redstep x)
         | Pred x -> Pred (redstep x)
         | IsZero x -> IsZero (redstep x)
         | IntOp _ as x -> x))
  ;;

  let%test "single reduction step" =
    let t =
      Parser.(
        App
          (App (Abs (Abs (App (Var (Some None), Var None))), Var "z"), Var "z"))
    in
    let result = redstep t in
    result = App (Abs (App (Var (Some "z"), Var None)), Var "z")
  ;;

  (** [reduce t] fully reduces the term [t]. *)
  let reduce t =
    let rec f t =
      let red = redstep t in
      if red = t then t else f red
    in
    f t
  ;;

  let%test "full reduction" =
    let t =
      Parser.(
        App
          (App (Abs (Abs (App (Var (Some None), Var None))), Var "z"), Var "z"))
    in
    let result = reduce t in
    result = App (Var "z", Var "z")
  ;;
end
