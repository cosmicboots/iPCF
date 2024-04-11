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
let root_reduction =
  Parser.(
    function
    | App (Abs s, t) -> subst s t (* Beta reduction *)
    | IfThenElse (Const (Bool True), t, _) -> t
    | IfThenElse (Const (Bool False), _, f) -> f
    | Let (Box m, n) -> subst n m
    | Fix m -> subst m (Box (Fix m))
    | Succ (Const (Nat n)) -> Const (Nat (Succ n))
    | Pred (Const (Nat (Succ n))) -> Const (Nat n)
    | IsZero (Const (Nat Zero)) -> Const (Bool True)
    | IsZero (Const (Nat _)) -> Const (Bool False)
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
  | Let (m, n) -> Let (redstep m, redstep n)
  | Fix m -> Fix (redstep m)
  | Succ x -> Succ (redstep x)
  | Pred x -> Pred (redstep x)
  | IsZero x -> IsZero (redstep x)
;;

(* Quoted terms *)
type 'a values =
  | NVal of int
  | BVal of bool
  | FVal of ('a -> 'a Parser.terms)
  | QVal of 'a Parser.terms
  | Error

let lift_nat : 'a. (int -> 'a values) -> 'a values -> 'a values =
  fun f -> function
  | NVal x -> f x
  | _ -> Error
;;

let rec eval : 'a. ('a -> 'a values) -> 'a Parser.terms -> 'a values =
  fun env t ->
  match t with
  | Var x -> env x
  | Const (Nat x) -> NVal (Parser.nat_to_int x)
  | Const (Bool b) ->
    BVal
      (match b with
       | True -> true
       | False -> false)
  | Succ x -> lift_nat (fun x -> NVal (x + 1)) (eval env x)
  | Pred x -> lift_nat (fun x -> NVal (x - 1)) (eval env x)
  | App (t1, t2) -> ()
  | Abs t ->
    FVal
      (fun x ->
        Parser.bind_terms
          (function
            | None -> Var x
            | Some y -> Var y)
          t)
  | IfThenElse (c, t, f) ->
    (match eval env c with
     | BVal true -> eval env t
     | BVal false -> eval env f
     | _ -> Error)
  | IsZero x -> lift_nat (fun x -> BVal (x = 0)) (eval env x)
  (* Quoted terms *)
  | Box x -> QVal x
  | Let (x, y) -> ()
  | Fix _ -> Error
;;

let%test "single reduction step" =
  let t =
    Parser.(
      App (App (Abs (Abs (App (Var (Some None), Var None))), Var "z"), Var "z"))
  in
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
  let t =
    Parser.(
      App (App (Abs (Abs (App (Var (Some None), Var None))), Var "z"), Var "z"))
  in
  let result = reduce t in
  result = App (Var "z", Var "z")
;;
