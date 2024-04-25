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
    | IfThenElse (Const (Bool true), t, _) -> t
    | IfThenElse (Const (Bool false), _, f) -> f
    | Unbox (Box m, n) -> subst n m
    | Fix m -> subst m (Box (Fix m))
    | Succ (Const (Nat n)) -> Const (Nat (n + 1))
    | Pred (Const (Nat n)) -> Const (Nat (n - 1))
    | IsZero (Const (Nat 0)) -> Const (Bool true)
    | IsZero (Const (Nat _)) -> Const (Bool false)
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
  | Unbox (m, n) -> Unbox (redstep m, redstep n)
  | Fix m -> Fix (redstep m)
  | Succ x -> Succ (redstep x)
  | Pred x -> Pred (redstep x)
  | IsZero x -> IsZero (redstep x)
;;

type eval_error =
  | Eval_error of string
  | Not_implemented

let lift_nat f = function
  | Parser.Const (Nat x) -> Ok (f x)
  | _ -> Error (Eval_error "Failed to lift nat")
;;

let rec eval
  : 'a.
  ('a -> 'a Parser.terms)
  -> 'a Parser.terms
  -> ('a Parser.terms, eval_error) result
  =
  fun env t ->
  match t with
  | Parser.Var x -> Ok (env x)
  | Const _ as x -> Ok x
  | Succ x ->
    Result.bind (eval env x) (fun r ->
      lift_nat (fun x -> Parser.Const (Nat (x + 1))) r)
  | Pred x ->
    Result.bind (eval env x) (fun r ->
      lift_nat (fun x -> Parser.Const (Nat (x - 1))) r)
  (* Maybe this is the right rule for app...?
     Need to double check that subst doesn't do anything weird in relation to
     quoted values. *)
  | App (Abs t1, t2) -> eval env (subst t1 t2)
  | App (_t1, _t2) -> Error (Eval_error "Can't apply a non-abstraction")
  | Abs _ as x ->
    Ok x (* Not sure that evaluation should occur inside an abstraction *)
  | IfThenElse (c, t, f) ->
    let ( let* ) = Result.bind in
    let* res = eval env c in
    (match res with
     | Const (Bool true) -> eval env t
     | Const (Bool false) -> eval env f
     | _ ->
       Error
         (Eval_error "Failed to evaluate if-then-else. Conditional not a bool."))
  | IsZero x ->
    Result.bind (eval env x) (fun r ->
      lift_nat (fun x -> Parser.Const (Bool (x = 0))) r)
  (* Quoted terms *)
  | Box _ as x -> Ok x
  (* Unboxes the term m into the term n *)
  | Unbox (Box m, _n) ->
    let ( let* ) = Result.bind in
    let* m' = eval env m in
    Ok (subst _n m')
  | Unbox _ -> Error (Eval_error "Can't unbox a non-boxed term")
  | Fix _ -> Error Not_implemented
;;

let%test "eval" =
  let t : string Parser.terms =
    Parser.(Unbox (Box (Const (Nat 5)), Var None))
  in
  let result =
    Result.get_ok @@ eval (fun _ -> raise (Invalid_argument "")) t
  in
  Printf.printf "%s\n" ([%show: string Parser.terms] result);
  result = Parser.(Const (Nat 5))
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
