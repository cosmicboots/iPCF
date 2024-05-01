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

type eval_error =
  | Eval_error of string
  | Not_implemented
[@@deriving show]

let lift_nat f = function
  | Parser.Const (Nat x) -> Ok (f x)
  | _ -> Error (Eval_error "Failed to lift nat")
;;

type 'a operation =
  | Term of 'a Parser.terms
  | IntOp of ('a Parser.terms -> 'a Parser.terms)

let rec eval
  : 'a.
  ('a -> 'a operation)
  -> 'a Parser.terms
  -> ('a Parser.terms, eval_error) result
  =
  fun env t ->
  let ( let* ) = Result.bind in
  match t with
  | Parser.Var x as x' ->
    (match env x with
     | Term t -> eval env t
     | IntOp _ ->
       (* Ignore intensional operations as those are handled in applications *)
       Ok x')
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
  | App (t1, t2) ->
    (* Reduce the first term *)
    let* t1' = eval env t1 in
    (match t1' with
     (* We can directly apply the abstracion substitution *)
     | Abs t -> eval env (subst t t2)
     (* Otherwise, we have to check if it's an intensional operation *)
     | Var x ->
       (match env x with
        | IntOp f -> Ok (f t2)
        | _ -> Error (Eval_error "Failed to evaluate application."))
     | _ -> Error (Eval_error "Failed to evaluate application."))
  | Abs _ as x ->
    Ok x (* Not sure that evaluation should occur inside an abstraction *)
  | IfThenElse (c, t, f) ->
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
  | Fix m -> Ok (subst m (Box (Fix m)))
;;

let%test "eval" =
  let t : string Parser.terms =
    Parser.(Unbox (Box (Const (Nat 5)), Var None))
  in
  let result =
    Result.get_ok @@ eval (fun _ -> raise (Invalid_argument "")) t
  in
  result = Parser.(Const (Nat 5))
;;
