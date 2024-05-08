module Operations (E : Moduletypes.Eval) = struct
  include Parser.IntOps

  type gt =
    [ `Bool
    | `Nat
    ]

  (** This internal type is used to spell out custom types for any intensional operation *)
  type types =
    [ `Box of types
    | `Arrow of types * types
    | `Ground of gt
    | `Var
    ]

  let get_type = function
    | IsApp -> `Arrow (`Box `Var, `Box (`Ground `Bool))
    | IsAbs -> `Arrow (`Box `Var, `Box (`Ground `Bool))
    | NumberOfVars -> `Arrow (`Box `Var, `Box (`Ground `Nat))
    | IsNormalForm -> `Arrow (`Box `Var, `Ground `Bool)
    | Tick -> `Arrow (`Box `Var, `Box `Var)
  ;;

  (* Mutual recursion used to allow nested intensional operations *)
  let rec exec op t =
    match op with
    | IsApp -> is_app t
    | IsAbs -> is_abs t
    | NumberOfVars -> number_of_vars t
    | IsNormalForm -> is_normal_form t
    | Tick -> one_step_reduction t

  and is_app : 'a. 'a Parser.terms -> 'a Parser.terms = function
    | Parser.Box (App _) -> Parser.(Box (Const (Bool true)))
    | _ -> Parser.(Box (Const (Bool false)))

  and is_abs : 'a. 'a Parser.terms -> 'a Parser.terms = function
    | Parser.Box (Abs _) -> Parser.(Box (Const (Bool true)))
    | _ -> Parser.(Box (Const (Bool false)))

  and number_of_vars : 'a. 'a Parser.terms -> 'a Parser.terms =
    fun t ->
    let rec f : 'a. 'a Parser.terms -> int =
      fun term ->
      match term with
      | Parser.Var _ -> 1
      | App (t1, t2) -> f t1 + f t2
      | Abs t -> f t
      | Box _ -> 0
      | Const _ -> 0
      | Succ x -> f x
      | Pred x -> f x
      | IfThenElse (c, t, e) -> f c + f t + f e
      | IsZero x -> f x
      | Unbox (_, n) -> f n
      | Fix _ -> 0
      | IntOp _ -> 0
    in
    match t with
    | Box t -> Parser.(Box (Const (Nat (f t))))
    | _ -> Parser.Const (Nat 0)

  (* This function takes [string Parser.terms] which would be the type of closed terms *)
  and is_normal_form = function
    | Box e ->
      let e' = E.reduce e in
      Parser.(Const (Bool (e = e')))
    | _ ->
      raise
        (Invalid_argument
           "If this is reached, the typechecker failed elsewhere")

  and one_step_reduction = function
    | Parser.Box e ->
      let e' = E.redstep e in
      Parser.Box e'
    | _ ->
      raise
        (Invalid_argument
           "If this is reached, the typechecker failed elsewhere")
  ;;

  (*
     let%test "is_normal_form" =
     (is_normal_form Parser.(Box (App (Abs (Var None), Const (Bool true))))
     = Parser.(Box (Const (Bool false))))
     && is_normal_form Parser.(Box (Const (Bool true)))
     = Parser.(Box (Const (Bool true)))
     ;;
  *)

  let%test "number_of_vars" =
    number_of_vars Parser.(Box (Abs (Abs (Var None))))
    = Parser.(Box (Const (Nat 1)))
  ;;
end
