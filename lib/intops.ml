module Operations = struct
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

  (* Mutual recursion used to allow nested intensional operations *)
  let rec t =
    [ "isApp", (is_app, `Arrow (`Box `Var, `Box (`Ground `Bool)))
    ; "isAbs", (is_abs, `Arrow (`Box `Var, `Box (`Ground `Bool)))
    ; "numberOfVars", (number_of_vars, `Arrow (`Box `Var, `Box (`Ground `Nat)))
    ; "isNormalForm", (is_normal_form, `Arrow (`Box `Var, `Box (`Ground `Bool)))
    ]

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
    in
    match t with
    | Box t -> Parser.(Box (Const (Nat (f t))))
    | _ -> Parser.Const (Nat 0)

  (* This function takes [string Parser.terms] which would be the type of closed terms *)
  and is_normal_form = function
    | Box e ->
      let e' =
        Evaluator.reduce (fun x -> List.assoc_opt x t |> Option.map fst) e
      in
      Parser.Box (Const (Bool (e = e')))
    | _ ->
      raise
        (Invalid_argument
           "If this is reached, the typechecker failed elsewhere")
  ;;

  let exec (name : string) (args : 'a Parser.terms) : 'a Parser.terms =
    let f, _ = List.assoc name t in
    f args
  ;;

  (** [contains x] is [true] if [x] is a valid intensional operation *)
  let contains x = List.mem_assoc x t

  let keys = List.map fst t
end

let%test "is_normal_form" =
  (Operations.is_normal_form
     Parser.(Box (App (Abs (Var None), Const (Bool true))))
   = Parser.(Box (Const (Bool false))))
  && Operations.is_normal_form Parser.(Box (Const (Bool true)))
     = Parser.(Box (Const (Bool true)))
;;

let%test "number_of_vars" =
  Operations.number_of_vars Parser.(Box (Abs (Abs (Var None))))
  = Parser.(Box (Const (Nat 1)))
;;
