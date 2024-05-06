let is_app : 'a. 'a Parser.terms -> 'a Parser.terms = function
  | Parser.Box (App _) -> Parser.(Box (Const (Bool true)))
  | _ -> Parser.(Box (Const (Bool false)))
;;

let is_abs : 'a. 'a Parser.terms -> 'a Parser.terms = function
  | Parser.Box (Abs _) -> Parser.(Box (Const (Bool true)))
  | _ -> Parser.(Box (Const (Bool false)))
;;

let number_of_vars : 'a. 'a Parser.terms -> 'a Parser.terms =
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
  | Box t -> Parser.Const (Nat (f t))
  | _ -> Parser.Const (Nat 0)
;;

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

  let t =
    [ "isApp", (is_app, `Arrow (`Box `Var,  `Box (`Ground `Bool)))
    ; "isAbs", (is_abs, `Arrow (`Box `Var, `Box (`Ground `Bool)))
    ; "numberOfVars", (number_of_vars, `Arrow (`Box `Var, `Ground `Nat))
    ]
  ;;

  let keys = List.map fst t
end
