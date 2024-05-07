module type Eval = sig
  val reduce : 'a Parser.terms -> 'a Parser.terms
  val redstep : 'a Parser.terms -> 'a Parser.terms
end

module type Ops = sig
  type t = Parser.IntOps.t

  type gt =
    [ `Bool
    | `Nat
    ]

  type types =
    [ `Box of types
    | `Arrow of types * types
    | `Ground of gt
    | `Var
    ]

  val exec : t -> 'a Parser.terms -> 'a Parser.terms
  val get_type : t -> types
end
