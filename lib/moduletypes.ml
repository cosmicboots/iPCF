module type Eval = sig
  (** Evaluator module signature *)

  (** [reduce t] reduces the term [t] to normal form. *)
  val reduce : 'a Parser.terms -> 'a Parser.terms

  (** [redstep t] reduces the term [t] by one reduction step. *)
  val redstep : 'a Parser.terms -> 'a Parser.terms
end

module type Ops = sig
  (** Intensional operations module signature *)

  (** The type of intensional operations. *)
  type t = Parser.IntOps.t

  (** The ground types used in intensional operations. *)
  type gt =
    [ `Bool
    | `Nat
    ]

  (** The type signature of intensional operations. *)
  type types =
    [ `Box of types
    | `Arrow of types * types
    | `Ground of gt
    | `Var
    ]

  (** [exec o t] executes the intensional operation [o] on the term [t]. *)
  val exec : t -> 'a Parser.terms -> 'a Parser.terms

  (** [get_type o] returns the type of the intensional operation [o]. *)
  val get_type : t -> types
end
