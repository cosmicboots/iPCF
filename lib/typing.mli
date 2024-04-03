module Type : sig
  type ground_type =
    | Nat
    | Bool

  type t

  val compare : t -> t -> int
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

type 'a context

module ConstraintCtx : sig
  type t
end

val init_context : 'a context
val check : 'a context -> 'a Parser.terms -> Type.t * ConstraintCtx.t
val infer_type : 'a context -> 'a Parser.terms -> Type.t
