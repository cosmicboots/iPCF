module Type : sig
  type ground_type =
    | Nat
    | Bool

  type t

  val compare : t -> t -> int
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

type constraint_ctx
type 'a context

val init_context : 'a context
val check : 'a context -> 'a Parser.terms -> Type.t * constraint_ctx
