type type_error = NotInContext

val pp_type_error : Format.formatter -> type_error -> unit
val show_type_error : type_error -> string

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

val check
  :  'a context
  -> 'a Parser.terms
  -> (Type.t * ConstraintCtx.t, type_error) result

val infer_type : 'a context -> 'a Parser.terms -> (Type.t, type_error) result
