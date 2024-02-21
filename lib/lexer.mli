type t

val pp : Format.formatter -> t -> unit
val show : t -> string
val lex : string -> t list
