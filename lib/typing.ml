type type_var = string

type ground_type =
  | Nat
  | Bool

type types =
  | Ground of ground_type
  | Arrow of types * types
  | Box of types

(** A type scheme indicates bound type variables as polymorphic types *)
type scheme = Forall of string list * types

module Context = Map.Make (String)

let extend ctx (k, v) = Context.add k v ctx

type type_error = string

(* TODO: This is 100% the wrong type for infer, but I'm leaving it for now *)
type 'a infer = (int, type_error) result



