let ( let* ) = Result.bind

module Type = struct
  type ground_type =
    | Nat
    | Bool
  [@@deriving show, ord]

  type t =
    | Ground of ground_type
    | Arrow of t * t
    (** [Arrow t1 t2] represents the function type from [t1] to [t2] *)
    | Box of t
  [@@deriving show, ord]

  type error =
    | Mismatch of t * t
    (** [Mismatch (t1, t2)] indicates that [t1] was expected but [t2] was
        found. *)
    | NotFunction of t
    | NotInScope of string
    | Unimplemented
  [@@deriving show]
end

(* Ensure we can use types as keys in a map *)
module _ : Map.OrderedType = Type

(** A context is a mapping from variables to types *)
type 'a context = 'a -> Type.t

(** A type environment is a mapping from types to types *)
module TypeEnv = struct
  include Map.Make (Type) [@@deriving show]

  (** [show env] pretty-prints the type environment [env] *)
  let show env =
    List.fold_left
      (fun acc (k, v) ->
        Printf.sprintf "%s\n%s -> %s" acc (Type.show k) (Type.show v))
      ""
      (bindings env)
  ;;
end

type type_env = Type.t TypeEnv.t

let rec check
  : 'a. 'a context -> 'a Parser.terms -> (Type.t * type_env, Type.error) result
  =
  fun ctx -> function
  | Var x -> Ok (ctx x, TypeEnv.empty)
  (*
     | Abs t ->
     let ctx' = Option.fold ctx in
     let* t' = check t in
     Ok (Arrow (x, t'))
  *)
  | Const (Nat _) -> Ok (Ground Nat, TypeEnv.empty)
  | Const (Bool _) -> Ok (Ground Bool, TypeEnv.empty)
  | Succ t ->
    (match check ctx t with
     | Ok (Ground Nat, _env) -> Ok (Ground Nat, TypeEnv.empty)
     | Ok (t, _env) -> Error (Mismatch (Ground Nat, t))
     | Error e -> Error e)
  | App (t1, t2) ->
    let* t1', _env1 = check ctx t1 in
    let* t2', _env1 = check ctx t2 in
    (match t1' with
     | Arrow (a, b) when a = t2' -> Ok (b, TypeEnv.empty)
     | Arrow (a, _) -> Error (Mismatch (a, t2'))
     | _ -> Error (NotFunction t1'))
  | IfThenElse (c, t, e) ->
    let* c', _env = check ctx c in
    if c' = Ground Bool
    then
      let* t', _env = check ctx t in
      let* e', _env = check ctx e in
      if t' = e' then Ok (t', TypeEnv.empty) else Error (Mismatch (t', e'))
    else Error (Mismatch (Ground Bool, c'))
  | _ -> Error Unimplemented
;;

(*
   let%test "if then else" =
  List.fold_left
    (fun acc (tst, sol) -> acc && check (Parser.parse @@ Lexer.lex tst) = sol)
    true
    [ "if true then 0 else succ 0", Ok (Ground Nat)
    ; "if 0 then 0 else succ 0", Error (Mismatch (Ground Bool, Ground Nat))
    ; "if true then 0 else true", Error (Mismatch (Ground Nat, Ground Bool))
    ]
;;

let%test "check natural" =
  List.fold_left
    (fun acc (tst, sol) -> acc && check (Parser.parse @@ Lexer.lex tst) = sol)
    true
    [ "succ true", Error (Mismatch (Ground Nat, Ground Bool))
    ; "succ 0", Ok (Ground Nat)
    ]
;;
*)
module Subst = Map.Make (String)

(** A type scheme indicates bound type variables as polymorphic types *)
type scheme = Forall of string list * Type.t

(* TODO: This is probably the wrong type for infer, but I'm leaving it for now *)
module Infer = struct
  type t = { next_var_id : int }
end
