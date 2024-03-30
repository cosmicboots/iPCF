let ( let* ) = Result.bind

module Type = struct
  type ground_type =
    | Nat
    | Bool
  [@@deriving show { with_path = false }, ord]

  type t =
    | Ground of ground_type
    | Arrow of t * t
    (** [Arrow t1 t2] represents the function type from [t1] to [t2] *)
    | Box of t
    | Forall of int
    (** [Forall s] represents a polymorphic type with the name [s] *)
  [@@deriving ord]

  let show t =
    let rec show' = function
      | Ground x -> show_ground_type x
      (* Second arrow case is to handle order of operation parentheses *)
      | Arrow ((Arrow _ as t1), t2) ->
        Printf.sprintf "(%s) -> %s" (show' t1) (show' t2)
      | Arrow (t1, t2) -> Printf.sprintf "%s -> %s" (show' t1) (show' t2)
      | Box t -> Printf.sprintf "Box %s" (show' t)
      | Forall s -> Printf.sprintf "'%c" @@ Char.chr (s + Char.code 'a')
    in
    show' t
  ;;

  let pp ppf t = Format.fprintf ppf "%s" (show t)

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

(** A type environment is a mapping from types to types *)
module ConstraintCtx = struct
  include Map.Make (Type) [@@deriving show]

  (** [show env] pretty-prints the type environment [env] *)
  let show env =
    List.fold_left
      (fun acc (k, v) ->
        Printf.sprintf "%s%s = %s, " acc (Type.show k) (Type.show v))
      "{ "
      (bindings env)
    ^ "}"
  ;;

  (** [union c1 c2] returns the union of the two type constraint environments
      [c1] and [c2].

      If a key is present in both [c1] and [c2], the value from [c2] is used. *)
  let union = union (fun _ _ x -> Some x)
end

type constraint_ctx = Type.t ConstraintCtx.t

(** A context is a mapping from variables to types *)
type 'a context = 'a -> Type.t

let init_context _ = raise @@ Invalid_argument "Not in context"
let next_var_id = ref 0

(** [get_var_id ()] returns a fresh variable identifier and increments the
    current global state. *)
let get_var_id () =
  let id = !next_var_id in
  next_var_id := id + 1;
  id
;;

let rec check : 'a. 'a context -> 'a Parser.terms -> Type.t * constraint_ctx =
  fun ctx -> function
  | Var x -> ctx x, ConstraintCtx.empty
  | Abs t ->
    let id = get_var_id () in
    let ctx' = function
      | None -> Type.Forall id
      | Some x -> ctx x
    in
    let t', env = check ctx' t in
    Type.Arrow (Forall id, t'), env
  | Const (Nat _) -> Ground Nat, ConstraintCtx.empty
  | Const (Bool _) -> Ground Bool, ConstraintCtx.empty
  | Succ e ->
    let t, c = check ctx e in
    Ground Nat, ConstraintCtx.add t (Type.Ground Nat) c
  | App (e1, e2) ->
    (* Generate a new type variable for the result of the application *)
    let a = Type.Forall (get_var_id ()) in
    (* Infer the types of the two expressions *)
    let t1, c1 = check ctx e1 in
    let t2, c2 = check ctx e2 in
    (* Merge the constraints from the two expressions and create a new
       constraint for the application *)
    let c =
      ConstraintCtx.union c1 c2 |> ConstraintCtx.add t1 (Type.Arrow (t2, a))
    in
    a, c
  | IfThenElse (c, t, e) ->
    (* Generate a new type variable for the result of the if-then-else *)
    let a = Type.Forall (get_var_id ()) in
    (* Infer types for subexpressions *)
    let t1, c1 = check ctx c in
    let t2, c2 = check ctx t in
    let t3, c3 = check ctx e in
    (* Generate new constraint context *)
    let c =
      ConstraintCtx.union c1 c2
      |> ConstraintCtx.union c3
      |> ConstraintCtx.add t1 (Type.Ground Bool)
      |> ConstraintCtx.add a t2
      |> ConstraintCtx.add a t3
    in
    a, c
  | _ -> raise @@ Invalid_argument "Not implemented"
;;

let%test "if then else" =
  List.fold_left
    (fun acc (tst, sol) ->
      (* Reset global state *)
      next_var_id := 0;
      let t, c = check init_context (Parser.parse @@ Lexer.lex tst) in
      Printf.printf
        " |- %s : %s -| %s"
        tst
        (Type.show t)
        (ConstraintCtx.show c);
      let chk = t = sol in
      if not chk
      then Printf.printf " <--- FAILED\n\tExpected: %s" (Type.show sol);
      print_endline "";
      acc && chk)
    true
    [ "0", Ground Nat
    ; "succ 0", Ground Nat
    ; "true", Ground Bool
    ; {|\x . x|}, Arrow (Forall 0, Forall 0)
    ; {|\x . succ x|}, Arrow (Ground Nat, Ground Nat)
    ; ( {|\x . \y . x y|}
      , Arrow (Arrow (Forall 0, Forall 2), Arrow (Forall 0, Forall 2)) )
    ]
;;

(*
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
