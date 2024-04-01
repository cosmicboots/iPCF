module Type = struct
  type ground_type =
    | Nat
    | Bool
  [@@deriving show { with_path = false }, ord, eq]

  type t =
    [ `Ground of ground_type
    | `Arrow of t * t
      (** [Arrow t1 t2] represents the function type from [t1] to [t2] *)
    | `Box of t [@warning "-37"]
    | `Var of int
      (** [Var s] represents a polymorphic type with the name [s] *)
    ]
  [@@deriving ord, eq]

  let show t =
    let rec show' = function
      | `Ground x -> show_ground_type x
      (* Second arrow case is to handle order of operation parentheses *)
      | `Arrow ((`Arrow _ as t1), t2) ->
        Printf.sprintf "(%s) -> %s" (show' t1) (show' t2)
      | `Arrow (t1, t2) -> Printf.sprintf "%s -> %s" (show' t1) (show' t2)
      | `Box t -> Printf.sprintf "Box %s" (show' t)
      | `Var s -> Printf.sprintf "'%c" @@ Char.chr (s + Char.code 'a')
    in
    show' t
  ;;

  let pp ppf t = Format.fprintf ppf "%s" (show t)
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
      | None -> `Var id
      | Some x -> ctx x
    in
    let t', env = check ctx' t in
    `Arrow (`Var id, t'), env
  | Const (Nat _) -> `Ground Nat, ConstraintCtx.empty
  | Const (Bool _) -> `Ground Bool, ConstraintCtx.empty
  | Succ e ->
    let t, c = check ctx e in
    `Ground Nat, ConstraintCtx.add t (`Ground Type.Nat) c
  | App (e1, e2) ->
    (* Generate a new type variable for the result of the application *)
    let a = `Var (get_var_id ()) in
    (* Infer the types of the two expressions *)
    let t1, c1 = check ctx e1 in
    let t2, c2 = check ctx e2 in
    (* Merge the constraints from the two expressions and create a new
       constraint for the application *)
    let c =
      ConstraintCtx.union c1 c2 |> ConstraintCtx.add t1 (`Arrow (t2, a))
    in
    a, c
  | IfThenElse (c, t, e) ->
    (* Generate a new type variable for the result of the if-then-else *)
    let a = `Var (get_var_id ()) in
    (* Infer types for subexpressions *)
    let t1, c1 = check ctx c in
    let t2, c2 = check ctx t in
    let t3, c3 = check ctx e in
    (* Generate new constraint context *)
    let c =
      ConstraintCtx.union c1 c2
      |> ConstraintCtx.union c3
      |> ConstraintCtx.add t1 (`Ground Type.Bool)
      |> ConstraintCtx.add a t2
      |> ConstraintCtx.add a t3
    in
    a, c
  | Box e ->
    let t, c = check ctx e in
    `Box t, c
  | Let (m, n) ->
    (* Generate a new type variable for the result of the let box expression *)
    let t = `Var (get_var_id ()) in
    (* Unwrap the let box binding variable context *)
    let ctx' = function
      | None -> t
      | Some x -> ctx x
    in
    (* Infer types for the two expressions *)
    let tm, c1 = check ctx m in
    let tn, c2 = check ctx' n in
    let c = ConstraintCtx.union c1 c2 |> ConstraintCtx.add tm (`Box t) in
    tn, c
  | Fix e ->
    let a = `Var (get_var_id ()) in
    let ctx' = function
      | None -> `Box a
      | Some x -> ctx x
    in
    let t, c = check ctx' e in
    t, ConstraintCtx.add a t c
;;

let%test "check tests" =
  List.fold_left
    (fun acc (tst, sol) ->
      (* Reset global state *)
      next_var_id := 0;
      let t, c = check init_context (Parser.parse @@ Lexer.lex tst) in
      let chk = t = sol in
      if not chk
      then
        Printf.printf
          "%s : %s -| %s\nExpected: %s\n\n"
          tst
          (Type.show t)
          (ConstraintCtx.show c)
          (Type.show sol);
      acc && chk)
    true
    [ "0", `Ground Type.Nat
    ; "succ 0", `Ground Nat
    ; "true", `Ground Bool
    ; {|\x . x|}, `Arrow (`Var 0, `Var 0)
    ; {|\x . succ x|}, `Arrow (`Ground Nat, `Ground Nat)
    ; ( {|\x . \y . x y|}
      , `Arrow (`Arrow (`Var 0, `Var 2), `Arrow (`Var 0, `Var 2)) )
    ; {| (\x . if x then 0 else succ 0) true |}, `Ground Nat
    ; {| fix x in 0 |}, `Ground Nat
    ; {| let box x <- box 0 in x |}, `Ground Nat
    ]
;;

module SubstKey = struct
  type t = [ `Var of int ] [@@deriving ord]
end

module SubstMap = struct
  include Map.Make (SubstKey)

  let _show m =
    let rec f = function
      | (`Var x, v) :: r ->
        Printf.sprintf "%s = %s, %s" (Type.show (`Var x)) (Type.show v) (f r)
      | _ -> ""
    in
    f (bindings m)
  ;;
end

type substitutions = Type.t SubstMap.t

let subst_ctx : substitutions -> constraint_ctx -> constraint_ctx =
  fun subst ctx ->
  ConstraintCtx.map
    (fun t ->
      match t with
      | `Var x -> SubstMap.find_opt (`Var x) subst |> Option.value ~default:t
      | _ -> t)
    ctx
;;

let _unify ctx =
  let rec unify' s ctx =
    match ConstraintCtx.choose_opt ctx with
    | None -> s
    | Some (t1, t2) ->
      let ctx = ConstraintCtx.remove t1 ctx in
      if Type.equal t1 t2
      then
        (* [t1 = t2] constraint is trivially satisfied, remove it from the
           context *)
        unify' s ctx
      else (
        match t1, t2 with
        | `Arrow (t1, t3), `Arrow (t2, t4) ->
          ctx |> ConstraintCtx.add t1 t2 |> ConstraintCtx.add t3 t4 |> unify' s
        | `Box t1, `Box t2 -> unify' s @@ ConstraintCtx.add t1 t2 ctx
        | (`Ground _ as t2), (`Var _ as t1) | (`Var _ as t1), (`Ground _ as t2)
          ->
          (* Perform substitution in the rest of the context *)
          let ctx = subst_ctx s ctx in
          (* Add substitution to solution *)
          let s = SubstMap.add t1 t2 s in
          ctx |> unify' s
        | _ -> raise @@ Invalid_argument "Unification failed")
  in
  unify' SubstMap.empty ctx
;;

let%test "unify" =
  let tst = {| (\x . if x then 0 else succ 0) true |} in
  let sol = `Ground Type.Nat in
  let t, c = check init_context (Parser.parse @@ Lexer.lex tst) in
  let _s = _unify c in
  Printf.printf
    "unify | %s : %s -| %s\nExpected: %s\n\n"
    tst
    (Type.show t)
    (ConstraintCtx.show c)
    (Type.show sol);
  t = sol
;;
