let ( let* ) = Result.bind

type type_error =
  | NotInContext
  | UnificationError of string
[@@deriving show { with_path = false }]

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

  let rec type_of_int x =
    let make code = String.make 1 (Char.chr (code + Char.code 'a')) in
    if x / 26 = 0 then make x else type_of_int ((x / 26) - 1) ^ make (x mod 26)
  ;;

  let%test "type_of_int tests" =
    List.fold_left
      (fun acc (x, sol) -> acc && type_of_int x = sol)
      true
      [ 0, "a"; 1, "b"; 25, "z"; 26, "aa"; 27, "ab"; 28, "ac"; 52, "ba" ]
  ;;

  let show t =
    let rec show' = function
      | `Ground x -> show_ground_type x
      (* Second arrow case is to handle order of operation parentheses *)
      | `Arrow ((`Arrow _ as t1), t2) ->
        Printf.sprintf "(%s) -> %s" (show' t1) (show' t2)
      | `Arrow (t1, t2) -> Printf.sprintf "%s -> %s" (show' t1) (show' t2)
      | `Box t -> Printf.sprintf "Box (%s)" (show' t)
      | `Var s -> Printf.sprintf "'%s" @@ type_of_int s
    in
    show' t
  ;;

  let pp ppf t = Format.fprintf ppf "%s" (show t)
end

(* Ensure we can use types as keys in a map *)
module _ : Map.OrderedType = Type

(** A type environment is a mapping from types to types *)
module ConstraintCtx = struct
  (*include Map.Make (Type) [@@deriving show]*)

  module TypeSetItem = struct
    type t = Type.t * Type.t [@@deriving ord]
  end

  include Set.Make (TypeSetItem) [@@deriving show]

  (** [show env] pretty-prints the type environment [env] *)
  let show env =
    List.fold_left
      (fun acc (k, v) ->
        Printf.sprintf "%s%s = %s, " acc (Type.show k) (Type.show v))
      "{ "
      (elements env)
    ^ "}"
  ;;
end

(** A context is a mapping from variables to types *)
type 'a context = 'a -> (Type.t, type_error) result

let init_context _ = Error NotInContext
let next_var_id = ref 0

(** [get_var_id ()] returns a fresh variable identifier and increments the
    current global state. *)
let get_var_id () =
  let id = !next_var_id in
  next_var_id := id + 1;
  id
;;

let rec check :
          'a.
          'a context
          -> 'a Parser.terms
          -> (Type.t * ConstraintCtx.t, type_error) result
  =
  fun ctx -> function
  | Var x ->
    let* ctx' = ctx x in
    Ok (ctx', ConstraintCtx.empty)
  | Abs t ->
    let id = get_var_id () in
    let ctx' = function
      | None -> Ok (`Var id)
      | Some x ->
        let* ctx' = ctx x in
        Ok ctx'
    in
    let* t', env = check ctx' t in
    Ok (`Arrow (`Var id, t'), env)
  | Const (Nat _) -> Ok (`Ground Nat, ConstraintCtx.empty)
  | Const (Bool _) -> Ok (`Ground Bool, ConstraintCtx.empty)
  | Succ e ->
    let* t, c = check ctx e in
    Ok (`Ground Type.Nat, ConstraintCtx.add (t, `Ground Type.Nat) c)
  | App (e1, e2) ->
    (* Generate a new type variable for the result of the application *)
    let a = `Var (get_var_id ()) in
    (* Infer the types of the two expressions *)
    let* t1, c1 = check ctx e1 in
    let* t2, c2 = check ctx e2 in
    (* Merge the constraints from the two expressions and create a new
       constraint for the application *)
    let c =
      ConstraintCtx.union c1 c2 |> ConstraintCtx.add (t1, `Arrow (t2, a))
    in
    Ok (a, c)
  | IfThenElse (c, t, e) ->
    (* Generate a new type variable for the result of the if-then-else *)
    let a = `Var (get_var_id ()) in
    (* Infer types for subexpressions *)
    let* t1, c1 = check ctx c in
    let* t2, c2 = check ctx t in
    let* t3, c3 = check ctx e in
    (* Generate new constraint context *)
    let c =
      ConstraintCtx.union c1 c2
      |> ConstraintCtx.union c3
      |> ConstraintCtx.add (t1, `Ground Type.Bool)
      |> ConstraintCtx.add (a, t2)
      |> ConstraintCtx.add (a, t3)
    in
    Ok (a, c)
  | Box e ->
    let* t, c = check ctx e in
    Ok (`Box t, c)
  | Let (m, n) ->
    (* Generate a new type variable for the result of the let box expression *)
    let t = `Var (get_var_id ()) in
    (* Unwrap the let box binding variable context *)
    let ctx' = function
      | None -> Ok t
      | Some x ->
        let* ctx' = ctx x in
        Ok ctx'
    in
    (* Infer types for the two expressions *)
    let* tm, c1 = check ctx m in
    let* tn, c2 = check ctx' n in
    let c = ConstraintCtx.union c1 c2 |> ConstraintCtx.add (tm, `Box t) in
    Ok (tn, c)
  | Fix e ->
    let a = `Var (get_var_id ()) in
    let ctx' = function
      | None -> Ok (`Box a)
      | Some x ->
        let* ctx' = ctx x in
        Ok ctx'
    in
    let* t, c = check ctx' e in
    Ok (t, ConstraintCtx.add (a, t) c)
;;

module SubstKey = struct
  type t = [ `Var of int ] [@@deriving ord]
end

module SubstMap = struct
  include Map.Make (SubstKey)

  let _show m =
    let rec f = function
      | (`Var x, v) :: r ->
        Printf.sprintf "{%s / %s}, %s" (Type.show (`Var x)) (Type.show v) (f r)
      | _ -> ""
    in
    f (bindings m)
  ;;

  let apply : Type.t -> Type.t t -> Type.t =
    fun t s ->
    let rec apply' = function
      | `Ground x -> `Ground x
      | `Arrow (t1, t2) -> `Arrow (apply' t1, apply' t2)
      | `Box t -> `Box (apply' t)
      | `Var x ->
        (match find_opt (`Var x) s with
         | None -> `Var x
         | Some t -> apply' t)
    in
    let rec stabilize f t =
      let r1 = f t in
      let r2 = f r1 in
      if Type.equal r1 r2 then r1 else stabilize f r2
    in
    stabilize apply' t
  ;;
end

type substitutions = Type.t SubstMap.t

(** [subst_ctx subst ctx] applies the substitutions [subst] to the constraint
    context [ctx]. *)
let subst_ctx : substitutions -> ConstraintCtx.t -> ConstraintCtx.t =
  fun subst ctx ->
  ConstraintCtx.map
    (fun ((k, v) as t) ->
      match k with
      | `Var x ->
        let v = SubstMap.find_opt (`Var x) subst |> Option.value ~default:v in
        k, v
      | _ -> t)
    ctx
;;

let unify ctx =
  let rec unify' s ctx =
    match ConstraintCtx.choose_opt ctx with
    | None -> Ok s
    | Some (t1, t2) ->
      let ctx = ConstraintCtx.remove (t1, t2) ctx in
      if Type.equal t1 t2
      then
        (* [t1 = t2] constraint is trivially satisfied, remove it from the
           context *)
        unify' s ctx
      else (
        match t1, t2 with
        | `Arrow (t1, t3), `Arrow (t2, t4) ->
          ctx
          |> ConstraintCtx.add (t1, t2)
          |> ConstraintCtx.add (t3, t4)
          |> unify' s
        | `Box t1, `Box t2 -> unify' s @@ ConstraintCtx.add (t1, t2) ctx
        | t2, (`Var _ as t1) | (`Var _ as t1), t2 ->
          (* Perform substitution in the rest of the context *)
          let ctx = subst_ctx s ctx in
          (* Add substitution to solution *)
          let s = SubstMap.add t1 t2 s in
          ctx |> unify' s
        | t1, t2 ->
          Error
            (UnificationError
               (Printf.sprintf "%s = %s" (Type.show t1) (Type.show t2))))
  in
  unify' SubstMap.empty ctx
;;

let%test "check tests" =
  (* Reset global state *)
  next_var_id := 0;
  List.fold_left
    (fun acc (tst, sol) ->
      (* Reset global state *)
      next_var_id := 0;
      let t, c =
        Result.get_ok @@ check init_context (Parser.parse @@ Lexer.lex tst)
      in
      let s = unify c in
      let pt = SubstMap.apply t @@ Result.get_ok s in
      let chk = pt = sol in
      if not chk
      then
        Printf.printf
          "%s : %s : %s -| %s\nExpected: %s\n\n"
          tst
          (Type.show t)
          (Type.show pt)
          (ConstraintCtx.show c)
          (Type.show sol);
      acc && chk)
    true
    [ "0", `Ground Type.Nat
    ; "succ 0", `Ground Nat
    ; "true", `Ground Bool
    ; {|\x . x|}, `Arrow (`Var 0, `Var 0)
    ; ( {| \x . \y . x y |}
      , `Arrow (`Arrow (`Var 1, `Var 2), `Arrow (`Var 1, `Var 2)) )
    ; {| fix x in 0 |}, `Ground Nat
    ; {| let box x <- box 0 in x |}, `Ground Nat
    ; {| (\x . if x then 0 else succ 0) true |}, `Ground Nat
    ; {|\x . succ x|}, `Arrow (`Ground Nat, `Ground Nat)
    ]
;;

let infer_type ctx e =
  (* Reset global state *)
  next_var_id := 0;
  let* t, c = check ctx e in
  let* s = unify c in
  Ok (SubstMap.apply t s)
;;
