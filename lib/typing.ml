let ( let* ) = Result.bind

type type_var = string

type ground_type =
  | Nat
  | Bool
[@@deriving show]

type type_ =
  | Ground of ground_type
  | Arrow of type_ * type_
      (** [Arrow t1 t2] represents the function type from [t1] to [t2] *)
  | Box of type_
[@@deriving show]

type type_error =
  | Mismatch of type_ * type_
      (** [Mismatch (t1, t2)] indicates that [t1] was expected but [t2] was
          found. *)
  | NotFunction of type_
  | NotInScope of string
  | Unimplemented
[@@deriving show]

(* TODO: iPCF has both a Γ and a Δ *)
module Context = struct
  type t = (string * type_) list

  let empty = []

  (** [extend ctx (k, v)] extends the type environment [ctx] with a new binding
      from [k] to [v]. *)
  let extend ctx (k, v) = (k, v) :: ctx

  (** [lookup ctx k] returns the type bound to [k] in the type environment
      [ctx]. *)
  let lookup ctx k =
    try Ok (List.assoc k ctx) with
    | Not_found -> Error (NotInScope k)
  ;;

  (** [restrict ctx k] removes the binding for [k] from the type environment
      [ctx]. *)
  let restrict ctx k = List.remove_assoc k ctx
end

let rec check : 'a. 'a Parser.terms -> (type_, type_error) result = function
  | Const (Nat _) -> Ok (Ground Nat)
  | Const (Bool _) -> Ok (Ground Bool)
  | Succ t ->
    (match check t with
     | Ok (Ground Nat) -> Ok (Ground Nat)
     | Ok t -> Error (Mismatch (Ground Nat, t))
     | Error e -> Error e)
    (*
       | Abs t ->
       let* t' = check t in
       Ok (Arrow (x, t))
    *)
  | App (t1, t2) ->
    let* t1' = check t1 in
    let* t2' = check t2 in
    (match t1' with
     | Arrow (a, b) when a = t2' -> Ok b
     | Arrow (a, _) -> Error (Mismatch (a, t2'))
     | _ -> Error (NotFunction t1'))
  | IfThenElse (c, t, e) ->
    let* c' = check c in
    if c' = Ground Bool
    then
      let* t' = check t in
      let* e' = check e in
      if t' = e' then Ok t' else Error (Mismatch (t', e'))
    else Error (Mismatch (Ground Bool, c'))
  | _ -> Error Unimplemented
;;

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

module Subst = Map.Make (String)

module type Substitutable = sig
  type t

  (** Apply a substitution to a type *)
  val apply : 'a Subst.t -> t -> t

  val free_vars : t -> string list
end

module TypeEnv (M : Substitutable) = struct
  include Map.Make (String)

  let apply s env = map (M.apply s) env
  let free_vars _t = []
end

module Scheme (M : Substitutable) = struct
  (** A type scheme indicates bound type variables as polymorphic types *)
  type t = Forall of string list * type_

  let apply s (Forall (a, t)) =
    let s' = List.fold_right Subst.remove a s in
    Forall (a, M.apply s' t)
  ;;
end

(* TODO: This is probably the wrong type for infer, but I'm leaving it for now *)
module Infer = struct
  type t = { next_var_id : int }
end
