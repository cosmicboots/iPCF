type parse_error = Parser_error of unit
[@@deriving show { with_path = false }]

type ground_terms =
  | Bool of bool
  | Nat of int
[@@deriving show { with_path = false }, eq, ord]

module IntOps = struct
  (** Intensional operations *)

  type t =
    | IsApp
    | IsAbs
    | NumberOfVars
    | IsNormalForm
    | Tick
  [@@deriving show { with_path = false }, eq, ord]

  (** [map] is an associated list of intensional operations from their string
      representation. *)
  let map =
    [ "isApp", IsApp
    ; "isAbs", IsAbs
    ; "numberOfVars", NumberOfVars
    ; "isNormalForm", IsNormalForm
    ; "tick", Tick
    ]
  ;;

  (** [of_string s] converts [s] to an intensional operation. *)
  let of_string s = List.assoc_opt s map

  (** [idents] is a list of all the intensional operation string
      representations. *)
  let idents = List.map fst map
end

(** ['a terms] is the type of lambda terms. *)
type 'a terms =
  (* Basic lambda terms *)
  | Var of 'a
  | Const of ground_terms
  | Succ of 'a terms
  | Pred of 'a terms
  | Add of 'a terms * 'a terms
  | Mult of 'a terms * 'a terms
  | App of 'a terms * 'a terms
  | Abs of 'a option terms
  | IfThenElse of 'a terms * 'a terms * 'a terms
  | IsZero of 'a terms
  (* Boxed terms *)
  | Box of 'a terms
  | Unbox of 'a terms * 'a option terms
  | Fix of 'a option terms
  | IntOp of IntOps.t
[@@deriving show { with_path = false }, eq, ord]

(** [show_terms a] returns a pretty formatted string of [a]. *)
let show_terms a =
  let rec pp_terms' (a : string terms) =
    match a with
    | Var x -> x
    | Const (Bool true) -> "true"
    | Const (Bool false) -> "false"
    | Const (Nat x) -> string_of_int x
    | App (x, y) -> Printf.sprintf "(%s %s)" (pp_terms' x) (pp_terms' y)
    | Abs _ -> Printf.sprintf "(fun)"
    | IfThenElse (c, t, e) ->
      Printf.sprintf
        "if %s then %s else %s"
        (pp_terms' c)
        (pp_terms' t)
        (pp_terms' e)
    | Succ x -> Printf.sprintf "succ %s" (pp_terms' x)
    | Pred x -> Printf.sprintf "pred %s" (pp_terms' x)
    | Add (x, y) -> Printf.sprintf "(%s + %s)" (pp_terms' x) (pp_terms' y)
    | Mult (x, y) -> Printf.sprintf "(%s * %s)" (pp_terms' x) (pp_terms' y)
    | IsZero x -> Printf.sprintf "%s?" (pp_terms' x)
    | Box x -> Printf.sprintf "box %s" (pp_terms' x)
    | Unbox _ -> "(let)"
    | Fix _ -> "(fix)"
    | IntOp x -> Printf.sprintf "IntOp.%s" (IntOps.show x)
  in
  pp_terms' a
;;

(** [bind_terms f a] performs a monadic bind on the term [a] using the function
    [f]. *)
let rec bind_terms : 'a 'b. ('a -> 'b terms) -> 'a terms -> 'b terms =
  fun f a ->
  let f' : 'a -> 'b = function
    | None -> Var None
    | Some x -> bind_terms (fun a -> Var (Some a)) (f x)
  in
  match a with
  | Var x -> f x
  | App (x, y) -> App (bind_terms f x, bind_terms f y)
  | Abs r -> Abs (bind_terms f' r)
  | Const x -> Const x
  | IfThenElse (c, t, e) ->
    IfThenElse (bind_terms f c, bind_terms f t, bind_terms f e)
  | Succ x -> Succ (bind_terms f x)
  | Pred x -> Pred (bind_terms f x)
  | Add (x, y) -> Add (bind_terms f x, bind_terms f y)
  | Mult (x, y) -> Mult (bind_terms f x, bind_terms f y)
  | IsZero x -> IsZero (bind_terms f x)
  (* Boxed terms *)
  | Box m -> Box (bind_terms f m)
  | Fix m -> Fix (bind_terms f' m)
  | Unbox (m, n) -> Unbox (bind_terms f m, bind_terms f' n)
  | IntOp x -> IntOp x
;;

(** [capture ident term] captures all free occurences of [ident] in [term]. *)
let capture ident term =
  bind_terms
    (fun y -> if String.equal ident y then Var None else Var (Some y))
    term
;;

(** A wrapper to hold parsed expressions and lexer tokens during the parsing
    step *)
type 'a wrapped_token =
  | Tok of Lexer.t
  | PE of 'a terms
[@@deriving show]

(** [parse input] is the result of parsing the input tokens [input]. *)
let parse (input : Lexer.t list) =
  (* [sr i s] shift-reduce parses the input tokens [i] onto the output stack
     [s] *)
  let rec sr i s =
    match i, s with
    (* === Reduction rules === *)
    (* Parentheses *)
    | i, Tok Lexer.Rparen :: PE x :: Tok Lexer.Lparen :: r -> sr i (PE x :: r)
    (* IsZero *)
    | Lexer.IsZero :: i, PE e :: r -> sr i (PE (IsZero e) :: r)
    (* Application *)
    | i, PE y :: PE x :: r -> sr i (PE (App (x, y)) :: r)
    (* If statement *)
    | ( i
      , PE else_body
        :: Tok Lexer.Else
        :: PE then_body
        :: Tok Lexer.Then
        :: PE cond
        :: Tok Lexer.If
        :: r ) -> sr i (PE (IfThenElse (cond, then_body, else_body)) :: r)
    (* Successor of const *)
    | i, PE (Const (Nat x)) :: Tok Lexer.Succ :: r ->
      sr i (PE (Const (Nat (x + 1))) :: r)
    (* Successor of arbitrary expression *)
    | i, PE x :: Tok Lexer.Succ :: r -> sr i (PE (Succ x) :: r)
    (* Pred of const *)
    | i, PE (Const (Nat x)) :: Tok Lexer.Pred :: r ->
      sr i (PE (Const (Nat (x - 1))) :: r)
    (* Pred of arbitrary expression *)
    | i, PE x :: Tok Lexer.Pred :: r -> sr i (PE (Pred x) :: r)
    (* Add and mult TODO *)
    | i, PE y :: Tok Lexer.Plus :: PE x :: r -> sr i (PE (Add (x, y)) :: r)
    | i, PE y :: Tok Lexer.Star :: PE x :: r -> sr i (PE (Mult (x, y)) :: r)
    (* Arbitrary number literals *)
    | i, Tok (Lexer.Number n) :: r -> sr i (PE (Const (Nat n)) :: r)
    (* Box *)
    | i, PE x :: Tok Lexer.Box :: r -> sr i (PE (Box x) :: r)
    (* === Shift rules === *)
    (* Ground types are complete expressions *)
    | Lexer.True :: i, r -> sr i (PE (Const (Bool true)) :: r)
    | Lexer.False :: i, r -> sr i (PE (Const (Bool false)) :: r)
    | Lexer.Zero :: i, r -> sr i (PE (Const (Nat 0)) :: r)
    | Lexer.Ident x :: i, r -> sr i (PE (Var x) :: r)
    | Lexer.Lparen :: i, r -> sr i (Tok Lparen :: r)
    (* === Lower precedence === *)
    (* Abstraction *)
    | i, PE body :: Tok Lexer.Dot :: PE (Var x) :: Tok Lexer.Backslash :: r ->
      sr i (PE (Abs (capture x body)) :: r)
    (* Fix *)
    | i, PE body :: Tok Lexer.In :: PE (Var x) :: Tok Lexer.Fix :: r ->
      sr i (PE (Fix (capture x body)) :: r)
    (* Let *)
    | ( i
      , PE n
        :: Tok Lexer.In
        :: PE m
        :: Tok Lexer.Dash
        :: Tok Lexer.Lt
        :: PE (Box (Var u))
        :: Tok Lexer.Let
        :: r ) -> sr i (PE (Unbox (m, capture u n)) :: r)
    (* Move arbitrary token to the stack *)
    | t :: i, r -> sr i (Tok t :: r)
    | [], r :: [] -> Ok r
    | _ -> Error (Parser_error ())
  in
  match sr input [] with
  | Ok (PE x) ->
    Ok
      (bind_terms
         (fun x ->
           (* Replace all free variables with intensional operations *)
           match IntOps.of_string x with
           | Some name -> IntOp name
           | None -> Var x)
         x)
  | _ -> Error (Parser_error ())
;;

let%test_module _ =
  (module struct
    let run_test tst sol =
      let tokens = Result.get_ok @@ Lexer.lex tst in
      let ast = Result.get_ok @@ parse tokens in
      if ast <> sol
      then
        Printf.printf
          {|Testing [%s]
Expected: %s
Got:      %s

|}
          tst
          ([%show: string terms] sol)
          ([%show: string terms] ast);
      ast = sol
    ;;

    let%test {|parser:  \ x . \ y . x y))|} =
      run_test
        {|\ x . \ y . x y|}
        (Abs (Abs (App (Var (Some None), Var None))))
    ;;

    let%test "parser intop" = run_test "isApp" (IntOp IntOps.IsApp)

    let%test "add/mult" =
      run_test
        {|1 + (2 * 3)|}
        (Add (Const (Nat 1), Mult (Const (Nat 2), Const (Nat 3))))
    ;;

    let%test "postfix" =
      run_test {|(\x . x) 1?|} (App (Abs (Var None), IsZero (Const (Nat 1))))
    ;;

    let%test {|\f . \x . f ( x )|} =
      run_test
        {| \f . \x . f ( x )|}
        (Abs (Abs (App (Var (Some None), Var None))))
    ;;

    let%test {|parser: If conditionals + succ|} =
      run_test
        {|if x? then succ z else succ 0|}
        (IfThenElse (IsZero (Var "x"), Succ (Var "z"), Const (Nat 1)))
    ;;

    let%test {| (\x . if x then 0 else succ 0) true |} =
      run_test
        {| (\x . if x then 0 else succ 0) true |}
        (App
           ( Abs (IfThenElse (Var None, Const (Nat 0), Const (Nat 1)))
           , Const (Bool true) ))
    ;;
  end)
;;

let%test "fix binding" =
  List.fold_left
    (fun acc (tst, sol) ->
      acc && Result.get_ok @@ parse @@ Result.get_ok @@ Lexer.lex tst = sol)
    true
    [ "fix x in x", Fix (Var None)
    ; "box x", Box (Var "x")
    ; "fix x in x", Fix (Var None)
    ; "let box x <- n in x", Unbox (Var "n", Var None)
    ]
;;
