exception Parser_error of unit

type mybool =
  | True
  | False
[@@deriving show { with_path = false }, eq, ord]

type nat =
  | Zero
  | Succ of nat
[@@deriving show { with_path = false }, eq, ord]

type ground_terms =
  | Bool of mybool
  | Nat of nat
[@@deriving show { with_path = false }, eq, ord]

type 'a terms =
  (* Basic lambda terms *)
  | Var of 'a
  | Const of ground_terms
  | Succ of 'a terms
  | App of 'a terms * 'a terms
  | Abs of 'a option terms
  | IfThenElse of 'a terms * 'a terms * 'a terms
  (* Boxed terms *)
  | Box of 'a terms
  | Let of 'a terms * 'a option terms
  | Fix of 'a option terms
[@@deriving show { with_path = false }, eq, ord]

let show_terms a =
  let rec pp_terms' (a : string terms) =
    match a with
    | Var x -> x
    | Const (Bool True) -> "true"
    | Const (Bool False) -> "false"
    | Const (Nat Zero) -> "0"
    | Const (Nat x) ->
      let rec const_of_nat = function
        | Zero -> 0
        | Succ x -> 1 + const_of_nat x
      in
      string_of_int @@ const_of_nat x
    | App (x, y) -> Printf.sprintf "(%s %s)" (pp_terms' x) (pp_terms' y)
    | Abs _ -> Printf.sprintf "(fun)"
    | IfThenElse (c, t, e) ->
      Printf.sprintf
        "if %s then %s else %s"
        (pp_terms' c)
        (pp_terms' t)
        (pp_terms' e)
    | Succ x -> Printf.sprintf "succ %s" (pp_terms' x)
    | Box x -> Printf.sprintf "box %s" (pp_terms' x)
    | Let _ -> "(let)"
    | Fix _ -> "(fix)"
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
  (* Boxed terms *)
  | Box m -> Box (bind_terms f m)
  | Fix m -> Fix (bind_terms f' m)
  | Let (m, n) -> Let (bind_terms f m, bind_terms f' n)
;;

(** [capture ident term] captures all free occurences of [ident] in [term]. *)
let capture ident term =
  bind_terms
    (fun y -> if String.equal ident y then Var None else Var (Some y))
    term
;;

type 'a wrapped_token =
  | Tok of Lexer.t
  | PE of 'a terms
[@@deriving show]

let parse (input : Lexer.t list) =
  (* [sr i s] shift-reduce parses the input tokens [i] onto the output stack
     [s] *)
  let rec sr i s =
    (*
       Printf.printf
       "i: %s\nstack: %s\n\n"
       ([%derive.show: Lexer.t list] i)
       ([%derive.show: string wrapped_token list] s);
    *)
    match i, s with
    (* === Reduction rules === *)
    (* Parentheses *)
    | i, Tok Lexer.Rparen :: PE x :: Tok Lexer.Lparen :: r -> sr i (PE x :: r)
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
      sr i (PE (Const (Nat (Succ x))) :: r)
    (* Successor of arbitrary expression *)
    | i, PE x :: Tok Lexer.Succ :: r -> sr i (PE (Succ x) :: r)
    (* Abstraction *)
    (* Arbitrary number literals *)
    | i, Tok (Lexer.Number n) :: r ->
      let rec expand_lit = function
        | 0 -> Zero
        | n -> Succ (expand_lit (n - 1))
      in
      sr i (PE (Const (Nat (expand_lit n))) :: r)
    (* Box *)
    | i, PE x :: Tok Lexer.Box :: r -> sr i (PE (Box x) :: r)
    (* === Shift rules === *)
    (* Ground types are complete expressions *)
    | Lexer.True :: i, r -> sr i (PE (Const (Bool True)) :: r)
    | Lexer.False :: i, r -> sr i (PE (Const (Bool False)) :: r)
    | Lexer.Zero :: i, r -> sr i (PE (Const (Nat Zero)) :: r)
    | Lexer.Ident x :: i, r -> sr i (PE (Var x) :: r)
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
        :: r ) -> sr i (PE (Let (m, capture u n)) :: r)
    (* Move token to the stack *)
    | t :: i, r -> sr i (Tok t :: r)
    | [], r :: [] -> r
    | _ -> raise (Parser_error ())
  in
  match sr input [] with
  | PE x -> x
  | _ -> raise (Parser_error ())
;;

let%test "fix binding" =
  List.fold_left
    (fun acc (tst, sol) -> acc && parse @@ Lexer.lex tst = sol)
    true
    [ "fix x in x", Fix (Var None)
    ; "box x", Box (Var "x")
    ; "fix x in x", Fix (Var None)
    ; "let box x <- n in x", Let (Var "n", Var None)
    ]
;;

let%test {|parser:  \ x . \ y . x y))|} =
  let prog = {|\ x . \ y . x y|} in
  let tokens = Lexer.lex prog in
  parse tokens = Abs (Abs (App (Var (Some None), Var None)))
;;

let%test {|parser: If conditionals + succ|} =
  let prog = {|if x then succ z else succ 0|} in
  let tokens = Lexer.lex prog in
  parse tokens = IfThenElse (Var "x", Succ (Var "z"), Const (Nat (Succ Zero)))
;;
