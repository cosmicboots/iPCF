(*
   type ground =
   | Nat
   | Bool
   [@@deriving show]

   type types =
   | Ground of ground
   | Arrow of types * types
   | Box of types
   [@@deriving show]
*)

type mybool =
  | True
  | False
[@@deriving show]

type nat =
  | Zero
  | Succ of nat
  | Pred of nat
[@@deriving show]

type ground_terms =
  | Bool of mybool
  | Nat of nat
[@@deriving show]

type 'a terms =
  (* Basic lambda terms *)
  | Var of 'a
  | Const of ground_terms
  | App of 'a terms * 'a terms
  | Abs of 'a option terms
  | IfThenElse of 'a terms * 'a terms * 'a terms
  (* Boxed terms *)
  | Box of 'a terms
  | Let of 'a * 'a terms * 'a terms
  | BoxVar of 'a
  | Fix of 'a * 'a terms
[@@deriving show]

(** [bind_terms f a] performs a monadic bind on the term [a] using the function
    [f]. *)
let rec bind_terms : 'a 'b. ('a -> 'b terms) -> 'a terms -> 'b terms =
  fun f a ->
  match a with
  | Var x -> f x
  | App (x, y) -> App (bind_terms f x, bind_terms f y)
  | Abs r ->
    let f' : 'a -> 'b = function
      | None -> Var None
      | Some x -> bind_terms (fun a -> Var (Some a)) (f x)
    in
    Abs (bind_terms f' r)
  | Const x -> Const x
  (* Boxed terms *)
  (*
     | Box x -> Box (bind_terms f x)
     | BoxVar x -> BoxVar x
     | Fix (z, m) -> Fix (z, bind_terms f m)
     | Let (u, m, n) -> Let (u, bind_terms f m, bind_terms f n)
  *)
  | _ -> raise (Invalid_argument "Not implemented")
;;

(** [caapture ident term] captures all free occurences of [ident] in [term]. *)
let capture ident term =
  bind_terms
    (fun y -> if String.equal ident y then Var None else Var (Some y))
    term
;;

(* TODO: Add context type definition here *)

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
    (* === Shift rules === *)
    (* Ground types are complete expressions *)
    | Lexer.True :: i, r -> sr i (PE (Const (Bool True)) :: r)
    | Lexer.False :: i, r -> sr i (PE (Const (Bool False)) :: r)
    | Lexer.Zero :: i, r -> sr i (PE (Const (Nat Zero)) :: r)
    | Lexer.Succ :: i, r -> sr i (PE (Const (Nat (Succ Zero))) :: r)
    | Lexer.Ident x :: i, r -> sr i (PE (Var x) :: r)
    (* === Lower precedence === *)
    (* Abstraction *)
    | i, PE body :: Tok Lexer.Dot :: PE (Var x) :: Tok Lexer.Backslash :: r ->
      sr i (PE (Abs (capture x body)) :: r)
    (* Move token to the stack *)
    | t :: i, r -> sr i (Tok t :: r)
    | [], r :: [] -> r
    | _ -> raise (Invalid_argument "TODO")
  in
  sr input []
;;

let%expect_test "parser: ( x . ( y . x y))" =
  let prog = {|\ x . \ y . x y|} in
  let tokens = Lexer.lex prog in
  Printf.printf
    "AST: %s\n"
    ([%derive.show: string wrapped_token] (parse tokens));
  [%expect
    {|
    AST: (Parser.PE
       (Parser.Abs
          (Parser.Abs (Parser.App ((Parser.Var (Some None)), (Parser.Var None)))))) |}]
;;
