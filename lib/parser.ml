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
  | _ -> raise (Invalid_argument "TODO")
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
  let rec sr i s =
    (*
       Printf.printf
       "i: %s\nstack: %s\n\n"
       ([%derive.show: Lexer.t list] i)
       ([%derive.show: string wrapped_token list] s);
    *)
    match s with
    (* Reduction rules *)
    | PE y :: PE x :: r -> sr i (PE (App (x, y)) :: r)
    | Tok Lexer.Rparen
      :: PE body
      :: Tok Lexer.Dot
      :: PE (Var x)
      :: Tok Lexer.Backslash
      :: Tok Lexer.Lparen
      :: r -> sr i (PE (Abs (capture x body)) :: r)
    | r ->
      (* Shift Rules rules *)
      (match i with
       (*
          Parsing is done when no reduction rules can be applied and the input
          is empty
       *)
       | [] -> r
       (* Ground types are complete expressions *)
       | Lexer.True :: i -> sr i (PE (Const (Bool True)) :: r)
       | Lexer.False :: i -> sr i (PE (Const (Bool False)) :: r)
       | Lexer.Zero :: i -> sr i (PE (Const (Nat Zero)) :: r)
       | Lexer.Succ :: i -> sr i (PE (Const (Nat (Succ Zero))) :: r)
       | Lexer.Ident x :: i -> sr i (PE (Var x) :: r)
       (* Move token to the stack *)
       | t :: i -> sr i (Tok t :: r))
  in
  sr input []
;;

let%expect_test "parser" =
  let prog = {|(\ x . (\ y . x y))|} in
  let tokens = Lexer.lex prog in
  Printf.printf
    "Tokens: %s\nAST: %s\n"
    ([%derive.show: Lexer.t list] tokens)
    ([%derive.show: string wrapped_token list] (parse tokens));
  [%expect
    {|
    Tokens: [Lexer.Lparen; Lexer.Backslash; (Lexer.Ident "x"); Lexer.Dot; Lexer.Lparen;
      Lexer.Backslash; (Lexer.Ident "y"); Lexer.Dot; (Lexer.Ident "x");
      (Lexer.Ident "y"); Lexer.Rparen; Lexer.Rparen]
    AST: [(Parser.PE
        (Parser.Abs
           (Parser.Abs (Parser.App ((Parser.Var (Some None)), (Parser.Var None))))))
      ] |}]
;;
