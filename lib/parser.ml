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
  | App of 'a terms * 'a terms
  | Abs of 'a option terms
  (* Boxed terms *)
  | Box of 'a terms
  | Let of 'a * 'a terms * 'a terms
  | BoxVar of 'a
  | Fix of 'a * 'a terms
[@@deriving show]

(* TODO: Add context type definition here *)

type 'a wrapped_token =
  | Tok of Lexer.t
  | PE of 'a terms
[@@deriving show]

let parse (input : Lexer.t list) =
  let rec sr i = function
    (* Reduction rules *)
    | PE x :: PE y :: r -> sr i (PE (App (x, y)) :: r)
    | PE body :: Tok Lexer.Dot :: Tok (Lexer.Ident _) :: r ->
      sr i (PE (Abs (Some body)) :: r)
    | r ->
      (* Shift Rules rules *)
      (match i with
       | [] -> r
       | Lexer.True :: i -> sr i (PE (Var (Bool True)) :: r)
       | Lexer.False :: i -> sr i (PE (Var (Bool False)) :: r)
       | Lexer.Zero :: i -> sr i (PE (Var (Nat Zero)) :: r)
       | Lexer.Succ :: i -> sr i (PE (Var (Nat (Succ Zero))) :: r)
       | _ -> raise (Invalid_argument "Unexpected token when parsing"))
  in
  sr input []
;;

let%expect_test "parser" =
  Printf.printf
    "%s\n"
    ([%derive.show: ground_terms wrapped_token list]
       (parse [ Lexer.True; Lexer.False ]));
  [%expect {| |}]
;;
