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
  | Token of Lexer.t
  | PE of 'a terms
[@@deriving show]

let parse (input : Lexer.t list) =
  let rec f s = function
    | Lexer.EOF :: _ | [] -> s
    | Lexer.True :: r -> f (PE (Var (Bool True)) :: s) r
    | _ -> raise (Invalid_argument "Unexpected token when parsing")
  in
  f [] input
;;

let%expect_test "parser" =
  Printf.printf
    "%s\n"
    ([%derive.show: ground_terms wrapped_token list]
       (parse [ Lexer.True; Lexer.EOF ]));
  [%expect {| [(Parser.PE (Parser.Var (Parser.Bool Parser.True)))] |}]
;;
