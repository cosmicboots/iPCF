(*
   lambdas -> \ x -> M
*)

(** Tokens *)
type t =
  | Comma
  | Ident of string
  (* Bools *)
  | True
  | False
  (* Natural numbers *)
  | Zero
  | Succ
  | Pred
  (* Paranthesis *)
  | Lparen
  | Rparen
  (* Lambda *)
  | Backslash
  | Dot
  (* Box / Keywords *)
  | Box
  | Fix
  | Let
  | In
  | Lt (* < *)
  | Dash (* - *)
[@@deriving show]

(** [string_of_chars s] returns a string from a list of [s] characters *)
let string_of_chars s = String.concat "" (List.map (String.make 1) s)

(** [chars_of_string s] returns a list of characters from a string [s] *)
let chars_of_string s = List.init (String.length s) (String.get s)

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_alphanum c = is_alpha c || is_digit c

(** Lexer *)
let lex s =
  let rec f tokens = function
    | [] -> List.rev tokens
    (* Bools *)
    | 't' :: 'r' :: 'u' :: 'e' :: s -> f (True :: tokens) s
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: s -> f (False :: tokens) s
    (* Natural numbers *)
    | 'z' :: 'e' :: 'r' :: 'o' :: s -> f (Zero :: tokens) s
    | '0' :: s -> f (Zero :: tokens) s
    | 's' :: 'u' :: 'c' :: 'c' :: s -> f (Succ :: tokens) s
    | 'p' :: 'r' :: 'e' :: 'd' :: s -> f (Pred :: tokens) s
    (* Lambda *)
    | '\\' :: s -> f (Backslash :: tokens) s
    | '.' :: s -> f (Dot :: tokens) s
    (* Box / Keywords *)
    (*
       Syntax examples:
       let box u <- M in N
       fix z in M
    *)
    | 'b' :: 'o' :: 'x' :: s -> f (Box :: tokens) s
    | 'f' :: 'i' :: 'x' :: s -> f (Fix :: tokens) s
    | 'l' :: 'e' :: 't' :: s -> f (Let :: tokens) s
    | 'i' :: 'n' :: s -> f (In :: tokens) s
    | '<' :: s -> f (Lt :: tokens) s
    | '-' :: s -> f (Dash :: tokens) s
    (* Other *)
    | ',' :: s -> f (Comma :: tokens) s
    | '(' :: s -> f (Lparen :: tokens) s
    | ')' :: s -> f (Rparen :: tokens) s
    | ' ' :: s | '\n' :: s -> f tokens s (* drop spaces *)
    | c :: s when is_alpha c ->
      let rec g acc = function
        | c :: s when is_alphanum c -> g (c :: acc) s
        | cs -> Ident (string_of_chars @@ List.rev acc), cs
      in
      let ident, s = g [ c ] s in
      f (ident :: tokens) s
    | c :: _ ->
      raise (Invalid_argument ("unexpected character" ^ String.make 1 c))
  in
  f [] @@ chars_of_string s
;;

let%expect_test "lexer" =
  Printf.printf "%s"
  @@ [%derive.show: t list]
  @@ lex
       {|
    (\ x . z)
    longIdent1 .
    true false
    let in box <- fix
    zero 0 succ pred x|};
  [%expect
    {|
    [Lexer.Lparen; Lexer.Backslash; (Lexer.Ident "x"); Lexer.Dot;
      (Lexer.Ident "z"); Lexer.Rparen; (Lexer.Ident "longIdent1"); Lexer.Dot;
      Lexer.True; Lexer.False; Lexer.Let; Lexer.In; Lexer.Box; Lexer.Lt;
      Lexer.Dash; Lexer.Fix; Lexer.Zero; Lexer.Zero; Lexer.Succ; Lexer.Pred;
      (Lexer.Ident "x")] |}]
;;
