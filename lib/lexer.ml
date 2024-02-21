(*
   lambdas -> \ x -> M
*)

(** Tokens *)
type t =
  | Lparen
  | Rparen
  | Backslash
  | Dot
  | Comma
  | Ident of string
  | EOF
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
    | [] -> List.rev (EOF :: tokens)
    | '\\' :: s -> f (Backslash :: tokens) s
    | '.' :: s -> f (Dot :: tokens) s
    | ',' :: s -> f (Comma :: tokens) s
    | '(' :: s -> f (Lparen :: tokens) s
    | ')' :: s -> f (Rparen :: tokens) s
    | ' ' :: s | '\n' :: s -> f tokens s (* drop spaces *)
    | c :: s when is_alpha c ->
      let rec g acc = function
        | c :: s when c = ' ' || c = '\n' ->
          ( Ident (string_of_chars @@ List.rev acc)
          , c :: s (* space or newline completes ident *) )
        | c :: s when is_alphanum c -> g (c :: acc) s
        | cs ->
          raise
            (Invalid_argument
               ("unexpected character in ident: " ^ string_of_chars cs))
      in
      let ident, s = g [ c ] s in
      f (ident :: tokens) s
    | c :: _ ->
      raise (Invalid_argument ("unexpected character" ^ String.make 1 c))
  in
  f [] @@ chars_of_string s
;;

let%expect_test "lexer" =
  Printf.printf "%s" @@ [%derive.show: t list] @@ lex "\\ x . y z () .";
  [%expect
    {|
    [Lexer.Backslash; (Lexer.Ident "x"); Lexer.Dot; (Lexer.Ident "y");
      (Lexer.Ident "z"); Lexer.Lparen; Lexer.Rparen; Lexer.Dot; Lexer.EOF] |}]
;;
