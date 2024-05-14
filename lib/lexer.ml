type lexing_error = Unexpected_character of char
[@@deriving show { with_path = false }]

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
  | Number of int
  | IsZero
  | Plus
  | Star
  (* Paranthesis *)
  | Lparen
  | Rparen
  (* Lambda *)
  | Backslash
  | Dot
  (* Conditional *)
  | If
  | Then
  | Else
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

(** [is_alpha c] returns true if [c] is an alphabetic character *)
let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

(** [is_digit c] returns true if [c] is a digit *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(** [is_alphanum c] returns true if [c] is an alphanumeric character *)
let is_alphanum c = is_alpha c || is_digit c

(** [lex s] returns a list of tokens from a string [s]. *)
let lex s =
  let rec f tokens = function
    | [] -> Ok (List.rev tokens)
    (* Bools *)
    | 't' :: 'r' :: 'u' :: 'e' :: s -> f (True :: tokens) s
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: s -> f (False :: tokens) s
    (* Natural numbers *)
    | 'z' :: 'e' :: 'r' :: 'o' :: s -> f (Zero :: tokens) s
    | '0' :: s -> f (Zero :: tokens) s
    | 's' :: 'u' :: 'c' :: 'c' :: s -> f (Succ :: tokens) s
    | 'p' :: 'r' :: 'e' :: 'd' :: s -> f (Pred :: tokens) s
    | '+' :: s -> f (Plus :: tokens) s
    | '*' :: s -> f (Star :: tokens) s
    | '?' :: s -> f (IsZero :: tokens) s
    (* Lambda *)
    | '\\' :: s -> f (Backslash :: tokens) s
    | '.' :: s -> f (Dot :: tokens) s
    (* Conditional *)
    | 'i' :: 'f' :: s -> f (If :: tokens) s
    | 't' :: 'h' :: 'e' :: 'n' :: s -> f (Then :: tokens) s
    | 'e' :: 'l' :: 's' :: 'e' :: s -> f (Else :: tokens) s
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
    | c :: s when is_digit c ->
      let rec g acc = function
        | c :: s when is_digit c -> g (c :: acc) s
        | cs -> Number (int_of_string @@ string_of_chars @@ List.rev acc), cs
      in
      let num, s = g [ c ] s in
      f (num :: tokens) s
    | c :: _ -> Error (Unexpected_character c)
  in
  f [] @@ chars_of_string s
;;

let%test "lexer" =
  Result.get_ok
  @@ lex
       {|
    (\ x . z)
    longIdent1 .
    true false
    if then else
    let in box <- fix
    zero 0 succ pred x
    1234? + *|}
  = [ Lparen
    ; Backslash
    ; Ident "x"
    ; Dot
    ; Ident "z"
    ; Rparen
    ; Ident "longIdent1"
    ; Dot
    ; True
    ; False
    ; If
    ; Then
    ; Else
    ; Let
    ; In
    ; Box
    ; Lt
    ; Dash
    ; Fix
    ; Zero
    ; Zero
    ; Succ
    ; Pred
    ; Ident "x"
    ; Number 1234
    ; IsZero
    ; Plus
    ; Star
    ]
;;
