let ( let* ) = Result.bind

type error =
  | ParseError of Parser.parse_error
  | TypeError of Typing.type_error
  | LexingError of Lexer.lexing_error
  | ReplError of string

let ( let$ ) r code =
  match r with
  | Ok x -> code x
  | Error x -> Error (ParseError x)
;;

let ( let+ ) r code =
  match r with
  | Ok x -> code x
  | Error x -> Error (TypeError x)
;;

let ( let& ) r code =
  match r with
  | Ok x -> code x
  | Error x -> Error (LexingError x)
;;

module Context = Map.Make (String) [@@deriving show]

let evaluate ctx term =
  (* Lexing *)
  let& tokens = Lexer.lex term in
  (* Parsing *)
  let$ ast = Parser.parse tokens in
  let* ast =
    try Ok (Parser.bind_terms (fun x -> Context.find x ctx) ast) with
    | Not_found -> Error (ReplError "Variable not found in REPL context")
  in
  (* Type inference *)
  let+ t = ast |> Typing.infer_type Typing.init_context in
  (* Evaluation *)
  let eval_res = ast |> Evaluator.reduce in
  Ok (eval_res, t, ctx)
;;

let print ?(debug = false) term type_ =
  let eval_str =
    if debug then [%show: string Parser.terms] term else Parser.show_terms term
  in
  ANSITerminal.(
    printf [ magenta ] "%s" eval_str;
    printf [ blue ] " : ";
    printf [ green ] "%s" (Typing.Type.show type_));
  Printf.printf "\n%!"
;;

let print_error error =
  (match error with
   | ParseError e ->
     ANSITerminal.(
       printf [ red ] "ParseError: %s" @@ Parser.show_parse_error e)
   | TypeError e ->
     ANSITerminal.(printf [ red ] "TypeError: %s" @@ Typing.show_type_error e)
   | ReplError e -> ANSITerminal.(printf [ red ] "Error: %s" e)
   | LexingError e ->
     ANSITerminal.(
       printf [ red ] "LexingError: %s" @@ Lexer.show_lexing_error e));
  Printf.printf "\n%!"
;;

let run debug =
  let assign_re = Re.Perl.compile_pat {|^(\w+)\s*:=\s*(.*)$|} in
  print_endline
    {|
=========================
Welcome to the iPCF REPL!
=========================

You can exit the REPL with either [exit] or [CTRL+D]
|};
  let rec loop ctx =
    let cmd =
      try Ocamline.read ~prompt:"iPCF>" ~brackets:[ '(', ')' ] () with
      | End_of_file -> ":quit"
      | Stdlib.Sys.Break -> ""
    in
    match cmd with
    | "" -> loop ctx
    | ":quit" -> ()
    | ":ctx" ->
      Context.bindings ctx
      |> List.iter (fun (k, v) ->
        let t = Result.get_ok @@ Typing.infer_type Typing.init_context v in
        ANSITerminal.(
          printf [ magenta ] "%s" k;
          printf [ blue ] " : ";
          printf [ green ] "%s" (Typing.Type.show t));
        Printf.printf "\n%!");
      loop ctx
    | _ ->
      let matches = Re.all assign_re cmd in
      if List.length matches = 1
      then (
        (* Assignment *)
        let m = List.hd matches in
        let ident = Re.Group.get m 1 in
        let term = Re.Group.get m 2 in
        let res = evaluate ctx term in
        let ctx =
          match res with
          | Ok (term, type_, ctx) ->
            let ctx = Context.add ident term ctx in
            print (Var ident) type_;
            ctx
          | Error e ->
            print_error e;
            ctx
        in
        loop ctx)
      else (
        (* Evaluation *)
        let res = evaluate ctx cmd in
        (match res with
         | Ok (term, type_, _) -> print ~debug term type_
         | Error e -> print_error e);
        loop ctx)
  in
  loop Context.empty
;;
