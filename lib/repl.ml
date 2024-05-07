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

exception Unbound_variable of string

(** Regular expression for assignment *)
let assign_re = Re.Perl.compile_pat {|^(\w+)\s*:=\s*(.*)$|}

(** [evaluate ctx term] evaluates [term] assuming the context [ctx] *)
let evaluate ctx term =
  (* Lexing *)
  let& tokens = Lexer.lex term in
  (* Parsing *)
  let$ ast = Parser.parse tokens in
  let* ast =
    Ok
      (Parser.bind_terms
         (fun x ->
           match Context.find_opt x ctx with
           | Some x -> x
           | None -> Var x)
         ast)
  in
  (* Type inference *)
  let+ t = Typing.infer_type Typing.init_context ast in
  (* Evaluation *)
  let eval_res =
    Evaluator.reduce
      (fun x -> List.assoc_opt x Intops.Operations.t |> Option.map fst)
      ast
  in
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

let handle_line ?(debug = false) ctx line =
  let matches = Re.all assign_re line in
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
    ctx)
  else (
    (* Evaluation *)
    let res = evaluate ctx line in
    (match res with
     | Ok (term, type_, _) -> print ~debug term type_
     | Error e -> print_error e);
    ctx)
;;

let run debug =
  print_endline
    {|
=========================
Welcome to the iPCF REPL!
=========================

You can exit the REPL with either [exit] or [CTRL+D]
|};
  let rec loop ctx =
    let line =
      try Ocamline.read ~prompt:"iPCF>" ~brackets:[ '(', ')' ] () with
      | End_of_file -> ":quit"
      | Stdlib.Sys.Break -> ""
    in
    let args = String.trim line |> String.split_on_char ' ' in
    match List.hd args with
    | "" -> loop ctx
    | ":quit" -> ()
    | ":ctx" ->
      let f =
        List.iter (fun (k, v) ->
          let t = Result.get_ok @@ Typing.infer_type Typing.init_context v in
          ANSITerminal.(
            printf [ magenta ] "%s" k;
            printf [ blue ] " : ";
            printf [ green ] "%s" (Typing.Type.show t));
          Printf.printf "\n%!")
      in
      Context.bindings ctx |> f;
      let int_terms =
        List.map (fun (k, _) ->
          ( k
          , Evaluator.reduce
              (fun x -> List.assoc_opt x Intops.Operations.t |> Option.map fst)
              (Parser.Var k) ))
        @@ List.filter
             (fun (k, _) -> not @@ Context.mem k ctx)
             Intops.Operations.t
      in
      int_terms |> f;
      loop ctx
    | ":load" | ":l" ->
      (match List.nth_opt args 1 with
       | Some filename ->
         (try
            let ic = open_in filename in
            let rec f ic ctx =
              try
                let line = input_line ic in
                f ic @@ handle_line ctx line
              with
              | End_of_file ->
                close_in_noerr ic;
                ctx
              | e ->
                close_in_noerr ic;
                raise e
            in
            loop @@ f ic ctx
          with
          | Sys_error e ->
            print_error @@ ReplError e;
            loop ctx)
       | None ->
         print_error @@ ReplError ":load requires a filepath";
         loop ctx)
    | _ -> handle_line ~debug ctx line |> loop
  in
  loop Context.empty
;;
