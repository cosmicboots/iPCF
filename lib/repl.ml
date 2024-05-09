let ( let* ) = Result.bind

module rec IntOpsImpl : Moduletypes.Ops = Intops.Operations (EvalImpl)
and EvalImpl : Moduletypes.Eval = Evaluator.Reduction (IntOpsImpl)

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
let evaluate ?(type_only = false) ctx term =
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
  if type_only
  then Ok (ast, t, ctx)
  else (
    let eval_res = EvalImpl.reduce ast in
    Ok (eval_res, t, ctx))
;;

let print ?(debug = false) ?(ident = None) term type_ =
  let eval_str =
    if debug then [%show: string Parser.terms] term else Parser.show_terms term
  in
  (if Option.is_some ident
   then ANSITerminal.(printf [ magenta ] "%s := " (Option.get ident)));
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

let handle_line ?(type_only = false) ?(debug = false) ctx line =
  let matches = Re.all assign_re line in
  if List.length matches = 1
  then (
    (* Assignment *)
    let m = List.hd matches in
    let ident = Re.Group.get m 1 in
    let term = Re.Group.get m 2 in
    let res = evaluate ~type_only:true ctx term in
    let ctx =
      match res with
      | Ok (term, type_, ctx) ->
        let ctx = Context.add ident term ctx in
        print ~ident:(Some ident) ~debug term type_;
        ctx
      | Error e ->
        print_error e;
        ctx
    in
    ctx)
  else (
    (* Evaluation *)
    let res = evaluate ~type_only ctx line in
    (match res with
     | Ok (term, type_, _) -> print ~debug term type_
     | Error e -> print_error e);
    ctx)
;;

let completion_callback ctx partial_line ln_completions =
  let args = String.split_on_char ' ' partial_line in
  let files =
    if List.length args > 1 && (List.hd args = ":load" || List.hd args = ":l")
    then (
      try
        let filepath = String.concat " " @@ List.tl args in
        let dir =
          if Sys.file_exists filepath
             && Sys.is_directory filepath
             && String.ends_with ~suffix:"/" filepath
          then filepath
          else Filename.dirname filepath
        in
        Sys.readdir dir
        |> Array.to_list
        |> List.filter (fun x ->
          Filename.check_suffix x ".ipcf" || Sys.is_directory x)
        |> List.map (fun x -> Filename.concat dir x)
      with
      | Sys_error _ -> [])
    else []
  in
  if List.length files > 0
  then
    Printf.printf
      "\r\n%s\n%!"
      ANSITerminal.(
        sprintf
          [ blue ]
          "%s"
          (String.concat "\t" @@ List.map Filename.basename files));
  if partial_line <> ""
  then
    List.filter
      (fun x ->
        String.starts_with
          ~prefix:(String.lowercase_ascii partial_line)
          (String.lowercase_ascii x))
      (List.map (fun (k, _) -> k) @@ Context.bindings ctx
       |> List.append Parser.IntOps.idents
       |> List.append (List.map (fun itm -> List.hd args ^ " " ^ itm) files))
    |> List.iter (LNoise.add_completion ln_completions)
;;

let run debug =
  print_endline
    {|
=========================
Welcome to the iPCF REPL!
=========================

You can exit the REPL with either [:quit] or [CTRL+D]
|};
  let rec loop ctx =
    let hints_callback line =
      if line = ":load " || line = ":l "
      then Some ("<filepath>", LNoise.Cyan, false)
      else None
    in
    let line =
      try
        Ocamline.read
          ~prompt:"iPCF>"
          ~brackets:[ '(', ')' ]
          ~completion_callback:(completion_callback ctx)
          ~hints_callback
          ()
      with
      | End_of_file -> ":quit"
      | Stdlib.Sys.Break -> ""
    in
    let args = String.trim line |> String.split_on_char ' ' in
    match List.hd args with
    | "" -> loop ctx
    | ":help" | ":h" ->
      ANSITerminal.(
        printf [ magenta ] "Commands available:\n";
        printf [ green ] ":help\t";
        printf [ blue ] "Print out this help message\n";
        printf [ green ] ":quit\t";
        printf [ blue ] "Quit out of the interpreter\n";
        printf [ green ] ":ctx\t";
        printf [ blue ] "Print out all the intems in the current context\n";
        printf [ green ] ":t\t";
        printf [ blue ] "Print out the type for any given expression\n";
        printf [ green ] ":load\t";
        printf [ blue ] "Load sequence of interpreter commands from a file\n");
      Printf.printf "%!";
      loop ctx
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
        List.map (fun k ->
          ( k
          , EvalImpl.reduce
              (Lexer.lex k |> Result.get_ok |> Parser.parse |> Result.get_ok) ))
        @@ List.filter (fun k -> not @@ Context.mem k ctx) Parser.IntOps.idents
      in
      int_terms |> f;
      loop ctx
    | ":t" ->
      if List.length args < 2
      then (
        print_error @@ ReplError ":t requires an expression";
        loop ctx)
      else (
        let line = List.tl args in
        handle_line ~type_only:true ~debug ctx (String.concat " " line) |> loop)
    | ":load" | ":l" ->
      (match List.nth_opt args 1 with
       | Some filename ->
         (try
            let ic = open_in filename in
            let rec f ic ctx =
              try
                let line = input_line ic in
                f ic @@ handle_line ~debug ctx line
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
