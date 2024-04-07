let ( let* ) = Result.bind

module Context = Map.Make (String) [@@deriving show]

let evaluate ctx term =
  (* Lexing *)
  let tokens = Lexer.lex term in
  (* Parsing *)
  let ast = Parser.parse tokens in
  let ast = Parser.bind_terms (fun x -> Context.find x ctx) ast in
  (* Type inference *)
  let* t = ast |> Typing.infer_type Typing.init_context in
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
      | End_of_file -> "quit"
      | Stdlib.Sys.Break -> ""
    in
    match cmd with
    | "" -> loop ctx
    | ":quit" -> ()
    | ":ctx" ->
      Context.bindings ctx
      |> List.iter (fun (k, v) ->
        Printf.printf "%s := %s\n%!" k (Parser.show_terms v));
      loop ctx
    | _ ->
      let matches = Re.all assign_re cmd in
      if List.length matches = 1
      then (
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
            ANSITerminal.(
              printf [ red ] "TypeError: %s" @@ Typing.show_type_error e);
            Printf.printf "\n%!";
            ctx
        in
        loop ctx)
      else (
        let res = evaluate ctx cmd in
        (match res with
         | Ok (term, type_, _) -> print ~debug term type_
         | Error e ->
           ANSITerminal.(
             printf [ red ] "TypeError: %s" @@ Typing.show_type_error e);
           Printf.printf "\n%!");
        loop ctx)
  in
  loop Context.empty
;;
