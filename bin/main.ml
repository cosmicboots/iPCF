open Cmdliner
open Ipcf

let debug =
  Arg.(
    value
    & flag
    & info [ "debug" ] ~doc:"Print internal representation of terms")
;;

let repl_t = Term.(const Repl.run $ debug)
let repl_cmd = Cmd.v (Cmd.info "repl" ~doc:"Start the interactive REPL") repl_t
let cmd = Cmd.group ?default:(Some repl_t) (Cmd.info "ipcf") [ repl_cmd ]
let () = exit @@ Cmd.eval cmd
