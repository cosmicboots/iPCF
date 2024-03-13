(** [subst s t] substitutes the the term [t] in the term [s].

    This is the shown as [[s/t]] in written notation.*)
let subst s t =
  Parser.bind_terms
    (function
      | Some x -> Var x
      | None -> t)
    s
;;
