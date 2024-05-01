let is_app : 'a. 'a Parser.terms -> 'a Parser.terms = function
  | Parser.Box (App _) -> Parser.Const (Bool true)
  | _ -> Parser.Const (Bool false)
;;

module Operations = struct
  let t = [ "isApp", is_app ]
  let keys = List.map fst t
end
