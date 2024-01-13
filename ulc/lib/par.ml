type ast = 
    Var of char
  | Abs of ast * ast
  | App of ast * ast


(* Should this function return an ast option ? *)
let check_abs_structure = function
  | Abs (v, _) ->
      let is_var = function
        | Var (_) -> true
        | _ -> false
      in is_var v
  | _ -> false


