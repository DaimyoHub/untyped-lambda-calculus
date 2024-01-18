exception Reduction_error


module Detail =
struct

let rec substitute_in_expr_with s x =
  let open Parser.Detail in
    function
    | Abs(x, e) -> Abs(x, substitute_in_expr_with s x e)
    | App(e1, e2) ->
        App(substitute_in_expr_with s x e1, substitute_in_expr_with s x e2)
    | Var(y) when y = s -> x
    | e -> e

end


let rec beta_reduce =
  let open Parser.Detail in
    function
    | App(Abs(Var(s), expr), x) ->
        let new_expr = Detail.substitute_in_expr_with s x expr in
          beta_reduce new_expr
    | Abs(x, e) -> Abs(x, e)
    | _ -> raise Reduction_error
