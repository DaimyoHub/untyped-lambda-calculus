exception Reduction_error


let rec substitute_in_expr_with s x =
  let open Par.Ast in
    function
    | Abs(x, e) -> Abs(x, substitute_in_expr_with s x e)
    | App(e1, e2) ->
        App(substitute_in_expr_with s x e1, substitute_in_expr_with s x e2)
    | Var(y) when y = s -> x
    | e -> e



let rec beta_reduce =
  let open Par.Ast in
    function
    | App(Abs(Var(s), expr), x) ->
        let new_expr = substitute_in_expr_with s x expr in
          beta_reduce new_expr
    | Abs(x, e) -> Abs(x, e)
    | _ -> raise Reduction_error
