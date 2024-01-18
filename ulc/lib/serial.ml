let serialize_token =
  let open Lex.Token in
    function
    | Var c -> Printf.sprintf "var(%c)" c
    | Blk -> "Blk"
    | Dot -> "dot"
    | Lbd -> "lbd"
    | Lpar -> "lpar"
    | Rpar -> "rpar"
    | Unknown -> "unknown"

let print_token_list token_list =
  print_string "[";
  let tok_str x = Printf.sprintf " %s " (serialize_token x) in
  List.iter (fun x -> print_string (tok_str x)) token_list;
  print_endline "]"

let rec serialize_ast =
  let open Par.Ast in
    function
    | Var c -> Printf.sprintf "Var(%c)" c
    | Abs (l, r) ->
        Printf.sprintf "Abs(%s, %s)" (serialize_ast l) (serialize_ast r)
    | App (l, r) ->
        Printf.sprintf "App(%s, %s)" (serialize_ast l) (serialize_ast r)

let print_ast ast = print_endline (Printf.sprintf "# %s" (serialize_ast ast))