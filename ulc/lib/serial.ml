module Detail =
struct

let serialize_token =
  let open Lexer.Detail in
    function
    | Var c -> Printf.sprintf "var(%c)" c
    | Blk -> "Blk"
    | Dot -> "dot"
    | Lbd -> "lbd"
    | Lpar -> "lpar"
    | Rpar -> "rpar"
    | Unknown -> "unknown"

let rec serialize_ast =
  let open Parser.Detail in
    function
    | Var c -> Printf.sprintf "Var(%c)" c
    | Abs (l, r) ->
        Printf.sprintf "Abs(%s, %s)" (serialize_ast l) (serialize_ast r)
    | App (l, r) ->
        Printf.sprintf "App(%s, %s)" (serialize_ast l) (serialize_ast r)

end


let print_token_list token_list =
  print_string "[";
  let tok_str x = Printf.sprintf " %s " (Detail.serialize_token x) in
  List.iter (fun x -> print_string (tok_str x)) token_list;
  print_endline "]"


let print_ast ast = print_endline (Printf.sprintf "# %s" (Detail.serialize_ast ast))