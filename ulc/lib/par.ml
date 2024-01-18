module Ast =
struct

type ast = 
    Var of char
  (* Abs of char * ast *)
  | Abs of ast * ast
  | App of ast * ast

end


(* Should this function return an ast option ? *)
let check_abs_structure = function
  | Ast.Abs (Ast.Var _, _) -> true
  | _ -> false


exception Syntax_error


module Parse =
struct

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

let rec abs token_list =
  let open Lex.Token in
    match token_list with
    | Lbd :: s ->
        let v, s2 = var s in let s3 = Lex.skip_dot s2 in let t, s4 = parse s3 in
          Ast.Abs (v, t), s4
    | Lpar :: _ -> app token_list
    | _ -> raise Syntax_error

and var token_list =
  let open Lex.Token in
    match token_list with
    | Var c :: s -> Ast.Var c, s
    | _ -> raise Syntax_error 

and app token_list =
  let open Lex.Token in
    match token_list with
    | Lpar :: s ->
        let t1, s2 = try abs s with Syntax_error -> var s in
        let t2, s3 = try var s2 with Syntax_error -> abs s2 in
          Ast.App (t1, t2), Lex.skip_rpar s3
    | _ -> raise Syntax_error

and parse token_list =
  try abs token_list with Syntax_error -> raise Syntax_error
    
end


let parse token_list = fst (Parse.parse token_list)