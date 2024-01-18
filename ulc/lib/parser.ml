exception Syntax_error


module Detail =
struct

type ast = 
    Var of char
  (* Abs of char * ast *)
  | Abs of ast * ast
  | App of ast * ast


(* Should this function return an ast option ? *)
let check_abs_structure = function
  | Abs (Var _, _) -> true
  | _ -> false


let rec abs token_list =
  let open Lexer.Detail in
    match token_list with
    | Lbd :: s ->
        let v, s2 = var s in
          let s3 = skip_dot s2 in let t, s4 = parse s3 in
            Abs (v, t), s4
    | Lpar :: _ -> app token_list
    | _ -> raise Syntax_error

and var token_list =
  let open Lexer.Detail in
    match token_list with
    | Var c :: s -> Var c, s
    | _ -> raise Syntax_error 

and app token_list =
  let open Lexer.Detail in
    match token_list with
    | Lpar :: s ->
        let t1, s2 = try abs s with Syntax_error -> var s in
        let t2, s3 = try var s2 with Syntax_error -> abs s2 in
          App (t1, t2), skip_rpar s3
    | _ -> raise Syntax_error

and parse token_list =
  try abs token_list with Syntax_error -> raise Syntax_error
    
end

let parse token_list = fst (Detail.parse token_list)