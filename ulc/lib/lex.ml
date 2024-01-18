module Token =
struct

type token =
    Var of char
  | Blk
  | Dot
  | Lbd
  | Lpar
  | Rpar
  | Unknown

end


module Recognize =
struct

let lbd = function
  | s :: l :: b :: d :: t -> 
    if s = '\\' && l = 'l' && b = 'b' && d = 'd' then
      Some(Token.Lbd, t)
    else None
  | _ -> None

let var = function
  | c :: _ -> 
    let cd = Char.code c in
    if 97 <= cd && cd <= 122 then Some(Token.Var(c)) else None
  | _ -> None

let token str = 
  let open Token in
  match str with
  | [] -> Unknown, []
  | ' ' :: s -> Blk, s
  | '.' :: s -> Dot, s
  | '(' :: s -> Lpar, s
  | ')' :: s -> Rpar, s
  | '\\' :: _ ->
      (function 
       | Some(Lbd, t) -> Lbd, t
       | _ -> Unknown, []
      ) (lbd str)
  | _ :: s ->
      (function
       | Some(Var(c)) -> Var(c), s
       | _ -> Unknown, []
      ) (var str)

end


let _string_to_char_list str =
  let len = String.length str in
  let rec convert i = 
    if i = len then [] else str.[i] :: convert (i + 1)
  in
  convert 0

let tokenize_string str =
  let rec tokenize = function
    | [] -> []
    | x :: s ->
        let tok, t = Recognize.token (x :: s) in
          tok :: tokenize t
  in
  _string_to_char_list str |> tokenize

let rec remove_blanks_from_tokens = function
  | [] -> []
  | Token.Blk :: s -> remove_blanks_from_tokens s
  | x :: s -> x :: remove_blanks_from_tokens s

let skip_dot = function
  | Token.Dot :: s -> s
  | s -> s

let skip_rpar = function
  | Token.Rpar :: s -> s
  | s -> s

let lex str = str |> tokenize_string |> remove_blanks_from_tokens
