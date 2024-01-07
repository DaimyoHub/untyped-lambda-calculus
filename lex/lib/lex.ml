type token =
    Var of char
  | Blk
  | Dot
  | Lbd
  | Lpar
  | Rpar
  | Unknown

module Recognize =
struct

let lbd = function
  | s :: l :: b :: d :: t -> 
    if s = '\\' && l = 'l' && b = 'b' && d = 'd' then
      Some(Lbd, t)
    else None
  | _ -> None

let var = function
  | c :: _ -> 
    let cd = Char.code c in
    if 97 <= cd && cd <= 122 then Some(Var(c)) else None
  | _ -> None

let token str = 
  match str with
  | [] -> Unknown, []
  | ' ' :: s -> Blk, s
  | '.' :: s -> Dot, s
  | '(' :: s -> Lpar, s
  | ')' :: s -> Rpar, s
  | '\\' :: _ -> begin
    let r = lbd str in
      match r with 
      | Some(Lbd, t) -> Lbd, t
      | _ -> Unknown, []
    end
  | _ :: s -> begin
    let r = var str in
      match r with
      | Some(Var(c)) -> Var(c), s
      | _ -> Unknown, []
    end

end


module Lexer =
struct

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
  | Blk :: s -> remove_blanks_from_tokens s
  | x :: s -> x :: remove_blanks_from_tokens s


let lex str = str |> tokenize_string |> remove_blanks_from_tokens


end