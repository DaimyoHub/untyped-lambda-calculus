module Detail =
struct

type token =
    Var of char
  | Blk
  | Dot
  | Lbd
  | Lpar
  | Rpar
  | Unknown


let recongize_lbd = function
  | s :: l :: b :: d :: t -> 
    if s = '\\' && l = 'l' && b = 'b' && d = 'd' then
      Some(Lbd, t)
    else None
  | _ -> None

let recognize_var = function
  | c :: _ -> 
    let cd = Char.code c in
    if 97 <= cd && cd <= 122 then Some(Var(c)) else None
  | _ -> None

let recognize_token = function
  | [] -> Unknown, []
  | ' ' :: s -> Blk, s
  | '.' :: s -> Dot, s
  | '(' :: s -> Lpar, s
  | ')' :: s -> Rpar, s
  | '\\' :: s ->
      (function 
       | Some(Lbd, t) -> Lbd, t
       | _ -> Unknown, []
      ) (recongize_lbd ('\\' :: s))
  | x :: s ->
      (function
       | Some(Var(c)) -> Var(c), s
       | _ -> Unknown, []
      ) (recognize_var (x :: s))

let string_to_char_list str =
  let len = String.length str in
  let rec convert i = 
    if i = len then [] else str.[i] :: convert (i + 1)
  in
  convert 0

let tokenize_string str =
  let rec tokenize = function
    | [] -> []
    | x :: s ->
        let tok, t = recognize_token (x :: s) in
          tok :: tokenize t
  in
  string_to_char_list str |> tokenize

let rec remove_blanks_from_tokens = function
  | [] -> []
  | Blk :: s -> remove_blanks_from_tokens s
  | x :: s -> x :: remove_blanks_from_tokens s

let skip_dot = function
  | Dot :: s -> s
  | s -> s

let skip_rpar = function
  | Rpar :: s -> s
  | s -> s

end


let lex str = str |> Detail.tokenize_string |> Detail.remove_blanks_from_tokens
