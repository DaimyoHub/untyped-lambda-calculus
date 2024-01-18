(*open Ulc


let () =
  let lbd () = 
    let o = Recognize.lbd ['\\'; 'l'; 'b'; 'd'] 
    and n = Recognize.lbd ['\\'; 'a'; 'b'; 'c'] in
    match o, n with
    | Some(Lbd, _), None -> true 
    | _ -> false 
  in
  let var () =
    let r = ref true in
    for i = 0 to 127 do
      let res = Recognize.var (Char.chr i :: []) in
      if i < 97 || i > 122 then
        match res with
        | None -> ()
        | _ -> r := false
      else
        match res with
        | Some(Var(c)) when Char.code c = i -> ()
        | _ -> r := false
    done;
    !r
  in
  let token () =
    let v, _ = Recognize.token [] 
    and d, _ = Recognize.token ('.' :: []) 
    and l, _ = Recognize.token ('(' :: [])
    and r, _ = Recognize.token (')' :: [])
    and f, _ = Recognize.token ('%' :: [])
    and s, _ = Recognize.token ('a' :: [])
    and p, _ = Recognize.token (' ' :: [])
    in
    (v = Unknown) && (d = Dot) && (l = Lpar) && (r = Rpar) && (f = Unknown)
    && (s = Var('a')) && (p = Blk)
  in
  let rlbd = lbd () and rvar = var () and rtok = token () in
  let slbd = if rlbd then "ok" else "no"
  and svar = if rvar then "ok" else "no"
  and stok = if rtok then "ok" else "no"
  in
  print_endline
    (Printf.sprintf "recognize : \n\tlbd : %s \n\tvar : %s \n\ttok : %s"
    slbd svar stok); ()

let print_tokens tokens = 
  List.iter (
    fun x -> match x with
      | Var(_) -> print_endline "var"
      | Blk -> print_endline "blk"
      | Dot -> print_endline "dot"
      | Lbd -> print_endline "lbd"
      | Lpar -> print_endline "lpar"
      | Rpar -> print_endline "rpar"
      | Unknown -> print_endline "unknown"
    ) tokens 


let () = 
  let str_to_char () = 
    let v = Lexer._string_to_char_list "" 
    and o = Lexer._string_to_char_list "a"
    and l = Lexer._string_to_char_list "abcd"
    and s = Lexer._string_to_char_list "  "
    in
    (v = []) && (o = ['a']) && (l = ['a'; 'b'; 'c'; 'd']) && (s = [' '; ' '])
  in
  let tokenize_str () =
    let v = Lexer.tokenize_string ""
    and n = Lexer.tokenize_string "a bc%"
    and o = Lexer.tokenize_string "\\lbd c . (c c)"
    in
    (v = []) && (n = [Var('a'); Blk; Var('b'); Var('c'); Unknown])
    && (o = [Lbd; Blk; Var('c'); Blk; Dot; Blk; Lpar; Var('c'); Blk; Var('c'); Rpar])
  in
  let remove_blanks () =
    let o = Lexer.remove_blanks_from_tokens (Lexer.tokenize_string "\\lbd c . (c c)") in
      o = [Lbd; Var('c'); Dot; Lpar; Var('c'); Var('c'); Rpar]
  in
  let r_str_to_char = str_to_char () 
  and r_tokenize_str = tokenize_str ()
  and r_remove_blanks = remove_blanks ()
  in
  let s_str_to_char = if r_str_to_char then "ok" else "no"
  and s_tokenize_str = if r_tokenize_str then "ok" else "no"
  and s_remove_blanks = if r_remove_blanks then "ok" else "no"
  in
  print_endline
    (Printf.sprintf "lexer : \n\tstr_to_char : %s \n\ttokenize_str : %s \n\tremove_blanks : %s"
    s_str_to_char s_tokenize_str s_remove_blanks);
*)