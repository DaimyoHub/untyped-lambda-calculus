let print_tokens tokens = 
  let open Lex in
  List.iter (
    fun x -> match x with
      | Var _ -> print_endline "var"
      | Blk -> print_endline "Blk"
      | Dot -> print_endline "dot"
      | Lbd -> print_endline "lbd"
      | Lpar -> print_endline "lpar"
      | Rpar -> print_endline "rpar"
      | Unknown -> print_endline "unknown"
    ) tokens 