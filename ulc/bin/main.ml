let () = 
  let open Ulc in
  let tl = Lexer.lex "(\\lbd x . (x x) y)" in
  let ast = Parser.parse tl in
  let nast = Reduc.beta_reduce ast in
  Serial.print_ast nast