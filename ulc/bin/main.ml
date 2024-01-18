let () = 
  let open Ulc in
  let tl = Lex.lex "(\\lbd x . (x x) y)" in
  let ast = Par.parse tl in
  let nast = Reduc.beta_reduce ast in
  Serial.print_ast nast