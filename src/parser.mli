type token =
  | FLOAT of (float)
  | INT of (int)
  | V
  | F
  | EOE
  | NEWLINE

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Model.model
