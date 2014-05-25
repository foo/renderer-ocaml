(* autor : Maciej Pacut *)

{
open Parser
}
let integer = ['0'-'9']+
let float = '-'* ['0'-'9']* '.' ['0'-'9']+
let comment = '#' [^ '\n']* '\n'?
  
rule next = parse
  | integer as num      { INT (int_of_string num) }
  | float as num        { FLOAT (float_of_string num) }
  | 'v'                 { V }
  | 'f'                 { F }
  | ' '                 { next lexbuf }
  | '\n'                { NEWLINE }
  | comment             { next lexbuf }
  | "usemtl Material\n" { next lexbuf }
  | "usemtl (null)\n"   { next lexbuf }
  | "s off\n"           { next lexbuf }
  | eof                 { EOE }
