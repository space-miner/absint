{
  open Parser
  exception Error of string
}

let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let var = letter+

rule token = parse
     [' ' '\t' '\n'] { token lexbuf }
   | "if"      { IF }
   | "else"    { ELSE }
   | "while"   { WHILE }
   | "assume"  { ASSUME } 
   | "skip"    { SKIP }
   | ";"       { SEMICOLON }
   | "("       { LPAREN }
   | ")"       { RPAREN }
   | "{"       { LBRACKET }
   | "}"       { RBRACKET }
   | "-"       { SUB }
   | "+"       { PLUS }
   | "%"       { MOD }
   | "true"    { TRUE }
   | "false"   { FALSE }
   | "<"       { LT }
   | "if"      { IF }
   | eof       { EOF }
   | "else"    { ELSE }
   | "="       { EQ }
   | var	   { VAR (Lexing.lexeme lexbuf)}
   | int       { CONST (Z.of_string (Lexing.lexeme lexbuf)) }
   