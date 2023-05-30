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
   | "while"   { WHILE }
   | "assume"  { ASSUME } 
   | ";"       { SEMICOLON }
   | "("       { LPAREN }
   | ")"       { RPAREN }
   | "{"       { LBRACKET }
   | "}"       { RBRACKET }
   | "-"       { SUB }
   | "+"       { ADD }
   | "[]"      { CHOICE }   
   | "<"       { LESS }
   | eof       { EOF }
   | "="       { EQUAL }
   | var	   { VAR (Lexing.lexeme lexbuf)}
   | int       { CONST (Z.of_string (Lexing.lexeme lexbuf)) }
   