
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR of (string)
  | UMINUS
  | SUB
  | SEMICOLON
  | RPAREN
  | RBRACKET
  | LPAREN
  | LESS
  | LBRACKET
  | EQUAL
  | EOF
  | CONST of (Z.t)
  | CHOICE
  | ASSUME
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.prog)
