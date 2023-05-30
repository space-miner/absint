%{
  open Syntax;;
  let x = ref 0;;
  let nextLbl x =
    let res = !x in 
    let _ = x := !x + 1 in
    res
%}

%token SUB ADD UMINUS 
%token LESS
%token LPAREN RPAREN EQUAL
%token SEMICOLON WHILE LBRACKET RBRACKET EOF
%token CHOICE
%token ASSUME 
%token <string> VAR 
%token <Z.t> CONST


%left SEMICOLON
%nonassoc CHOICE
%left SUB ADD
%nonassoc UMINUS

%start prog
%type <prog> prog
%type <expr> expr
%type <cond> cond
%type <cmd> cmd
 
%%

prog:
  | c = cmd EOF                             { Prog c }

cmd:
  | c1 = cmd SEMICOLON c2 = cmd             { Seq (nextLbl x, c1, nextLbl x, c2)}
  | ASSUME c = cond                         { Assume (nextLbl x, c) }
  | c1 = cmd CHOICE c2 = cmd                { Choice (nextLbl x, c1, c2) } 
  | WHILE LPAREN b = cond RPAREN
    LBRACKET c = cmd RBRACKET               { While (nextLbl x, b, c)}  
  | v = VAR EQUAL e = expr                     { Assign (nextLbl x, v, e) }

cmpop:
  | LESS  {Less}
  | EQUAL  {Equal}

cond:
  | e1 = expr cmp = cmpop e2 = expr         { Cmp (cmp, e1, e2)}

expr:
  | n = CONST                              { Const n }
  | x = VAR                                { Var x }
  | e1 = expr SUB e2 = expr                { Binop (Sub, e1, e2) }
  | e1 = expr ADD e2 = expr                { Binop (Add, e1, e2) }
  | SUB e = expr                           { Binop (Sub, (EConst Z.zero), e) } %prec UMINUS
  | LPAREN e = expr RPAREN                 { e }

%%



