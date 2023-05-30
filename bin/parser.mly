%{
  open AbSyn;;
  let x = ref 0;;
  let nextLbl x =
    let res = !x in 
    let _ = x := !x + 1 in
    res
%}

%token SUB ADD SKIP UMINUS
%token LT 
%token LPAREN RPAREN EQ
%token SEMICOLON IF ELSE WHILE LBRACKET RBRACKET EOF
%token ASSUME 
%token <string> VAR 
%token <int> CONST

%nonassoc ELSE
%left MINUS PLUS 

%start prog
%type <prog> prog
%type <expr> expr
%type <cond> cond
%type <cmd> cmd

%%

prog:
  | c = cmd EOF                             { Prog c }

cmd:
  | c1 = cmd c2 = cmd                       { CSeq (nextLbl x, c1, nextLbl x, c2)}
  | ASSUME c = cond SEMICOLON               { CAssume (nextLbl x, c) }
  | IF LPAREN b = cond RPAREN LBRACKET c1 = cmd RBRACKET
    ELSE LBRACKET c2 = cmd RBRACKET         { CSIfelse (nextLbl x, b, c1, c2) } 
  | WHILE LPAREN b = cond RPAREN
    LBRACKET c = cmd RBRACKET               { CWhile (nextLbl x, b, c)}  
  | SKIP SEMICOLON                          { CSkip }


cmpop:
  | LT  {Lt}
  | EQ  {Eq}

cond:
  | e1 = VAR cmp = cmpop e2 = expr         { CmpVarConst (cmp, e1, e1)}

expr:
  | n = CONST                              { EConst n }
  | x = VAR                                { EVar x }
  | e1 = expr SUB e2 = expr                { EBinop (Sub, e1, e2) }
  | e1 = expr ADD e2 = expr                { EBinop (Add, e1, e2) }
  | SUB e = expr                           { EBinop (Sub, (EConst Z.zero), e) } %prec UMINUS
  | LPAREN e = expr RPAREN                 { e }

%%



