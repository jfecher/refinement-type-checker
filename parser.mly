%token Tok_EOF
%token Tok_LParen
%token Tok_RParen
%token Tok_Backslash
%token Tok_Dot
%token Tok_Unit
%token Tok_Let
%token Tok_Eq
%token Tok_In
%token Tok_If
%token Tok_Then
%token Tok_Else
%token Tok_Assert
%token Tok_Colon
%token Tok_Int
%token Tok_Bool
%token <int> Tok_IntLit
%token <bool> Tok_BoolLit
%token <string> Tok_Ident
%token <string> Tok_Operator

%start <Expr.expr> parse_expr
%%

parse_expr: e=expr; Tok_EOF   { e }

(* All operators have the same precedence for simplicity *)
expr: e1=term op=Tok_Operator e2=expr                         { Expr.FnCall(Expr.Identifier(op), [e1; e2]) }
    | Tok_Let; x=Tok_Ident; Tok_Eq; e1=expr; Tok_In; e2=expr  { Expr.Let(x, e1, e2) }
    | Tok_Assert; cond=expr; Tok_In; body=expr                { Expr.Assert(cond, body) }
    | Tok_If c=expr Tok_Then t=expr Tok_Else e=expr           { Expr.If(c, t, e) }
    | Tok_Backslash; l=lambda                                 { l }
    | t=term                                                  { t }
    ;

term: t=value          { t }
    | f=term; x=value  { match f with
                         | Expr.FnCall(f', args) -> Expr.FnCall(f', List.append args [x])
                         | _ -> Expr.FnCall(f, [x]) }
    ;

value: Tok_LParen; t=expr; Tok_RParen  { t }
     | s=Tok_Ident                     { Expr.Identifier(s) }
     | x=Tok_IntLit                    { Expr.IntLit(x) }
     | x=Tok_BoolLit                   { Expr.BoolLit(x) }
     ;

(* Support \a b c. shorthand *)
lambda: args=lambda_args; Tok_Dot; t=expr    { Expr.Lambda(args, t) }
      | args=lambda_args; l=lambda           { Expr.Lambda(args, l) }
      ;

lambda_args: arg=Tok_Ident                     { [arg] }
           | args=lambda_args; next=Tok_Ident  { List.append args [next] }
           ;
