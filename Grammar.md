# Lexemes

```
"->" = ARROW
"("  = LPAREN
")"  = RPAREN
"{"  = LBRACE
"}"  = RBRACE
":"  = COLON
"="  = EQUALS
","  = COMMA
[a-zA-Z][a-zA-Z0-9_]* = IDENT
```

# Grammar

```ebnf
<expr>
  ::= <record-expr>
    | <function>
    | <function-app>

<record-expr> ::= LBRACE <record-expr-values> RBRACE

<record-expr-values>
  ::= <empty>
    | <record-expr-value> COMMA <record-expr-values>

<record-expr-value> ::= IDENT EQUALS <expr>

<function> ::= <record> ARROW <expr>

<function-app> ::= <function> <expr>
```
