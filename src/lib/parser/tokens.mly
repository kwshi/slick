%token <string> LOWER_IDENT UPPER_IDENT
%token <Z.t> INT
%token <string> STRING
%token BACKSLASH
%token CASE
%token DEF
%token LPAREN RPAREN
%token LBRACE RBRACE
%token ARROW
%token EQUALS
%token SEMICOLON COLON COMMA DOT
%token PIPE
%token WALRUS
%token EOF

%token PLUS MINUS
%token PLUS_PLUS
%token ASTERISK SLASH MOD
%token GE GT LE LT EQ NE
%token AND OR
%token POW

%left OR
%left AND

%left PLUS_PLUS
%left PLUS MINUS
%left ASTERISK SLASH MOD
%right POW

%%
