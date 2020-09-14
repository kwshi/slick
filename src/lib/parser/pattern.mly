%%

%public let pattern :=
  | ~ = UPPER_IDENT; ~ = pattern_atom; <Pat.Variant>
  | ~ = pattern_atom; <>

%public let pattern_atom :=
  | v = UPPER_IDENT; { Pat.Variant (v, Pat.Record []) }
  | LBRACE; ~ = list_sep(COMMA, record_pattern_entry); RBRACE; <Pat.Record>
  | ~ = literal_pattern; <Pat.Literal>
  | ~ = LOWER_IDENT; <Pat.Var>
  | LPAREN; ~ = pattern; RPAREN; <>


let record_pattern_entry :=
  | k = LOWER_IDENT; { (k, Pat.Var k) }
  | ~ = LOWER_IDENT; EQUALS; ~ = pattern; <>

let literal_pattern :=
  | ~ = STRING; <Pat.String>
  | ~ = INT;    <Pat.Int>
