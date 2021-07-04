%%

let list_rev(i) :=
  | {[]}
  | l = list_rev(i); ~ = i; {i :: l}

let list_min1_rev(i) ==
  | l = list_rev(i); ~ = i; {i :: l}

let list_sep_min1_rev(sep, i) :=
  | ~ = i; {[i]}
  | l = list_sep_min1_rev(sep, i); sep; ~ = i; {i :: l}

let list_sep_min2_rev(sep, i) :=
  | a = i; sep; b = i; {[b; a]}
  | l = list_sep_min2_rev(sep, i); sep; ~ = i; {i :: l}

let list_sep_rev(sep, i) ==
  | {[]}
  | ~ = list_sep_min1_rev(sep, i); <>

%public let list(i) ==
  | ~ = list_rev(i); <List.rev>

%public let list_min1(i) ==
  | ~ = list_min1_rev(i); <List.rev>

%public let option(a) ==
  | {None}
  | ~ = a; <Some>
  (* TODO used to be able to just do `a; <Some> (ref. page 13 on menhir manual), bug report? *)

%public let list_sep(sep, i) ==
  | ~ = list_sep_rev(sep, i); <List.rev>

%public let list_sep_min1(sep, i) ==
  | ~ = list_sep_min1_rev(sep, i); <List.rev>

%public let list_sep_min2(sep, i) ==
  | ~ = list_sep_min2_rev(sep, i); <List.rev>

%public let prefix(pre, a) ==
  | pre; ~ = a; <>

%public let suffix(a, suf) ==
  | ~ = a; suf; <>
