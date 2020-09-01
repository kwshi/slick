# currently, using the `fix` combinator is our mechanism
# for implementing recursive functions (btw, `fix` type-checks!
# try typing `fix` in the REPL).  Eventually, we aim to
# implement recursion via more sophisticated ways.
def fix f:
  (\x -> f (\v -> x x v)) (\x -> f (\v -> x x v))

# project euler solution #1
def euler1:
  fix (\f -> \{below=n} ->
    case n:
    # base cases
    | 0 -> 0
    | 1 -> 0
    | 2 -> 0
    | 3 -> 0
    | _ ->
      (case {three=(n-1)%3, five=(n-1)%5}:
       | {three=0, five} -> (n-1) + f {below=n-1}
       | {three, five=0} -> (n-1) + f {below=n-1}
       | _ -> f {below=n-1}
      )
  )

def bool_not a:
  case a:
  | True -> False
  | False -> True

def bool_xor {a, b}:
  a && bool_not b || b && bool_not a

def fibonacci:
  go := fix (\go -> \{a, b} -> \n ->
    case n:
    | 0 -> b
    | _ -> go {a=b, b=a+b} (n-1) 
  );
  go {a=0, b=1}
   
def factorial:
  fix (\fact -> \n ->
    case n:
    | 0 -> 1
    | _ -> n * fact (n-1)
  )

def digit_to_string n:
  case n:
  | 0 -> "0"
  | 1 -> "1"
  | 2 -> "2"
  | 3 -> "3"
  | 4 -> "4"
  | 5 -> "5"
  | 6 -> "6"
  | 7 -> "7"
  | 8 -> "8"
  | 9 -> "9"

def int_to_string:
  fix (\int_to_string -> \n ->
    case n/10:
    | 0 -> digit_to_string n
    | _ -> int_to_string (n/10) ++ digit_to_string (n%10)
  )

def fizz_buzz:
  fix (\fb -> \n ->
    case n:
    | 0 -> {}
    | n ->
      # currently, we don't have the syntax for "sequencing"
      # function calls yet (i.e., first do this, then that)
      # so we use the following hack, but we plan to introduce
      # nicer syntax for it soon
      (ignore := fb (n-1);
       print
         (int_to_string n ++ " " ++ 
	  (case {fizz=n%3, buzz=n%5}:
           | {fizz=0, buzz=0} -> "fizz buzz"
           | {fizz=0, buzz} -> "fizz"
           | {fizz, buzz=0} -> "buzz"
           | whatever -> ""
	   )
	 )
      )
  )