# currently, using the `fix` combinator is our mechanism
# for implementing recursive functions (btw, `fix` type-checks!
# try typing `fix` in the REPL).  Eventually, we aim to
# implement recursion via more sophisticated ways.
def fix f:
  (\x -> f (\v -> x x v)) (\x -> f (\v -> x x v))

# in a slick module, everything must be under a `def`
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