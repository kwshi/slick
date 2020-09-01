def fix f:
  (\x -> f (\v -> x x v)) (\x -> f (\v -> x x v))

# in a slick module, everything must be under a `def`
def euler1:
  fix (\f -> \{below=n} ->
    case n <= 3:
    | True -> 0
    | False ->
      (case ((n-1) % 3 == 0) || ((n-1) % 5 == 0):
       | True -> (n-1) + f {below=n-1}
       | False -> f {below=n-1}
      )
  )