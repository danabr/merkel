external reverse : int list -> int list = "lists_reverse"

let rec fib n =
  if n <= 1 then 1 else (fib (n-1)) + (fib (n-2))

let rec fibf n =
  if n <= 1. then 1. else (fibf (n-.1.)) +. (fibf (n-.2.))

let hello s = "hello, " ^ s

let rev x = reverse x
