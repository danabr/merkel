external reverse : int list -> int list = ": lists reverse"

let rec fib n =
  if n <= 2 then 1 else (fib (n-1)) + (fib (n-2))

let rec fibf n =
  if n <= 2. then 1. else (fibf (n-.1.)) +. (fibf (n-.2.))

let rev x = reverse x

