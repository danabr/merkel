
let rec count = function
  | []      -> 0
  | x :: xs -> 1 + (count xs) 
