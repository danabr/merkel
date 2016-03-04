
let rec count = function
  | []      -> 0
  | x :: xs -> 1 + (count xs) 

let rec two_in_second_pos = function
  | _ :: 2 :: _ -> true
  | _           -> false
