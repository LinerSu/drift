

let rec sum (x:int):int(*<:{v:Int | v >= 3 * x - 3 }*) =
  if x <= 0
  then 0
  else x + sum (x - 1)

let main (n:int(*-:{v:Int | v > 0}*)) =
    assert (2 * n - 1 <= sum n)