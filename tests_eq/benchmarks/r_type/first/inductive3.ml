

let rec f x =
  if x < -1 then
    f (-2)
  else if x <= 1 then
    2 * x - 1
  else
    x

let main n =
    if n >= 2 then assert(f n >= 0)

let _ = main 17