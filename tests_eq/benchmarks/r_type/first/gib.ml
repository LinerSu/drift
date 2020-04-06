(*
let rec gib a b n =
  if n=0 then a
  else if n=1 then b
  else gib a b (n-1) + gib a b (n-2)

let main n =
  assert (gib 0 1 n >= 0)
*)


let rec gib ga gb gn =
  if gn = 0 then ga
  else if gn = 1 then gb
  else gib ga gb (gn - 1) + gib ga gb (gn - 2)

let main (n:int) (a:int) (b:int) = 
	if a >= 0 && b >= 0 && n >= 0 then
  		assert(gib a b n >= 0)
  	else assert(true)

let _ = main 10 1 2
let _ = main 0 2 4
let _ = main 3 0 5
let _ = main 54 23 0
let _ = main (-34) 75 (-94)