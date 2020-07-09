let rec fold_left (f:int->int->int) acc xs =
  match xs with
      [] -> acc
    | x::xs' -> fold_left f (f acc x) xs'

let rec make_list n =
  if n < 0 then []
  else n :: make_list (n - 1)

let add x y = x + y

let main (n:int) (m:int) =
  if n > 0 then
	  let xs = make_list n in
	  assert (fold_left add m xs >= m)
  else ()

let _ = main 1 1
let _ = main 1 (-1)