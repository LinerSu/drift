(*
 * Input data error
 *)
 

let compose x g h = g (h x)

let id ix = ix 

let add ay = ay + 1

let main n =
	if n <= 0 then assert (compose n add id > 1)

let _ = main (-100)