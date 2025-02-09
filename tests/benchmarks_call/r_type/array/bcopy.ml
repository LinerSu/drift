
let make_array n = n
let arraysize src = src
let update des i x = assert (0 <= i && i < des)
let sub src i = assert (0 <= i && i < src); 0

let rec bcopy_aux src des i m =
  if i >= m
  then ()
  else
    begin
      update des i (sub src i);
      bcopy_aux src des (i+1) m
    end

let bcopy src des = bcopy_aux src des 0 (arraysize src)

let main_p n m =
  let array1 = make_array n in
  let array2 = make_array m in
  if n<=m then bcopy array1 array2 else ()

let main (w:unit) =
	let _ = main_p 5 10 in
	let _ = main_p 12 26 in
  (* let _ = 
			for i = 1 to 1000 do
				main_p (Random.int 1000) (Random.int 1000)
			done in *)
	()

let _ = main ()