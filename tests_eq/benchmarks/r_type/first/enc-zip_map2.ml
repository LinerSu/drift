

let rec loop x = loop x

let rec zip x y =
 if x = 0 then
   if y = 0 then 0
     else loop y
 else if y = 0 then loop x
   else 1 + zip (x - 1) (y - 1)

let rec map x =
  if x = 0 then x else 1 + map (x - 1)

let main n =
	if n >= 0 then assert(map (zip n n) = n)

let _ = main 30