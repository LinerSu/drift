
let rec loop a b nd_bool ed = 
	let aold = !a in
	b := !b + 1;
	a := !a + 1;
	assert(!a = aold + 1);
	if ed = 2 then ()
	else if nd_bool then
		loop b (ref 3) nd_bool (ed + 1)
	else loop b a nd_bool (ed + 1)

let main (nd:bool(*-:{v:Bool | true}*)) = 
	let x = ref 1 in 
  	let y = ref 2 in
  	loop x y nd 0