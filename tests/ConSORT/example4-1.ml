
let main = 
	let x = ref 0 in 
	let y = x in
	x := 1; 
	y := 2;
	assert(!y = !x)