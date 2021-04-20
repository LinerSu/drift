
let main = 
	let x = ref 5 in 
  	let y = x in
  	y := 4; 
  	assert(!y = 4)