
let main =
	let p = ref 3 in
	let q = ref 5 in
	p := !p + 1; (* let a = ref (! a + 1) in () *)
	q := !q + 1;
	assert(!p = 4)