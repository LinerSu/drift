
let main = 
    let x = ref 1 in
    let y = x in
    x := 0;
    let z3 = !x in 
    assert(z3 = 0)