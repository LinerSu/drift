

let rec loop lx ly ln = 
        let ny = 
            if (lx <= ln) then ly + 1 
            else if (lx >= ln+1) then ly - 1 
            else (-4) in
        if (ny < 0) then 
            if ln >= 0 then 
                if ny = (-1) then (lx < 2 * ln + 3)
                else false
            else false
        else loop (lx+1) ny ln 

let main n =
    let x = 0 in
    let y = 0 in
    if(n < 0) then
    assert(loop x y n = false)

let _ = main (-3)