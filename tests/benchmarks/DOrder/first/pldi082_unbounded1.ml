
(*
Delay widen = 300
opt = 300
*)

let main (n(*-:{v:Int | v >= 0}*)) =

    let rec loop lx ly ln = 
        let ny = 
            if (lx <= ln) then ly + 1 
            else if (lx >= ln+1) then ly - 1 
            else (-4) in
        if (ny < 0) then 
            if ln >= 0 then 
                if ny = (-1) then (lx < 2 * ln + 3)
                else true
            else true
        else loop (lx+1) ny ln 
    in

    let x = 0 in
    let y = 0 in
    assert(loop x y n = true)
(* in assert (main 10 = true) *)