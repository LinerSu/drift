(*
USED: PLDI2011 as e-simpl
USED: PEPM2013 as e-simpl
FROM: Leroy & Pessaux, "Type-Based Analysis of Uncaught Exceptionis, " TOPLAS, 2000

let ff n = if n >= 0 then () else raise (Failer 0)
let main n = try ff n with Failer 0 -> ()
*)


let f fn fk = if fn >= 0 then assert(true) else fk 0
let g gn = assert (gn = 0)

let main (n:int) =
    f n g

let _ = main (-3)
let _ = main 15
let _ = main 30
let _ = main (-43)
let _ = main 0
let _ = main (-3434)
