
let main (z(*-:{v:Int | true}*)) =
    let rec loopa ax ay az = 
        if (ay < 20) then
            loopa (ax+10) (ay+1) az
        else 
            ax > az || ay < az + 1
    in

    let x = 0 in
    let y = 0 in
    assert (loopa x y z = true)

(* in assert (main 10 = true) *)