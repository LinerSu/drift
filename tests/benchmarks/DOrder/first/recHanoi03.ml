  
(*
 * recHanoi.c
 *
 *  Created on: 17.07.2013
 *      Author: Stefan Wissert
 *)


(*
 * This function returns the optimal amount of steps,
 * needed to solve the problem for n-disks
 *)

let main (k(*-:{v:Int | true}*)) =
    let rec hanoi n =
        if (n <= 1) then 1
        else 2 * (hanoi (n - 1)) + 1
    in

    let n = 13 in
    let result = hanoi n in
    assert (result >= n)
(* in
main ()  *)