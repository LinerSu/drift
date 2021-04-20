
let rec mac91 (x:int(*<:{v:Int | v <= 101 }*)):int(*<:{v:Int | v = 91 }*) =
  if x > 100
  then x - 10
  else mac91 (mac91 (x + 11))

let main (n:int(*-:{v:Int | true}*)) =
    assert (mac91 n = 91)