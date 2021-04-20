
type mylist[*<:{v:mylist | length: {l: Int | l = lp + 1 }, 
				item: {e: Int | e = i or e = ep }}*] =
  | Nil [*<:{v:mylist | length: {l: Int | l = 0 }, item: {e: Int | False }} *]
  | List of int [*<:item: {i: Int | True } *]
    * mylist [*<:{v:mylist | length: {lp: Int | True }, item: {ep: Int | True }} *]


let main = 
  let l1 = Nil (* {v:mylist | length: {l: Int | l = 0 }, item: Bot *) 
  in
  let l2 = List(1, l1) (* {v:mylist | length: {l: Int | l = 1 }, item: {e: Int | e = 1} *)
  in
  ()