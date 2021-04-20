
type bstree [*<:{v:bstree | depth: {dp: Int | dp = ldp + 1 or dp = rdp + 1 }, 
		value: {val: Int | val < rval and val > lval or val = it } }*] =
  | Empty [*<:{v:bstree | depth: {dp: Int | dp = 0 }, value: {val: Int | False }}*]
  | Node of int [*<:value: {it: Int | True } *]
      * bstree [*<:{v:bstree | depth: {ldp: Int | True }, value: {lval: Int | True } }*]
      * bstree [*<:{v:bstree | depth: {rdp: Int | True }, value: {rval: Int | True } }*]

let main = 
  let et = Empty (* {v:bstree | depth: {dp: Int | dp = 0 }, value: Bot *) 
  in
  let et1 = Node(1, et, et) (* {v:bstree | depth: {dp: Int | dp = 1 }, value: {val: Int | v = 1} *)
  in
  ()