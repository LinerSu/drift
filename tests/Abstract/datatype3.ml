

type iorb [*<:{v:iorb | integer: {i: Int | i1 or i2 }, boolean: {b: Bool | b1 or b2 }*] = 
  | Int of int  [*<:{v:iorb | integer: {i1: Int | True }, boolean: {b1: Bool | False }*]
  | Bool of bool [*<:{v:iorb | integer: {i2: Int | False }, boolean: {b2: Bool | True }*]

let main = 
  let a1 = Int 1 (* {v:iorb | integer: {i: Int | i = 1 }, boolean: Bot *) 
  in
  let a2 = Bool true (* {v:iorb | integer: Bot, boolean: {b1: Bool | true} *)
  in
  ()