let ( !! ) x = [| x |]

let rec pgcd a b =
  if a = b then a
  else if a > b then
    let r = a mod b in
    if r = 0 then b
    else pgcd b r
  else pgcd b a

let ppcm a b = a * b / (pgcd a b)

let map2 f u v =
  let m = Array.length u
  and n = Array.length v in
  Array.init
    (ppcm m n)
    (fun k -> 
      let i = k mod m
      and j = k mod n in
      f u.(i) v.(j))

let ( = ) x y  = map2 ( = ) x y
let ( < ) x y  = map2 ( < ) x y
let ( > ) x y  = map2 ( > ) x y
let ( <> ) x y = map2 ( <> ) x y
let ( <= ) x y = map2 ( <= ) x y
let ( >= ) x y = map2 ( >= ) x y
let ( + ) x y  = map2 ( + ) x y
let ( - ) x y  = map2 ( - ) x y
let ( * ) x y  = map2 ( * ) x y
let ( / ) x y  = map2 ( / ) x y
let ( +. ) x y = map2 ( +. ) x y
let ( -. ) x y = map2 ( -. ) x y
let ( *. ) x y = map2 ( *. ) x y
let ( /. ) x y = map2 ( /. ) x y
let ( && ) x y = map2 ( && ) x y
let ( || ) x y = map2 ( || ) x y
let not = Array.map not
