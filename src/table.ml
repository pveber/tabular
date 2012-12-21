let ( !! ) x = [| x |]

let rec gcd a b =
  if a = b then a
  else if a > b then
    let r = a mod b in
    if r = 0 then b
    else gcd b r
  else gcd b a

let lcm a b = a * b / (gcd a b)

let map2 f u v =
  let m = Array.length u
  and n = Array.length v in
  Array.init
    (lcm m n)
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

let count x = Array.fold_left Pervasives.(fun accu b -> if b then accu + 1 else accu) 0 x




















