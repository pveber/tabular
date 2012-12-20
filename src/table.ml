module type S = sig
  type row
  type table
  type s = < row : row ; table :table >
  val row_of_array : string array -> row
  val table_to_channel : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    out_channel ->
    table -> unit
  val table_to_file : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    table -> string -> unit
  val latex_table_to_channel : 
    ?line_numbers:bool ->
    out_channel -> table -> unit
  val table_of_channel : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    in_channel -> table
  val table_of_file : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    string -> table
  val table_of_stream :
    row Stream.t -> table
  val stream_of_channel : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    in_channel -> row Stream.t
  val stream_to_channel : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    out_channel -> 
    row Stream.t ->     
    unit
end

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

let count x = Array.fold_left Pervasives.(fun accu b -> if b then accu + 1 else accu) 0 x




















