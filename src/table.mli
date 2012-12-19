(*

bed#sub (bed#st > 1)

Table.(bed#sub (bed#st > !!1))
    
*)

module type S = sig
  type row
  type table
  type s = < row : row ; table :table >
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

val ( !! ) : 'a -> 'a array

val ( = ) : 'a array -> 'a array -> bool array
val ( < ) : 'a array -> 'a array -> bool array
val ( > ) : 'a array -> 'a array -> bool array
val ( <= ) : 'a array -> 'a array -> bool array
val ( >= ) : 'a array -> 'a array -> bool array
val ( <> ) : 'a array -> 'a array -> bool array

val ( + ) : int array -> int array -> int array
val ( - ) : int array -> int array -> int array
val ( / ) : int array -> int array -> int array
val ( * ) : int array -> int array -> int array

val ( +. ) : float array -> float array -> float array
val ( -. ) : float array -> float array -> float array
val ( /. ) : float array -> float array -> float array
val ( *. ) : float array -> float array -> float array

val ( && ) : bool array -> bool array -> bool array
val ( || ) : bool array -> bool array -> bool array
val not : bool array -> bool array

val count : bool array -> int










