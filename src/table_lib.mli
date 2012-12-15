val id : 'a -> 'a
val ( |! ) : 'a -> ('a -> 'b) -> 'b

val int_of_string : exn -> string -> int
val float_of_string : exn -> string -> float

module Option : sig
  type 'a t = 'a option
  val value : 'a t -> default:'a -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module String : sig
  include module type of String with type t = string
  val split : sep:char -> string -> string array
end

module Array : sig
  val sub : 'a array -> index:bool array -> 'a array
end

module Stream : sig
  include module type of Stream with type 'a t = 'a Stream.t
  val next: 'a t -> 'a option
  val next_exn: 'a t -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val to_array : 'a t -> 'a array
  val lines_of : in_channel -> string t
end

val input : 
  header:bool -> 
  row_of_array:(string array -> 'row) ->
  of_stream:('row Stream.t -> 'table) ->
  in_channel ->
  'table

val output : 
  header:bool -> 
  list_of_row:('row -> string list) ->
  out_channel ->
  < length : int ; labels : string list ; row : int -> 'row ; .. > ->
  unit



















