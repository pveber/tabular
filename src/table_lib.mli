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
  val empty : unit -> 'a t
  val next: 'a t -> 'a option
  val next_exn: 'a t -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val to_array : 'a t -> 'a array
  val lines_of : in_channel -> string t
  val init : int -> f:(int -> 'a) -> 'a t
end

module type TabularType = sig
  type row
  type table = private < labels : string list; length : int; row : int -> row; stream : row Stream.t ; .. >
  val list_of_row : row -> string list
  val row_of_array : string array -> row
  val table_of_stream : row Stream.t -> table
end

module Impl(X : TabularType) : sig

  type s = < row : X.row ; table :X.table >
  val table_to_channel : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    out_channel -> X.table -> unit
  val table_to_file : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    X.table -> string -> unit
  val latex_table_to_channel : 
    ?line_numbers:bool ->
    out_channel -> X.table -> unit
  val table_of_channel : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    in_channel -> X.table
  val table_of_file : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    string -> X.table
  val table_of_stream :
    X.row Stream.t -> X.table
  val stream_of_channel : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    in_channel -> X.row Stream.t
  val stream_to_channel : 
    ?line_numbers:bool ->
    ?sep:char ->
    out_channel -> 
    X.row Stream.t ->     
    unit
end
