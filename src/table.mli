(*
module Bed : sig
  type row = {
    chr : string ;
    st : int ;
    ed : int ;
  }
  class table : object
    method chr : string array
    method st : int array
    method ed : int array
    method row : int -> row
    method sub : bool array -> table
  end
  val output : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    out_channel -> table -> unit
  val latex_output : 
    ?line_numbers:bool ->
    out_channel -> table -> unit
  val input : 
    ?line_numbers:bool ->
    ?header:bool ->
    ?sep:char ->
    in_channel -> table -> unit
end

bed#sub (bed#st > 1)
    
*)
