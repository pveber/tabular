module type T = sig
  module Row : sig
    type t
    val labels : string list
    val of_array : string array -> t
    val to_list : t -> string list
  end
  module Table : sig
    type t
    val stream : t -> Row.t Stream.t
    val of_stream : Row.t Stream.t -> t
  end
end

module type Impl = functor (X : T) ->
sig 
  module Row : sig
    val stream_of_channel : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      in_channel -> X.Row.t Stream.t
    val stream_to_channel : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      out_channel -> 
      X.Row.t Stream.t ->     
      unit
  end
  module Table : sig
    val to_channel : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      out_channel -> X.Table.t -> unit
    val to_file : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      X.Table.t -> string -> unit
    val latex_to_channel : 
      ?line_numbers:bool ->
      out_channel -> X.Table.t -> unit
    val of_channel : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      in_channel -> X.Table.t
    val of_file : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      string -> X.Table.t
  end
end




















