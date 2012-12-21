module type Gen = sig
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

module type Impl = functor (G : Gen) ->
sig 
  module Row : sig
    include module type of G.Row with type t = G.Row.t
    val stream_of_channel : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      in_channel -> t Stream.t
    val stream_to_channel : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      out_channel -> 
      t Stream.t ->     
      unit
  end
  module Table : sig
    include module type of G.Table with type t = G.Table.t
    val to_channel : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      out_channel -> t -> unit
    val to_file : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      t -> string -> unit
    val latex_to_channel : 
      ?line_numbers:bool ->
      out_channel -> t -> unit
    val of_channel : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      in_channel -> t
    val of_file : 
      ?line_numbers:bool ->
      ?header:bool ->
      ?sep:char ->
      string -> t
  end
end




















