module Option : sig
  type 'a t = 'a option
  val value : 'a t -> default:'a -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module Array : sig
  val sub : 'a array -> index:bool array -> 'a array
end
