module Option : sig
  type 'a t = 'a option
  val value : 'a t -> default:'a -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
end
