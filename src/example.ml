module String = struct
  include String
  let of_string x = x
  let to_string x = x
end

module X : sig
  type table bed = {
    chr : String ;
    st "start" : int ;
    ed "end"   : int ;
    strand : [`Sense "+" | `Antisense "-"] ;
  }
end =
struct
  type table bed = {
    chr : String ;
    st "start" : int ;
    ed "end"   : int ;
    strand : [`Sense "+" | `Antisense "-"] ;
  }
end
