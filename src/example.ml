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
