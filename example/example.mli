module Bed3 : sig
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
  }
  [@@deriving tabular]
end
