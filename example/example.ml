module Bed3 = struct
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
  }
  [@@deriving tabular]
end
