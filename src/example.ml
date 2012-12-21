module String = struct
  include String
  let of_string x = x
  let to_string x = x
end

module Bed : sig
  type table bed = {
    chr        : String ;
    st "chrom_start" : int ;
    ed "chrom_end"   : int ;
    strand     : [`Sense "+" | `Antisense "-"] ;
  }
end =
struct
  type table bed = {
    chr        : String ;
    st "chrom_start" : int ;
    ed "chrom_end"   : int ;
    strand     : [`Sense "+" | `Antisense "-"] ;
  }
end

let bed = 
  Bed.Table.of_stream (
    Stream.of_list [
      { Bed.chr = "chr1" ; st = 1 ; ed = 3 ; strand = `Sense } ;
      { Bed.chr = "chr1" ; st = 3 ; ed = 5 ; strand = `Sense } ;
      { Bed.chr = "chr1" ; st = 4 ; ed = 28 ; strand = `Antisense } ;
      { Bed.chr = "chr4" ; st = 1 ; ed = 3 ; strand = `Sense } ;
    ]
  )

let filtered_table = Table.(bed#sub (bed#strand = !!`Sense && bed#st > !!1))
let () = 
  Bed.Table.to_channel stdout filtered_table ;
  print_newline () ;
  Bed.Table.latex_to_channel stdout filtered_table ;
  print_newline ()


    










