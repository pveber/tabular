TABULAR
=====

`tabular` is a syntax extension derived from
[col](https://github.com/pveber/col), which is useful to handle data
that fit under tabular format. As in `col`, `tabular` generates helper
types, functions and objects from a record type-like definition.

The main evolution with respect to `col` is the representation of a
table as an object, and the introduction of operations that enable
manipulations similar to those used in [R](http://www.r-project.org/),
with data frames.

Here is an example:
```ocaml
type tabular bed = {
  chr        : string ;
  st "start" : int ;
  ed "end"   : int ;
  strand     : [`Sense "+" | `Antisense "-"] ;
}

let bed = 
  Bed.of_stream (
    Stream.of_list [
      { Bed.chr = "chr1" ; st = 1 ; ed = 3 ; strand = `Sense } ;
      { Bed.chr = "chr1" ; st = 3 ; ed = 5 ; strand = `Sense } ;
      { Bed.chr = "chr1" ; st = 4 ; ed = 28 ; strand = `Antisense } ;
      { Bed.chr = "chr4" ; st = 1 ; ed = 3 ; strand = `Sense } ;
    ]
  )

(* Filters the above [bed] value in order to keep intervals on the positive
   strand and prints the result in tabular format *)
let () = 
  Bed.output 
    stdout 
    Tabular.(bed#sub (bed#strand = !!`Sense && bed#st > !!1))
```

The `type tabular` declaration generates the following signature:
```
module Bed :
  sig
    type tabular_t =
      { chr : String.t; st : int; ed : int;
        strand : [ | `Sense | `Antisense ]
      }
   
    type data = tabular_t
   
    module Row :
      sig
        type t = tabular_t
       
        val labels : string list
         
        val of_array : string array -> t
         
        val to_list : t -> string list
         
        val stream_of_channel :
          ?line_numbers: bool ->
            ?header: bool -> ?sep: char -> in_channel -> t Stream.t
         
        val stream_to_channel :
          ?line_numbers: bool ->
            ?header: bool -> ?sep: char -> out_channel -> t Stream.t -> unit
         
      end
     
    module Obj :
      sig
        class type t =
          object
            method chr : String.t
            method st : int
            method ed : int
            method strand : [ | `Sense | `Antisense ]
          end
         
        val of_row : Row.t -> t
         
        val to_row : t -> Row.t
         
      end
     
    module Table :
      sig
        class type t =
          object
            method chr : String.t array
            method st : int array
            method ed : int array
            method strand : [ | `Sense | `Antisense ] array
            method row : int -> Row.t
            method sub : bool array -> t
            method length : int
            method labels : string list
            method stream : Row.t Stream.t
          end
         
        val of_stream : Row.t Stream.t -> t
         
        val stream : t -> Row.t Stream.t
         
        val to_channel :
          ?line_numbers: bool ->
            ?header: bool -> ?sep: char -> out_channel -> t -> unit
         
        val to_file :
          ?line_numbers: bool ->
            ?header: bool -> ?sep: char -> t -> string -> unit
         
        val latex_to_channel :
          ?line_numbers: bool -> out_channel -> t -> unit
         
        val of_channel :
          ?line_numbers: bool ->
            ?header: bool -> ?sep: char -> in_channel -> t
         
        val of_file :
          ?line_numbers: bool -> ?header: bool -> ?sep: char -> string -> t
         
      end
     
  end
```

This work is still in development. Next tasks before a first release are:
- take into account all options for input/output
- reintroduce col's "type tag" construct
