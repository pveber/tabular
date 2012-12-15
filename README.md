TABLE
=====

`table` is a syntax extension derived from
[col](https://github.com/pveber/col), which is useful to handle data
that fit under tabular format. As in `col`, `table` generates helper
types, functions and objects from a record type-like definition.

The main evolution with respect to `col` is the representation of a
table as an object, and the introduction of operations that enable
manipulations similar to those used in [R](http://www.r-project.org/),
with data frames.

Here is an example:
```ocaml
type table bed = {
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
    Table.(bed#sub (bed#strand = !!`Sense && bed#st > !!1))
```

This work is still in development. Next tasks before a first release are:
- take into account all options for input/output
- an example of the generated signature for documentation
- reintroduce col's "type tag" construct
