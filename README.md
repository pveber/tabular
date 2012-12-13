TABLE
=====

`table` is a syntax extension derived from [col](https://github.com/pveber/col), which is useful to handle data that fit under tabular format. As in `col`, `table` generates helper types, functions and objects from a record type-like definition.

```
type table bed = {
  chr        : String ;
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

let () = 
  Bed.output 
    stdout 
    Table.(bed#sub (bed#strand = !!`Sense && bed#st > !!1))
```


