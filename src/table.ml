type ('row, 'a) t = {
  col_names : string list ;
  cols : col list ;
  row : col list -> int -> 'row
}
and col =
  | String of string array
  | Float of float array
  | Int of int array
  | Bool of bool array

