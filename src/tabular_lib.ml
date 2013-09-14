open Printf

let id = fun x -> x
let ( |! ) x f = f x

let bool_of_string (*~field*) s =
  let field = "" in
  try bool_of_string s
  with _ -> (
    let msg = sprintf "Failed while parsing field %s of type bool: %S" field s in
    failwith msg
  )

let int_of_string (* ~field *) s =
  let field = "" in
  try int_of_string s
  with _ -> (
    let msg = sprintf "Failed while parsing field %s of type int: %S" field s in
    failwith msg
  )

let float_of_string (* ~field *) s =
  let field = "" in
  try float_of_string s
  with _ -> (
    let msg = sprintf "Failed while parsing field %s of type float: %S" field s in
    failwith msg
  )

let string_of_float = string_of_float
let string_of_int = string_of_int
let string_of_bool = string_of_bool

let option_of_string f s =
  if s = "" then None
  else Some (f s)

module Option = struct
  type 'a t = 'a option

  let value o ~default = match o with
    | Some x -> x
    | None -> default

  let map o ~f = match o with
    | Some x -> Some (f x)
    | None -> None

  let of_string f s =
    if s = "" then None
    else Some (f s)

  let to_string f = function
  | None -> ""
  | Some x -> f x

end

module Array = struct
  include Array

  (* borrowed from Jane St core *)
  let filter_opt t =
    let n = length t in
    let res_size = ref 0 in
    let first_some = ref None in
    for i = 0 to n - 1 do
      begin match t.(i) with
      | None -> ()
      | Some _ as s ->
	if !res_size = 0 then first_some := s;
	incr res_size;
      end;
    done;
    match !first_some with
    | None -> [||]
    | Some el ->
      let result = make !res_size el in
      let pos = ref 0 in
      for i = 0 to n - 1 do
	begin match t.(i) with
	| None -> ()
	| Some x ->
          result.(!pos) <- x;
          incr pos;
	end;
      done;
      result

  let sub xs ~index =
    filter_opt (mapi (fun i x -> if index.(i) then Some x else None) xs)
end

module String =
struct
  include String

  let count_occurences ch s =
    let accu = ref 0 in
      for i = 0 to String.length s - 1 do
	if s.[i] = ch then incr accu
      done ;
      !accu

  let split ~sep x =
    let n = String.length x in
    let m = count_occurences sep x + 1 in
    let res = Array.make m "" in
    let rec search k i j =
      if j >= n then res.(k) <- String.sub x i (j - i)
      else (
	if x.[j] = sep then (
	  res.(k) <- String.sub x i (j - i) ;
	  search (k + 1) (j + 1) (j + 1)
	)
	else search k i (j + 1)
      )
    in
      search 0 0 0 ;
      res
end

(** This code is taken from Biocaml *)
module Stream = struct
  include Stream

  let empty () = from (fun _ -> None)

  let next_exn = next
  let next s = try Some (next_exn s) with Stream.Failure -> None

  let rec foldi xs ~init ~f =
    match next xs with
    | None -> init
    | Some x -> foldi xs ~init:(f (count xs - 1) init x) ~f
(* [count xs - 1] because of the call to [next], which increased the
   stream count by one *)

  let fold xs ~init ~f = foldi xs ~init ~f:(fun _ -> f)

  let to_list t =
    List.rev (fold ~init:[] ~f:(fun l b -> b::l) t)

  let to_array t = Array.of_list (to_list t)

  let map xs ~f =
    let aux _ = Option.map (next xs) ~f in
    from aux

  let filter_map xs ~f =
    let rec aux i =
      match next xs with
      | Some x -> (
	match f x with
	| None -> aux i
	| x -> x
      )
      | None -> None
    in
    from aux



  let lines_of ic =
    from (fun _ -> try Some (input_line ic) with End_of_file -> None)

  let init n ~f =
    if n < 0 then empty ()
    else (
      let aux i =
        if i < n then Some (f i)
        else None
      in
      from aux
    )


  let skip_one xs =
    junk xs ;
    from (fun _ -> next xs)
end

let row_conversion_fail expected got =
  let to_string l = Printf.sprintf "[ %s ]" (String.concat " ; " (List.map (sprintf "%S") l)) in
  let msg = Printf.sprintf "Couln't parse a row. Expected:\n%s\nbut got:\n%s\n" (to_string expected) (to_string got) in
  failwith msg

let input ~header ~comment_char ~row_of_array ~of_stream ic =
  Stream.lines_of ic
  |! (if header then Stream.skip_one else id)
  |! Stream.filter_map ~f:(
    fun x ->
      if x = "" || x.[0] = comment_char
      then None
      else Some (String.split ~sep:'\t' x)
  )
  |! Stream.map ~f:row_of_array
  |! of_stream

let output_list sep oc = function
| [] -> ()
| h :: t ->
    output_string oc h ;
    List.iter (fun x -> output_string oc sep ; output_string oc x) t

let output ?header ~list_of_row oc rows =
  begin
    match header with
    | None -> ()
    | Some labels ->
        output_list "\t" oc labels ;
        output_char oc '\n'
  end ;
  Stream.iter
    (fun r ->
      output_list "\t" oc (list_of_row r) ;
      output_char oc '\n')
    rows

let replace ~char ~by x =
  let rec aux i =
    try
      let j = String.index_from x i char in
      (String.sub x i (j - i)) ^ by ^ (aux (j + 1))
    with Not_found -> String.(
      if i = 0 then x
      else sub x i (length x - i) (* slight optimisation *)
    )
  in
  aux 0

let latex_escape = replace ~char:'_' ~by:"\\_"

let latex_output ~header ~list_of_row oc xs =
  fprintf oc
    "\\begin{tabular}{%s}\n"
    (List.map (fun _ -> "c") header |! String.concat "") ;

  output_list
    " & " oc
    (List.map (fun l -> sprintf "{ \\bf %s }" (latex_escape l)) header) ;
  output_string oc "\\\\\n\\hline\n" ;

  Stream.iter
    (fun r ->
      list_of_row r
      |! List.map latex_escape
      |! output_list " & " oc ;
      output_string oc "\\\\\n")
    xs ;

  output_string oc "\\end{tabular}"

open Tabular_sig

module Impl(X : T) = struct
  module Row = struct
    include X.Row
    let stream_of_channel ?(line_numbers = false) ?(header = false) ?(sep = '\t') ic =
      input ~header ~comment_char:'\000' ~row_of_array:of_array ~of_stream:(fun x -> x) ic

    let stream_to_channel ?(line_numbers = false) ?(header = false) ?(sep = '\t') oc rows =
      output ?header:(if header then Some labels else None) ~list_of_row:to_list oc rows
  end

  module Table = struct
    include X.Table
    let string_of_row r = String.concat "\t" (Row.to_list r)

    let to_channel
        ?(line_numbers = false)
        ?(header = true)
        ?(sep = '\t')
        oc table =
      output ?header:(if header then Some Row.labels else None) ~list_of_row:Row.to_list oc (stream table)

    let to_file ?line_numbers ?header ?sep table path =
      let oc = open_out path in
      to_channel ?line_numbers ?header ?sep oc table ;
      close_out oc

    let latex_to_channel ?(line_numbers = false) oc table =
      latex_output ~header:Row.labels ~list_of_row:Row.to_list oc (stream table)

    let of_channel ?(line_numbers = false) ?(header = false) ?(sep = '\t') ?(comment_char = '\000') ic =
      input ~header ~comment_char ~row_of_array:Row.of_array ~of_stream:of_stream ic

    let of_file ?line_numbers ?header ?sep ?comment_char path =
      let ic = open_in path in
      let r = of_channel ?line_numbers ?header ?sep ic in
      close_in ic ; r
  end

end



















