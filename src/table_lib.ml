module Option = struct
  type 'a t = 'a option

  let value o ~default = match o with
    | Some x -> x
    | None -> default

  let map o ~f = match o with
    | Some x -> Some (f x)
    | None -> None
end

    
module Array = struct
  open Array

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
