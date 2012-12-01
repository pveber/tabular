module Option = struct
  type 'a t = 'a option

  let value o ~default = match o with
    | Some x -> x
    | None -> default

  let map o ~f = match o with
    | Some x -> Some (f x)
    | None -> None
end

    
