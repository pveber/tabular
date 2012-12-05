(*

bed#sub (bed#st > 1)

Table.(bed#sub (bed#st > !!1))
    
*)

val ( !! ) : 'a -> 'a array

val ( = ) : 'a array -> 'a array -> bool array
val ( < ) : 'a array -> 'a array -> bool array
val ( > ) : 'a array -> 'a array -> bool array
val ( <= ) : 'a array -> 'a array -> bool array
val ( >= ) : 'a array -> 'a array -> bool array
val ( <> ) : 'a array -> 'a array -> bool array

val ( + ) : int array -> int array -> int array
val ( - ) : int array -> int array -> int array
val ( / ) : int array -> int array -> int array
val ( * ) : int array -> int array -> int array

val ( +. ) : float array -> float array -> float array
val ( -. ) : float array -> float array -> float array
val ( /. ) : float array -> float array -> float array
val ( *. ) : float array -> float array -> float array

