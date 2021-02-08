
module Sign = struct 
    type t = Positive | Negative 
    let sign (x : float) = if x < 0. then Negative else Positive
end