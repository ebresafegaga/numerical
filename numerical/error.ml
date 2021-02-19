

module type S = sig 
    val calc : float -> float -> float
    val within : float -> float -> eps:float -> bool 
end

module Absolute : S = struct 
    let calc a b = Float.abs (b -. a)
    let within a b ~eps = calc a b <= eps
end

module Relative : S = struct 
    let calc a b = Float.abs @@ (b -. a) /. b

    let within a b ~eps = calc a b <= eps
end