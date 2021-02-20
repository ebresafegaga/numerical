
type c_func = float -> float -> float -> float -> float

let c_fp a b fa fb = 
    b -. ((fb *. (a -. b)) /. (fa -. fb)) 

let c_bisect a b (_:float) (_:float) = (a +. b) /. 2.

module Make (CF : sig val cf: c_func end) : Iterate.Algorithm = struct 

    type result = 
        { a : float; b : float; c : float; 
          fa: float; fb: float; fc: float }

    type initial = float * float 

    let create_initial = function 
        | [x;y] -> x, y 
        | _ -> failwith "Expected exaclty two elements for initial"

    let root { c; _ } = c

    let pp { a; b; c; fa; fb; fc } = 
        Printf.printf "a = %f \t c = %f \t b = %f \n" a c b ; 
        Printf.printf "f(a) = %f \t f(c) = %f \t f(b) = %f \n\n" fa fc fb

    let bracket_with (a, b) f ~bracketer = 
        let fa, fb = f a, f b in
        let c = bracketer a b fa fb in
        let fc = f c in
        { a; b; c; fa; fb; fc }
    
    let algorithm = bracket_with ~bracketer:CF.cf

    let next { a; b; c; fa; fc; _ } = 
        let fa_fc = fa *. fc in 
        if fa_fc < 0. then (a, c) else (c, b)
end 

module Bisection = Make (struct let cf = c_bisect end)

module FalsePosition = Make (struct let cf = c_fp end)