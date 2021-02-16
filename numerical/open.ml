(* 
  Open methods for numerical analysis
*)

module Newton : Iterate.Algorithm = struct 

type result = { x: float; fx: float; fx': float }

type initial = float 

let create_initial = function 
    | [x] -> x 
    | _ -> failwith "Expected exactly one element for inital"

let root { x; _ } = x

let next = root 

let pp { x; fx; fx' } = 
    Printf.printf "x = %f \n" x ; 
    Printf.printf "f(x) = %f \n" fx ;
    Printf.printf "f'(x) = %f \n" fx' 

let algorithm x f =
    let fx' = Deriv.derivative f x in
    let fx = f x in
    let x =  x -. (fx /. fx') in
    { x; fx; fx' }
    
end 


module Secant : Iterate.Algorithm = struct 

    type result = { x0: float; x1: float ; fx0: float; fx1: float }
    
    type initial = float * float
    
    let create_initial = function 
        | [x0;x1] -> x0, x1 
        | _ -> failwith "Expected exactly two elements for inital"
    
    let root result = result.x1
    
    let next { x0; x1 } = (x0, x1)
    
    let pp result = 
        Printf.printf "Xi-2 = %f \n" result.x0 ; 
        Printf.printf "Xi-1 = %f \n" result.x1 ; 
        Printf.printf "f(Xi-2) = %f \n" result.fx0 ;
        Printf.printf "f(Xi-1) = %f \n" result.fx1   
    
    let algorithm (x0, x1) f =
        let fx0, fx1 = f x0, f x1 in 
        let x = x1 -. fx1 *. ((x1 -. x0) /. (fx1 -. fx0)) in
        { x0 = x1; x1 = x; fx0; fx1 }
        
    end 