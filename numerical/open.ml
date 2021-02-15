(* 
  Open methods for numerical analysis
*)

module Newton : Iterate.Algorithm = struct 

type result = { x: float; fx: float; fx': float option }

type initial = float 

let create_initial = function 
    | [x] -> x 
    | _ -> failwith "Expected exactly one element for inital"

let root { x; _ } = x

let next = root 

let pp { x; fx; fx' } = 
    Printf.printf "x = %f \n" x ; 
    Printf.printf "f(x) = %f \n" fx ;
    if Option.is_some fx' then 
        fx'
        |> Option.value ~default:0.
        |> Printf.printf "f'(x) = %f \n" 

let algorithm x f =
    let fx' = Deriv.derivative f x in
    let fx = f x in
    let x =  x -. (fx /. fx') in
    { x; fx; fx' = Some fx' }
    
end 