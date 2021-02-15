
type result = 
    { a : float; b : float; c : float; 
      fa: float; fb: float; fc: float }

let pp { a; b; c; fa; fb; fc } = 
    Printf.printf "a = %f \t c = %f \t b = %f \n" a c b ; 
    Printf.printf "f(a) = %f \t f(c) = %f \t f(b) = %f \n\n" fa fc fb

let c_fp a b fa fb = 
    b -. ((fb *. (fa -. fb)) /. (a -. b)) 

let c_bisect a b _ _ = (a +. b) /. 2.

let bracket_with (a, b) f ~bracketer = 
    let fa, fb = f a, f b in
    let c = bracketer a b fa fb in
    let fc = f c in
    { a; b; c; fa; fb; fc }

let bisect = bracket_with ~bracketer:c_bisect

let false_position = bracket_with ~bracketer:c_fp

let next { a; b; c; fa; fb; fc } = 
    let fa_fc = fa *. fc in 
    if fa_fc < 0. then (a, c) else (c, b)

let iterate (a, b) f ~algorithm = 
    (a, b)
    |> Seq.unfold (fun endpoints ->  
        let result = algorithm endpoints f in 
        Some (result, next result))

let rec within seq eps =
    let open Seq in 
    match seq () with 
    | Cons ({ c = a } as x, xs) -> 
        (match xs () with 
        | Cons ({ c = b } as y, rest) when Float.abs (b -. a) <= eps -> 
            [ x;y ] |> List.to_seq
        | _ -> Seq.cons x (within xs eps))
    | _ -> failwith "seq should be infinite"