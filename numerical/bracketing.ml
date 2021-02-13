
module Seq = struct 
    include Seq 
    let head (seq: 'a Seq.t) : 'a option = 
        match seq () with 
        | Cons (x, _) -> Some x
        | Nil -> None
    
    let head_exn (seq: 'a Seq.t) : 'a = 
        match seq () with 
        | Cons (x, _) -> x
        | _ -> failwith "head of empty sequence"

    let rec take n seq = 
        match seq (), n with 
        | _, 0 -> []
        | Cons (x, xs), n -> x :: take (pred n) xs
        | Nil, _ -> [] (* This is not okay! *)
end

type result = { a : float; b : float; c : float; fa: float; fb: float; fc: float }

let pp out { a; b; c; fa; fb; fc } = 
    Format.fprintf out "a = %f \t c = %f \t b = %f \n" a c b ; 
    Format.fprintf out "f(a) = %f \t f(c) = %f \t f(b) = %f \n\n" fa fc fb

let bisect (a, b) f =
    let fa, fb = f a, f b in
    let c = (a +. b) /. 2. in
    let fc = f c in
    { a; b; c; fa; fb; fc }

let bisect_next { a; b; c; fa; fb; fc } = 
    let fa_fc = fa *. fc in 
    if fa_fc < 0. then (a, c) else (c, b) 

let bisect_iterate (a, b) f = 
    (a, b)
    |> Seq.unfold (fun endpoints ->  
        let result = bisect endpoints f in 
        Some (result, bisect_next result))

let rec within eps seq =
    let open Seq in 
    match seq () with 
    | Cons (a, xs) -> 
        (match xs () with 
        | Cons (b, rest) when Float.abs (b -. a) <= eps -> b 
        | _ -> within eps xs)
    | _ -> failwith "seq should be infinite"