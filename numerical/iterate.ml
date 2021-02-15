

module type Algorithm = sig 
    type result
    type initial 
    val create_initial : float list -> initial
    val algorithm : initial -> (float -> float) -> result 
    val next : result -> initial
    val pp : result -> unit 
    val root : result -> float 
end

module type Intf = sig 
    include Algorithm
    val within : result Seq.t -> float -> result Seq.t
    val iterate : initial -> (float -> float) -> result Seq.t
end

module Make (A : Algorithm) : Intf = struct 
    include A 

    let rec within seq eps =
        let open Seq in 
        match seq () with 
        | Cons (x, xs) -> 
            (match xs () with 
            | Cons (y, _) when Float.abs (root y -. root x) <= eps -> 
                [ x;y ] |> List.to_seq
            | _ -> Seq.cons x (within xs eps))
        | _ -> failwith "seq should be infinite"
    
    let iterate initial f = 
        initial
        |> Seq.unfold (fun prev ->  
            let result = algorithm prev f in 
            Some (result, next result))
end