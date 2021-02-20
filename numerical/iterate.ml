

module type Algorithm = sig 
    type result
    type initial 
    val create_initial : float list -> initial
    val algorithm : initial -> (float -> float) -> result 
    val next : result -> initial
    val pp : result -> unit 
    val root : result -> float 
end

module Trivial : Algorithm = struct 
    type result = unit 
    type initial = unit 
    let create_initial _ = () 
    let algorithm _ _ = () 
    let next _ = () 
    let pp _ = () 
    let root _ = Float.zero
end

module type Iterate= sig 
    include Algorithm
    val within : result Seq.t -> float -> result Seq.t
    val iterate : initial -> (float -> float) -> result Seq.t
    val error : initial -> (float -> float) -> float Seq.t
end

module Make (A : Algorithm) (E : Error.Error) : Iterate = struct 
    include A

    let rec within seq eps =
        let open Seq in 
        match seq () with 
        | Cons (x, xs) -> 
            begin match xs () with 
            | Cons (y, _) when E.within ~eps (root x) (root y) -> List.to_seq [x;y]
            | _ -> Seq.cons x (within xs eps) end
        | _ -> failwith "seq should be infinite"
    
    
    (* let iterate_abs ~left ~right initial f = 
        initial
        |> Seq.unfold (fun prev ->  
            let result = algorithm prev f in 
            Some (result, next result)) *)

    let iterate initial f = 
        initial
        |> Seq.unfold (fun prev ->  
            let result = algorithm prev f in 
            Some (result, next result))
    
    let error initial f = 
        (initial, 0.) 
        |> Seq.unfold (fun (prev, r0) ->
            let result = algorithm prev f in 
            let r1 = root result in
            let err = E.calc r0 r1 in
            Some (err, (next result, r1)))
end