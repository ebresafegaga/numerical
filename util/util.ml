
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
        | _, 0 -> Seq.empty
        | Cons (x, xs), n -> cons x (take (pred n) xs)
        | Nil, _ -> Seq.empty (* This is not okay! *)
    
    let iteri f seq =
        let rec aux seq n = match seq () with
            | Nil -> ()
            | Cons (x, next) ->
                f n x;
                aux next (n+1)
        in
    aux seq 0

end