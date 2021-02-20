
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

    let rec take n seq () = 
        match seq (), n with 
        | _, 0 -> Nil
        | Cons (x, xs), n -> Cons (x, (take (pred n) xs))
        | Nil, _ -> Nil
    
    let iteri f seq =
        let rec aux seq n = match seq () with
            | Nil -> ()
            | Cons (x, next) ->
                f n x;
                aux next (n+1)
        in
        aux seq 0
    
    let rec combine_with ~f xs ys () = 
        match xs (), ys () with 
        | Cons (x, xs), Cons (y, ys) -> Cons ((f x y), (combine_with ~f xs ys))
        | Nil, _ | _, Nil -> Nil

end

let (>>) f g a = a |> f |> g

module String = struct 
    include String
    let to_list = String.to_seq >> List.of_seq
    let of_list = List.to_seq >> String.of_seq
end
