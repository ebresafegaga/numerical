
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