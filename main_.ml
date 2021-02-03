

let rec fix f = f (fun x -> (fix f) x)

(* let rec fix f = f (fix f) *)

let fix1 =
  let rec fix f = lazy (f (fix f)) in
  fix
  
let fact =
  fix (fun fact n ->
         match n with
         | n when n <= 1 -> 1
         | n -> fact (n-1) + fact (n-2))

let map  =
  fix (fun map f xs ->
      match xs with
      | x :: xs -> f x :: map f xs
      | [] -> [])
  
let () =
  let n = 5 in
  Printf.printf "The factorial of %d is %d" n (fact n)
