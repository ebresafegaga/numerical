
module Sign = struct 
    type t = Positive | Negative 
    let sign (x : float) = if x < 0. then Negative else Positive
end

(*
    Error = b-c / 2

*)

type result =
    { a : float;
      b : float;
      c : float;
      fa: float; 
      fb: float; 
      fc: float }

type endpoint = {aP : float; bP : float }

let bisect f calc {aP = a; bP = b} k =
    let fa, fb = f a, f b in
    let c = calc fa fb a b in
    let fc = f c in
    let fa_fc = fa *. fc in
    let fb_fc = fb *. fc in
    match Sign.sign fa_fc, Sign.sign fb_fc with 
    | Positive, Negative -> (k { a; b; c; fa; fb; fc }), { aP = c; bP = b} 
    | Negative, Positive -> (k { a; b; c; fa; fb; fc }), { aP = a; bP = c}
    | _ -> failwith "bisection failed"

let bisect_calc _fa _fb a b = (a +. b) /. 2.
let falsi_calc fa fb a b = 
    b -. ((fb *. (a -. b)) /. (fa -. fb))

let f x = (x*.x*.x) -. (3. *. x) +. 1.

(* TODO change console color to show how we're picking the new endpoints *)
let print { a; b; c; fa; fb; fc } = 
    Printf.printf "a = %f \t c = %f \t b = %f \n" a c b ; 
    Printf.printf "f(a) = %f \t f(c) = %f \t f(b) = %f \n\n" fa fc fb

 
let main f c a b =
    let rec progn n a b = 
        Printf.printf "\n\n"  ;
        Printf.printf "ITERATION %d \n" n ;
        let _, {aP = nextA; bP = nextB} = bisect f c { aP = a; bP = b } print in
        print_string "Continue? " ;
        let s = read_line () in   
        match String.lowercase_ascii s with 
        | "y" | "yes" -> progn (n+1) nextA nextB
        | _ -> ()
    in 
    progn 1 a b

let () = 
    print_string "f(x) =  " ;
    let fx = read_line () |> String.trim  in
    let exp = Parser.parseAll Lang.exp fx in
    let f = Lang.eval exp in
    print_string "a =  " ;
    let a = read_float () in 
    print_string "b = ";
    let b = read_float () in 
    print_string "Bisection (B/b) or Regular Falsi (RF/rf) ? " ;
    match read_line () |> String.lowercase_ascii |> String.trim with 
    | "b" -> main f bisect_calc a b
    | _ -> main f falsi_calc a b

