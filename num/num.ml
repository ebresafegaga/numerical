open Numerical
open Util

let func = ref ""
let iter = ref 0 
let a = ref 0. 
let b = ref 0.


let usage = "Usage: num [options]"
let anonymous str = ()
let options = Arg.align 
    [ ( "--method", 
        Arg.Symbol (["bisection"; "FP"], fun str -> ()), 
        " Specify the numerical analysis algorithm to use"); 
      
       ( "--function",
         Arg.Set_string func, 
         " Specify a function to solve");
    
       ( "--iterations", 
         Arg.Set_int iter, 
         " Specify the number of iterations");
    
       ( "-a", 
         Arg.Set_float a, 
         " Specify the left endpoint for a bisection");
       
       ( "-b",
         Arg.Set_float b, 
         " Specify the right endpoint for a bisection")
       ]

let num func iter a b =
    let f = 
        (match Parser.parseAll Lang.parse_exp func with 
        | value -> value
        | exception Failure msg -> 
            Printf.printf "Error while parsing function \n"; 
            exit 1)
        |> Lang.eval
    in
    let result = 
        Bracketing.bisect_iterate (a, b) f 
        |> Seq.take iter
    in
    let i = ref 1 in
    result 
    |> List.iter (fun result -> 
        Printf.printf "\t Iteration %d \n" !i;
        Bracketing.pp result;
        Printf.printf "\n"; 
        incr i)

let () = 
    Arg.parse options anonymous usage;
    num !func !iter !a !b

