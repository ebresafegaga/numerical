open Numerical
open Util

let func = ref ""
let iter = ref 0 
let a = ref 0. 
let b = ref 0.
let within : float option ref = ref None


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

       ( "--within", 
         Arg.Float (fun w -> within := Some w), 
         " Specify the error. Iteration stops when the abolute error is less than this value");
    
       ( "-a", 
         Arg.Set_float a, 
         " Specify the left endpoint for a bisection");
       
       ( "-b",
         Arg.Set_float b, 
         " Specify the right endpoint for a bisection")
       ]

let num func iter a b within =
    let f = 
        (match Parser.parseAll Lang.parse_exp func with 
        | value -> value
        | exception Failure msg -> 
            Printf.printf "Error while parsing function \n"; 
            exit 1)
        |> Lang.eval
    in
    let result = Bracketing.bisect_iterate (a, b) f in
    within
    |> Option.fold ~none:(Seq.take iter result) ~some:(fun w -> Bracketing.within w result)
    |> List.iteri (fun index result -> 
        Printf.printf "\t Iteration %d \n" (succ index) ;
        Bracketing.pp result ;
        Printf.printf "\n" )

let () = 
    Arg.parse options anonymous usage;
    num !func !iter !a !b !within

