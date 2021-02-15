open Numerical
open Util
open Language

let func = ref ""
let init : float list ref = ref [0.; 1.]
let iter = ref 0 
let a = ref 0. 
let b = ref 0.
let within : float option ref = ref None
let algorithm = ref ""
let handle_algorithm str = algorithm := str

let usage = "Usage: num [options]"
let anonymous str = ()
let options = Arg.align 
    [ ( "--function",
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
         " Specify the right endpoint for a bisection");
        
        ( "-x", 
          Arg.Set_float a, 
          " Specify the initial guess for newtons method");
        
        ( "--method", 
          Arg.Symbol (["bisection"; "FP"; "newton"], handle_algorithm), 
          " Specify the numerical analysis algorithm to use");   ]

let num func iter init within alg =
    let f = 
        (match Parser.parseAll Lang.parse_exp func with 
        | value -> value
        | exception Failure msg -> 
            Printf.eprintf "Error while parsing function \n"; 
            exit 1)
        |> Lang.eval
    in
    let module A =
        (val 
            match alg with  
            | "bisection" -> (module Bracketing.Bisection)
            | "FP" -> (module Bracketing.FalsePosition)
            | "newton" -> (module Open.Newton)
            | _ -> 
                Printf.eprintf "Please specify a numerical algorithm to use \n" ; 
                exit 1
            : Iterate.Algorithm)
    in
    let module I = Iterate.Make (A) in
    let result = I.iterate (I.create_initial init) f in 
    within
    |> Option.fold ~none:(Seq.take iter result) ~some:(I.within result)
    |> Seq.iteri (fun index result -> 
        Printf.printf "\t Iteration %d \n" (succ index) ;
        I.pp result ;
        Printf.printf "\n")

let () = 
    Arg.parse options anonymous usage ;
    (* TODO: remove this nasty code *)
    (match !algorithm with 
    | "bisection" | "FP" -> init := [!a; !b]
    | _ -> init := [!a]) ;
    num !func !iter !init !within !algorithm

