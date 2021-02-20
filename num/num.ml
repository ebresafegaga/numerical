open Numerical
open Util
open Language

let func = ref ""
let init : float list ref = ref []
let iter = ref 0 
let a = ref 0. 
let b = ref 0.
let within : float option ref = ref None
let algorithm = ref ""

let init_algorithm () = 
    (* Still not happy about this *)
    match !algorithm with 
    | "bisection" | "FP" | "secant" -> init := [!a; !b]
    | _ -> init := [!a]

let handle_algorithm str = algorithm := str

let handle_error str = ()

let usage = "Usage: num [options]"
let anonymous str = ()

let algorithms = ["bisection"; "FP"; "newton"; "secant"]
let errors = ["absolute";"relative"]

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
          Arg.Symbol (algorithms, handle_algorithm), 
          " Specify the numerical analysis algorithm to use");
        
        ( "--error", 
          Arg.Symbol (errors, handle_error), 
          " Specify how the error should be calculated to use")  ]

let num func iter init within alg =
    let f = 
        match Parser.parseAll Lang.exp func with 
        | value -> Lang.eval value
        | exception Failure e -> 
            Printf.eprintf "Error while parsing function \n" ; 
            exit 1
    in
    let (module Algorithm : Iterate.Algorithm) =
        match alg with  
        | "bisection" -> (module Bracketing.Bisection)
        | "FP" -> (module Bracketing.FalsePosition)
        | "newton" -> (module Open.Newton)
        | "secant" -> (module Open.Secant)
        | _ -> 
            Printf.eprintf "Please specify a numerical algorithm to use \n" ; 
            exit 1
    in
    let module I = Iterate.Make (Algorithm) (Error.Relative) in
    let result = I.iterate (I.create_initial init) f in 
    let error = I.error (I.create_initial init) f in
    within
    |> Option.fold ~none:(Seq.take iter result) ~some:(I.within result)
    |> Seq.combine_with ~f:(fun a b -> (a, b)) error
    |> Seq.iteri (fun index (error, result) -> 
        Printf.printf "\t Iteration %d \n" (succ index) ;
        I.pp result ;
        Printf.printf "Error = %f\n\n" error ;
        Printf.printf "\n")
    
let () = 
    Arg.parse options anonymous usage ;
    init_algorithm () ;
    num !func !iter !init !within !algorithm