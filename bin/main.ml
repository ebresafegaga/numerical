open Numerical
let usage = "Usage: numerical [options]"
let anonymous str = ()

let options = Arg.align 
    [ ( "--method", 
        Arg.Symbol (["bisection"], fun str -> ()), 
        " Specify the numerical analysis algorithm to use."); 
      
       ( "--function",
         Arg.String (fun str -> ()), 
         " Specify a function to solve." );
    
       ( "--iterations", 
         Arg.Float (fun a -> ()), 
         " Specify the number of iterations.");
    
       ( "-a", 
         Arg.Float (fun a -> ()), 
         " Specify the left endpoint for a bisection.");
       
       ( "-b",
         Arg.Float (fun b -> ()), 
         " Specify the right endpoint for a bisection.")
       ] 

let () = 
    Arg.parse options anonymous usage

