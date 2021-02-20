

let parse = function
    | "bisection" -> `Bisection
    | "FP" -> `FP
    | "newton" -> `Newton ""
    | "secant" -> `Secant 9  
