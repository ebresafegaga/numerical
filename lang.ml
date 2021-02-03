(*
    THE ALGEBRAIC LANGUAGE
    This language doesn't handle precedence, so you have to be explicit about them with parens 
    or else everything just associates to the right (I know; it's really weird).
    exp := constant 
           (exp)
           x -- yes all variable names have to be called "x"
           exp * exp 
           exp ^ exp  -- little caveat here the other exp has to be a constant (or maybe not?)
           exp - exp 
           exp + exp 
           cos exp 
           sin exp 
*)

type exp = 
    | Constant of float 
    | Variable
    | Mul of exp * exp 
    | Sum of exp * exp 
    | Diff of exp * exp 
    | Pow of exp * exp 
    | Cos of exp 
    | Sin of exp

let rec eval exp x = 
    let eval' = Fun.flip eval x in
    match exp with 
    | Constant value -> value 
    | Variable -> x  
    | Mul (a, b) -> eval' a *. eval' b  
    | Sum (a, b) -> eval' a +. eval' b 
    | Diff (a, b) -> eval' a -. eval' b
    | Pow (a, x) -> eval' a ** eval' x
    | Cos a -> cos (eval' a) 
    | Sin a -> sin (eval' a)

(* PARSING *)

open Parser

let binary exp = 
    let mul a b = Mul (a, b) in
    let sum a b = Sum (a, b) in
    let diff a b = Diff (a, b) in
    let pow a x = Pow (a, x) in
    let mulp = map (Fun.const mul) (char '*') in
    let sump = map (Fun.const sum) (char '+') in
    let diffp = map (Fun.const diff) (char '-') in
    let powp = map (Fun.const pow) (char '^') in 
    chain (mulp <|> sump <|> diffp <|> powp) exp

let primary exp = 
    (char '(' *> spaces *> exp <* spaces  <* char ')') <|>
    (map (Fun.const Variable) (char 'x')) <|>
    (map (fun x -> Constant x) number) 

let exp = 
    fix @@ fun exp -> 
        ((fun e -> Cos e) <$> (string "cos") *> spaces *> exp) <|>
        ((fun e -> Sin e) <$> (string "sin") *> spaces *> exp) <|> 
        (binary (spaces *> (primary exp) <* spaces)) 