open Util
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
    | Div of exp * exp 
    | Sum of exp * exp 
    | Diff of exp * exp 
    | Pow of exp * exp 
    | Cos of exp 
    | Sin of exp
    | Log of exp
    | Pi 
    | E

let mul a b = Mul (a, b)
let div a b = Div (a, b)
let sum a b = Sum (a, b) 
let diff a b = Diff (a, b)
let pow a x = Pow (a, x) 
let sin' x = Sin x
let cos' x = Cos x
let log' x = Log x
let constant x = Constant x

let e = 2.71828182845904509

let rec eval exp x =
    let eval' = Fun.flip eval x in
    match exp with 
    | Constant value -> value 
    | Variable -> x  
    | Mul (a, b) -> eval' a *. eval' b  
    | Div (a, b) -> eval' a /. eval' b 
    | Sum (a, b) -> eval' a +. eval' b 
    | Diff (a, b) -> eval' a -. eval' b
    | Pow (a, x) -> eval' a ** eval' x
    | Cos a -> cos (eval' a) 
    | Sin a -> sin (eval' a)
    | Pi -> Float.pi
    | E -> e
    | Log x -> log (eval' x)

(* PARSING *)

open Parser

let binary exp = 
    let mulp = mul <$ (char '*') in
    let divp = div <$ (char '/') in
    let sump = sum <$ (char '+') in
    let diffp = diff <$ (char '-') in
    let powp = pow <$ (char '^') in 
    chain (mulp <|> divp <|> sump <|> diffp <|> powp) exp

let primary exp = 
    (char '(' *> spaces *> exp <* spaces  <* char ')') <|>
    (Variable <$ (char 'x')) <|>
    (constant <$> number) <|>
    (E <$ (char 'e')) <|> 
    (Pi <$ (string "pi"))

let exp = 
    fix @@ fun exp -> 
        (cos' <$> (string "cos") *> spaces *> exp) <|>
        (sin' <$> (string "sin") *> spaces *> exp) <|> 
        (log' <$> (string "log") *> spaces *> exp) <|> 
        (binary (spaces *> (primary exp) <* spaces)) 