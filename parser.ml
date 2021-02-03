
(*
   A MINIMAL PARSER COMBINATOR LIBRARY 
*)

let (>>) f g a = a |> f |> g

let rec fix f = f (fun x -> (fix f) x)

module String = struct 
    include String
    let to_list = String.to_seq >> List.of_seq
    let of_list = List.to_seq >> String.of_seq
end

type 'a parser = string -> ('a * string) option

let fail : 'a parser = Fun.const None 

let pure (x: 'a) : 'a parser = fun str -> Some (x, str)

let charp p : char parser = fun str -> 
    match String.to_list str with 
    | x :: xs when p x -> Some (x, String.of_list xs)
    | _ -> None

let char c : char parser = charp ((=) c)

let any_char : char parser = charp (Fun.const true)

let (<$>) (f: 'a -> 'b) (pa: 'a parser) : 'b parser = fun str -> 
    match pa str with 
    | Some (x, rest) -> Some (f x, rest)
    | None -> None

let ($>) p value = (Fun.const value) <$> p

let (<$) value p = p $> value

let map = (<$>)

let (<*>) (pf : ('a -> 'b) parser) (pa : 'a parser) : 'b parser = fun str -> 
    match pf str with 
    | Some (f, rest) ->
        (match pa rest with 
        | Some (a, rest) -> Some (f a, rest)
        | _ -> None)
    | _ -> None 

let (<<*>>) (pf : ('a -> 'b) parser) (pa : 'a parser lazy_t) : 'b parser = fun str -> 
    match pf str with 
    | Some (f, rest) ->
        (match Lazy.force pa rest with 
        | Some (a, rest) -> Some (f a, rest)
        | _ -> None)
    | _ -> None 

let (<*) pa pb = pure (fun a _ -> a) <*> pa <*> pb

let ( *> ) pa pb = pure (fun _ b -> b) <*> pa <*> pb 

let (>>=) (pa: 'a parser) (f : 'a -> 'b parser) : 'b parser = fun str -> 
    match pa str with 
    | Some (a, rest) -> f a rest
    | None -> None

let (<|>) (pa : 'a parser) (pb: 'a parser) : 'a parser = fun str -> 
    match pa str with 
    | Some (_, _) as result -> result 
    | None -> pb str

let eof : unit parser = fun str -> 
    match str with 
    | "" -> Some ((), str)
    | _ -> None 

let sequence (xs: 'a parser list) : 'a list parser = 
    List.fold_right (fun a s -> List.cons <$> a <*> s) xs (pure [])

(* TODO : replace lazy applicative operator with  fix *)
let rec many p = 
    pure List.cons <*> p <<*>> lazy (many p) <|> pure []

let choose xs = List.fold_left (<|>) fail xs

let space = () <$ (char ' ' <|> char '\n' <|> char '\t')

let spaces = () <$ many space

let digit = 
    let is_digit = function 
        | '0' .. '9' -> true 
        | _ -> false 
    in
    (Char.escaped >> int_of_string) <$> charp is_digit

let digits = many digit

let mag xs = List.fold_left (fun s a -> s*10 + a) 0 xs

let mag_frac xs = List.fold_right (fun a s -> (a+.s) *. 0.1) xs 0.

let integer = 
    digits >>= (function [] -> fail | x -> pure (mag x))

let float = 
    pure (fun whole _ decimal -> 
            let whole = whole |> float_of_int 
            and decimal = decimal |> List.map float_of_int in 
            whole +. mag_frac decimal) 
    <*> integer <*> char '.' <*> digits

let number = float <|> (map float_of_int integer)

let signed_number = (~-.) <$> char '-' *> number

let string =
    String.to_list >> List.map char >> sequence >> map String.of_list

let rec chain op e =
    (e >>= fun e' -> 
           op >>= fun op' -> 
           chain op e >>= fun rest -> 
           pure (op' e' rest)) <|> e

let runParser p str = 
    match p str with 
    | Some (x, _) -> x
    | None -> failwith "Parse Error!"

let parseAll p str = 
    let p = p <* eof in 
    runParser p str