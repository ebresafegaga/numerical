open Util
open Language 
open Lang 

let e = Fun.flip eval

let k = Int.to_float >> constant
let c = constant
let binary op x y = op (c x) (c y)
let product = binary mul
let addition = binary sum
let subtraction = binary diff 
let division = binary diff

let fpairs = 
    let fgen = QCheck.Gen.float in
    QCheck.Gen.pair fgen fgen
    |> QCheck.Gen.small_list
    |> QCheck.make ~print:(fun e -> 
        let sf = Format.str_formatter in
        Format.pp_print_list 
            ~pp_sep:(fun f () -> Format.fprintf f " , ")
            (fun f (x, y) -> Format.fprintf f "(%f, %f)" x y)
            sf e ;
        let s = Format.flush_str_formatter () in
        "[ " ^ s ^ " ]")


let swap (a, b) = (b, a)

let create l = List.map (fun elem -> elem, swap elem) l

let comm p ((a, b), (c, d)) = p a b = p c d

let predicate ps list = 
    list 
    |> List.concat_map (fun elem -> 
        ps 
        |> List.map (fun p ->
            comm (fun a b -> eval (p a b) 0.) elem))

let ps_comm = [ addition; product ]

let commutativity = 
    QCheck.Test.make
        ~name:"Addition/Subtraction commutative"
        ~count:50
        fpairs
        (create >> predicate ps_comm >> List.for_all Fun.id)
