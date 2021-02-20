open Util
(* 
let is_even n = n mod 2 = 0

let rec is_sorted = function 
    | [] | [_] -> true
    | a :: b :: rest -> a < b && is_sorted (b :: rest) 


let g = QCheck.make

let q = QCheck.gen

let i = QCheck.Gen.int

let t = 
    QCheck.Test.make
        ~count:10
        (QCheck.make i) 
        is_even

(* let () = QCheck_runner.run_tests_main [t] *)

(* let result = 
    QCheck_runner.run_tests 
        ~colors:true 
        ~verbose:true
        [t] *)

let double x = x * 2 

let igen = QCheck.Gen.small_int
let iabtr = QCheck.make igen

let itest = 
    QCheck.Test.make
        ~count:15
        iabtr
        (fun i -> double i = (i + i))


module M : sig type 'a repo = private { n :'a } end = struct 
    type 'a repo = { n : 'a }
    let create n = {n}
end

let result = 
    QCheck_runner.run_tests 
        ~verbose:true
        ~colors:true
        [itest]

let o = QCheck.Observable.make 

let t = Alcotest.check Alcotest.string

let o = 
    Alcotest.check' 
        Alcotest.string 
        ~msg:""
        ~expected:("stri" ^ "ng")
        ~actual:"string"


let d x = Alcotest.check (Alcotest.float Float.epsilon) *)


let tests = [Lang_test.commutativity]

let suite = 
    tests 
    |> List.map QCheck_alcotest.to_alcotest

let suite = 
    Alcotest.run "Numerical" [
        "Language", suite 
    ]

(* let result = 
    QCheck_runner.run_tests
        ~verbose:true
        ~colors:true
        tests *)