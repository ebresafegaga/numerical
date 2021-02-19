(* open Util 

let list_all = Sys.readdir >> Array.to_list >> List.filter Sys.is_directory

let rec read_all handle =
    match Unix.readdir handle with 
    | "." | ".." -> read_all handle 
    |  elem -> elem :: read_all handle 
    | exception End_of_file -> []

let d () = 
    Unix.getcwd ()
    |> Unix.opendir
    |> read_all

let s = Str.regexp "[^p]"

let a = {| " I still cant bekive;fd s;' mm|}

let r = Str.regexp {||}

let exec () =
    d ()
    |> List.filter Sys.is_directory
    |> List.concat_map (fun dir -> 
        let h = Unix.opendir dir in 
        read_all h
        |> List.map (Filename.concat dir))
    |> List.iter (fun file -> 
        (*if Filename.check_suffix ".cmo" file then 
            Sys.remove file*)
        print_endline file)

let () = 
    let name = "" in
    print_endline name
    (* exec () ;
    let str = "parser.cmo" in 
    if Str.string_match s str 0 then 
        Printf.printf "yessssss"
    else 
        Printf.printf "nooooooo"  *) *)


let () = 
    print_endline "heyyy" ;
    let n = read_float () in
    ()