module Seq :
  sig
    type 'a t = unit -> 'a node
    and 'a node = 'a Seq.node = Nil | Cons of 'a * 'a t
    val empty : 'a t
    val return : 'a -> 'a t
    val cons : 'a -> 'a t -> 'a t
    val append : 'a t -> 'a t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val filter : ('a -> bool) -> 'a t -> 'a t
    val filter_map : ('a -> 'b option) -> 'a t -> 'b t
    val flat_map : ('a -> 'b t) -> 'a t -> 'b t
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val iter : ('a -> unit) -> 'a t -> unit
    val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
    val head : 'a Seq.t -> 'a option
    val head_exn : 'a Seq.t -> 'a
    val take : int -> 'a t -> 'a Seq.t
    val iteri : (int -> 'a -> 'b) -> 'a t -> unit
    val combine_with : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c Seq.t
  end
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
module String :
  sig
    external length : string -> int = "%string_length"
    external get : string -> int -> char = "%string_safe_get"
    external set : bytes -> int -> char -> unit = "%string_safe_set"
    external create : int -> bytes = "caml_create_string"
    val make : int -> char -> string
    val init : int -> (int -> char) -> string
    val copy : string -> string
    val sub : string -> int -> int -> string
    val fill : bytes -> int -> int -> char -> unit
    val blit : string -> int -> bytes -> int -> int -> unit
    val concat : string -> string list -> string
    val iter : (char -> unit) -> string -> unit
    val iteri : (int -> char -> unit) -> string -> unit
    val map : (char -> char) -> string -> string
    val mapi : (int -> char -> char) -> string -> string
    val trim : string -> string
    val escaped : string -> string
    val index : string -> char -> int
    val index_opt : string -> char -> int option
    val rindex : string -> char -> int
    val rindex_opt : string -> char -> int option
    val index_from : string -> int -> char -> int
    val index_from_opt : string -> int -> char -> int option
    val rindex_from : string -> int -> char -> int
    val rindex_from_opt : string -> int -> char -> int option
    val contains : string -> char -> bool
    val contains_from : string -> int -> char -> bool
    val rcontains_from : string -> int -> char -> bool
    val uppercase : string -> string
    val lowercase : string -> string
    val capitalize : string -> string
    val uncapitalize : string -> string
    val uppercase_ascii : string -> string
    val lowercase_ascii : string -> string
    val capitalize_ascii : string -> string
    val uncapitalize_ascii : string -> string
    type t = string
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val split_on_char : char -> string -> string list
    val to_seq : t -> char Seq.t
    val to_seqi : t -> (int * char) Seq.t
    val of_seq : char Seq.t -> t
    external unsafe_get : string -> int -> char = "%string_unsafe_get"
    external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"
    external unsafe_blit : string -> int -> bytes -> int -> int -> unit
      = "caml_blit_string" [@@noalloc]
    external unsafe_fill : bytes -> int -> int -> char -> unit
      = "caml_fill_string" [@@noalloc]
    val to_list : String.t -> char list
    val of_list : char list -> String.t
  end
