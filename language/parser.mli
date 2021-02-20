type 'a parser
val fix : ('a parser -> 'a parser) -> 'a parser
val fail : 'a parser
val pure : 'a -> 'a parser
val charp : (char -> bool) -> char parser
val char : char -> char parser
val any_char : char parser
val ( <$> ) : ('a -> 'b) -> 'a parser -> 'b parser
val ( $> ) : 'a parser -> 'b -> 'b parser
val ( <$ ) : 'a -> 'b parser -> 'a parser
val map : ('a -> 'b) -> 'a parser -> 'b parser
val ( <*> ) : ('a -> 'b) parser -> 'a parser -> 'b parser
val ( <<*>> ) : ('a -> 'b) parser -> 'a parser lazy_t -> 'b parser
val ( <* ) : 'a parser -> 'b parser -> 'a parser
val ( *> ) : 'a parser -> 'b parser -> 'b parser
val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser
val ( <|> ) : 'a parser -> 'a parser -> 'a parser
val eof : unit parser
val sequence : 'a parser list -> 'a list parser
val many : 'a parser -> 'a list parser
val choose : 'a parser list -> 'a parser
val space : unit parser
val spaces : unit parser
val digit : int parser
val digits : int list parser
val mag : int list -> int
val mag_frac : float list -> float
val integer : int parser
val float : float parser
val number : float parser
val string : String.t -> String.t parser
val chain : ('a -> 'a -> 'a) parser -> 'a parser -> 'a parser
val runParser : ('a -> ('b * 'c) option) -> 'a -> 'b
val parseAll : 'a parser -> string -> 'a
