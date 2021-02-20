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
val mul : exp -> exp -> exp
val div : exp -> exp -> exp
val sum : exp -> exp -> exp
val diff : exp -> exp -> exp
val pow : exp -> exp -> exp
val sin' : exp -> exp
val cos' : exp -> exp
val log' : exp -> exp
val constant : float -> exp
val e : float
val eval : exp -> float -> float
val exp : exp Parser.parser
