

let derivative ?(dx = 0.00001) f x =
    (f (x +. dx) -. f x) /. dx
