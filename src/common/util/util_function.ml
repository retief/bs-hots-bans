let debug v = Js.log v; v
let flip = Util_kernel.flip
let (%) = Util_kernel.compose
let compose = Util_kernel.compose
let const x _ = x
let on c f a b = c (f a) (f b)
let neg f x = not @@ (f x)
let try_apply f x = try Some (f x) with | _ -> None
let (|?>) x f = try_apply f x
let bool_of_string_safe s = s |?> bool_of_string
let int_of_string_safe s = s |?> int_of_string
let float_of_string_safe s = s |?> float_of_string
