module Base = struct
  type 'a t = 'a option
  let (>>=) v f = match v with
    | Some x -> f x
    | None  -> None
  let pure x = Some x
  let mzero = None
  let mplus mx my = match mx with
    | Some _ -> mx
    | None  -> my
  let map_impl = `Define_using_bind
end
include Base
include Util_monad.Monad_make(Base)
include Util_monad.Monad_plus_make(Base)

let map_default a f = function
  | None  -> a
  | Some v -> f v

let is_some = function
  | None  -> false
  | Some _ -> true

let is_none = function
  | None  -> true
  | Some _ -> false

let unwrap_exn = function
  | None -> raise (Invalid_argument "Can't unwrap None")
  | Some x -> x

let unwrap default = function
  | None -> default
  | Some v -> v

let cons_maybe m l = match m with
  | None -> l
  | Some v -> v :: l

let concat l = Util_kernel.fold_right cons_maybe l []

let map_concat f l = Util_kernel.map f l |> concat
