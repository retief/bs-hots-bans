module Base = {
  type t 'a = option 'a;
  let (>>=) v f => switch v {
    | Some x => f x
    | None => None;
  };
  let pure x => Some x;
  let mzero = None;
  let mplus mx my => switch mx {
    | Some _ => mx
    | None => my
  };
  let map = `Define_using_bind;
};
include Base;
include Util_monad.Monad_make Base;
include Util_monad.Monad_plus_make Base;

let map_default a f => fun
  | None => f a
  | Some v => f v;

let is_some = fun
  | None => false
  | Some _ => true;

let is_none = fun
  | None => true
  | Some _ => false;

let unwrap_exn = fun
  | None => raise (Invalid_argument "Can't unwrap None")
  | Some x => x;

let cons_maybe m l => switch m {
  | None => l
  | Some v => [v, ...l]
};
let concat l => Util_kernel.fold_right cons_maybe l [];
let map_concat f l => Util_kernel.map f l |> concat;
