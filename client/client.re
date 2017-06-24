open Util;

module type Monad = {
  type t 'a;
  let (>>=) : t 'a => ('a => t 'a) => t 'a;
  let pure : 'a => t 'a;
};

module Maybe = {
  type t 'a =
    | Just 'a
    | Nothing;
  let (>>=) v f => switch v {
    | Just x => f x
    | Nothing => Nothing;
  };
  let pure x => Just x;
};

module ListM = {
  type t 'a = list 'a;
  let (>>=) l f => List.map f l |> List.concat;
  let pure v => [v];
};

module MComb = fun (M: Monad) => {
  include M;
  let (>>) a b => a >>= fun _ => b;
  let (=<<) f v => v >>= f;
};

let l1 = [1, 2, 3, 4];
let l2 = [5, 6, 7, 8];

let res = ListM.(
  l1 >>= fun v1 =>
  l2 >>= fun v2 =>
  v1 + v2 |> pure
);

module ListComb = (MComb ListM);
let res' = ListComb.(
  l1 >>= fun v1 => (fun v2 => v1 + v2 |> pure) =<< l2
);

let first = Maybe.(
  fun | [v, ..._] => Just v
      | [] => Nothing
);

let res'' = Maybe.(
  first [1] >>= fun v1 =>
  first [] >>= fun v2 =>
  v1 + v2 |> pure
);

Js.log res;
Js.log res';
Js.log res'';
List.length res |> Js.log;
List.replicate 10 "a" |> List.length |> Js.log;
