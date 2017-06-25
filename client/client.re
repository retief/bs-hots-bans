open Util;

let l1 = [1, 2, 3, 4];
let l2 = [5, 6, 7, 8];

let res = Monad.List.(
  l1 >>= fun v1 =>
  l2 >>= fun v2 =>
  v1 + v2 |> pure
);

let res' = Monad.List.(
  l1 >>= fun v1 => (fun v2 => v1 + v2 |> pure) =<< l2
);

Js.log res;
Js.log res';
List.length res |> Js.log;
List.replicate 10 "a" |> List.length |> Js.log;
