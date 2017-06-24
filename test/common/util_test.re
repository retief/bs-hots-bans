open Util;
open Bs_mocha;
from_pair_suites "Util.List" [
  ("replicate works", fun () => Eq (List.replicate 4 1) [1, 1, 1, 1]),
  ("range works with no start", fun () => Eq [0, 1, 2, 3, 4] (List.range 5)),
  ("range works with start", fun () => Eq [2, 3, 4, 5] (List.range start::2 6)),
  ("return is empty when start = stop", fun () => Eq [] (List.range start::1 1)),
  ("invalid range params return empty list", fun () => Eq [] (List.range start::5 1)),
  ("minimal map works", fun () => Eq [2, 3, 4] (List.map ((+) 1) [1, 2, 3])),
  ("map works", fun () => {
    let lengths = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let f x => x + 1;
    let init = List.map (fun x => List.replicate x 1) lengths;
    let expected = List.map (fun x => List.replicate x 2) lengths;
    let actual = List.map (List.map f) init;
    Eq expected actual;
  }),
  ("map doesn't cause stack overflow", fun () =>
    Eq 10_000 (List.replicate 10_000 1 |> List.map (fun a => a) |> List.length)),
  /* ("append works", fun () => {
    let lengths = []
  }) */
];
