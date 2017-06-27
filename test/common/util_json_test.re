open Util;

module Type1 = {
  module Base = {
    type t = { arr: array t, arr2: array float, lst: list int, i: int, f: float, s: string };
    let make i f s arr arr2 lst => { arr, arr2, lst, i, f, s};
    module FloatArray = Json.Array Json.Float;
    module IntList = Json.List Json.Int;
    let rec from_json j => Option.(Json.jdict_of_json j >>= fun d =>
      make <$> Json.Int.get d "i"
      <-> Json.Float.get d "f" <-> Json.String.get d "s"
      <-> Json.get_array from_json d "arr"
      <-> FloatArray.get d "arr2" <-> IntList.get d "lst");

    let rec to_json = fun | { arr, arr2, i, f, s, lst} => Json.jobj_of_list [
      ("i", Json.Int.to_json i),
      ("s", Json.String.to_json s),
      ("arr", Json.jarray_of_array to_json arr),
      ("arr2", FloatArray.to_json arr2),
      ("lst", IntList.to_json lst),
      ("f", Json.Float.to_json f),
    ];
  };
  include Json.Extend Base;
};

let example_t1 : Type1.t = { i: 1, f: 1.2, s: "asdf", lst: [4, 5], arr2: [|1., 2.|], arr: [|
  { arr: [| |], f: 1.3, i: 3, s: "asdfs", lst: [1], arr2: [|3.|] },
  { arr: [| |], f: 1.5, i: 5, s: "fdsa", lst: [2, 3], arr2: [|4.|]},
|]};

open Bs_mocha;
from_pair_suites "Util.Json" [
  ("Type1 from_json/to_json works", fun () =>
    Eq (Some example_t1) (Type1.from_json @@ Type1.to_json example_t1)),
  ("Type1 parse/stringify works", fun () =>
    Eq (Some example_t1) (Type1.parse @@ Type1.stringify example_t1)),
];
/* module Type2 = {
  type t 'i =
    | Cons 'i (t2 'i)
    | End;
  let rec fromJson j =>

}; */
