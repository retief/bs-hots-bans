open Util;

module Type1 = {
  module Base = {
    type t = { arr: array t, lst: list int, i: int, f: float, s: string };
    let rec from_json j => Monad.Option.({
      Js.Json.decodeObject j >>= fun d =>
      Js.Dict.get d "i" >>= Json.Int.from_json >>= fun i =>
      Js.Dict.get d "s" >>= Json.String.from_json >>= fun s =>
      Js.Dict.get d "arr" >>= Json.array_of_jarray from_json >>= fun arr =>
      Js.Dict.get d "lst" >>= Json.list_of_jarray Json.Int.from_json >>= fun lst =>
      Js.Dict.get d "f" >>= Json.Float.from_json >>= fun f =>
      pure { arr, i, s, lst, f};
    });

    let rec to_json = fun | { arr, i, f, s, lst} => Js.Dict.fromList [
      ("i", Json.Int.to_json i),
      ("s", Json.String.to_json s),
      ("arr", Json.jarray_of_array to_json arr),
      ("lst", Json.jarray_of_list Json.Int.to_json lst),
      ("f", Json.Float.to_json f),
    ] |> Js.Json.object_;
  };
  include Json.Json_make Base;
};

let example_t1 : Type1.t = { i: 1, f: 1.2, s: "asdf", lst: [4, 5], arr: [|
  { arr: [| |], f: 1.3, i: 3, s: "asdfs", lst: [1] },
  { arr: [| |], f: 1.5, i: 5, s: "fdsa", lst: [2, 3]},
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
