module type Jsonable = {
  type t;
  let from_json : Js.Json.t => option t;
  let to_json : t => Js.Json.t;
};

module Extend = fun (M : Jsonable) => {
  include M;
  let parse s => try (Js.Json.parseExn s |> M.from_json) {
    | _ => None
  };
  let stringify v => M.to_json v |> Js.Json.stringify;
  let get d s => Util_option.(Js.Dict.get d s >>= M.from_json);
  let (@.) d s => get d s;
};

module String = Extend {
  type t = string;
  let from_json = Js.Json.decodeString;
  let to_json = Js.Json.string;
};

module Int = Extend {
  type t = int;
  let from_json n => Util_option.(
    map truncate @@ Js.Json.decodeNumber n
  );
  let to_json n => Js.Json.number (float n);
};

module Float = Extend {
  type t = float;
  let from_json = Js.Json.decodeNumber;
  let to_json = Js.Json.number;
};

let list_of_jarray f a => Util_option.(
  Js.Json.decodeArray a |> map Array.to_list >>= mapM f
);
let array_of_jarray f a => try {
  let arr = Js.Json.decodeArray a;
  Util_option.(Some (Array.map (Util_kernel.compose unwrap_exn f) @@ unwrap_exn arr));
} {
  | Invalid_argument _ => None
};

let jarray_of_list f l => Js.Json.array @@ Array.of_list @@ Util_kernel.map f l;
let jarray_of_array f l => Js.Json.array @@ Array.map f l;

let get_array f d s => Util_option.(Js.Dict.get d s >>= array_of_jarray f);
let get_list f d s => Util_option.(Js.Dict.get d s >>= list_of_jarray f);

module List = fun (M: Jsonable) => Extend {
  type t = list M.t;
  let from_json a => list_of_jarray M.from_json a;
  let to_json l => Js.Json.array @@ Array.of_list @@ Util_kernel.map M.to_json l;
};

module Array = fun (M: Jsonable) => Extend {
  type t = array M.t;
  let from_json a => array_of_jarray M.from_json a;
  let to_json arr => Js.Json.array @@ Array.map M.to_json arr;
};

let jdict_of_json = Js.Json.decodeObject;
let jobj_of_list l => Js.Dict.fromList l |> Js.Json.object_;
