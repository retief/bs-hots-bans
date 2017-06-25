module type Jsonable = {
  type t;
  let from_json : Js.Json.t => option t;
  let to_json : t => Js.Json.t;
};

module Json_make = fun (M : Jsonable) => {
  include M;
  let parse s => try (Js.Json.parseExn s |> M.from_json) {
    | _ => None
  };
  let stringify v => M.to_json v |> Js.Json.stringify;
};

module String = Json_make {
  type t = string;
  let from_json = Js.Json.decodeString;
  let to_json = Js.Json.string;
};

module Int = Json_make {
  type t = int;
  let from_json n => Util_monad.Option.(
    map truncate @@ Js.Json.decodeNumber n
  );
  let to_json n => Js.Json.number (float n);
};

module Float = Json_make {
  type t = float;
  let from_json = Js.Json.decodeNumber;
  let to_json = Js.Json.number;
};

let list_of_jarray f a => Util_monad.Option.(
  Js.Json.decodeArray a |> map Array.to_list >>= mapM f
);

let array_of_jarray f a => try {
  let arr = Js.Json.decodeArray a;
  let unpack_exn = fun
    | None => raise (Invalid_argument "Unexpected None")
    | Some v => v;
  Some (Array.map (Util_kernel.compose unpack_exn f) @@ unpack_exn arr);
} {
  | Invalid_argument _ => None
};

let jarray_of_list f l => Js.Json.array @@ Array.of_list @@ Util_kernel.map f l;
let jarray_of_array f l => Js.Json.array @@ Array.map f l;

module List = fun (M: Jsonable) => {
  type t = list M.t;
  let from_json a => list_of_jarray M.from_json a;
  let to_json l => Js.Json.array @@ Array.of_list @@ Util_kernel.map M.to_json l;
};

module Array = fun (M: Jsonable) => {
  type t = array M.t;
  let from_json a => array_of_jarray M.from_json a;
  let to_json arr => Js.Json.array @@ Array.map M.to_json arr;
};
