module type Jsonable  = sig
  type t
  val from_json : Js.Json.t -> t option
  val to_json : t -> Js.Json.t
end

module Extend(M:Jsonable) = struct
  include M
  let parse s = try Js.Json.parseExn s |> M.from_json
    with | _ -> None
  let stringify v = M.to_json v |> Js.Json.stringify
  let get d s = let open Util_option in Js.Dict.get d s >>= M.from_json
  let (@.) d s = get d s
end

module String = Extend(struct
  type t = string
  let from_json = Js.Json.decodeString
  let to_json = Js.Json.string
end)

module Int = Extend(struct
  type t = int
  let from_json n = let open Util_option in
    map truncate @@ Js.Json.decodeNumber n
  let to_json n = Js.Json.number (float n)
end)

module Float = Extend(struct
  type t = float
  let from_json = Js.Json.decodeNumber
  let to_json = Js.Json.number
end)

let list_of_jarray f a = let open Util_option in
  Js.Json.decodeArray a |> map Array.to_list >>= mapM f

let array_of_jarray f a =
  try
    let arr = Js.Json.decodeArray a in
    let open Util_option in
    Some (Array.map (Util_kernel.compose unwrap_exn f) @@ unwrap_exn arr)
  with | Invalid_argument _ -> None

let jarray_of_list f l = Js.Json.array @@ Array.of_list @@ Util_kernel.map f l
let jarray_of_array f l = Js.Json.array @@ Array.map f l

let get_array f d s = let open Util_option in
  Js.Dict.get d s >>= array_of_jarray f
let get_list f d s = let open Util_option in
  Js.Dict.get d s >>= list_of_jarray f

module List(M:Jsonable) = Extend(struct
  type t = M.t list
  let from_json a = list_of_jarray M.from_json a
  let to_json l = Js.Json.array @@ Array.of_list @@ Util_kernel.map M.to_json l
end)

module Array(M:Jsonable) = Extend(struct
  type t = M.t array
  let from_json a = array_of_jarray M.from_json a
  let to_json arr = Js.Json.array @@ (Array.map M.to_json arr)
end)

let jdict_of_json = Js.Json.decodeObject
let jobj_of_list l = (Js.Dict.fromList l) |> Js.Json.object_
