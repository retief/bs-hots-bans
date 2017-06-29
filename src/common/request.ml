open Util.Promise
type 't response
external get_response : string -> Js.Json.t response Js.Promise.t =
  "get" [@@bs.module "axios"]
external get_response_html : string ->
  (_ [@bs.as {json|{"responseType": "text"}|json}]) -> string response Js.Promise.t =
  "get" [@@bs.module "axios"]

external data : 't response -> 't = "data" [@@bs.get]

let get_json s = data <$> get_response s
let get_html s = data <$> get_response_html s
