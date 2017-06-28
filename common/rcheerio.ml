open Util
type element
type cheerio
type gcheerio
type _ context =
  | Selector: string context
  | Element: element context
  | Cheerio: cheerio context

external load : string -> gcheerio = ""[@@bs.module "cheerio"]
external select : gcheerio -> ((_)[@bs.as ""]) -> string -> cheerio =
  "call" [@@bs.send ]
external select_context : gcheerio ->((_)[@bs.as ""]) -> string ->
  (('a context)[@bs.ignore ]) -> 'a -> cheerio = "call" [@@bs.send ]
external wrap : gcheerio -> ((_)[@bs.as ""]) -> element -> cheerio =
  "call" [@@bs.send ]
external each : (int -> element -> unit) -> cheerio = ""[@@bs.send.pipe :cheerio]
external text : cheerio -> string = ""[@@bs.send ]
external html : cheerio -> string Js.undefined = ""[@@bs.send ]

let map_el f c =
  let res = ref [] in
  let mapper _ e = res := f e :: !res in
  each mapper c |> ignore;
  List.rev !res
let map g f c = map_el (f % wrap g) c
