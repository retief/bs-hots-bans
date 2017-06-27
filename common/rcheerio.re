open Util;
type element;
type cheerio;
type gcheerio;

type context _ =
  | Selector : context string
  | Element : context element
  | Cheerio : context cheerio;

external load : string => gcheerio = "" [@@bs.module "cheerio"];

external select : gcheerio => (_ [@bs.as ""]) => string => cheerio = "call" [@@bs.send];
external select_context : gcheerio => (_ [@bs.as ""]) => string =>
  context 'a [@bs.ignore] => 'a => cheerio = "call" [@@bs.send];
external wrap : gcheerio => (_ [@bs.as ""]) => element => cheerio = "call" [@@bs.send];

external each : (int => element => unit) => cheerio = "" [@@bs.send.pipe: cheerio];

external text : cheerio => Js.undefined string = "" [@@bs.send];
external html : cheerio => Js.undefined string = "" [@@bs.send];

let map_el f c => {
  let res = ref [];
  let mapper _ e => res := [f e, ...!res];
  each mapper c |> ignore;
  List.rev !res;
};

let map g f c => map_el (f % wrap g) c;
