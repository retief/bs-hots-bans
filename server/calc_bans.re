open Util;
type error;
type client_request;
external request : _[@bs.as ""] => string => Js.Promise.t string =
  "call" [@@bs.module "request-promise-native"];

type hero = { name: string, group: string};
module Hero = Json.Extend {
  open Option;
  open Json.String;
  type t = hero;
  let make name group => { name, group };

  let from_json j => Json.jdict_of_json j >>= fun dict =>
    make <$> dict @. "PrimaryName" <-> dict @. "Group";
  let to_json { name, group } => Json.jobj_of_list [
    ("PrimaryName", to_json name),
    ("Group", to_json group)
  ];
};

module HeroList = Json.Array Hero;
Util.Promise.(debug <$> (HeroList.parse <$> request "https://api.hotslogs.com/Public/Data/Heroes"));
