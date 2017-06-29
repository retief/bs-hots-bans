open Util

type hero = { name: string; group: string; }
module Hero = Json.Extend(struct
  open Option
  open Json.String
  type t = hero
  let make name group = { name; group }
  let from_json j = Json.jdict_of_json j >>= fun dict ->
    make <$> (dict @. "PrimaryName") <*> (dict @. "Group")
  let to_json { name; group } = Json.jobj_of_list
    [ "PrimaryName", to_json name;
      "Group", to_json group]
end)

type hero_detail = {
  name: string;
  map: string;
  games: int;
  winrate: float;
  power: int;
}
let make_hd name map games winrate = {
  name; map; games; winrate;
  power = truncate @@ float games *. (winrate -. 50.)
}
module Hero_detail = Json.Extend(struct
  open Option
  open Json
  type t = hero_detail
  let from_json j = jdict_of_json j >>= fun dict ->
    make_hd <$> String.get dict "name"
    <*> String.get dict "map"
    <*> Int.get dict "games"
    <*> Float.get dict "winrate"
  let to_json { name; map; games; winrate } = Json.jobj_of_list
    [ "name", String.to_json name;
      "map", String.to_json map;
      "games", Int.to_json games;
      "winrate", Float.to_json winrate]
end)

type hero_data = { heroes: hero list; details: hero_detail list; time: float }
module Hero_data = Json.Extend(struct
  open Option
  module Hero_list = Json.List(Hero)
  module Hero_detail_list = Json.List(Hero_detail)
  type t = hero_data
  let from_json j = Json.jdict_of_json j >>= fun dict ->
    Hero_list.get dict "heroes" >>= fun heroes ->
    Hero_detail_list.get dict "details" >>= fun details ->
    Json.Float.get dict "time" >>= fun time ->
    pure { heroes; details; time }
  let to_json { heroes; details; time } = Json.jobj_of_list
    [ "heroes", Hero_list.to_json heroes;
      "details", Hero_detail_list.to_json details;
      "time", Json.Float.to_json time]
end)
