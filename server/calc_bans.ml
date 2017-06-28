open Util
type error
type client_request
external request : ((_)[@bs.as ""]) -> string -> string Js.Promise.t =
  "call" [@@bs.module "request-promise-native"]

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

let float_chars = [%re "/[.0-9]+/g"]
let int_chars = [%re "/[0-9]+/g"]
let switch r s =
  let break = ref false in
  let buf = Buffer.create 5 in
  while not (!break) do
    match Js.Re.exec s r with
      | None -> break := true
      | Some result -> Buffer.add_string buf @@ (Js.Re.matches result).(0)
  done;
  match Buffer.length buf = 0 with
    | true -> None
    | false -> Some (Buffer.contents buf)

let parse_hd name = function
  | _::mp::g::wr::_ -> let open Option in
    switch int_chars g >>= int_of_string_safe >>= fun games ->
    switch float_chars wr >>= float_of_string_safe >>= fun winrate ->
    make_hd name mp games winrate |> pure
  | _ -> None

module Hero_list = Json.List(Hero)
let get_heroes () = let open Promise in
  request "https://api.hotslogs.com/Public/Data/Heroes"
  |$> Hero_list.parse
  |$> Option.unwrap []

let get_hero_details name gc = let open Rcheerio in
  let process_row e = select_context gc "td" Element e
    |> map gc text |> parse_hd name
  in
  select gc "#winRateByMap tr" |> map_el process_row |> Option.concat

let get_hero_page name = let open Promise in
  request ("https://www.hotslogs.com/Sitewide/HeroDetails?Hero=" ^ name)
  |$> Rcheerio.load

let get_all_hero_details () = let open Promise in
  let get_details ({ name } : hero) = get_hero_page name |$> get_hero_details name
  in
  catch (fun _ -> pure ([], []))
    ( get_heroes () >>= fun heroes ->
      mapM get_details heroes >>= fun details ->
      pure (heroes, List.concat details))

let _ = Promise.map Js.log @@ get_all_hero_details ()
