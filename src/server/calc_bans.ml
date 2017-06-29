open Util
open Data
type error

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
  Request.get_json "https://api.hotslogs.com/Public/Data/Heroes"
  |$> Hero_list.from_json
  |$> Option.unwrap []

let get_hero_details name gc = let open Rcheerio in
  let process_row e = select_context gc "td" Element e
    |> map gc text |> parse_hd name
  in
  select gc "#winRateByMap tr" |> map_el process_row |> Option.concat

let get_hero_page name = let open Promise in
  Request.get_html ("https://www.hotslogs.com/Sitewide/HeroDetails?Hero=" ^ name)
  |$> Rcheerio.load

let dummy_data () = {
  heroes = [
    { group="Assassin"; name="Thrall" };
    { group="Assassin"; name="Falstad" };
    { group="Support"; name="Kharazim" };
    { group="Support"; name="Brightwing" };
  ];
  details = [
    make_hd "Thrall" "Battlefield of Eternity" 1000 50.;
    make_hd "Falstad" "Battlefield of Eternity" 1000 49.;
    make_hd "Kharazim" "Battlefield of Eternity" 1000 51.;
    make_hd "Brightwing" "Battlefield of Eternity" 1000 52.;
    make_hd "Thrall" "Garden of Terror" 1000 51.;
    make_hd "Falstad" "Garden of Terror" 1000 50.;
    make_hd "Kharazim" "Garden of Terror" 1000 51.;
    make_hd "Brightwing" "Garden of Terror" 1000 49.;
  ];
  time = Js.Date.getTime @@ Js.Date.make ();
}

let get_all_hero_details () = let open Promise in
  let get_details ({ name } : hero) = get_hero_page name |$> get_hero_details name
  in
  pure @@ dummy_data ()
  (* get_heroes () >>= fun heroes ->
  mapM get_details heroes >>= fun details ->
  pure { heroes; details=List.concat details; time = Js.Date.getTime @@ Js.Date.make () } *)
