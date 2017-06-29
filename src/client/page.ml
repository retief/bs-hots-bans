[@@@bs.config {jsx=2}]
open Data
open Util

type bans_state = { current_map: string; current_classes: string list }
module String_map = Map.Make(struct
  type t = string
  let compare = compare
end)

let folder map str = String_map.add str true map
let make_map = List.fold_left folder String_map.empty
let uniq = List.map (fun (a, _) -> a)
  % String_map.bindings
  % make_map

let get_hero_rows { heroes; details } { current_map; current_classes } =
  let name_if_equal (h:hero) c = if (c = h.group) then Some h.name else None in
  let heroes = debug @@ make_map @@ Option.map_concat
    (fun h -> List.some_option (name_if_equal h) current_classes) heroes
  in
  let matches d = d.map = current_map && String_map.mem d.name heroes in
  List.filter matches details
  |> List.sort (flip @@ on compare (fun {power} -> power))

let component = ReasonReact.statefulComponent "Page"
let lit_string = ReasonReact.stringToElement
let list_el = ReasonReact.arrayToElement % Array.of_list
let get_target el = ReactEventRe.Form.target el |> ReactDOMRe.domElementToObj

let make ~data _children =
  let { details; heroes } = data in
  let all_maps = List.map (fun { map } -> map) details |> uniq in
  let all_classes = List.map (fun { group } -> group) heroes |> uniq in

  let set_map event state _self =
    let target = get_target event in
    let value = target |?> (fun o -> o##value) in
    Option.map_default ReasonReact.NoUpdate
      (fun new_map -> ReasonReact.Update { state with current_map = new_map }) value
  in

  let set_class event state _self =
    let open Option in
    let target = get_target event in
    let { current_classes } = state in
    let new_classes =
      target |?> (fun o -> o##value) >>= fun value ->
      target |?> (fun o -> o##checked) >>= fun checked ->
      let filtered = List.filter ((<>) value) current_classes in
      pure @@
        match Js.to_bool checked with
        | true -> value :: filtered
        | false -> filtered
    in
    map_default ReasonReact.NoUpdate
      (fun nc -> ReasonReact.Update { state with current_classes = nc }) new_classes
  in

  let header = h3 ~children:[lit_string "Hots Bans"] () [@JSX] in

  let map_options = all_maps |> List.map (fun m ->
    option ~value:m ~key:m ~children:[lit_string m] () [@JSX])
  in

  let class_options self selected = all_classes |> List.map (fun c -> list_el
    [ label ~htmlFor:("class_select_" ^ c) ~key:(c^"la") ~children:[lit_string (c ^ ":")] () [@JSX];
      input ~_type:"checkbox" ~checked:(Js.Boolean.to_js_boolean @@ List.some ((=) c) selected)
        ~value:c ~key:(c^"in") ~id:("class_select_" ^ c)
        ~onChange:(self.ReasonReact.update set_class) () [@JSX]])
  in

  let filter_bar self classes = form ~children:[
    label ~htmlFor:"map_select" ~children:[lit_string "Select Map:"] () [@JSX];
    select ~id:"map_select" ~onChange:(self.ReasonReact.update set_map) ~children:[list_el map_options] () [@JSX];
    list_el @@ class_options self classes;
  ] () [@JSX]
  in

  let make_row { map; power; winrate; games; name; } =
    tr ~key: (name ^ map) ~children:[
      td ~children:[lit_string name] () [@JSX];
      td ~children:[lit_string @@ string_of_int games] () [@JSX];
      td ~children:[lit_string @@ string_of_float winrate] () [@JSX];
      td ~children:[lit_string @@ string_of_int power] () [@JSX];

    ] () [@JSX]
  in
  {
    component with
    initialState = (fun () -> { current_map = "Battlefield of Eternity"; current_classes = all_classes });
    render = fun state self ->
      let rows = debug @@ get_hero_rows data state in
      div ~children:[
        header;
        filter_bar self state.current_classes;
        table ~children:[
          thead ~children:[
            tr ~children:[
              td ~children:[lit_string "Name"] () [@JSX];
              td ~children:[lit_string "Games"] () [@JSX];
              td ~children:[lit_string "Winrate"] () [@JSX];
              td ~children:[lit_string "Power"] () [@JSX];
            ] () [@JSX];
          ] () [@JSX];
          tbody ~children:[
            list_el @@ List.map make_row rows
          ] () [@JSX];
        ] () [@JSX];
      ] () [@JSX]
  }
