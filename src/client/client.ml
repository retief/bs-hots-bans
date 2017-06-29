[@@@bs.config {jsx=2}]
open Util
let default_data = { Data.heroes = []; Data.details = []; time = 0. }

let render data =
  ReactDOMRe.renderToElementWithId
    (Page.createElement ~data ~children:[] () [@JSX]) "base";;
let _ =
  let open Promise in
  render % Option.unwrap default_data % Data.Hero_data.from_json <$> Request.get_json "/ban-data"
