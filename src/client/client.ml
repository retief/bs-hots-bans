[@@@bs.config {jsx=2}]
open Util
let render data =
  ReactDOMRe.renderToElementWithId
    (Page.createElement ~data ~children:[] () [@JSX]) "base"
let _ =
  let open Promise in
    render % ignore % debug <$> Request.get_html "/ban-data"
