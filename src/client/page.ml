[@@@bs.config {jsx=2}]
let component = ReasonReact.statelessComponent "Page"
let make ~data  _children =
  let handle_click _event _ _ = Js.log data in
  {
    component with
    render = fun () ->
      fun self -> div
        ~onClick:(self.handle handle_click)
        ~children:[ReasonReact.stringToElement "foo"] () [@JSX]
  }
