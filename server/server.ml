open Express
open Util

external from_p_exn :
  (Request.t -> Response.t -> Next.t -> done_ Promise.t) -> Middleware.t =
  "%identity"

let from_p f = from_p_exn @@ fun req res next ->
  let out = f req res next in
  Promise.catch
    (fun _ -> Promise.pure @@ Response.sendString res "Error occurred")
    out

let app = App.make ();;

App.get app ~path:"/" @@ Middleware.from @@ fun _ res _ ->
  Response.sendString res "Hello";;

App.get app ~path:"/ban-data" @@ from_p @@ fun _ res _ ->
  let open Promise in
  Calc_bans.get_all_hero_details ()
  |$> Response.sendJson res % Calc_bans.Hero_data.to_json;;

App.listen app ~port:3000;;
