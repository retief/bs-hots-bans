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

let dir : string = [%bs.raw "process.cwd()" ];;

App.get app ~path:"/ban-data" @@ from_p @@ fun _ res _ ->
  let open Promise in
  Calc_bans.get_all_hero_details ()
  |$> Response.sendJson res % Calc_bans.Hero_data.to_json;;

Static.(App.useOnPath app ~path:"/static" @@ asMiddleware @@
  make (dir ^ "/static") @@ defaultOptions ());;

App.get app ~path:"/" @@ Middleware.from @@ fun _ res _ ->
  Response.sendFile res (debug (dir ^ "/static/index.html")) Js.Undefined.empty;;

App.listen app ~port:3000;;
