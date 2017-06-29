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

let app = App.make ()

let dir : string = [%bs.raw "process.cwd()" ]

let day = 1000. *. 60. *. 60. *. 24.

let current_time () = Js.Date.getTime @@ Js.Date.make ()

let current_data = ref { Data.heroes = []; Data.details = []; Data.time = 0. };;

App.get app ~path:"/ban-data" @@ from_p @@ fun _ res _ ->
  let open Promise in
  Response.sendJson res % Data.Hero_data.to_json <$>
  if (!current_data).time +. day < current_time ()
  then
    Calc_bans.get_all_hero_details () >>= fun data -> begin
      current_data := data;
      pure data
    end
  else begin
    pure !current_data
  end;;

Static.(App.useOnPath app ~path:"/static" @@ asMiddleware @@
  make (dir ^ "/static") @@ defaultOptions ());;

App.get app ~path:"/" @@ Middleware.from @@ fun _ res _ ->
  Response.sendFile res (debug (dir ^ "/static/index.html")) Js.Undefined.empty;;

App.listen app ~port:3000;;
