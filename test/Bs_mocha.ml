external describe : string -> ((unit -> unit)[@bs ]) -> unit = "describe"
[@@bs.val ]
external it : string -> (unit -> unit) -> unit = "it"[@@bs.val ]
external eq : 'a -> 'a -> unit = "deepEqual"[@@bs.val ][@@bs.module "assert"]
external neq : 'a -> 'a -> unit = "notDeepEqual"[@@bs.val ][@@bs.module "assert"]
external ok : Js.boolean -> unit = "ok"[@@bs.val ][@@bs.module "assert"]
external fail : 'a -> 'a -> string Js.undefined -> string -> unit =
  "fail" [@@bs.val ][@@bs.module "assert"]
external dump : 'a array -> unit = "console.log"[@@bs.val ][@@bs.splice ]
external throws : (unit -> unit) -> unit =
  "throws"[@@bs.val ][@@bs.module "assert"]
  [@@ocaml.doc " There is a problem --\n    it does not return [unit ]\n "]

let assert_equal = eq
let assert_notequal = neq
let assert_ok a = ok (Js.Boolean.to_js_boolean a)
let assert_fail msg = fail () () (Js.Undefined.return msg) ""
let from_suites name suite = describe name @@
  (fun () -> List.iter (fun (name,code) -> it name code) suite) [@bs]

type eq =
  | Eq: 'a* 'a -> eq
  | Neq: 'a* 'a -> eq
  | Ok: bool -> eq
  | Approx: float* float -> eq
  | ApproxThreshold: float* float* float -> eq
  | ThrowAny: (unit -> unit) -> eq
  | Fail: unit -> eq
  | FailWith: string -> eq

type pair_suites = (string* (unit -> eq)) list

let close_enough ?(threshold= 0.0000001)  a b = abs_float (a -. b) < threshold
let from_pair_suites name suites =
  describe name @@ (fun () ->
    suites |> List.iter @@ fun (name,code) ->
      it name @@ fun _ -> match code () with
        | Eq (a,b) -> assert_equal a b
        | Neq (a,b) -> assert_notequal a b
        | Ok a -> assert_ok a
        | Approx (a,b) -> if not (close_enough a b) then assert_equal a b
        | ApproxThreshold (t,a,b) ->
          if not (close_enough ~threshold:t a b) then assert_equal a b
        | ThrowAny fn -> throws fn
        | Fail _ -> assert_fail "failed"
        | FailWith msg -> assert_fail msg)[@bs]
