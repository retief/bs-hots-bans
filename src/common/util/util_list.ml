include List
let hd = function | [] -> None | a::_ -> Some a
let tl = function | [] -> None | _::tl -> Some tl
let nth t n = if n < 0
  then None
  else
    let rec nth_aux t n =
      match t with
        | [] -> None
        | a::t -> match n = 0 with
          | true -> Some a
          | false  -> nth_aux t (n - 1)
    in
      nth_aux t n

let stack_max = Util_kernel.stack_max
let slow_append l l' = rev_append (rev l) l'
let rec count_append l l' count = match l' with
  | [] -> l
  | _ -> match l with
    | [] -> l'
    | v1::[] -> v1 :: l'
    | v1::v2::[] -> v1 :: v2 :: l'
    | v1::v2::v3::[] -> v1 :: v2 :: v3 :: l'
    | v1::v2::v3::v4::[] -> v1 :: v2 :: v3 :: v4 :: l'
    | v1::v2::v3::v4::v5::[] -> v1 :: v2 :: v3 :: v4 :: v5 :: l'
    | v1::v2::v3::v4::v5::tl -> v1 :: v2 :: v3 :: v4 :: v5 ::
      match count > stack_max with
        | true  -> slow_append tl l'
        | false  -> count_append tl l' (count + 1)
let append l l' = count_append l l' 0

let (@) = append

let slow_map = Util_kernel.slow_map
let count_map = Util_kernel.count_map
let map = Util_kernel.map

let fold_right = Util_kernel.fold_right

let concat l = fold_right append l []
let map_concat f l = (map f l) |> concat

module Monad_base = struct
  type 'a t = 'a list
  let (>>=) l f = map f l |> concat
  let pure v = [v]
  let mzero = []
  let mplus = (@)
  let map_impl = `Custom map
end
include Monad_base
include Util_monad.Monad_make(Monad_base)
include Util_monad.Monad_plus_make(Monad_base)

let rec slow_filter p l acc =
  match l with
  | [] -> acc
  | v::tl ->
    match p v with
    | true  -> slow_filter p tl (v :: acc)
    | false  -> slow_filter p tl acc
let rec count_filter i p l =
  match l with
  | [] -> []
  | _ when i > stack_max -> rev (slow_filter p l [])
  | v::tl ->
    match p v with
    | true  -> v :: count_filter (i + 1) p tl
    | false  -> count_filter i p tl
let filter p l = count_filter 0 p l

let rev_mapi f l =
  let rec iter i res = function
    | [] -> res
    | v::tl -> iter (i + 1) (f i v :: res) tl
  in
    iter 0 [] l
let mapi f l = rev (rev_mapi f l)

let map2 f l1 l2 = rev (rev_map2 f l1 l2)

let fold_right2 f l1 l2 accu =
  fold_left2 (fun a  -> fun b  -> fun c  -> f b c a) accu (rev l1) (rev l2)

let replicate n v =
  let rec iter i res =
    match i <= 0 with
    | true  -> res
    | false  -> iter (i - 1) (v :: res) in
  iter n []

let rec some_option f = function
  | [] -> None
  | v::tl ->
    match f v with
    | Some _ as v' -> v'
    | None  -> some_option f tl

let some f l =
  let mapper v' =
    match f v' with
    | true -> Some v'
    | false -> None
  in
    match some_option mapper l with
    | Some _ -> true
    | None  -> false

let all f l = not @@ some (fun v  -> not (f v)) l

let merge cmp l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [],l2 -> rev_append acc l2
    | l1,[] -> rev_append acc l1
    | h1::t1,h2::t2 ->
      match cmp h1 h2 <= 0 with
      | true  -> loop (h1 :: acc) t1 l2
      | false  -> loop (h2 :: acc) l1 t2
  in
    loop [] l1 l2

let range ?(start= 0) stop =
  if start >= stop
  then []
  else
    let rec iter n accu =
      match n < start with
      | true  -> accu
      | false  -> iter (n - 1) (n :: accu)
    in
      iter (stop - 1) []
