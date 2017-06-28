let fold_right f l accu = List.fold_left (fun a b -> f b a) accu @@ List.rev l
let fold_left = List.fold_left
let flip f a b = f b a
let compose f g x = f @@ g x
let stack_max = 1000
let slow_map f l = List.rev @@ List.rev_map f l
let rec count_map f l count = match l with
  | [] -> []
  | v1::[] -> [f v1]
  | v1::v2::[] -> [f v1; f v2]
  | v1::v2::v3::[] -> [f v1; f v2; f v3]
  | v1::v2::v3::v4::[] -> [f v1; f v2; f v3; f v4]
  | v1::v2::v3::v4::v5::[] -> [f v1; f v2; f v3; f v4; f v5]
  | v1::v2::v3::v4::v5::tl -> f v1 :: f v2 :: f v3 :: f v4 :: f v5 ::
    match count > stack_max with
      | true  -> slow_map f tl
      | false  -> count_map f tl @@ count + 1
let map f l = count_map f l 0
