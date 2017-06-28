open Util
open Bs_mocha
let _ = from_pair_suites "Util"
  [ ("flip works", fun () -> Eq (2, flip (-) 1 3));
    ("const works", fun () -> Eq (1, (const 1 "foo")));
    ("% works", fun () -> Eq ("5", string_of_int % (+) 2 @@ 3));
    ("% works deeply", fun () -> Eq (-2, (-) 4 % ( * ) 2 % (+) 1 @@ 2));
    ("on works", fun () -> Eq (3, on (-) int_of_string "5" "2"));
    ("neg works when true", fun () -> Eq (false, neg ((=) 1) 1));
    ("neg works when true", fun () -> Eq (true, neg ((=) 2) 1))]

let incr_if_pos n = match n > 0 with | true -> Some (n + 1) | false -> None

let _ = let open List in
  from_pair_suites "Util.List"
    [ ("hd works on non-empty lists", fun () -> Eq (Some 1, hd [1; 2; 3]));
      ("hd doesn't error on empty lists", fun () -> Eq (None, hd []));
      ("tl works on non-empty lists", fun () -> Eq (Some [2; 3], tl [1; 2; 3]));
      ("tl works on empty lists", fun () -> Eq (None, tl []));
      ("nth works on valid indexes", fun () -> Eq (Some 2, nth [1; 2; 3] 1));
      ("nth works when it goes off the end of the list", fun () -> Eq (None, nth [1; 2; 3] 5));
      ("nth works on negative indexes", fun () -> Eq (None, nth [1; 2; 3] (-1)));
      ("nth doesn't cause stack overflow", fun () ->
        Eq (Some 1, nth (replicate 10000 1) 9999));
      ("minimal append works", fun () ->
        Eq ([1; 2; 3; 4; 5], append [1; 2] [3; 4; 5]));
      ("append works", fun () ->
        let lengths = range ~start:0 11 in
        let expected_list = replicate 10 1 in
        let expected = map (const expected_list) lengths in
        let actual = map2 (fun n1 n2 -> append (replicate n1 1) (replicate n2 1))
          lengths @@ (rev lengths)
        in
        Eq (expected, actual));
      ("append doesn't cause stack overflow", fun () ->
        Eq (20000, length @@ append (replicate 10000 1) (replicate 10000 1)));
      ("minimal map works", fun () ->
        Eq ([2; 3; 4], map ((+) 1) [1; 2; 3]));
      ("map works", fun () ->
        let lengths = range ~start:1 11 in
        let f x = x + 1 in
        let init = map range lengths in
        let expected = map (fun n -> range ~start:1 @@ n + 1) lengths in
        let actual = map (map f) init in
        Eq (expected, actual));
      ("map doesn't cause stack overflow", fun () ->
        Eq (10000, replicate 10000 1 |> map (fun a -> a) |> length));
      ("fold_right works", fun () -> Eq (range 4, fold_right (fun a b -> a :: b) (range 4) []));
      ("fold_right doesn't cause stack overflow", fun () ->
        Eq (10000, fold_right (+) (replicate 10000 1) 0));
      ("concat works", fun () ->
        Eq ([1; 2; 3; 0; 1; 2; (-1); 0; 1], concat [range ~start:1 4; range 3; range ~start:(-1) 2]));
      ("concat on few long lists doesn't cause stack overflow", fun () ->
        Eq (30000, length @@ concat @@ replicate 3 @@ replicate 10000 1));
      ("concat on many short lists doesn't cause stack overflow", fun () ->
        Eq (30000, length @@ concat @@ replicate 10000 @@ replicate 3 1));
      ("map_concat works", fun () ->
        Eq ([1; 1; 1; 2; 2; 2], map_concat (replicate 3) [1; 2]));
      ("map_concat doesn't cause stack overflow on long results",
        fun () -> Eq (30000, length @@ map_concat (replicate 10000) [1; 2; 3]));
      ("map_concat doesn't cause stack overflow on long inputs", fun () ->
        Eq (30000, length @@ map_concat (replicate 3) @@ range 10000));
      ("filter works", fun () -> Eq ([1; 2; 3; 4], filter ((<) 0) [-1; 1; -2; 2; 3; 4; 0]));
      ("filter doesn't cause stack overflow", fun () ->
        Eq (10000, length @@ filter ((<) 0) @@ append (replicate 10000 1) (replicate 10000 0)));
      ("mapi works", fun () -> Eq (range ~start:(-1) 3, mapi (-) @@ replicate 4 1));
      ("mapi doesn't cause stack overflow",
        fun () -> Eq (10000, length @@ mapi (+) @@ replicate 10000 1));
      ("map2 works", fun () -> Eq (range 4, map2 (-) (range ~start:1 5) @@ replicate 4 1));
      ("map2 doesn't cause stack overflow",
        fun () -> Eq (10000, length @@ map2 (+) (replicate 10000 1) @@ replicate 10000 1));
      ("fold_right2 works", fun () ->
        let inc_mult_sub x y z = ((x + 1) * y) - z in
        Eq (10, fold_right2 inc_mult_sub [1; 2; 3] [2; 3; 4] 1));
      ("fold_right2 doesn't cause stack overflow", fun () ->
        Eq (20000, fold_right2 (fun a b c -> a + b + c) (replicate 10000 1) (replicate 10000 1) 0));
      ("replicate works", fun () -> Eq (replicate 4 1, [1; 1; 1; 1]));
      ("replicate doesn't cause stack overflow", fun () ->
        Eq (10000, length @@ replicate 10000 1));
      ("some_option finds stuff", fun () ->
        Eq (Some 2, some_option incr_if_pos [-1; 0; 1; 4])) ;
      ("some_option doesn't find stuff", fun () ->
        Eq (None, some_option incr_if_pos @@ replicate 4 (-1)));
      ("some_option doesn't cause stack overflow",
        fun () -> Eq (Some 2, some_option incr_if_pos @@ append (replicate 10000 0) [1]));
      ("some finds stuff", fun () -> Eq (true, some ((=) 1) @@ range ~start:(-1) 4));
      ("some doesn't find stuff", fun () -> Eq (false, some ((=) 1) @@ replicate 10 0));
      ("some doesn't cause stack overflow",
        fun () -> Eq (true, some ((=) 1) @@ append (replicate 10000 0) [1]));
      ("all returns true", fun () -> Eq (true, all ((=) 1) @@ replicate 5 1));
      ("all returns false", fun () -> Eq (false, all ((=) 1) [1; 1; 1; 0; 1]));
      ("all doesn't cause stack overflow", fun () ->
        Eq (false, all ((=) 1) @@ append (replicate 10000 1) [0]));
      ("merge works", fun () ->
        Eq ([1; 2; 2; 3; 4; 5], merge compare [1; 2; 3; 4] [2; 5]));
      ("merge doesn't cause stack overflow", fun () ->
        Eq (20000, length @@ merge compare (replicate 10000 1) (replicate 10000 1)));
      ("range works with no start", fun () -> Eq ([0; 1; 2; 3; 4], range 5));
      ("range works with start", fun () -> Eq ([2; 3; 4; 5], range ~start:2 6));
      ("return is empty when start = stop", fun () -> Eq ([], range ~start:1 1));
      ("invalid range params return empty list", fun () -> Eq ([], range ~start:5 1));
      ("range doesn't cause stack overflow", fun () -> Eq (10000, length @@ range 10000))]
let _ = let open Option in
  from_pair_suites "Util.Option"
    [ ("map_default works on some", fun () -> Eq (4, map_default 1 ((+) 1) @@ Some 3));
      ("map_default works on none", fun () -> Eq (1, map_default 1 ((+) 1) None));
      ("is_some works on some", fun () -> Eq (true, is_some @@ Some 1));
      ("is_some works on none", fun () -> Eq (false, is_some None));
      ("is_none works on some", fun () -> Eq (false, is_none @@ Some 1));
      ("is_none works on none", fun () -> Eq (true, is_none None));
      ("unwrap_exn works on some", fun () -> Eq (2, unwrap_exn @@ Some 2));
      ("unwrap_exn throws on none", fun () -> ThrowAny (fun () -> unwrap_exn None));
      ("unwrap works on some", fun () -> Eq (2, unwrap 3 @@ Some 2));
      ("unwrap works on none", fun () -> Eq (3, unwrap 3 None));
      ("concat works", fun () ->
        Eq ([1; 2; 3; 4], concat [Some 1; None; Some 2; Some 3; None; None; Some 4]));
      ("concat doesn't cause stack overflow", fun () ->
        Eq (10000, List.length @@ concat @@
          List.append (List.replicate 10000 (Some 2)) (List.replicate 10000 None)));
      ("map_concat works", fun () ->
        Eq ([2; 3; 4; 5], map_concat incr_if_pos [(-1); 1; (-2); 2; 3; 4; 0]));
      ("map_concat doesn't cause stack overflow", fun () ->
        Eq (10000, List.length @@ map_concat incr_if_pos @@
          List.append (List.replicate 10000 1) (List.replicate 10000 0)))]
