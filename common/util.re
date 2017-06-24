let debug v => {
  Js.log v;
  v;
};

module List = {
  include List;

  let hd = fun
    | [] => None
    | [a, ..._] => Some a;

  let tl = fun
    | [] => None
    | [_, ...tl] => Some tl;

  let nth t n => if (n < 0) {
    None
  } else {
    let rec nth_aux t n => switch t {
      | [] => None
      | [a, ...t] => n == 0 ? Some a : nth_aux t (n-1);
    };
    nth_aux t n;
  };

  let stack_max = 1_000;

  let slow_append l l' => rev_append (rev l) l';
  let rec count_append l l' count => switch l' {
    | [] => l
    | _ => switch l {
      | [] => l'
      | [v1] => [v1, ...l']
      | [v1, v2] => [v1, v2, ...l']
      | [v1, v2, v3] => [v1, v2, v3, ...l']
      | [v1, v2, v3, v4] => [v1, v2, v3, v4, ...l']
      | [v1, v2, v3, v4, v5] => [v1, v2, v3, v4, v5, ...l']
      | [v1, v2, v3, v4, v5, ...tl] => [v1, v2, v3, v4, v5,
        ...(count > stack_max ? slow_append tl l' : count_append tl l' (count + 1))]
    }
  };
  let append l l' => count_append l l' 0;
  let (@) = append;

  let slow_map f l => rev (rev_map f l);
  let rec count_map f l count => switch l {
    | [] => []
    | [v1] => [f v1]
    | [v1, v2] => [f v1, f v2]
    | [v1, v2, v3] => [f v1, f v2, f v3]
    | [v1, v2, v3, v4] => [f v1, f v2, f v3, f v4]
    | [v1, v2, v3, v4, v5] => [f v1, f v2, f v3, f v4, f v5]
    | [v1, v2, v3, v4, v5, ...tl] => [f v1, f v2, f v3, f v4, f v5,
      ...(count > stack_max ? slow_map f tl : count_map f tl (count + 1))]
  };
  let map f l => count_map f l 0;

  let rec slow_filter p l acc => switch l {
    | [] => acc
    | [v, ...tl] => p v [@bs] ? slow_filter p tl [v, ...acc] : slow_filter p tl acc
  };
  let rec count_filter i p l => switch l {
    | [] => []
    | _ when i > stack_max => rev (slow_filter p l [])
    | [v, ...tl] => p v [@bs] ? [v, ...count_filter (i - 1) p tl] : count_filter i p tl
  };
  let filter p l => count_filter 0 p l;

  let rec slow_map_option p l acc => switch l {
    | [] => acc
    | [v, ...tl] => switch (p v [@bs]) {
      | Some v' => slow_map_option p tl [v', ...acc]
      | None =>  slow_map_option p tl acc
    }
  };
  let rec count_map_option i p l => switch l {
    | [] => []
    | _ when i > stack_max => rev (slow_map_option p l [])
    | [v, ...tl] => switch (p v [@bs]) {
      | Some v' => [v', ...count_map_option (i - 1) p tl]
      | None => count_map_option i p tl
    }
  };
  let map_option p l => count_map_option 0 p l;

  let rev_mapi f l => {
    let rec iter i res => fun
      | [] => res
      | [v, ...tl] => iter (i + 1) [f i v] tl;
    iter 0 [] l;
  };

  let mapi f l => rev (rev_mapi f l);

  let fold_right f l accu => switch l {
    | [] => accu
    | _ => fold_left (fun a b => f b a) accu (rev l)
  };

  let concat l => fold_right append l [];

  let map2 f l1 l2 => rev (rev_map2 f l1 l2);
  let fold_right2 f l1 l2 accu => fold_left2 (fun a b c => f b c a) accu l1 l2;

  let replicate n v => {
    let rec iter i res => i <= 0 ? res : iter (i - 1) [v, ...res];
    iter n [];
  };

  let rec some_option f => fun
    | [] => None
    | [v, ...tl] => switch (f v) {
      | (Some _) as v' => v'
      | None => some_option f tl
    };

  let rec some f => fun
    | [] => false
    | [v, ...tl] => f v ? true : some f tl;

  let rec all f => fun
    | [] => true
    | [v, ...tl] => f v ? all f tl : false;

  let merge cmp l1 l2 => {
    let rec loop acc l1 l2 => switch (l1, l2) {
      | ([], l2) => rev_append acc l2
      | (l1, []) => rev_append acc l1
      | ([h1, ...t1], [h2, ...t2]) =>
        cmp h1 h2 <= 0 ?
        loop [h1, ...acc] t1 l2 :
        loop [h2, ...acc] l1 t2
    };
    loop [] l1 l2;
  };

  let range ::start=0 stop => {
    if (start >= stop) { [] }
    else {
      let rec iter n accu => (n < start) ? accu : iter (n - 1) [n, ...accu];
      iter (stop - 1) [];
    }
  }
};
