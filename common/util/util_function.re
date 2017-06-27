let debug v => {
  Js.log v;
  v;
};

let flip = Util_kernel.flip;
let (%) = Util_kernel.compose;
let compose = Util_kernel.compose;
let const x _ => x;
let on c f a b => c (f a) (f b);
let neg f x => not @@ f x;
