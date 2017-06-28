open Util_kernel
module type Functor = sig type 'a t val map : ('a -> 'b) -> 'a t -> 'b t end
module type Monad_base  = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val pure : 'a -> 'a t
  val map_impl : [ `Define_using_bind  | `Custom of ('a -> 'b) -> 'a t -> 'b t ]
end
module type Monad_plus_base = sig
  include Monad_base
  val mzero : 'a t
  val mplus : 'a t -> 'a t -> 'a t
end
module type Monad = sig
  include Monad_base
  val (>>) : 'a t -> 'b t -> 'b t
  val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  val ap : ('a -> 'b) t -> 'a t -> 'b t
  val (<->) : ('a -> 'b) t -> 'a t -> 'b t
  val sequence : 'a t list -> 'a t
  val sequence_ : 'a t list -> unit t
  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
  val mapM_ : ('a -> 'b t) -> 'a list -> unit t
end
module type Monad_plus = sig
  include Monad
  include (Monad_plus_base with type 'a t :=  'a t)
  val msum : 'a t list -> 'a t
  val mfilter : ('a -> bool) -> 'a t -> 'a t
end
module Monad_make(M:Monad_base) = struct
  open M
  let bind = (>>=)
  let (>>) a b = a >>= (fun _  -> b)
  let (=<<) f v = v >>= f
  let map = match map_impl with
    | `Define_using_bind -> fun f mx -> mx >>= fun x -> pure @@ f x
    | `Custom f -> f
  let (<$>) = map
  let (|$>) v f = map f v
  let ap mf mx = mf >>= (flip map mx)
  let (<->) = ap
  let (|->) v f = ap f v
  let sequence l =
    let k m m' = m >>= (fun v  -> m' >>= (fun v'  -> pure (v :: v'))) in
      (fold_right k l) @@ (pure [])
  let sequence_ l = fold_right (>>) l (pure ())
  let mapM f l = (Util_kernel.map f l) |> sequence
  let mapM_ f l = (Util_kernel.map f l) |> sequence_
end
module Monad_plus_make(M:Monad_plus_base) = struct
  open M
  let msum l = fold_left mplus mzero l
  let filter f m = m >>= fun v ->
    match f v with
      | true  -> pure v
      | false  -> mzero
end
