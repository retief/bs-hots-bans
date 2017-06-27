module Base = {
  include Js.Promise;
  let (>>=) m f => then_ f m;
  let pure = resolve;
  let map_impl = `Define_using_bind;
};
include Base;
include Util_monad.Monad_make Base;
