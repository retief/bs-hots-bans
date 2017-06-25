include Util_function;
module List = Util_list;
module Json = Util_json;
module Monad = Util_monad;
module Array = {
  include Array;
  let try_get a n => n >= 0 && n < length a ? Some a.(n) : None;
};
