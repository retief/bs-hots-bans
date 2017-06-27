/* Load order is Util_kernel => Util_function => Util_monad
=> Util_datatype => Util_json */

include Util_function;
module List = Util_list;
module Json = Util_json;
module Monad = Util_monad;
module Option = Util_option;
module Promise = Util_promise;
module Array = {
  include Array;
  let try_get a n => n >= 0 && n < length a ? Some a.(n) : None;
};
