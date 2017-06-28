include Util_function
module List = Util_list
module Json = Util_json
module Monad = Util_monad
module Option = Util_option
module Promise = Util_promise
module Array = struct
  include Array
  let try_get a n = match (n >= 0) && (n < (length a)) with
    | true  -> Some a.(n)
    | false  -> None
end
