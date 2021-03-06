// Generated by BUCKLESCRIPT VERSION 1.8.0, PLEASE EDIT WITH CARE
'use strict';

var $$Array       = require("bs-platform/lib/js/array.js");
var Caml_array    = require("bs-platform/lib/js/caml_array.js");
var Util_function = require("./util_function.js");

function try_get(a, n) {
  var match = +(n >= 0 && n < a.length);
  if (match !== 0) {
    return /* Some */[Caml_array.caml_array_get(a, n)];
  } else {
    return /* None */0;
  }
}

var $$Array$1 = /* module */[
  /* init */$$Array.init,
  /* make_matrix */$$Array.make_matrix,
  /* create_matrix */$$Array.create_matrix,
  /* append */$$Array.append,
  /* concat */$$Array.concat,
  /* sub */$$Array.sub,
  /* copy */$$Array.copy,
  /* fill */$$Array.fill,
  /* blit */$$Array.blit,
  /* to_list */$$Array.to_list,
  /* of_list */$$Array.of_list,
  /* iter */$$Array.iter,
  /* map */$$Array.map,
  /* iteri */$$Array.iteri,
  /* mapi */$$Array.mapi,
  /* fold_left */$$Array.fold_left,
  /* fold_right */$$Array.fold_right,
  /* sort */$$Array.sort,
  /* stable_sort */$$Array.stable_sort,
  /* fast_sort */$$Array.fast_sort,
  /* try_get */try_get
];

var debug = Util_function.debug;

var flip = Util_function.flip;

var $percent = Util_function.$percent;

var compose = Util_function.compose;

var $$const = Util_function.$$const;

var on = Util_function.on;

var neg = Util_function.neg;

var try_apply = Util_function.try_apply;

var $pipe$unknown$great = Util_function.$pipe$unknown$great;

var bool_of_string_safe = Util_function.bool_of_string_safe;

var int_of_string_safe = Util_function.int_of_string_safe;

var float_of_string_safe = Util_function.float_of_string_safe;

var List = 0;

var Json = 0;

var Monad = 0;

var Option = 0;

var Promise = 0;

exports.debug                = debug;
exports.flip                 = flip;
exports.$percent             = $percent;
exports.compose              = compose;
exports.$$const              = $$const;
exports.on                   = on;
exports.neg                  = neg;
exports.try_apply            = try_apply;
exports.$pipe$unknown$great  = $pipe$unknown$great;
exports.bool_of_string_safe  = bool_of_string_safe;
exports.int_of_string_safe   = int_of_string_safe;
exports.float_of_string_safe = float_of_string_safe;
exports.List                 = List;
exports.Json                 = Json;
exports.Monad                = Monad;
exports.Option               = Option;
exports.Promise              = Promise;
exports.$$Array              = $$Array$1;
/* No side effect */
