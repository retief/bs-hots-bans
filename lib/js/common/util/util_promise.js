// Generated by BUCKLESCRIPT VERSION 1.7.5, PLEASE EDIT WITH CARE
'use strict';

var Curry      = require("bs-platform/lib/js/curry.js");
var Util_monad = require("./util_monad.js");

function $great$great$eq(m, f) {
  return m.then(Curry.__1(f));
}

function pure(prim) {
  return Promise.resolve(prim);
}

var Base = /* module */[
  /* >>= */$great$great$eq,
  /* pure */pure,
  /* map_impl : Define_using_bind */-349054340
];

var include = Util_monad.Monad_make(Base);

var map_impl = /* Define_using_bind */-349054340;

var bind = include[0];

var $great$great = include[1];

var $eq$less$less = include[2];

var map = include[3];

var $less$$great = include[4];

var $pipe$$great = include[5];

var ap = include[6];

var $less$neg$great = include[7];

var $pipe$neg$great = include[8];

var sequence = include[9];

var sequence_ = include[10];

var mapM = include[11];

var mapM_ = include[12];

exports.Base            = Base;
exports.$great$great$eq = $great$great$eq;
exports.pure            = pure;
exports.map_impl        = map_impl;
exports.bind            = bind;
exports.$great$great    = $great$great;
exports.$eq$less$less   = $eq$less$less;
exports.map             = map;
exports.$less$$great    = $less$$great;
exports.$pipe$$great    = $pipe$$great;
exports.ap              = ap;
exports.$less$neg$great = $less$neg$great;
exports.$pipe$neg$great = $pipe$neg$great;
exports.sequence        = sequence;
exports.sequence_       = sequence_;
exports.mapM            = mapM;
exports.mapM_           = mapM_;
/* include Not a pure module */