// Generated by BUCKLESCRIPT VERSION 1.7.5, PLEASE EDIT WITH CARE
'use strict';

var Util                 = require("../common/util/util.js");
var Curry                = require("bs-platform/lib/js/curry.js");
var Buffer               = require("bs-platform/lib/js/buffer.js");
var Cheerio              = require("cheerio");
var Rcheerio             = require("../common/rcheerio.js");
var Util_json            = require("../common/util/util_json.js");
var Util_list            = require("../common/util/util_list.js");
var Caml_array           = require("bs-platform/lib/js/caml_array.js");
var Util_option          = require("../common/util/util_option.js");
var Util_promise         = require("../common/util/util_promise.js");
var RequestPromiseNative = require("request-promise-native");

function make(name, group) {
  return /* record */[
          /* name */name,
          /* group */group
        ];
}

function from_json(j) {
  return Util_option.$great$great$eq(Util_json.jdict_of_json(j), (function (dict) {
                return Curry._2(Util_option.$less$neg$great, Curry._2(Util_option.$less$$great, make, Curry._2(Util_json.$$String[/* @. */5], dict, "PrimaryName")), Curry._2(Util_json.$$String[/* @. */5], dict, "Group"));
              }));
}

function to_json(param) {
  return Util_json.jobj_of_list(/* :: */[
              /* tuple */[
                "PrimaryName",
                Curry._1(Util_json.$$String[/* to_json */1], param[/* name */0])
              ],
              /* :: */[
                /* tuple */[
                  "Group",
                  Curry._1(Util_json.$$String[/* to_json */1], param[/* group */1])
                ],
                /* [] */0
              ]
            ]);
}

var Hero = Util_json.Extend(/* module */[
      /* from_json */from_json,
      /* to_json */to_json
    ]);

function make_hd(name, map, games, winrate) {
  return /* record */[
          /* name */name,
          /* map */map,
          /* games */games,
          /* winrate */winrate,
          /* power */games * (winrate - 50) | 0
        ];
}

var float_chars = (/[.0-9]+/g);

var int_chars = (/[0-9]+/g);

function $$switch(r, s) {
  var $$break = /* false */0;
  var buf = Buffer.create(5);
  while(!$$break) {
    var match = r.exec(s);
    if (match !== null) {
      Buffer.add_string(buf, Caml_array.caml_array_get(match, 0));
    } else {
      $$break = /* true */1;
    }
  };
  var match$1 = +(Buffer.length(buf) === 0);
  if (match$1 !== 0) {
    return /* None */0;
  } else {
    return /* Some */[Buffer.contents(buf)];
  }
}

function parse_hd(name, param) {
  if (param) {
    var match = param[1];
    if (match) {
      var match$1 = match[1];
      if (match$1) {
        var match$2 = match$1[1];
        if (match$2) {
          var wr = match$2[0];
          var mp = match[0];
          return Util_option.$great$great$eq(Util_option.$great$great$eq($$switch(int_chars, match$1[0]), Util.int_of_string_safe), (function (games) {
                        return Util_option.$great$great$eq(Util_option.$great$great$eq($$switch(float_chars, wr), Util.float_of_string_safe), (function (winrate) {
                                      return Util_option.pure(make_hd(name, mp, games, winrate));
                                    }));
                      }));
        } else {
          return /* None */0;
        }
      } else {
        return /* None */0;
      }
    } else {
      return /* None */0;
    }
  } else {
    return /* None */0;
  }
}

var Hero_list = Util_json.List([
      Hero[0],
      Hero[1]
    ]);

function get_heroes() {
  return Curry._2(Util_promise.$pipe$$great, Curry._2(Util_promise.$pipe$$great, RequestPromiseNative.call("", "https://api.hotslogs.com/Public/Data/Heroes"), Hero_list[/* parse */2]), (function (param) {
                return Util_option.unwrap(/* [] */0, param);
              }));
}

function get_hero_details(name, gc) {
  var process_row = function (e) {
    return parse_hd(name, Rcheerio.map(gc, (function (prim) {
                      return prim.text();
                    }), gc.call("", "td", e)));
  };
  return Util_option.concat(Rcheerio.map_el(process_row, gc.call("", "#winRateByMap tr")));
}

function get_hero_page(name) {
  return Curry._2(Util_promise.$pipe$$great, RequestPromiseNative.call("", "https://www.hotslogs.com/Sitewide/HeroDetails?Hero=" + name), (function (prim) {
                return Cheerio.load(prim);
              }));
}

function get_all_hero_details() {
  var get_details = function (param) {
    var name = param[/* name */0];
    return Curry._2(Util_promise.$pipe$$great, get_hero_page(name), (function (param) {
                  return get_hero_details(name, param);
                }));
  };
  return Util_promise.$great$great$eq(get_heroes(/* () */0), (function (heroes) {
                  return Util_promise.$great$great$eq(Curry._2(Util_promise.mapM, get_details, heroes), (function (details) {
                                return Util_promise.pure(/* tuple */[
                                            heroes,
                                            Util_list.concat(details)
                                          ]);
                              }));
                })).catch((function () {
                return Util_promise.pure(/* tuple */[
                            /* [] */0,
                            /* [] */0
                          ]);
              }));
}

Curry._2(Util_promise.map, (function (prim) {
        console.log(prim);
        return /* () */0;
      }), get_all_hero_details(/* () */0));

exports.Hero                 = Hero;
exports.make_hd              = make_hd;
exports.float_chars          = float_chars;
exports.int_chars            = int_chars;
exports.$$switch             = $$switch;
exports.parse_hd             = parse_hd;
exports.Hero_list            = Hero_list;
exports.get_heroes           = get_heroes;
exports.get_hero_details     = get_hero_details;
exports.get_hero_page        = get_hero_page;
exports.get_all_hero_details = get_all_hero_details;
/* Hero Not a pure module */