// Generated by BUCKLESCRIPT VERSION 1.8.0, PLEASE EDIT WITH CARE
'use strict';

var Data         = require("../common/data.js");
var Util         = require("../common/util/util.js");
var Curry        = require("bs-platform/lib/js/curry.js");
var Buffer       = require("bs-platform/lib/js/buffer.js");
var Request      = require("../common/request.js");
var Cheerio      = require("cheerio");
var Rcheerio     = require("../common/rcheerio.js");
var Util_json    = require("../common/util/util_json.js");
var Caml_array   = require("bs-platform/lib/js/caml_array.js");
var Util_option  = require("../common/util/util_option.js");
var Util_promise = require("../common/util/util_promise.js");

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
                                      return Util_option.pure(Data.make_hd(name, mp, games, winrate));
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
      Data.Hero[0],
      Data.Hero[1]
    ]);

function get_heroes() {
  return Curry._2(Util_promise.$pipe$$great, Curry._2(Util_promise.$pipe$$great, Request.get_json("https://api.hotslogs.com/Public/Data/Heroes"), Hero_list[/* from_json */0]), (function (param) {
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
  return Curry._2(Util_promise.$pipe$$great, Request.get_html("https://www.hotslogs.com/Sitewide/HeroDetails?Hero=" + name), (function (prim) {
                return Cheerio.load(prim);
              }));
}

function dummy_data() {
  return /* record */[
          /* heroes : :: */[
            /* record */[
              /* name */"Thrall",
              /* group */"Assassin"
            ],
            /* :: */[
              /* record */[
                /* name */"Falstad",
                /* group */"Assassin"
              ],
              /* :: */[
                /* record */[
                  /* name */"Kharazim",
                  /* group */"Support"
                ],
                /* :: */[
                  /* record */[
                    /* name */"Brightwing",
                    /* group */"Support"
                  ],
                  /* [] */0
                ]
              ]
            ]
          ],
          /* details : :: */[
            Data.make_hd("Thrall", "Battlefield of Eternity", 1000, 50),
            /* :: */[
              Data.make_hd("Falstad", "Battlefield of Eternity", 1000, 49),
              /* :: */[
                Data.make_hd("Kharazim", "Battlefield of Eternity", 1000, 51),
                /* :: */[
                  Data.make_hd("Brightwing", "Battlefield of Eternity", 1000, 52),
                  /* :: */[
                    Data.make_hd("Thrall", "Garden of Terror", 1000, 51),
                    /* :: */[
                      Data.make_hd("Falstad", "Garden of Terror", 1000, 50),
                      /* :: */[
                        Data.make_hd("Kharazim", "Garden of Terror", 1000, 51),
                        /* :: */[
                          Data.make_hd("Brightwing", "Garden of Terror", 1000, 49),
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ],
          /* time */new Date().getTime()
        ];
}

function get_all_hero_details() {
  return Util_promise.pure(dummy_data(/* () */0));
}

exports.float_chars          = float_chars;
exports.int_chars            = int_chars;
exports.$$switch             = $$switch;
exports.parse_hd             = parse_hd;
exports.Hero_list            = Hero_list;
exports.get_heroes           = get_heroes;
exports.get_hero_details     = get_hero_details;
exports.get_hero_page        = get_hero_page;
exports.dummy_data           = dummy_data;
exports.get_all_hero_details = get_all_hero_details;
/* float_chars Not a pure module */