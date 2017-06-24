// Generated by BUCKLESCRIPT VERSION 1.7.5, PLEASE EDIT WITH CARE
'use strict';

var Util     = require("../../common/util.js");
var Block    = require("bs-platform/lib/js/block.js");
var Curry    = require("bs-platform/lib/js/curry.js");
var Bs_mocha = require("../bs_mocha.js");

Bs_mocha.from_pair_suites("Util.List", /* :: */[
      /* tuple */[
        "replicate works",
        (function () {
            return /* Eq */Block.__(0, [
                      Curry._2(Util.List[/* replicate */55], 4, 1),
                      /* :: */[
                        1,
                        /* :: */[
                          1,
                          /* :: */[
                            1,
                            /* :: */[
                              1,
                              /* [] */0
                            ]
                          ]
                        ]
                      ]
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "range works with no start",
          (function () {
              return /* Eq */Block.__(0, [
                        /* :: */[
                          0,
                          /* :: */[
                            1,
                            /* :: */[
                              2,
                              /* :: */[
                                3,
                                /* :: */[
                                  4,
                                  /* [] */0
                                ]
                              ]
                            ]
                          ]
                        ],
                        Curry._2(Util.List[/* range */60], /* None */0, 5)
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "range works with start",
            (function () {
                return /* Eq */Block.__(0, [
                          /* :: */[
                            2,
                            /* :: */[
                              3,
                              /* :: */[
                                4,
                                /* :: */[
                                  5,
                                  /* [] */0
                                ]
                              ]
                            ]
                          ],
                          Curry._2(Util.List[/* range */60], /* Some */[2], 6)
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "return is empty when start = stop",
              (function () {
                  return /* Eq */Block.__(0, [
                            /* [] */0,
                            Curry._2(Util.List[/* range */60], /* Some */[1], 1)
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "invalid range params return empty list",
                (function () {
                    return /* Eq */Block.__(0, [
                              /* [] */0,
                              Curry._2(Util.List[/* range */60], /* Some */[5], 1)
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "minimal map works",
                  (function () {
                      return /* Eq */Block.__(0, [
                                /* :: */[
                                  2,
                                  /* :: */[
                                    3,
                                    /* :: */[
                                      4,
                                      /* [] */0
                                    ]
                                  ]
                                ],
                                Curry._2(Util.List[/* map */42], (function (param) {
                                        return 1 + param | 0;
                                      }), /* :: */[
                                      1,
                                      /* :: */[
                                        2,
                                        /* :: */[
                                          3,
                                          /* [] */0
                                        ]
                                      ]
                                    ])
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "map works",
                    (function () {
                        var lengths = /* :: */[
                          1,
                          /* :: */[
                            2,
                            /* :: */[
                              3,
                              /* :: */[
                                4,
                                /* :: */[
                                  5,
                                  /* :: */[
                                    6,
                                    /* :: */[
                                      7,
                                      /* :: */[
                                        8,
                                        /* :: */[
                                          9,
                                          /* :: */[
                                            10,
                                            /* [] */0
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ];
                        var f = function (x) {
                          return x + 1 | 0;
                        };
                        var init = Curry._2(Util.List[/* map */42], (function (x) {
                                return Curry._2(Util.List[/* replicate */55], x, 1);
                              }), lengths);
                        var expected = Curry._2(Util.List[/* map */42], (function (x) {
                                return Curry._2(Util.List[/* replicate */55], x, 2);
                              }), lengths);
                        var actual = Curry._2(Util.List[/* map */42], Curry._1(Util.List[/* map */42], f), init);
                        return /* Eq */Block.__(0, [
                                  expected,
                                  actual
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "map doesn't cause stack overflow",
                      (function () {
                          return /* Eq */Block.__(0, [
                                    10000,
                                    Curry._1(Util.List[/* length */0], Curry._2(Util.List[/* map */42], (function (a) {
                                                return a;
                                              }), Curry._2(Util.List[/* replicate */55], 10000, 1)))
                                  ]);
                        })
                    ],
                    /* [] */0
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

/*  Not a pure module */