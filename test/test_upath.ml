open Alsdiff_base.Xml
open Alsdiff_base.Upath
open Test_utils.Utils

let sample_xml =
  Element {
    name = "root";
    attrs = [];
    childs = [
      Element {
        name = "a";
        attrs = [("id", "1")];
        childs = [
          Element { name = "b"; attrs = []; childs = [Data "hello"] };
          Element { name = "c"; attrs = [("val", "test")]; childs = [] };
        ];
      };
      Element {
        name = "a";
        attrs = [("id", "2")];
        childs = [
          Element { name = "d"; attrs = []; childs = [] };
          Element {
            name = "b";
            attrs = [("lang", "en")];
            childs = [Data "world"];
          };
        ];
      };
      Element {
        name = "e";
        attrs = [];
        childs = [
          Element { name = "child"; attrs = [("id", "e-child")]; childs = [] };
          Element {
            name = "f";
            attrs = [];
            childs = [
              Element { name = "b"; attrs = []; childs = [] }
            ];
          }
        ];
      };
      Element {
        name = "special";
        attrs = [("type", "magic")];
        childs = [
          Element { name = "child"; attrs = [("id", "s-child")]; childs = [] }
        ];
      };
      Element { name = "child"; attrs = [("type", "magic")]; childs = [] };
    ];
  }

let find_path_testable = Alcotest.(option (pair string xml_testable))

let test_find_path path_str expected () =
  let result = find_opt path_str sample_xml in
  Alcotest.check find_path_testable ("find_path " ^ path_str) expected result

let test_cases =
  [
    ("/root/a/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"];  }));
    ("/root/a[0]/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"];  }));
    ("/root/a[1]/b", Some ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"] }));
    ("/root/a@id=\"1\"/c", Some ("/root/a/c", Element { name = "c"; attrs = [("val", "test")]; childs = [];  }));
    ("/root/a@id=\"2\"/d", Some ("/root/a/d", Element { name = "d"; attrs = []; childs = [];  }));
    ("/root/a@id=\"3\"/d", None);
    ("/root/*/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"];  }));
    ("/root/**/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"];  }));
    ("/**/f/b", Some ("/root/e/f/b", Element { name = "b"; attrs = []; childs = [];  }));
    ("/root/e/**/b", Some ("/root/e/f/b", Element { name = "b"; attrs = []; childs = [];  }));
    ("/**/d", Some ("/root/a/d", Element { name = "d"; attrs = []; childs = [];  }));
    ("/root/a/nonexistent", None);
    ("/root/a[2]/b", None);
    ("/**/b@lang=\"en\"", Some ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"] }));
    ("/**/b@lang=\"fr\"", None);
    (* Tests for wildcard with attributes *)
    ("/*@id", Some ("/root/a", Element { name = "a"; attrs = [("id", "1")]; childs = [Element { name = "b"; attrs = []; childs = [Data "hello"]; }; Element { name = "c"; attrs = [("val", "test")]; childs = []; }]; }));
    ("/*@id=\"2\"", Some ("/root/a", Element { name = "a"; attrs = [("id", "2")]; childs = [Element { name = "d"; attrs = []; childs = []; }; Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"]; }]; }));
    ("/*@type", Some ("/root/special", Element { name = "special"; attrs = [("type", "magic")]; childs = [Element { name = "child"; attrs = [("id", "s-child")]; childs = []; }];  }));
    ("/*@type=\"magic\"", Some ("/root/special", Element { name = "special"; attrs = [("type", "magic")]; childs = [Element { name = "child"; attrs = [("id", "s-child")]; childs = []; }];  }));
    ("/*@type=\"nonexistent\"", None);
    (* Tests for multi wildcard with attributes *)
    ("/**@id", Some ("/root/a", Element { name = "a"; attrs = [("id", "1")]; childs = [Element { name = "b"; attrs = []; childs = [Data "hello"];  }; Element { name = "c"; attrs = [("val", "test")]; childs = []; }];  }));
    ("/**@id=\"2\"", Some ("/root/a", Element { name = "a"; attrs = [("id", "2")]; childs = [Element { name = "d"; attrs = []; childs = [] }; Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"] }]; }));
    ("/**@type", Some ("/root/special", Element { name = "special"; attrs = [("type", "magic")]; childs = [Element { name = "child"; attrs = [("id", "s-child")]; childs = []; }];  }));
    ("/**@type=\"magic\"", Some ("/root/special", Element { name = "special"; attrs = [("type", "magic")]; childs = [Element { name = "child"; attrs = [("id", "s-child")]; childs = []; }];  }));
    ("/**@type=\"nonexistent\"", None);
    (* Tests for multi wildcard with attributes followed by more path components *)
    ("/**@type/child", Some ("/root/special/child", Element { name = "child"; attrs = [("id", "s-child")]; childs = [];  }));
  ]

let filter_path_testable = Alcotest.(list (pair string xml_testable))

let test_find_all path_str expected () =
  let sort_results l =
    List.sort (fun (p1, x1) (p2, x2) ->
        let c = String.compare p1 p2 in
        if c <> 0 then c
        else String.compare (xml_to_string x1) (xml_to_string x2)
      ) l
  in
  let result = find_all path_str sample_xml |> sort_results in
  let expected = expected |> sort_results in
  Alcotest.check filter_path_testable ("filter_path " ^ path_str) expected result

let filter_test_cases =
  [
    ("/root/a/b",
     [
       ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"];  });
       ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"] });
     ]);
    ("/root/a@id=\"1\"/b",
     [
       ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"];  });
     ]);
    ("/root/a@id=\"2\"/b",
     [
       ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"] });
     ]);
    ("/root/*/b",
     [
       ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"];  });
       ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"] });
     ]);
    ("/root/nonexistent", []);
    ("/root/a/nonexistent", []);
    ("/**/b",
     [
       ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"];  });
       ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"] });
       ("/root/e/f/b", Element { name = "b"; attrs = []; childs = []; });
     ]);
    (* Tests for wildcard with attributes in find_all *)
    ("/*@id",
     [
       ("/root/a", Element { name = "a"; attrs = [("id", "1")]; childs = [Element { name = "b"; attrs = []; childs = [Data "hello"];  }; Element { name = "c"; attrs = [("val", "test")]; childs = []; }]; });
       ("/root/a", Element { name = "a"; attrs = [("id", "2")]; childs = [Element { name = "d"; attrs = []; childs = [];  }; Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"] }]; });
     ]);
    ("/*@type",
     [
       ("/root/special", Element { name = "special"; attrs = [("type", "magic")]; childs = [Element { name = "child"; attrs = [("id", "s-child")]; childs = []; }]; });
       ("/root/child", Element { name = "child"; attrs = [("type", "magic")]; childs = []; });
     ]);
    (* Tests for multi wildcard with attributes in find_all *)
    ("/**@id",
     [
       ("/root/a", Element { name = "a"; attrs = [("id", "1")]; childs = [Element { name = "b"; attrs = []; childs = [Data "hello"];  }; Element { name = "c"; attrs = [("val", "test")]; childs = []; }]; });
       ("/root/a", Element { name = "a"; attrs = [("id", "2")]; childs = [Element { name = "d"; attrs = []; childs = [];  }; Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"] }]; });
       ("/root/e/child", Element { name = "child"; attrs = [("id", "e-child")]; childs = []; });
       ("/root/special/child", Element { name = "child"; attrs = [("id", "s-child")]; childs = []; });
     ]);
    ("/**@type",
     [
       ("/root/special", Element { name = "special"; attrs = [("type", "magic")]; childs = [Element { name = "child"; attrs = [("id", "s-child")]; childs = []; }]; });
       ("/root/child", Element { name = "child"; attrs = [("type", "magic")]; childs = []; });
     ]);
    (* Edge cases for multi wildcard with attributes *)
    ("/**@type/child",
     [
       ("/root/special/child", Element { name = "child"; attrs = [("id", "s-child")]; childs = []; });
     ]);
    ("/**/child",
     [
       ("/root/e/child", Element { name = "child"; attrs = [("id", "e-child")]; childs = []; });
       ("/root/special/child", Element { name = "child"; attrs = [("id", "s-child")]; childs = []; });
       ("/root/child", Element { name = "child"; attrs = [("type", "magic")]; childs = []; });
     ]);
    ("/**@lang/nonexistent", []);
  ]

let sample_xml_nested =
  Element {
    name = "root";
    attrs = [];
    childs = [
      Element {
        name = "a";
        attrs = [("id", "1")];
        childs = [
          Element { name = "b"; attrs = []; childs = [
              Element { name = "c"; attrs = [("v", "1")]; childs = [] };
              Element { name = "c"; attrs = [("v", "2")]; childs = [] };
            ]; };
        ];
      };
      Element {
        name = "a";
        attrs = [("id", "2")];
        childs = [
          Element { name = "d"; attrs = []; childs = [
              Element { name = "b"; attrs = []; childs = [
                  Element { name = "c"; attrs = [("v", "3")]; childs = [] }
                ]; }
            ]; };
        ];
      };
    ];
  }

let test_find_path_nested path_str expected () =
  let result = find_opt path_str sample_xml_nested in
  Alcotest.check find_path_testable ("find_path_nested " ^ path_str) expected result

let nested_test_cases = [
  ("/root/a/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [
       Element { name = "c"; attrs = [("v", "1")]; childs = [];  };
       Element { name = "c"; attrs = [("v", "2")]; childs = [];  };
     ];  }));
  ("/root/a/d/b", Some("/root/a/d/b", Element { name = "b"; attrs = []; childs = [
       Element { name = "c"; attrs = [("v", "3")]; childs = [];  }
     ];  }));
  ("/root/**/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [
       Element { name = "c"; attrs = [("v", "1")]; childs = [];  };
       Element { name = "c"; attrs = [("v", "2")]; childs = [];  };
     ];  }));
  ("/root/a/b/c[1]", Some("/root/a/b/c", Element { name = "c"; attrs = [("v", "2")]; childs = [];  }));
  ("/root/a[1]/d/b", Some("/root/a/d/b", Element { name = "b"; attrs = []; childs = [
       Element { name = "c"; attrs = [("v", "3")]; childs = [];  }
     ];  }));
]

let test_find_all_nested path_str expected () =
  let sort_results l =
    List.sort (fun (p1, x1) (p2, x2) ->
        let c = String.compare p1 p2 in
        if c <> 0 then c
        else String.compare (xml_to_string x1) (xml_to_string x2)
      ) l
  in
  let result = find_all path_str sample_xml_nested |> sort_results in
  let expected = expected |> sort_results in
  Alcotest.check filter_path_testable ("find_all_nested " ^ path_str) expected result

let nested_filter_test_cases = [
  ("/root/a/b", [
      ("/root/a/b", Element { name = "b"; attrs = []; childs = [
           Element { name = "c"; attrs = [("v", "1")]; childs = [];  };
           Element { name = "c"; attrs = [("v", "2")]; childs = [];  };
         ]; });
    ]);
  ("/root/**/b", [
      ("/root/a/b", Element { name = "b"; attrs = []; childs = [
           Element { name = "c"; attrs = [("v", "1")]; childs = [];  };
           Element { name = "c"; attrs = [("v", "2")]; childs = [];  };
         ]; });
      ("/root/a/d/b", Element { name = "b"; attrs = []; childs = [
           Element { name = "c"; attrs = [("v", "3")]; childs = [];  }
         ]; });
    ]);
  ("/root/**/c", [
      ("/root/a/b/c", Element { name = "c"; attrs = [("v", "1")]; childs = []; });
      ("/root/a/b/c", Element { name = "c"; attrs = [("v", "2")]; childs = []; });
      ("/root/a/d/b/c", Element { name = "c"; attrs = [("v", "3")]; childs = []; });
    ]);
  ("/root/a/b/c", [
      ("/root/a/b/c", Element { name = "c"; attrs = [("v", "1")]; childs = []; });
      ("/root/a/b/c", Element { name = "c"; attrs = [("v", "2")]; childs = []; });
    ]);
  ("/root/a[0]/**/c", [
      ("/root/a/b/c", Element { name = "c"; attrs = [("v", "1")]; childs = []; });
      ("/root/a/b/c", Element { name = "c"; attrs = [("v", "2")]; childs = []; });
    ]);
  ("/root/a[1]/**/c", [
      ("/root/a/d/b/c", Element { name = "c"; attrs = [("v", "3")]; childs = []; });
    ]);
]

let test_find_attr_opt path attr expected tree () =
  let result = find_attr_opt path attr tree in
  Alcotest.(check (option (pair string string))) path expected result

let find_attr_test_cases = [
  ("/root/a/c@val", "val", Some ("/root/a/c", "test"));
  ("/root/a@id", "id", Some ("/root/a", "1"));
  ("/root/nonexistent@attr", "attr", None);
  ("/root/a/b@nonexistent", "nonexistent", None);
  (* Test case for single component path without attribute - this was the bug *)
  ("/root/a", "id", Some ("/root/a", "1"));
  (* Test case for single component path with different attribute *)
  ("/root/a/c", "val", Some ("/root/a/c", "test"));
]

(* Test parser flexibility - paths with and without leading slash *)
let test_parser_flexibility path_str () =
  let result_with_slash = Parser.parse_path ("/" ^ path_str) in
  let result_without_slash = Parser.parse_path path_str in
  match (result_with_slash, result_without_slash) with
  | (Ok with_slash, Ok without_slash) ->
    Alcotest.(check (module struct
                type t = Alsdiff_base.Upath.path
                let equal = Alsdiff_base.Upath.equal_path
                let pp = Alsdiff_base.Upath.pp_path
              end)) ("parser flexibility: " ^ path_str) with_slash without_slash
  | _ ->
    Alcotest.fail ("Both should succeed: " ^ path_str)

let parser_flexibility_test_cases = [
  "a/b/c";
  "root/a/b";
  "Name";
  "element@attr=\"value\"";
  "parent/child[0]/grandchild";
]

let () =
  Alcotest.run "Upath" [
    "find_path",
    List.map (fun (path, expected) ->
        Alcotest.test_case path `Quick (test_find_path path expected)
      )
      test_cases;
    "find_all",
    List.map (fun (path, expected) ->
        Alcotest.test_case path `Quick (test_find_all path expected)
      )
      filter_test_cases;
    "find_path_nested",
    List.map (fun (path, expected) ->
        Alcotest.test_case path `Quick (test_find_path_nested path expected)
      )
      nested_test_cases;
    "find_all_nested",
    List.map (fun (path, expected) ->
        Alcotest.test_case path `Quick (test_find_all_nested path expected)
      )
      nested_filter_test_cases;
    "find_attr_opt",
    List.map (fun (path, attr, expected) ->
        let test_func = test_find_attr_opt path attr expected sample_xml in
        Alcotest.test_case path `Quick test_func
      )
      find_attr_test_cases;
    "parser_flexibility",
    List.map (fun path_str ->
        Alcotest.test_case path_str `Quick (test_parser_flexibility path_str)
      )
      parser_flexibility_test_cases;
  ]
