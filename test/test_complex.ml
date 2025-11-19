open Alsdiff_base.Xml
open Alsdiff_base.Upath
open Test_utils.Utils

let complex_xml =
  Element {
    name = "archive";
    attrs = [];
    childs = [
      Element {
        name = "library";
        attrs = [("name", "music")];
        childs = [
          Element {
            name = "section";
            attrs = [("id", "A")];
            childs = [
              Element {
                name = "artist";
                attrs = [("name", "Artist1")];
                childs = [
                  Element {
                    name = "album";
                    attrs = [("title", "Album1.1"); ("year", "2020")];
                    childs = [
                      Element { name = "track"; attrs = [("no", "1")]; childs = [Data "Track1"] };
                      Element { name = "track"; attrs = [("no", "2"); ("feat", "Artist2")]; childs = [Data "Track2"] };
                    ];
                  };
                  Element {
                    name = "album";
                    attrs = [("title", "Album1.2"); ("year", "2022")];
                    childs = [
                      Element { name = "track"; attrs = [("no", "1")]; childs = [Data "TrackA"] };
                    ];
                  }
                ];
              }
            ];
          };
          Element {
            name = "section";
            attrs = [("id", "B")];
            childs = [
              Element {
                name = "artist";
                attrs = [("name", "Artist2")];
                childs = [
                  Element {
                    name = "album";
                    attrs = [("title", "Album2.1"); ("year", "2021")];
                    childs = [
                      Element { name = "track"; attrs = [("no", "1")]; childs = [Data "Single"] };
                    ];
                  }
                ];
              };
              Element {
                name = "genre";
                attrs = [("name", "electronic")];
                childs = [
                  Element {
                    name = "artist";
                    attrs = [("name", "Artist3")];
                    childs = [
                      Element {
                        name = "album";
                        attrs = [("title", "Album3.1"); ("year", "2023")];
                        childs = [
                          Element { name = "track"; attrs = [("no", "1")]; childs = [Data "E-Track1"] };
                          Element { name = "track"; attrs = [("no", "2")]; childs = [Data "E-Track2"] };
                        ];
                      }
                    ];
                  }
                ];
              }
            ];
          };
        ]
      };
      Element {
        name = "library";
        attrs = [("name", "pictures")];
        childs = [
          Element {
            name = "album";
            attrs = [("title", "Holidays"); ("year", "2022")];
            childs = [
              Element { name = "photo"; attrs = [("year", "2022"); ("location", "Beach")]; childs = [] };
              Element { name = "photo"; attrs = [("year", "2023"); ("location", "Mountain")]; childs = [] };
            ];
          };
        ];
      };
    ]
  }

let find_path_testable = Alcotest.(option (pair string xml_testable))

let test_find path_str expected () =
  let result = find_opt path_str complex_xml in
  Alcotest.check find_path_testable ("find " ^ path_str) expected result

let find_tests = [
  ("/archive/library@name=\"music\"/section@id=\"A\"/artist/album@year=\"2022\"/track",
    Some ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "1")]; childs = [Data "TrackA"] }));
  ("/**/album@year=\"2021\"/track",
    Some ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "1")]; childs = [Data "Single"] }));
  ("/archive/library[1]/*/photo[0]",
    Some ("/archive/library/album/photo", Element { name = "photo"; attrs = [("year", "2022"); ("location", "Beach")]; childs = [];  }));
  ("/**/track@feat=\"Artist2\"",
    Some ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "2"); ("feat", "Artist2")]; childs = [Data "Track2"] }));
  ("/archive/library@name=\"audiobooks\"/**/track", None);
  ("/archive/library/section[1]/artist/album/track[0]",
    Some ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "1")]; childs = [Data "Single"] }));
  ("/**/artist[2]/album/track[1]",
    Some ("/archive/library/section/genre/artist/album/track", Element { name = "track"; attrs = [("no", "2")]; childs = [Data "E-Track2"] }));
  ("/archive/library@name=\"music\"/section[0]/artist@name=\"Artist1\"/album[1]/track[0]",
    Some ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "1")]; childs = [Data "TrackA"] }));
]

let find_all_testable = Alcotest.(list (pair string xml_testable))

let test_find_all path_str expected () =
  let sort_results l =
    List.sort (fun (p1, x1) (p2, x2) ->
      let c = String.compare p1 p2 in
      if c <> 0 then c
      else String.compare (xml_to_string x1) (xml_to_string x2)
    ) l
  in
  let result = find_all path_str complex_xml |> sort_results in
  let expected = expected |> sort_results in
  Alcotest.check find_all_testable ("find_all " ^ path_str) expected result

let find_all_tests = [
  ("/**/track", [
    ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "1")]; childs = [Data "Track1"] });
    ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "2"); ("feat", "Artist2")]; childs = [Data "Track2"] });
    ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "1")]; childs = [Data "TrackA"] });
    ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "1")]; childs = [Data "Single"] });
    ("/archive/library/section/genre/artist/album/track", Element { name = "track"; attrs = [("no", "1")]; childs = [Data "E-Track1"] });
    ("/archive/library/section/genre/artist/album/track", Element { name = "track"; attrs = [("no", "2")]; childs = [Data "E-Track2"] });
  ]);
  ("/**/album@year=\"2022\"", [
    ("/archive/library/album", Element { name = "album"; attrs = [("title", "Holidays"); ("year", "2022")]; childs = [
      Element { name = "photo"; attrs = [("year", "2022"); ("location", "Beach")]; childs = [] };
      Element { name = "photo"; attrs = [("year", "2023"); ("location", "Mountain")]; childs = [] };
    ]; });
    ("/archive/library/section/artist/album", Element { name = "album"; attrs = [("title", "Album1.2"); ("year", "2022")]; childs = [Element { name = "track"; attrs = [("no", "1")]; childs = [Data "TrackA"] }]; });
  ]);
  ("/archive/library@name=\"music\"/**/artist", [
    ("/archive/library/section/artist", Element { name = "artist"; attrs = [("name", "Artist1")]; childs = [
        Element { name = "album"; attrs = [("title", "Album1.1"); ("year", "2020")]; childs = [
            Element { name = "track"; attrs = [("no", "1")]; childs = [Data "Track1"] };
            Element { name = "track"; attrs = [("no", "2"); ("feat", "Artist2")]; childs = [Data "Track2"] };
        ] };
        Element { name = "album"; attrs = [("title", "Album1.2"); ("year", "2022")]; childs = [
            Element { name = "track"; attrs = [("no", "1")]; childs = [Data "TrackA"] };
        ] };
    ]; });
    ("/archive/library/section/artist", Element { name = "artist"; attrs = [("name", "Artist2")]; childs = [
        Element { name = "album"; attrs = [("title", "Album2.1"); ("year", "2021")]; childs = [
            Element { name = "track"; attrs = [("no", "1")]; childs = [Data "Single"] };
        ] }
    ]; });
    ("/archive/library/section/genre/artist", Element { name = "artist"; attrs = [("name", "Artist3")]; childs = [
        Element { name = "album"; attrs = [("title", "Album3.1"); ("year", "2023")]; childs = [
            Element { name = "track"; attrs = [("no", "1")]; childs = [Data "E-Track1"] };
            Element { name = "track"; attrs = [("no", "2")]; childs = [Data "E-Track2"] };
        ] }
    ]; });
  ]);
  ("/**/artist@name=\"Artist1\"/**/track", [
    ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "1")]; childs = [Data "Track1"] });
    ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "2"); ("feat", "Artist2")]; childs = [Data "Track2"] });
    ("/archive/library/section/artist/album/track", Element { name = "track"; attrs = [("no", "1")]; childs = [Data "TrackA"] });
  ]);
]

let () =
  Alcotest.run "UpathComplex" [
    "find_complex",
    List.map (fun (path, expected) ->
      Alcotest.test_case path `Quick (test_find path expected)
    ) find_tests;
    "find_all_complex",
    List.map (fun (path, expected) ->
      Alcotest.test_case path `Quick (test_find_all path expected)
    ) find_all_tests;
  ]
