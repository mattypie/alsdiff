open Alsdiff_base.Xml

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
        ]
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
          }
        ]
      };
      Element {
        name = "e";
        attrs = [];
        childs = [
          Element {
            name = "f";
            attrs = [];
            childs = [
              Element { name = "b"; attrs = []; childs = [] }
            ]
          }
        ]
      }
    ]
  }

let () =
  print_endline "Testing path finding:";

  (* Test case 1: Valid path that exists *)
  Printf.printf "Testing /root/a/b: ";
  let result1 = Alsdiff_base.Upath.find_opt "/root/a/b" sample_xml in
  (match result1 with
   | Some (p, _) -> Printf.printf "Found path: %s\n" p
   | None -> Printf.printf "Path not found\n");

  (* Test case 2: Invalid path (wrong root) *)
  Printf.printf "Testing /a/b: ";
  let result2 = Alsdiff_base.Upath.find_opt "/a/b" sample_xml in
  (match result2 with
   | Some (p, _) -> Printf.printf "Found path: %s\n" p
   | None -> Printf.printf "Path not found\n");

  (* Test case 3: Non-existent path *)
  Printf.printf "Testing /xyz/a/b: ";
  let result3 = Alsdiff_base.Upath.find_opt "/xyz/a/b" sample_xml in
  match result3 with
  | Some (p, _) -> Printf.printf "Found path: %s\n" p
  | None -> Printf.printf "Path not found\n"
