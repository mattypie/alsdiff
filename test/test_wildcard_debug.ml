open Alsdiff_lib.Xml
open Alsdiff_lib.Upath

let sample_xml =
  Element {
    name = "root";
    attrs = [];
    childs = [
      Element {
        name = "parent1";
        attrs = [];
        childs = [
          Element { name = "child"; attrs = []; childs = [Data "direct_child"] }
        ]
      };
      Element {
        name = "parent2";
        attrs = [];
        childs = [
          Element {
            name = "intermediate";
            attrs = [];
            childs = [
              Element { name = "child"; attrs = []; childs = [Data "deep_child"] }
            ]
          }
        ]
      }
    ]
  }

let () =
  print_endline "Testing individual steps:";

  (* Test just matching direct children of root *)
  Printf.printf "Direct children of root:\n";
  let result1 = find_all "/root/*" sample_xml in
  (match result1 with
   | [] -> Printf.printf "  No matches found\n"
   | matches ->
     List.iter (fun (path, _) -> Printf.printf "  Found: %s\n" path) matches);

  Printf.printf "\n";

  (* Test the full path with single wildcard *)
  Printf.printf "Full path /root/*/child:\n";
  let result2 = find_all "/root/*/child" sample_xml in
  (match result2 with
   | [] -> Printf.printf "  No matches found\n"
   | matches ->
     List.iter (fun (path, _) -> Printf.printf "  Found: %s\n" path) matches);