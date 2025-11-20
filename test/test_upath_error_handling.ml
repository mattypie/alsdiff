open Alsdiff_base.Xml
open Alsdiff_base.Upath

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
          Element { name = "b"; attrs = []; childs = [Data "world"] };
        ];
      };
      Element {
        name = "text_container";
        attrs = [];
        childs = [Data "some text"];
      };
    ];
  }

(* Helper to assert that a specific exception is raised *)
let check_raises_path_not_found msg path_str f =
  try
    ignore (f ());
    Alcotest.fail (msg ^ ": Expected Path_not_found exception")
  with
  | Path_not_found { full_path; _ } ->
    Alcotest.(check string) (msg ^ ": full_path") path_str full_path
  | e ->
    Alcotest.fail (msg ^ ": Raised unexpected exception: " ^ Printexc.to_string e)

let check_raises_index_out_of_bounds msg path_str idx f =
  try
    ignore (f ());
    Alcotest.fail (msg ^ ": Expected Index_out_of_bounds exception")
  with
  | Index_out_of_bounds { full_path; index; _ } ->
    Alcotest.(check string) (msg ^ ": full_path") path_str full_path;
    Alcotest.(check int) (msg ^ ": index") idx index
  | e ->
    Alcotest.fail (msg ^ ": Raised unexpected exception: " ^ Printexc.to_string e)

let test_strict_mode_errors () =
  (* 1. No matching child elements found *)
  check_raises_path_not_found "Non-existent child" "/root/nonexistent" (fun () ->
    find_strict true "/root/nonexistent" sample_xml
  );

  (* 2. No elements matched wildcard pattern *)
  check_raises_path_not_found "Wildcard no match" "/root/*@id=\"99\"" (fun () ->
    find_strict true "/root/*@id=\"99\"" sample_xml
  );

  (* 3. No elements matched multi-wildcard pattern *)
  check_raises_path_not_found "Multi-wildcard no match" "/root/**@id=\"99\"" (fun () ->
    find_strict true "/root/**@id=\"99\"" sample_xml
  );

  (* 4. Index out of bounds *)
  check_raises_index_out_of_bounds "Index out of bounds" "/root/a[5]" 5 (fun () ->
    find_strict true "/root/a[5]" sample_xml
  );

  (* 5. Navigate into text node container (should succeed, not fail) *)
  let result = find_strict true "/root/text_container/." sample_xml in
  Alcotest.(check string) "Into text node container path" "/root/text_container" (fst result);

  (* 6. Search children of text node container (should fail because no matching children) *)
  check_raises_path_not_found "Children of text node container" "/root/text_container/child" (fun () ->
    find_strict true "/root/text_container/child" sample_xml
  );

  (* 7. Traverse descendants of text node container (should fail because no matching descendants) *)
  check_raises_path_not_found "Descendants of text node container" "/root/text_container/**/child" (fun () ->
    find_strict true "/root/text_container/**/child" sample_xml
  );

  (* 8. Cannot navigate to parent from root *)
  check_raises_path_not_found "Parent from root" "/.." (fun () ->
    find_strict true "/.." sample_xml
  );

  (* 9. Cannot navigate to parent (already at root) - slightly different context *)
  (* This is hard to trigger with find_strict on a simple tree because we usually start at root.
     But if we go down and back up too far, it might trigger if the logic allows.
     Actually, the current implementation of ParentNode checks if parent is None.
     Let's try to go up from root's child to root, then up again. *)
  check_raises_path_not_found "Parent beyond root" "/root/../.." (fun () ->
    find_strict true "/root/../.." sample_xml
  );

  (* 10. Attribute mismatch *)
  check_raises_path_not_found "Attribute mismatch" "/root/a@id=\"99\"" (fun () ->
    find_strict true "/root/a@id=\"99\"" sample_xml
  )

let test_detailed_exception_fields () =
  (* Verify Path_not_found fields *)
  try
    ignore (find_strict true "/root/a/nonexistent" sample_xml);
    Alcotest.fail "Expected Path_not_found"
  with
  | Path_not_found { full_path; matched_prefix; failed_component; cause } ->
    Alcotest.(check string) "full_path" "/root/a/nonexistent" full_path;
    Alcotest.(check (list string)) "matched_prefix" ["a"; "root"] matched_prefix;
    (match failed_component with
     | Tag (Raw "nonexistent", []) -> ()
     | _ -> Alcotest.fail "failed_component should be Tag(nonexistent)");
    Alcotest.(check string) "cause" "No matching child elements found" cause
  | _ -> Alcotest.fail "Unexpected exception"

let test_index_out_of_bounds_fields () =
  (* Verify Index_out_of_bounds fields *)
  try
    ignore (find_strict true "/root/a[5]" sample_xml);
    Alcotest.fail "Expected Index_out_of_bounds"
  with
  | Index_out_of_bounds { full_path; index; collection_length; component } ->
    Alcotest.(check string) "full_path" "/root/a[5]" full_path;
    Alcotest.(check int) "index" 5 index;
    Alcotest.(check int) "collection_length" 2 collection_length; (* There are 2 'a' elements *)
    (match component with
     | Index (5, Some (Raw "a")) -> ()
     | _ -> Alcotest.fail "component should be Index(5, a)")
  | _ -> Alcotest.fail "Unexpected exception"

let test_partial_success () =
  (* Scenario: find_all_strict should NOT raise if at least one path matches,
     even if some branches don't match in a wildcard search.
     However, find_strict (single result) might raise if it can't find ANY.

     Let's test find_all_strict with a wildcard that matches some but not all.
     Actually, find_all_seq_0 logic is:
     - strict=true: raises if NO matches found at a step where matches are expected?
     - Wait, looking at upath.ml:
       For Tag/Wildcard: "if strict && Seq.is_empty matched && not (Seq.is_empty states) then raise"

     So if we have multiple states (branches), and SOME match, 'matched' will not be empty.
     So it should NOT raise.
  *)

  (* /root/* matches 'a' (x2), 'text_container' (x1).
     /root/*/b matches 'b' inside 'a's. 'text_container' has no 'b'.

     When processing 'b':
     - states input: [a(1), a(2), text_container]
     - processing 'b':
       - a(1) -> has b -> match
       - a(2) -> has b -> match
       - text_container -> no b -> no match
     - matched result: [b(1), b(2)]
     - Seq.is_empty matched is FALSE.
     - So it should NOT raise.
  *)
  let results = find_all_strict true "/root/*/b" sample_xml in
  Alcotest.(check int) "Partial success count" 2 (List.length results)

let test_non_strict_mode () =
  (* Should raise standard Not_found or return empty list/None instead of detailed exceptions *)

  (* find_strict false -> raises Not_found *)
  Alcotest.check_raises "Expected Not_found exception" Not_found (fun () ->
    ignore (find_strict false "/root/nonexistent" sample_xml)
  );

  (* find_all_strict false -> returns empty list *)
  let results = find_all_strict false "/root/nonexistent" sample_xml in
  Alcotest.(check int) "Empty list for non-strict" 0 (List.length results)

let () =
  Alcotest.run "Upath Error Handling" [
    "strict_mode_errors", [
      Alcotest.test_case "Test strict mode errors" `Quick test_strict_mode_errors;
    ];
    "detailed_fields", [
      Alcotest.test_case "Detailed Path_not_found fields" `Quick test_detailed_exception_fields;
      Alcotest.test_case "Detailed Index_out_of_bounds fields" `Quick test_index_out_of_bounds_fields;
    ];
    "partial_success", [
      Alcotest.test_case "Partial success scenarios" `Quick test_partial_success;
    ];
    "non_strict_mode", [
      Alcotest.test_case "Test non-strict mode behavior" `Quick test_non_strict_mode;
    ];
  ]
