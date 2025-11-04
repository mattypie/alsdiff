open Alsdiff_base

let test_regex_match_xml_path =
  (* Try different paths to work with both dune exec and dune runtest *)
  if Sys.file_exists "test/regex_match.xml" then
    "test/regex_match.xml"
  else
    "regex_match.xml"

let test_load_regex_match_xml () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Verify we can load the file successfully *)
  Alcotest.(check string) "xml root name" "Root"
    (match xml with
     | Xml.Element { name = n; _ } -> n
     | _ -> failwith "Expected Element, got Data");

  (* Count the number of MacroControls elements *)
  let macro_controls = Upath.find_all "/**/'MacroControls.*'" xml in
  Alcotest.(check (int)) "macro controls count" 16 (List.length macro_controls)

let test_regex_pattern_all_controls () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Test regex pattern to match all MacroControls elements *)
  let results = Upath.find_all "/**/'MacroControls\\.[0-9]+'" xml in
  Alcotest.(check (int)) "all macro controls matched" 16 (List.length results);

  (* Verify specific elements are matched *)
  let paths = List.map fst results in
  Alcotest.(check bool) "MacroControls.0 found" true
    (List.mem "/Root/MacroControls.0" paths);
  Alcotest.(check bool) "MacroControls.15 found" true
    (List.mem "/Root/MacroControls.15" paths)

let test_regex_pattern_range_0_to_3 () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Test regex pattern to match MacroControls.0 through MacroControls.3 *)
  let results = Upath.find_all "/**/'MacroControls\\.[0-3]$'" xml in
  Alcotest.(check (int)) "macro controls 0-3 matched" 4 (List.length results);

  (* Verify correct elements are matched *)
  let paths = List.map fst results in
  Alcotest.(check bool) "MacroControls.0 found" true
    (List.mem "/Root/MacroControls.0" paths);
  Alcotest.(check bool) "MacroControls.3 found" true
    (List.mem "/Root/MacroControls.3" paths);
  Alcotest.(check bool) "MacroControls.4 not found" false
    (List.mem "/Root/MacroControls.4" paths)

let test_regex_pattern_range_1_to_5 () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Test regex pattern to match MacroControls.1 through MacroControls.5 *)
  let results = Upath.find_all "/**/'MacroControls\\.[1-5]$'" xml in
  Alcotest.(check (int)) "macro controls 1-5 matched" 5 (List.length results);

  (* Verify correct elements are matched *)
  let paths = List.map fst results in
  Alcotest.(check bool) "MacroControls.1 found" true
    (List.mem "/Root/MacroControls.1" paths);
  Alcotest.(check bool) "MacroControls.5 found" true
    (List.mem "/Root/MacroControls.5" paths);
  Alcotest.(check bool) "MacroControls.0 not found" false
    (List.mem "/Root/MacroControls.0" paths);
  Alcotest.(check bool) "MacroControls.6 not found" false
    (List.mem "/Root/MacroControls.6" paths)

let test_regex_pattern_single_digit () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Test regex pattern to match single digits only (0-9) *)
  let results = Upath.find_all "/**/'MacroControls\\.[0-9]$'" xml in
  Alcotest.(check (int)) "single digit macro controls matched" 10 (List.length results);

  (* Verify only single digit elements are matched *)
  let paths = List.map fst results in
  Alcotest.(check bool) "MacroControls.9 found" true
    (List.mem "/Root/MacroControls.9" paths);
  Alcotest.(check bool) "MacroControls.10 not found" false
    (List.mem "/Root/MacroControls.10" paths)

let test_regex_pattern_double_digit () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Test regex pattern to match double digits only (10-15) *)
  let results = Upath.find_all "/**/'MacroControls\\.[1][0-5]'" xml in
  Alcotest.(check (int)) "double digit macro controls matched" 6 (List.length results);

  (* Verify only double digit elements are matched *)
  let paths = List.map fst results in
  Alcotest.(check bool) "MacroControls.10 found" true
    (List.mem "/Root/MacroControls.10" paths);
  Alcotest.(check bool) "MacroControls.15 found" true
    (List.mem "/Root/MacroControls.15" paths);
  Alcotest.(check bool) "MacroControls.9 not found" false
    (List.mem "/Root/MacroControls.9" paths)

let test_raw_string_matching () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Test raw string matching (should not use regex) *)
  let results = Upath.find_all "/**/MacroControls.0" xml in
  Alcotest.(check (int)) "exact MacroControls.0 match" 1 (List.length results);

  (* Test non-existent exact match *)
  let no_results = Upath.find_all "/**/MacroControls.999" xml in
  Alcotest.(check (int)) "non-existent exact match" 0 (List.length no_results)

let test_regex_wildcard_pattern () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Test regex pattern with wildcard *)
  let results = Upath.find_all "/**/'MacroControls\\..*'" xml in
  Alcotest.(check (int)) "wildcard macro controls matched" 16 (List.length results)

let test_regex_plus_pattern () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Test regex pattern with + quantifier *)
  let results = Upath.find_all "/**/'MacroControls\\.[0-9]+'" xml in
  Alcotest.(check (int)) "plus quantifier macro controls matched" 16 (List.length results)

let test_regex_with_attributes () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Test regex pattern with attribute matching - look for Manual child elements with Value=0 *)
  let results = Upath.find_all "/**/'MacroControls\\.[0-3]$'/Manual@Value=0" xml in
  Alcotest.(check (int)) "macro controls with Manual Value=0" 3 (List.length results)

let test_regex_index_access () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Test regex pattern with index access *)
  let result = Upath.find_opt "/**/'MacroControls\\.[0-9]'[2]" xml in
  Alcotest.(check bool) "regex with index access found" true (Option.is_some result);

  (* Verify it's the third element (index 2) *)
  (match result with
   | Some (path, _) ->
     Alcotest.(check string) "correct element at index 2" "/Root/MacroControls.2" path
   | None -> Alcotest.fail "Expected to find element at index 2")

let test_regex_no_matches () =
  (* Load the regex test XML file *)
  let xml = Xml.read_file test_regex_match_xml_path in

  (* Test regex pattern that should not match anything *)
  let results = Upath.find_all "/**/'MacroControls\\.[A-Z]'" xml in
  Alcotest.(check (int)) "no letter macro controls matched" 0 (List.length results);

  (* Test regex pattern for non-existent range *)
  let no_results = Upath.find_all "/**/'MacroControls\\.[9][9-9]'" xml in
  Alcotest.(check (int)) "no 99+ matched" 0 (List.length no_results)

let () =
  Alcotest.run "RegexMatch" [
    "regex_matching", [
      Alcotest.test_case "load regex match XML" `Quick test_load_regex_match_xml;
      Alcotest.test_case "regex pattern all controls" `Quick test_regex_pattern_all_controls;
      Alcotest.test_case "regex pattern range 0-3" `Quick test_regex_pattern_range_0_to_3;
      Alcotest.test_case "regex pattern range 1-5" `Quick test_regex_pattern_range_1_to_5;
      Alcotest.test_case "regex pattern single digit" `Quick test_regex_pattern_single_digit;
      Alcotest.test_case "regex pattern double digit" `Quick test_regex_pattern_double_digit;
      Alcotest.test_case "raw string matching" `Quick test_raw_string_matching;
      Alcotest.test_case "regex wildcard pattern" `Quick test_regex_wildcard_pattern;
      Alcotest.test_case "regex plus pattern" `Quick test_regex_plus_pattern;
      Alcotest.test_case "regex with attributes" `Quick test_regex_with_attributes;
      Alcotest.test_case "regex index access" `Quick test_regex_index_access;
      Alcotest.test_case "regex no matches" `Quick test_regex_no_matches;
    ]
  ]
