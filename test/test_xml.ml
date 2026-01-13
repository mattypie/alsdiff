open Alsdiff_base.Xml
open Test_utils.Utils

let test_read_string () =
  let xml_str = "<root><a id=\"1\"><b>hello</b><c val=\"test\"/></a></root>" in
  let parsed = read_string xml_str in

  (* For this test, we'll verify that parsing produces the expected string representation *)
  (* We use string comparison instead of structural equality to avoid circular parent reference issues *)
  let expected_str = "<root><a id=\"1\"><b>hello</b><c val=\"test\"></c></a></root>" in
  let actual_str = xml_to_string parsed in

  Alcotest.(check string) "read_string" expected_str actual_str

let test_get_attr_missing () =
  let xml = Element { name = "Test"; attrs = []; childs = [] } in
  (try ignore (get_attr "Missing" xml); false
   with Xml_error (_, msg) when msg = "Attribute 'Missing' not found" -> true
      | _ -> false)
  |> Alcotest.(check bool) "get_attr missing attribute raises Xml_error" true

let test_get_attr_data_node () =
  let xml = Data "test" in
  (try ignore (get_attr "Id" xml); false
   with Xml_error (_, msg) when msg = "Cannot get attribute from XML Data node" -> true
      | _ -> false)
  |> Alcotest.(check bool) "get_attr on Data node raises Xml_error" true

let test_get_int_attr_invalid () =
  let xml = Element { name = "Test"; attrs = ["Id", "abc"]; childs = [] } in
  (try ignore (get_int_attr "Id" xml); false
   with Xml_error (_, msg) -> String.starts_with ~prefix:"Invalid integer value for attribute 'Id'" msg
      | _ -> false)
  |> Alcotest.(check bool) "get_int_attr with invalid value raises Xml_error" true

let test_get_float_attr_invalid () =
  let xml = Element { name = "Test"; attrs = ["Time", "12.34.56"]; childs = [] } in
  (try ignore (get_float_attr "Time" xml); false
   with Xml_error (_, msg) -> String.starts_with ~prefix:"Invalid float value for attribute 'Time'" msg
      | _ -> false)
  |> Alcotest.(check bool) "get_float_attr with invalid value raises Xml_error" true

let test_get_int64_attr_invalid () =
  let xml = Element { name = "Test"; attrs = ["Date", "invalid"]; childs = [] } in
  (try ignore (get_int64_attr "Date" xml); false
   with Xml_error (_, msg) -> String.starts_with ~prefix:"Invalid int64 value for attribute 'Date'" msg
      | _ -> false)
  |> Alcotest.(check bool) "get_int64_attr with invalid value raises Xml_error" true

let test_successful_operations () =
  let xml = Element {
      name = "Test";
      attrs = ["Id", "123"; "Time", "1.5"; "Date", "12345678901234"];
      childs = []
    } in
  let id = get_int_attr "Id" xml in
  let time = get_float_attr "Time" xml in
  let date = get_int64_attr "Date" xml in
  Alcotest.(check int) "get_int_attr success" 123 id;
  Alcotest.(check (float 0.001)) "get_float_attr success" 1.5 time;
  Alcotest.(check int64) "get_int64_attr success" 12345678901234L date

let () =
  Alcotest.run "Xml" [
    "read_string", [
      Alcotest.test_case "parse XML string" `Quick test_read_string
    ];
    "exception_handling", [
      Alcotest.test_case "get_attr missing attribute" `Quick test_get_attr_missing;
      Alcotest.test_case "get_attr on Data node" `Quick test_get_attr_data_node;
      Alcotest.test_case "get_int_attr invalid value" `Quick test_get_int_attr_invalid;
      Alcotest.test_case "get_float_attr invalid value" `Quick test_get_float_attr_invalid;
      Alcotest.test_case "get_int64_attr invalid value" `Quick test_get_int64_attr_invalid;
      Alcotest.test_case "successful operations" `Quick test_successful_operations;
    ]
  ]
