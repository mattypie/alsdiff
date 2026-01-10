open Alsdiff_output.Text_renderer
open Alsdiff_output.View_model

let test_detail_level_roundtrip () =
  (* Test that detail_level serializes and deserializes correctly *)
  let levels = [DLNone; Summary; Compact; Full] in
  List.iteri (fun i level ->
    let json = detail_level_to_yojson level in
    match detail_level_of_yojson json with
    | Ok parsed ->
        Alcotest.(check bool) (Printf.sprintf "detail_level %d roundtrip" i)
          (level = parsed) true
    | Error msg ->
        Alcotest.failf "detail_level %d failed: %s" i msg
  ) levels

let test_change_breakdown_roundtrip () =
  (* Test change_breakdown record *)
  let breakdown = { added = 5; removed = 3; modified = 2; } in
  let json = change_breakdown_to_yojson breakdown in
  match change_breakdown_of_yojson json with
  | Ok parsed ->
      Alcotest.(check int) "breakdown added" breakdown.added parsed.added;
      Alcotest.(check int) "breakdown removed" breakdown.removed parsed.removed;
      Alcotest.(check int) "breakdown modified" breakdown.modified parsed.modified
  | Error msg ->
      Alcotest.failf "change_breakdown failed: %s" msg

let test_per_change_override_roundtrip () =
  (* Test per_change_override *)
  let override = {
    added = Some Full;
    removed = Some Summary;
    modified = Some Compact;
    unchanged = Some DLNone;
  } in
  let json = per_change_override_to_yojson override in
  match per_change_override_of_yojson json with
  | Ok parsed ->
      (* Check fields match *)
      (match override.added, parsed.added with
       | Some a, Some b -> Alcotest.(check bool) "override added" (a = b) true
       | None, None -> ()
       | _ -> Alcotest.failf "override added mismatch");
      (match override.removed, parsed.removed with
       | Some a, Some b -> Alcotest.(check bool) "override removed" (a = b) true
       | None, None -> ()
       | _ -> Alcotest.failf "override removed mismatch")
  | Error msg ->
      Alcotest.failf "per_change_override failed: %s" msg

let test_detail_config_roundtrip () =
  (* Test detail_config with verbose preset *)
  let test_cfg = verbose in
  let json = detail_config_to_yojson test_cfg in
  match detail_config_of_yojson json with
  | Ok parsed ->
      Alcotest.(check bool) "config added" (test_cfg.added = parsed.added) true;
      Alcotest.(check bool) "config removed" (test_cfg.removed = parsed.removed) true;
      Alcotest.(check bool) "config modified" (test_cfg.modified = parsed.modified) true;
      Alcotest.(check bool) "config unchanged" (test_cfg.unchanged = parsed.unchanged) true;
      Alcotest.(check int) "type_overrides length"
        (List.length test_cfg.type_overrides) (List.length parsed.type_overrides);
      Alcotest.(check bool) "show_unchanged_fields"
        test_cfg.show_unchanged_fields parsed.show_unchanged_fields;
      Alcotest.(check string) "prefix_added" test_cfg.prefix_added parsed.prefix_added
  | Error msg ->
      Alcotest.failf "detail_config failed: %s" msg

let test_detail_config_with_overrides () =
  (* Test custom config with type_overrides *)
  let custom_cfg = {
    compact with
    type_overrides = [
      (DTDevice, {
        added = Some Full;
        removed = Some Summary;
        modified = Some Compact;
        unchanged = Some DLNone;
      });
    ];
    max_collection_items = Some 100;
    show_unchanged_fields = true;
  } in
  let json = detail_config_to_yojson custom_cfg in
  match detail_config_of_yojson json with
  | Ok parsed ->
      Alcotest.(check int) "type_overrides length" 1 (List.length parsed.type_overrides);
      let device_override = List.assoc_opt DTDevice parsed.type_overrides in
      Alcotest.(check bool) "device override exists"
        (Option.is_some device_override) true;
      (match device_override with
       | Some ov ->
           (match ov.added, Some Full with
            | Some a, Some b -> Alcotest.(check bool) "device override added" (a = b) true
            | _ -> Alcotest.failf "device override added mismatch")
       | None -> Alcotest.failf "device override not found");
      Alcotest.(check bool) "show_unchanged_fields" true parsed.show_unchanged_fields
  | Error msg ->
      Alcotest.failf "detail_config with overrides failed: %s" msg

let test_json_to_string () =
  (* Test converting to JSON string and back *)
  let test_cfg = compact in
  let json = detail_config_to_yojson test_cfg in
  let json_str = Yojson.Safe.to_string json in
  let parsed_json = Yojson.Safe.from_string json_str in
  match detail_config_of_yojson parsed_json with
  | Ok roundtrip_cfg ->
      Alcotest.(check bool) "roundtrip added" (test_cfg.added = roundtrip_cfg.added) true;
      Alcotest.(check bool) "roundtrip removed" (test_cfg.removed = roundtrip_cfg.removed) true
  | Error msg -> Alcotest.failf "roundtrip failed: %s" msg

let tests = [
  "detail_level roundtrip", `Quick, test_detail_level_roundtrip;
  "change_breakdown roundtrip", `Quick, test_change_breakdown_roundtrip;
  "per_change_override roundtrip", `Quick, test_per_change_override_roundtrip;
  "detail_config roundtrip", `Quick, test_detail_config_roundtrip;
  "detail_config with overrides", `Quick, test_detail_config_with_overrides;
  "json to string", `Quick, test_json_to_string;
]

let () =
  Alcotest.run "Detail config JSON tests" [
    "json_tests", tests
  ]
