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

let test_json_schema_generation () =
  (* Test JSON schema generation produces valid structure *)
  let schema = detail_config_json_schema () in
  match schema with
  | `Assoc fields ->
      Alcotest.(check bool) "has $schema"
        (List.mem_assoc "$schema" fields) true;
      Alcotest.(check bool) "has title"
        (List.mem_assoc "title" fields) true;
      Alcotest.(check bool) "has description"
        (List.mem_assoc "description" fields) true;
      Alcotest.(check bool) "has $defs"
        (List.mem_assoc "$defs" fields) true;
      Alcotest.(check bool) "has type"
        (List.mem_assoc "type" fields) true;
      Alcotest.(check bool) "has properties"
        (List.mem_assoc "properties" fields) true;
      Alcotest.(check bool) "has required"
        (List.mem_assoc "required" fields) true;
      (* Check $defs contains expected types *)
      (match List.assoc_opt "$defs" fields with
       | Some (`Assoc defs) ->
           Alcotest.(check bool) "has domain_type def"
             (List.mem_assoc "domain_type" defs) true;
           Alcotest.(check bool) "has detail_level def"
             (List.mem_assoc "detail_level" defs) true;
           Alcotest.(check bool) "has per_change_override def"
             (List.mem_assoc "per_change_override" defs) true;
           Alcotest.(check bool) "has note_display_style def"
             (List.mem_assoc "note_display_style" defs) true
       | _ -> Alcotest.failf "Expected $defs to be an object")
  | _ -> Alcotest.failf "Expected schema to be an object"

let test_schema_to_string () =
  (* Test schema to string produces valid JSON *)
  let schema_str = detail_config_schema_to_string () in
  Alcotest.(check bool) "non-empty schema" (String.length schema_str > 100) true;
  (* Verify it's valid JSON by parsing it *)
  let _ = Yojson.Basic.from_string schema_str in
  (* Verify expected content by checking the parsed JSON *)
  let schema = Yojson.Basic.from_string schema_str in
  match schema with
  | `Assoc fields ->
      Alcotest.(check bool) "schema_str has $schema"
        (List.mem_assoc "$schema" fields) true;
      Alcotest.(check bool) "schema_str has title"
        (List.mem_assoc "title" fields) true
  | _ -> Alcotest.failf "Expected schema to be an object"

(* ==================== Validation Tests ==================== *)

let test_validate_valid_config () =
  (* Test that a valid config passes validation.
     Note: We need to filter out null values since ppx_deriving_yojson serializes
     None as null, but the JSON schema doesn't allow null for optional fields.
     In practice, users should omit optional fields rather than set them to null. *)
  let valid_json = detail_config_to_yojson compact in
  let json_basic = Yojson.Safe.to_basic valid_json in
  (* Remove null fields to make it valid for schema validation *)
  let filtered_json = match json_basic with
    | `Assoc fields ->
        `Assoc (List.filter (fun (_, v) -> v <> `Null) fields)
    | other -> other
  in
  match validate_config_json filtered_json with
  | Ok () -> ()
  | Error err -> Alcotest.failf "Valid config should pass validation: %s" err.details

let test_validate_invalid_type () =
  (* Test that wrong type is rejected - using wrong format for detail_level *)
  let invalid_json = `Assoc [
    ("added", `String "Full");  (* Wrong - should be ["Full"] *)
    ("removed", `List [`String "Full"]);
    ("modified", `List [`String "Full"]);
    ("unchanged", `List [`String "DLNone"]);
    ("type_overrides", `List []);
    ("show_unchanged_fields", `Bool false);
    ("prefix_added", `String "+");
    ("prefix_removed", `String "-");
    ("prefix_modified", `String "*");
    ("prefix_unchanged", `String "");
    ("note_name_style", `List [`String "Sharp"]);
  ] in
  match validate_config_json invalid_json with
  | Ok () -> Alcotest.failf "Invalid format should fail validation"
  | Error _ -> ()  (* Expected *)

let test_validate_missing_required () =
  (* Test that missing required field is rejected *)
  let invalid_json = `Assoc [
    (* Missing "added" field *)
    ("removed", `List [`String "Full"]);
    ("modified", `List [`String "Full"]);
    ("unchanged", `List [`String "DLNone"]);
    ("type_overrides", `List []);
    ("show_unchanged_fields", `Bool false);
    ("prefix_added", `String "+");
    ("prefix_removed", `String "-");
    ("prefix_modified", `String "*");
    ("prefix_unchanged", `String "");
    ("note_name_style", `List [`String "Sharp"]);
  ] in
  match validate_config_json invalid_json with
  | Ok () -> Alcotest.failf "Missing required field should fail validation"
  | Error _ -> ()  (* Expected *)

let test_validate_unknown_field () =
  (* Test that unknown fields are rejected (additionalProperties: false) *)
  let valid_config = detail_config_to_yojson compact in
  let json_basic = Yojson.Safe.to_basic valid_config in
  let with_extra = match json_basic with
    | `Assoc fields -> `Assoc (("unknown_field", `String "value") :: fields)
    | _ -> Alcotest.failf "Expected object"
  in
  match validate_config_json with_extra with
  | Ok () -> Alcotest.failf "Unknown field should fail validation with additionalProperties: false"
  | Error _ -> ()  (* Expected *)

let test_validate_config_string () =
  (* Test string validation.
     Note: We need to filter out null values since ppx_deriving_yojson serializes
     None as null, but the JSON schema doesn't allow null for optional fields. *)
  let json_value = detail_config_to_yojson compact in
  let json_basic = Yojson.Safe.to_basic json_value in
  (* Remove null fields *)
  let filtered_json = match json_basic with
    | `Assoc fields ->
        `Assoc (List.filter (fun (_, v) -> v <> `Null) fields)
    | other -> other
  in
  let json_str = Yojson.Basic.to_string filtered_json in
  match validate_config_string json_str with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "Valid JSON string should pass: %s" msg

let test_validate_invalid_json_string () =
  (* Test invalid JSON string *)
  match validate_config_string "{ invalid json }" with
  | Ok () -> Alcotest.failf "Invalid JSON should fail"
  | Error _ -> ()  (* Expected *)

let tests = [
  "detail_level roundtrip", `Quick, test_detail_level_roundtrip;
  "change_breakdown roundtrip", `Quick, test_change_breakdown_roundtrip;
  "per_change_override roundtrip", `Quick, test_per_change_override_roundtrip;
  "detail_config roundtrip", `Quick, test_detail_config_roundtrip;
  "detail_config with overrides", `Quick, test_detail_config_with_overrides;
  "json to string", `Quick, test_json_to_string;
  "json schema generation", `Quick, test_json_schema_generation;
  "schema to string", `Quick, test_schema_to_string;
  "validate valid config", `Quick, test_validate_valid_config;
  "validate invalid type", `Quick, test_validate_invalid_type;
  "validate missing required", `Quick, test_validate_missing_required;
  "validate unknown field", `Quick, test_validate_unknown_field;
  "validate config string", `Quick, test_validate_config_string;
  "validate invalid json string", `Quick, test_validate_invalid_json_string;
]

let () =
  Alcotest.run "Detail config JSON tests" [
    "json_tests", tests
  ]
