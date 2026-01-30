open Alsdiff_output.Text_renderer
open Alsdiff_output.View_model

let test_detail_level_roundtrip () =
  (* Test that detail_level serializes and deserializes correctly *)
  let levels = [Ignore; Summary; Compact; Full] in
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
    unchanged = Some Ignore;
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
    Alcotest.(check string) "prefix_added" test_cfg.prefix_added parsed.prefix_added
  | Error msg ->
    Alcotest.failf "detail_config failed: %s" msg

let test_detail_config_with_overrides () =
  (* Test custom config with type_overrides *)
  let custom_cfg = {
    compact with
    type_overrides = [
      { domain_type = DTDevice; override = {
            added = Some Full;
            removed = Some Summary;
            modified = Some Compact;
            unchanged = Some Ignore;
          }; };
    ];
    max_collection_items = Some 100;
  } in
  let json = detail_config_to_yojson custom_cfg in
  match detail_config_of_yojson json with
  | Ok parsed ->
    Alcotest.(check int) "type_overrides length" 1 (Stdlib.List.length parsed.type_overrides);
    let device_entry = Stdlib.List.find_opt (fun (e : Alsdiff_output.Config.type_override_entry) -> e.Alsdiff_output.Config.domain_type = DTDevice) parsed.type_overrides in
    Alcotest.(check bool) "device override exists"
      (Option.is_some device_entry) true;
    (match device_entry with
     | Some entry ->
       (match entry.override.added, Some Full with
        | Some a, Some b -> Alcotest.(check bool) "device override added" (a = b) true
        | _ -> Alcotest.failf "device override added mismatch")
     | None -> Alcotest.failf "device override not found");
    Alcotest.(check bool) "max_collection_items" (parsed.max_collection_items = Some 100) true
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

(* Recursively filter null values from JSON.
   ppx_deriving_yojson serializes None as null, but the JSON schema
   expects optional fields to be absent rather than null. *)
let rec filter_nulls_recursive (json : Yojson.Basic.t) : Yojson.Basic.t =
  match json with
  | `Assoc fields ->
    `Assoc (fields
            |> List.filter (fun (_, v) -> v <> `Null)
            |> List.map (fun (k, v) -> (k, filter_nulls_recursive v)))
  | `List items ->
    `List (List.map filter_nulls_recursive items)
  | other -> other

let test_validate_valid_config () =
  (* Test that a valid config passes validation.
     Note: We need to filter out null values since ppx_deriving_yojson serializes
     None as null, but the JSON schema doesn't allow null for optional fields.
     In practice, users should omit optional fields rather than set them to null. *)
  let valid_json = detail_config_to_yojson compact in
  let json_basic = Yojson.Safe.to_basic valid_json in
  (* Recursively remove null fields to make it valid for schema validation *)
  let filtered_json = filter_nulls_recursive json_basic in
  match validate_config_json filtered_json with
  | Ok () -> ()
  | Error err -> Alcotest.failf "Valid config should pass validation: %s" err.details

let test_validate_invalid_type () =
  (* Test that wrong type is rejected - using wrong format for detail_level *)
  let invalid_json = `Assoc [
      ("added", `String "Full");  (* Wrong - should be ["Full"] *)
      ("removed", `List [`String "Full"]);
      ("modified", `List [`String "Full"]);
      ("unchanged", `List [`String "Ignore"]);
      ("type_overrides", `List []);
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
      ("unchanged", `List [`String "Ignore"]);
      ("type_overrides", `List []);
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
  (* Recursively remove null fields *)
  let filtered_json = filter_nulls_recursive json_basic in
  let json_str = Yojson.Basic.to_string filtered_json in
  match validate_config_string json_str with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "Valid JSON string should pass: %s" msg

let test_validate_invalid_json_string () =
  (* Test invalid JSON string *)
  match validate_config_string "{ invalid json }" with
  | Ok () -> Alcotest.failf "Invalid JSON should fail"
  | Error _ -> ()  (* Expected *)

let test_validate_business_logic_warnings () =
  (* Test that business logic validation catches invalid indent_width.
     Use 'full' preset instead of 'compact' because 'compact' uses partial
     overrides which create None values that conflict with JSON schema validation. *)
  let cfg_with_invalid_indent = {
    full with
    indent_width = -1;  (* Invalid - must be >= 0 *)
    max_collection_items = Some (-2);  (* Invalid - None or Some >= 0 *)
  } in
  (* Write config to temp file - no null filtering needed for 'full' preset *)
  let temp_file = Filename.temp_file "alsdiff_test_config_" ".json" in
  let json = detail_config_to_yojson cfg_with_invalid_indent in
  let json_str = Yojson.Safe.pretty_to_string json in
  let oc = open_out temp_file in
  output_string oc json_str;
  close_out oc;
  (* Validate and expect warning about invalid indent_width *)
  let result = validate_config_file temp_file in
  (* Clean up temp file *)
  (try Sys.remove temp_file with Sys_error _ -> ());
  match result with
  | Ok () -> Alcotest.failf "Config with invalid indent_width should fail validation"
  | Error msg ->
    (* Check error message contains expected warning *)
    Alcotest.(check bool) "error contains 'indent_width'"
      true (Re.execp (Re.compile (Re.str "indent_width")) msg);
    Alcotest.(check bool) "error contains '>= 0'"
      true (Re.execp (Re.compile (Re.str ">= 0")) msg);

    Alcotest.(check bool) "error contains 'max_collection_items'"
      true (Re.execp (Re.compile (Re.str "max_collection_items")) msg);
    Alcotest.(check bool) "error contains '>= 0'"
      true (Re.execp (Re.compile (Re.str ">= 0")) msg);
    ()

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
  "validate business logic warnings", `Quick, test_validate_business_logic_warnings;
]

let () =
  Alcotest.run "Detail config JSON tests" [
    "json_tests", tests
  ]
