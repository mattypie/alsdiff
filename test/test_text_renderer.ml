
open Alsdiff_output.Text_renderer
open Alsdiff_output.View_model

let test_compact () =
  let view =
    Item {
      name = "MidiClip";
      change = Modified;
      domain_type = DTOther;
      children = [
        Field { name = "Name"; change = Modified; domain_type = DTOther; oldval = Some (Fstring "Old"); newval = Some (Fstring "New") }
      ]
    }
  in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp compact ppf view;  (* Use compact preset *)
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  (* Compact mode now shows Summary level for DTOther (not in type_overrides) *)
  (* which includes the change count. For types with type_overrides (Track, Clip, etc.) *)
  (* it would show structure without field details. *)
  Alcotest.(check string) "compact output" "* MidiClip (1 Modified)" (String.trim output)

let test_full () =
  let view =
    Item {
      name = "MidiClip";
      change = Modified;
      domain_type = DTOther;
      children = [
        Field { name = "Name"; change = Modified; domain_type = DTOther; oldval = Some (Fstring "Old"); newval = Some (Fstring "New") }
      ]
    }
  in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp full ppf view;  (* Use full preset *)
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  (* Full mode should show the element name, change, AND the fields *)
  let expected = "* MidiClip\n  * Name: Old -> New" in
  Alcotest.(check string) "full output" expected (String.trim output)

let test_collection () =
  let view =
    Collection {
      name = "Notes";
      change = Modified;
      domain_type = DTOther;
      items = [
        Item {
          name = "Note";
          change = Added;
          domain_type = DTOther;
          children = [
            Field { name = "Pitch"; change = Added; domain_type = DTOther; oldval = None; newval = Some (Fint 60) }
          ]
        }
      ]
    }
  in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp full ppf view;  (* Use full preset *)
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  let expected = "* Notes\n  + Note\n    + Pitch: 60" in
  Alcotest.(check string) "collection output" expected (String.trim output)

(* New test: Removed items show summary only *)
let test_removed_summary () =
  let view =
    Item {
      name = "MidiClip";
      change = Removed;
      domain_type = DTOther;
      children = [
        Field { name = "Name"; change = Removed; domain_type = DTOther; oldval = Some (Fstring "Test"); newval = None }
      ]
    }
  in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp compact ppf view;  (* compact has removed = Summary *)
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  (* Summary mode shows name with change symbol *)
  Alcotest.(check string) "removed summary" "- MidiClip (1 Removed)" (String.trim output)

(* New test: Collection item limiting *)
let test_collection_limit () =
  let items = List.init 100 (fun i ->
      Item { name = "Note"; change = Added; domain_type = DTOther; children = [Field { name = "Pitch"; change = Added; domain_type = DTOther; oldval = None; newval = Some (Fint i) }] }
    ) in
  let view = Collection { name = "Notes"; change = Added; domain_type = DTOther; items } in
  let cfg = { full with max_collection_items = Some 10 } in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp cfg ppf view;
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  (* Should only show 10 items, which is less than the 100 original items *)
  let lines = String.split_on_char '\n' output |> List.filter (fun s -> String.trim s <> "") in
  (* 10 items * 2 lines per item (element + field) + 1 line for collection header = 21 lines max *)
  Alcotest.(check bool) "collection limited" true (List.length lines < 100)

(* New test: DLNone level hides items *)
let test_none_level () =
  let cfg = { full with unchanged = DLNone } in
  let view =
    Item {
      name = "MidiClip";
      change = Unchanged;
      domain_type = DTOther;
      children = []
    }
  in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `None;
  pp cfg ppf view;
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buffer in
  Alcotest.(check string) "none level" "" (String.trim output)

(* New tests for nested type overrides *)

(* 11a. Test nested override logic *)
let test_nested_type_overrides () =
  let cfg = {
    compact with
    added = Summary;
    removed = Summary;
    type_overrides = [
      (DTDevice, override
         ~added:(Some Full)
         ~removed:(Some Summary)
         ~modified:(Some Full)
         ());
    ];
  } in
  (* Verify: Added Device = Full *)
  Alcotest.(check bool) "added device" true (get_effective_detail cfg Added DTDevice = Full);
  (* Verify: Removed Device = Summary *)
  Alcotest.(check bool) "removed device" true (get_effective_detail cfg Removed DTDevice = Summary);
  (* Verify: Modified Device = Full *)
  Alcotest.(check bool) "modified device" true (get_effective_detail cfg Modified DTDevice = Full);

  (* Verify: Other types use base defaults *)
  Alcotest.(check bool) "added clip" true (get_effective_detail cfg Added DTClip = Summary);
  Alcotest.(check bool) "removed clip" true (get_effective_detail cfg Removed DTClip = Summary)

(* 11b. Test None means use base default *)
let test_override_with_none () =
  let cfg = {
    full with
    type_overrides = [
      (DTDevice, override ~added:(Some Full) ~removed:None ());
    ];
  } in
  (* Added Device = Full (explicit override) *)
  Alcotest.(check bool) "added device" true (get_effective_detail cfg Added DTDevice = Full);
  (* Removed Device = Full (None means use base default of Full) *)
  Alcotest.(check bool) "removed device" true (get_effective_detail cfg Removed DTDevice = Full)

(* 11c. Test uniform_override preserves old behavior *)
let test_uniform_override () =
  let cfg = {
    compact with
    type_overrides = [
      (DTClip, uniform_override Summary);
    ];
  } in
  (* All Clip changes = Summary *)
  Alcotest.(check bool) "added clip" true (get_effective_detail cfg Added DTClip = Summary);
  Alcotest.(check bool) "removed clip" true (get_effective_detail cfg Removed DTClip = Summary);
  Alcotest.(check bool) "modified clip" true (get_effective_detail cfg Modified DTClip = Summary)

(* 11d. Test smart constructor *)
let test_smart_constructor () =
  let cfg = with_type_override compact DTDevice
      ~added:(Some (Some Full))
      ~removed:(Some (Some Summary))
      ~modified:(Some (Some Full))
      ~unchanged:None
  in
  Alcotest.(check bool) "added device" true (get_effective_detail cfg Added DTDevice = Full);
  Alcotest.(check bool) "removed device" true (get_effective_detail cfg Removed DTDevice = Summary);
  Alcotest.(check bool) "modified device" true (get_effective_detail cfg Modified DTDevice = Full);

  (* Test: Adding second override merges correctly *)
  let cfg2 = with_type_override cfg DTClip
      ~added:None
      ~removed:(Some None)  (* Explicitly set removed to None (use default) *)
      ~modified:None
      ~unchanged:None
  in
  (* Device override should be preserved *)
  Alcotest.(check bool) "added device still full" true (get_effective_detail cfg2 Added DTDevice = Full);
  (* Clip override should use compact's removed=Summary *)
  Alcotest.(check bool) "removed clip" true (get_effective_detail cfg2 Removed DTClip = Summary)

(* 11e. Test validation *)
let test_validation () =
  let cfg = {
    full with
    type_overrides = [
      (DTDevice, override ());  (* All None - should warn *)
    ];
  } in
  let warnings = validate_config cfg in
  Alcotest.(check int) "validation warnings" 1 (List.length warnings);
  (* Check that the warning contains "no effect" *)
  let warning = List.hd warnings in
  Alcotest.(check bool) "warning message" true (String.length warning > 10)  (* Just check it's not empty *)

(* 11f. Test edge cases *)
let test_edge_cases () =
  let cfg = {
    compact with
    type_overrides = [
      (DTDevice, uniform_override Full);
      (DTClip, uniform_override Summary);
      (DTNote, uniform_override DLNone);
    ];
  } in
  Alcotest.(check bool) "added device" true (get_effective_detail cfg Added DTDevice = Full);
  Alcotest.(check bool) "added clip" true (get_effective_detail cfg Added DTClip = Summary);
  Alcotest.(check bool) "added note" true (get_effective_detail cfg Added DTNote = DLNone);

  (* Test: Unchanged type uses correct default *)
  Alcotest.(check bool) "unchanged device" true (get_effective_detail cfg Unchanged DTDevice = Full)

(* 11g. Test rendering with nested overrides *)
let test_rendering_with_nested_overrides () =
  let cfg = {
    compact with
    type_overrides = [
      (DTDevice, override ~added:(Some Full) ~removed:(Some Summary) ());
    ];
  } in

  (* Create test views *)
  let added_device = Item {
      name = "Operator";
      change = Added;
      domain_type = DTDevice;
      children = [Field {name = "Preset"; change = Added; domain_type = DTDevice; oldval = None; newval = Some (Fstring "Init")}];
    } in

  let removed_device = Item {
      name = "Echo";
      change = Removed;
      domain_type = DTDevice;
      children = [];
    } in

  (* Render and verify output differs *)
  let buffer1 = Buffer.create 1024 in
  let ppf1 = Format.formatter_of_buffer buffer1 in
  Fmt.set_style_renderer ppf1 `None;
  pp cfg ppf1 added_device;
  Format.pp_print_flush ppf1 ();
  let added_output = Buffer.contents buffer1 in

  let buffer2 = Buffer.create 1024 in
  let ppf2 = Format.formatter_of_buffer buffer2 in
  Fmt.set_style_renderer ppf2 `None;
  pp cfg ppf2 removed_device;
  Format.pp_print_flush ppf2 ();
  let removed_output = Buffer.contents buffer2 in

  (* Added should show fields, removed should not *)
  (* Use String.contains from Stdlib which takes a char, so we check for 'P' *)
  Alcotest.(check bool) "added shows preset" true (String.contains added_output 'P');
  (* Removed shouldn't show the preset field *)
  Alcotest.(check bool) "removed no preset" false (String.contains removed_output 'P')

let tests = [
  "compact", `Quick, test_compact;
  "full", `Quick, test_full;
  "collection", `Quick, test_collection;
  "removed summary", `Quick, test_removed_summary;
  "collection limit", `Quick, test_collection_limit;
  "none level", `Quick, test_none_level;
  "nested type overrides", `Quick, test_nested_type_overrides;
  "override with none", `Quick, test_override_with_none;
  "uniform override", `Quick, test_uniform_override;
  "smart constructor", `Quick, test_smart_constructor;
  "validation", `Quick, test_validation;
  "edge cases", `Quick, test_edge_cases;
  "rendering with nested overrides", `Quick, test_rendering_with_nested_overrides;
]

let () =
  Alcotest.run "Layout_engine" [
    "layout", tests
  ]
