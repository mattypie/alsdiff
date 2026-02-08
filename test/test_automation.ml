open Alcotest
open Alsdiff_base
open Alsdiff_live

(** Test parsing FloatEvents with and without curve parameters *)
let test_envelope_event_parsing () =
  (* Load automation XML that has events with curve parameters *)
  let xml = Xml.read_file "automation.xml" in
  let envelope_element = Upath.find "/Envelopes/AutomationEnvelope@Id=3" xml |> snd in
  let automation = Automation.create envelope_element in

  (* Find an event with curve parameters - Id=647 has all four curve attrs *)
  let event_with_curve =
    List.find (fun e -> e.Automation.EnvelopeEvent.id = 647) automation.events
  in

  (* Verify curve parameters are parsed correctly *)
  check bool "event has curve" true
    (match event_with_curve.Automation.EnvelopeEvent.curve with
     | Some c ->
       let eps = 1e-9 in
       abs_float (c.Automation.CurveControls.curve1_x -. 0.873421796761574498) < eps &&
       abs_float (c.Automation.CurveControls.curve1_y -. 0.249549293637680858) < eps &&
       abs_float (c.Automation.CurveControls.curve2_x -. 0.791465481368395674) < eps &&
       abs_float (c.Automation.CurveControls.curve2_y -. 0.76167530124402627) < eps
     | None -> false);

  (* Find an event without curve parameters - Id=41 has none *)
  let event_without_curve =
    List.find (fun e -> e.Automation.EnvelopeEvent.id = 41) automation.events
  in

  (* Verify curve is None *)
  check bool "event has no curve" true
    (event_without_curve.Automation.EnvelopeEvent.curve = None)

let test_envelope_event_diff_with_curve () =
  (* Test diffing events with curve changes *)
  let event1 = {
    Automation.EnvelopeEvent.id = 1;
    time = 0.0;
    value = Automation.FloatEvent 100.0;
    curve = Some { Automation.CurveControls.curve1_x = 0.5; curve1_y = 0.5;
                   curve2_x = 0.5; curve2_y = 0.5; };
  } in
  let event2 = {
    Automation.EnvelopeEvent.id = 1;
    time = 0.0;
    value = Automation.FloatEvent 100.0;
    curve = Some { Automation.CurveControls.curve1_x = 0.6; curve1_y = 0.6;
                   curve2_x = 0.6; curve2_y = 0.6; };
  } in

  let patch = Automation.EnvelopeEvent.diff event1 event2 in

  (match patch.Automation.EnvelopeEvent.Patch.curve with
   | `Modified cp ->
     (match cp.Automation.CurveControls.Patch.curve1_x with
      | `Modified { Diff.oldval; newval } ->
        check (float 1e-9) "curve1_x old" 0.5 oldval;
        check (float 1e-9) "curve1_x new" 0.6 newval
      | _ -> fail "Expected curve1_x to be Modified");
     (match cp.Automation.CurveControls.Patch.curve1_y with
      | `Modified { Diff.oldval; newval } ->
        check (float 1e-9) "curve1_y old" 0.5 oldval;
        check (float 1e-9) "curve1_y new" 0.6 newval
      | _ -> fail "Expected curve1_y to be Modified");
     (match cp.Automation.CurveControls.Patch.curve2_x with
      | `Modified { Diff.oldval; newval } ->
        check (float 1e-9) "curve2_x old" 0.5 oldval;
        check (float 1e-9) "curve2_x new" 0.6 newval
      | _ -> fail "Expected curve2_x to be Modified");
     (match cp.Automation.CurveControls.Patch.curve2_y with
      | `Modified { Diff.oldval; newval } ->
        check (float 1e-9) "curve2_y old" 0.5 oldval;
        check (float 1e-9) "curve2_y new" 0.6 newval
      | _ -> fail "Expected curve2_y to be Modified")
   | _ -> fail "Expected curve patch to be Modified")

let test_envelope_event_diff_add_remove_curve () =
  (* Test diffing when curve is added or removed *)
  let event_no_curve = {
    Automation.EnvelopeEvent.id = 1;
    time = 0.0;
    value = Automation.FloatEvent 100.0;
    curve = None;
  } in
  let event_with_curve = {
    Automation.EnvelopeEvent.id = 1;
    time = 0.0;
    value = Automation.FloatEvent 100.0;
    curve = Some { Automation.CurveControls.curve1_x = 0.5; curve1_y = 0.5;
                   curve2_x = 0.5; curve2_y = 0.5; };
  } in

  (* Test adding curve *)
  let patch_added = Automation.EnvelopeEvent.diff event_no_curve event_with_curve in
  (match patch_added.Automation.EnvelopeEvent.Patch.curve with
   | `Added c ->
     check (float 1e-9) "curve added c1x" 0.5 c.Automation.CurveControls.curve1_x;
     check (float 1e-9) "curve added c1y" 0.5 c.Automation.CurveControls.curve1_y;
     check (float 1e-9) "curve added c2x" 0.5 c.Automation.CurveControls.curve2_x;
     check (float 1e-9) "curve added c2y" 0.5 c.Automation.CurveControls.curve2_y
   | _ -> fail "Expected curve to be Added");

  (* Test removing curve *)
  let patch_removed = Automation.EnvelopeEvent.diff event_with_curve event_no_curve in
  (match patch_removed.Automation.EnvelopeEvent.Patch.curve with
   | `Removed c ->
     check (float 1e-9) "curve removed c1x" 0.5 c.Automation.CurveControls.curve1_x;
     check (float 1e-9) "curve removed c1y" 0.5 c.Automation.CurveControls.curve1_y;
     check (float 1e-9) "curve removed c2x" 0.5 c.Automation.CurveControls.curve2_x;
     check (float 1e-9) "curve removed c2y" 0.5 c.Automation.CurveControls.curve2_y
   | _ -> fail "Expected curve to be Removed")

(* ========================================================================= *)
(* CurveControls Module Tests                                                *)
(* ========================================================================= *)

(** Test CurveControls.has_same_id with structural equality *)
let test_curve_controls_has_same_id () =
  let c1 = { Automation.CurveControls.curve1_x = 0.5; curve1_y = 0.5;
             curve2_x = 0.5; curve2_y = 0.5; } in
  let c2 = { Automation.CurveControls.curve1_x = 0.5; curve1_y = 0.5;
             curve2_x = 0.5; curve2_y = 0.5; } in
  let c3 = { Automation.CurveControls.curve1_x = 0.6; curve1_y = 0.5;
             curve2_x = 0.5; curve2_y = 0.5; } in
  check bool "identical curves have same id" true
    (Automation.CurveControls.has_same_id c1 c2);
  check bool "different curves don't have same id" false
    (Automation.CurveControls.has_same_id c1 c3)

(** Test CurveControls.id_hash consistency *)
let test_curve_controls_id_hash () =
  let c1 = { Automation.CurveControls.curve1_x = 0.5; curve1_y = 0.5;
             curve2_x = 0.5; curve2_y = 0.5; } in
  let c2 = { Automation.CurveControls.curve1_x = 0.5; curve1_y = 0.5;
             curve2_x = 0.5; curve2_y = 0.5; } in
  check bool "identical curves have same hash" true
    (Automation.CurveControls.id_hash c1 = Automation.CurveControls.id_hash c2)

(** Test CurveControls.Patch.is_empty *)
let test_curve_controls_patch_is_empty () =
  let empty_patch = {
    Automation.CurveControls.Patch.curve1_x = `Unchanged;
    curve1_y = `Unchanged;
    curve2_x = `Unchanged;
    curve2_y = `Unchanged;
  } in
  let non_empty_patch = {
    Automation.CurveControls.Patch.curve1_x = `Modified { Diff.oldval = 0.5; newval = 0.6 };
    curve1_y = `Unchanged;
    curve2_x = `Unchanged;
    curve2_y = `Unchanged;
  } in
  check bool "empty curve patch is empty" true
    (Automation.CurveControls.Patch.is_empty empty_patch);
  check bool "non-empty curve patch is not empty" false
    (Automation.CurveControls.Patch.is_empty non_empty_patch)

(* ========================================================================= *)
(* EnvelopeEvent Module Tests (event_value Variants)                        *)
(* ========================================================================= *)

(** Test EnvelopeEvent.create with IntEvent *)
let test_envelope_event_create_with_int_event () =
  (* Since there's no IntEvent in test XML, construct XML string *)
  let int_event_xml = Xml.read_string {|
    <IntEvent Id="100" Time="0.0" Value="42"/>
  |} in
  let event = Automation.EnvelopeEvent.create int_event_xml in
  check int "event id" 100 event.Automation.EnvelopeEvent.id;
  let eps = 1e-9 in
  check bool "event time" true (abs_float (event.Automation.EnvelopeEvent.time -. 0.0) < eps);
  (match event.Automation.EnvelopeEvent.value with
   | Automation.IntEvent i -> check bool "IntEvent value" true (i = 42)
   | _ -> check bool "IntEvent value" false false)

(** Test EnvelopeEvent.create with EnumEvent *)
let test_envelope_event_create_with_enum_event () =
  let enum_event_xml = Xml.read_string {|
    <EnumEvent Id="200" Time="1.0" Value="201"/>
  |} in
  let event = Automation.EnvelopeEvent.create enum_event_xml in
  check int "event id" 200 event.Automation.EnvelopeEvent.id;
  let eps = 1e-9 in
  check bool "event time" true (abs_float (event.Automation.EnvelopeEvent.time -. 1.0) < eps);
  (match event.Automation.EnvelopeEvent.value with
   | Automation.EnumEvent e -> check bool "EnumEvent value" true (e = 201)
   | _ -> check bool "EnumEvent value" false false)

(** Test EnvelopeEvent diff with value type changes *)
let test_envelope_event_diff_value_type_change () =
  let event_int = {
    Automation.EnvelopeEvent.id = 1;
    time = 0.0;
    value = Automation.IntEvent 42;
    curve = None;
  } in
  let event_enum = {
    Automation.EnvelopeEvent.id = 1;
    time = 0.0;
    value = Automation.EnumEvent 201;
    curve = None;
  } in
  let patch = Automation.EnvelopeEvent.diff event_int event_enum in
  check bool "value type changed" true
    (match patch.Automation.EnvelopeEvent.Patch.value with
     | `Modified _ -> true
     | _ -> false)

(** Test EnvelopeEvent.has_same_id and id_hash *)
let test_envelope_event_identity_functions () =
  let event1 = {
    Automation.EnvelopeEvent.id = 123;
    time = 0.0;
    value = Automation.FloatEvent 100.0;
    curve = None;
  } in
  let event2 = {
    Automation.EnvelopeEvent.id = 123;
    time = 1.0;
    value = Automation.FloatEvent 200.0;
    curve = None;
  } in
  let event3 = {
    Automation.EnvelopeEvent.id = 456;
    time = 0.0;
    value = Automation.FloatEvent 100.0;
    curve = None;
  } in
  check bool "same id events have same_id" true
    (Automation.EnvelopeEvent.has_same_id event1 event2);
  check bool "different id events don't have same_id" false
    (Automation.EnvelopeEvent.has_same_id event1 event3);
  check bool "same id events have same hash" true
    (Automation.EnvelopeEvent.id_hash event1 = Automation.EnvelopeEvent.id_hash event2)

(** Test EnvelopeEvent.Patch.is_empty *)
let test_envelope_event_patch_is_empty () =
  let empty_patch = {
    Automation.EnvelopeEvent.Patch.time = `Unchanged;
    value = `Unchanged;
    curve = `Unchanged;
  } in
  let time_changed_patch = {
    Automation.EnvelopeEvent.Patch.time = `Modified { Diff.oldval = 0.0; newval = 1.0 };
    value = `Unchanged;
    curve = `Unchanged;
  } in
  check bool "empty patch is empty" true
    (Automation.EnvelopeEvent.Patch.is_empty empty_patch);
  check bool "patch with time change is not empty" false
    (Automation.EnvelopeEvent.Patch.is_empty time_changed_patch)

(* ========================================================================= *)
(* Automation Module Tests                                                    *)
(* ========================================================================= *)

(** Test Automation.has_same_id and id_hash *)
let test_automation_identity_functions () =
  let auto1 = {
    Automation.id = 1;
    target = 100;
    events = [];
  } in
  let auto2 = {
    Automation.id = 1;
    target = 100;
    events = [];
  } in
  let auto3 = {
    Automation.id = 2;
    target = 100;
    events = [];
  } in
  check bool "same id/target have same_id" true
    (Automation.has_same_id auto1 auto2);
  check bool "different id don't have same_id" false
    (Automation.has_same_id auto1 auto3);
  check bool "same id/target have same hash" true
    (Automation.id_hash auto1 = Automation.id_hash auto2)

(** Test Automation.Patch.is_empty *)
let test_automation_patch_is_empty () =
  let empty_patch = {
    Automation.Patch.id = 1;
    target = 100;
    events = [];
  } in
  let non_empty_patch = {
    Automation.Patch.id = 1;
    target = 100;
    events = [`Added { Automation.EnvelopeEvent.id = 1; time = 0.0;
                       value = Automation.FloatEvent 100.0; curve = None; }];
  } in
  check bool "empty patch is empty" true
    (Automation.Patch.is_empty empty_patch);
  check bool "patch with added event is not empty" false
    (Automation.Patch.is_empty non_empty_patch)

(** Alcotest test suite setup. *)
let () =
  run "Automation" [
    "envelope-event-parsing", [ test_case "Test EnvelopeEvent parsing with curve parameters" `Quick test_envelope_event_parsing ];
    "envelope-event-diff-curve", [ test_case "Test EnvelopeEvent diffing with curve changes" `Quick test_envelope_event_diff_with_curve ];
    "envelope-event-diff-add-remove", [ test_case "Test EnvelopeEvent diffing with add/remove curve" `Quick test_envelope_event_diff_add_remove_curve ];
    (* New test cases *)
    "curve-controls-has-same-id", [ test_case "Test CurveControls.has_same_id" `Quick test_curve_controls_has_same_id ];
    "curve-controls-id-hash", [ test_case "Test CurveControls.id_hash" `Quick test_curve_controls_id_hash ];
    "curve-controls-patch-is-empty", [ test_case "Test CurveControls.Patch.is_empty" `Quick test_curve_controls_patch_is_empty ];
    "envelope-event-create-int-event", [ test_case "Test EnvelopeEvent.create with IntEvent" `Quick test_envelope_event_create_with_int_event ];
    "envelope-event-create-enum-event", [ test_case "Test EnvelopeEvent.create with EnumEvent" `Quick test_envelope_event_create_with_enum_event ];
    "envelope-event-diff-value-type-change", [ test_case "Test EnvelopeEvent diff with value type changes" `Quick test_envelope_event_diff_value_type_change ];
    "envelope-event-identity-functions", [ test_case "Test EnvelopeEvent identity functions" `Quick test_envelope_event_identity_functions ];
    "envelope-event-patch-is-empty", [ test_case "Test EnvelopeEvent.Patch.is_empty" `Quick test_envelope_event_patch_is_empty ];
    "automation-identity-functions", [ test_case "Test Automation identity functions" `Quick test_automation_identity_functions ];
    "automation-patch-is-empty", [ test_case "Test Automation.Patch.is_empty" `Quick test_automation_patch_is_empty ];
  ]
