open Alcotest
open Alsdiff_base.Diff
open Alsdiff_live
open Alsdiff_live.Clip
open Alsdiff_output.View_model

(* ========== Helper Functions ========== *)

(* Helper: Check if a view is a Field and return it *)
let get_field = function
  | Field f -> f
  | Item _ -> failwith "Expected Field view, got Item"
  | Collection _ -> failwith "Expected Field view, got Collection"

(* Helper: Check if a view is an Item and return it *)
let get_item = function
  | Item i -> i
  | Field _ -> failwith "Expected Item view, got Field"
  | Collection _ -> failwith "Expected Item view, got Collection"

(* Helper: Check if a view is a Collection and return it *)
let get_collection = function
  | Collection c -> c
  | Field _ -> failwith "Expected Collection view, got Field"
  | Item _ -> failwith "Expected Collection view, got Item"

(* Helper: Find a sub-view by name *)
let find_view_by_name name views =
  try
    List.find (fun v ->
        match v with
        | Field f -> f.name = name
        | Item i -> i.name = name
        | Collection c -> c.name = name
      ) views
  with Not_found -> failwith ("View with name '" ^ name ^ "' not found")

(* Helper: Find item in collection items by checking name prefix *)
let find_item_in_collection name (col : collection) =
  try
    List.find (fun v ->
        match v with
        | Item i -> String.length i.name >= String.length name &&
                    String.sub i.name 0 (String.length name) = name
        | _ -> false
      ) col.items |> get_item
  with Not_found -> failwith ("Item starting with '" ^ name ^ "' not found in collection")


(* ========== ViewBuilder Module Tests ========== *)

let test_change_type_of () =
  (* Test Added *)
  let added_change = `Added 42 in
  check (Alcotest.of_pp (fun fmt ct ->
      Fmt.pf fmt "%s" (match ct with Added -> "Added" | Removed -> "Removed" | Modified -> "Modified" | Unchanged -> "Unchanged")
    )) "Added change" Added (ViewBuilder.change_type_of added_change);

  (* Test Removed *)
  let removed_change = `Removed "hello" in
  check (Alcotest.of_pp (fun fmt ct ->
      Fmt.pf fmt "%s" (match ct with Added -> "Added" | Removed -> "Removed" | Modified -> "Modified" | Unchanged -> "Unchanged")
    )) "Removed change" Removed (ViewBuilder.change_type_of removed_change);

  (* Test Modified *)
  let modified_change = `Modified { oldval = 1; newval = 2 } in
  check (Alcotest.of_pp (fun fmt ct ->
      Fmt.pf fmt "%s" (match ct with Added -> "Added" | Removed -> "Removed" | Modified -> "Modified" | Unchanged -> "Unchanged")
    )) "Modified change" Modified (ViewBuilder.change_type_of modified_change);

  (* Test Unchanged *)
  let unchanged_change = `Unchanged in
  check (Alcotest.of_pp (fun fmt ct ->
      Fmt.pf fmt "%s" (match ct with Added -> "Added" | Removed -> "Removed" | Modified -> "Modified" | Unchanged -> "Unchanged")
    )) "Unchanged change" Unchanged (ViewBuilder.change_type_of unchanged_change)


let test_build_field_added () =
  (* Create a simple field descriptor *)
  let fd : (int, int atomic_patch) field_descriptor = FieldDesc {
      name = "TestField";
      of_parent_value = (fun x -> x);
      of_parent_patch = (fun p -> `Modified p);
      wrapper = int_value;
    } in

  let change = `Added 42 in
  let field = ViewBuilder.build_field change fd ~domain_type:DTOther in

  check string "Field name" "TestField" field.name;
  check bool "Field is Added" true (field.change = Added);
  check bool "No old value" true (field.oldval = None);
  (match field.newval with
   | Some (Fint n) -> check int "New value" 42 n
   | _ -> fail "Expected Fint new value")


let test_build_field_removed () =
  let fd : (string, string atomic_patch) field_descriptor = FieldDesc {
      name = "StringField";
      of_parent_value = Fun.id;
      of_parent_patch = (fun p -> `Modified p);
      wrapper = string_value;
    } in

  let change = `Removed "goodbye" in
  let field = ViewBuilder.build_field change fd ~domain_type:DTOther in

  check string "Field name" "StringField" field.name;
  check bool "Field is Removed" true (field.change = Removed);
  (match field.oldval with
   | Some (Fstring s) -> check string "Old value" "goodbye" s
   | _ -> fail "Expected Fstring old value");
  check bool "No new value" true (field.newval = None)


let test_build_field_modified () =
  let fd : (float, float atomic_patch) field_descriptor = FieldDesc {
      name = "FloatField";
      of_parent_value = Fun.id;
      of_parent_patch = (fun p -> `Modified p);
      wrapper = float_value;
    } in

  let patch = { oldval = 1.5; newval = 2.5 } in
  let change = `Modified patch in
  let field = ViewBuilder.build_field change fd ~domain_type:DTOther in

  check string "Field name" "FloatField" field.name;
  check bool "Field is Modified" true (field.change = Modified);
  (match field.oldval, field.newval with
   | Some (Ffloat o), Some (Ffloat n) ->
     check (float 0.001) "Old value" 1.5 o;
     check (float 0.001) "New value" 2.5 n
   | _ -> fail "Expected Ffloat old and new values")


let test_build_item_from_fields () =
  let fd1 : (int * string, (int atomic_patch * string atomic_patch)) field_descriptor = FieldDesc {
      name = "IntField";
      of_parent_value = fst;
      of_parent_patch = (fun (p, _) -> `Modified p);
      wrapper = int_value;
    } in
  let fd2 : (int * string, (int atomic_patch * string atomic_patch)) field_descriptor = FieldDesc {
      name = "StrField";
      of_parent_value = snd;
      of_parent_patch = (fun (_, p) -> `Modified p);
      wrapper = string_value;
    } in

  let change = `Added (10, "hello") in
  let item = ViewBuilder.build_item_from_fields change
      ~name:"TestItem"
      ~domain_type:DTOther
      ~field_descs:[fd1; fd2] in

  check string "Item name" "TestItem" item.name;
  check bool "Item is Added" true (item.change = Added);
  check int "Two children" 2 (List.length item.children);

  (* Check first field *)
  let f1 = get_field (List.nth item.children 0) in
  check string "First field name" "IntField" f1.name;
  (match f1.newval with Some (Fint n) -> check int "Int value" 10 n | _ -> fail "Expected Fint");

  (* Check second field *)
  let f2 = get_field (List.nth item.children 1) in
  check string "Second field name" "StrField" f2.name;
  (match f2.newval with Some (Fstring s) -> check string "String value" "hello" s | _ -> fail "Expected Fstring")


(* ========== Create Function Tests ========== *)

let test_create_note_item_added () =
  let note = { MidiNote.id = 1; note = 60; time = 0.0; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in
  let change = `Added note in

  let item = create_note_item change in

  check bool "Name starts with 'Note'" true (String.starts_with ~prefix:"Note" item.name);
  check bool "Item is Added" true (item.change = Added);
  check bool "Has children" true (List.length item.children > 0);

  (* Check Time field exists *)
  let time_field = get_field (find_view_by_name "Time" item.children) in
  check bool "Time field is Added" true (time_field.change = Added);
  (match time_field.newval with
   | Some (Ffloat t) -> check (float 0.001) "Time value" 0.0 t
   | _ -> fail "Expected Ffloat for Time")


let test_create_note_item_modified () =
  let old_note = { MidiNote.id = 1; note = 60; time = 0.0; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in
  let new_note = { MidiNote.id = 1; note = 60; time = 0.5; duration = 1.5; velocity = 100.0; off_velocity = 64.0 } in

  let patch = MidiNote.diff old_note new_note in
  let change = `Modified patch in

  let item = create_note_item change in

  check bool "Item is Modified" true (item.change = Modified);

  (* Check Time field is modified *)
  let time_field = get_field (find_view_by_name "Time" item.children) in
  check bool "Time field is Modified" true (time_field.change = Modified);
  (match time_field.oldval, time_field.newval with
   | Some (Ffloat o), Some (Ffloat n) ->
     check (float 0.001) "Old time" 0.0 o;
     check (float 0.001) "New time" 0.5 n
   | _ -> fail "Expected Ffloat old and new for Time")


let test_create_midi_clip_item () =
  (* Setup data *)
  let old_midi_note = { MidiNote.id = 1; note = 60; time = 0.0; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in
  let new_midi_note = { MidiNote.id = 1; note = 60; time = 0.5; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in

  let note_change = MidiNote.diff old_midi_note new_midi_note in
  let notes_changes = [`Modified note_change] in

  let old_numer = 4 in
  let new_numer = 3 in
  let signature_patch = { TimeSignature.Patch.numer = `Modified { oldval = old_numer; newval = new_numer }; denom = `Unchanged } in

  let loop_patch = { Loop.Patch.start_time = `Unchanged; end_time = `Unchanged; on = `Modified { oldval = false; newval = true } } in

  let clip_patch = {
    MidiClip.Patch.
    id = 1;
    name = `Modified { oldval = "Clip A"; newval = "Clip B" };
    start_time = `Unchanged;
    end_time = `Unchanged;
    loop = `Modified loop_patch;
    signature = `Modified signature_patch;
    notes = notes_changes;
  } in

  let change = `Modified clip_patch in

  (* Execute *)
  let item = create_midi_clip_item change in

  (* Verify - item name contains MidiClip *)
  check bool "Item name contains MidiClip" true (String.starts_with ~prefix:"MidiClip" item.name);

  (* Check Name field *)
  let name_view = get_field (find_view_by_name "Name" item.children) in
  check string "Name field name" "Name" name_view.name;
  (match name_view.change with
   | Modified ->
     (match name_view.oldval, name_view.newval with
      | Some (Fstring o), Some (Fstring n) ->
        check string "Old name" "Clip A" o;
        check string "New name" "Clip B" n
      | _ -> fail "Invalid values for Name field")
   | _ -> fail "Expected Name field to be Modified");

  (* Check TimeSignature item *)
  let sig_item = get_item (find_view_by_name "TimeSignature" item.children) in
  let numer_view = get_field (find_view_by_name "Numerator" sig_item.children) in
  (match numer_view.change with
   | Modified ->
     (match numer_view.oldval, numer_view.newval with
      | Some (Fint o), Some (Fint n) ->
        check int "Old numer" 4 o;
        check int "New numer" 3 n
      | _ -> fail "Invalid values for Numerator field")
   | _ -> fail "Expected Numerator to be Modified");

  (* Check Loop item *)
  let loop_item = get_item (find_view_by_name "Loop" item.children) in
  let on_view = get_field (find_view_by_name "On" loop_item.children) in
  (match on_view.change with
   | Modified ->
     (match on_view.oldval, on_view.newval with
      | Some (Fbool o), Some (Fbool n) ->
        check bool "Old on" false o;
        check bool "New on" true n
      | _ -> fail "Invalid values for On field")
   | _ -> fail "Expected On field to be Modified");

  (* Check Notes collection *)
  let notes_collection = get_collection (find_view_by_name "Notes" item.children) in
  check int "Number of notes" 1 (List.length notes_collection.items);

  let note_item = find_item_in_collection "Note" notes_collection in
  let time_view = get_field (find_view_by_name "Time" note_item.children) in
  (match time_view.change with
   | Modified ->
     (match time_view.oldval, time_view.newval with
      | Some (Ffloat o), Some (Ffloat n) ->
        check (float 0.001) "Old note time" 0.0 o;
        check (float 0.001) "New note time" 0.5 n
      | _ -> fail "Invalid values for Note Time field")
   | _ -> fail "Expected Note Time to be Modified")


let test_create_audio_clip_item_added () =
  let sample_ref = { SampleRef.file_path = "/path/to/sample.wav"; crc = "abc123"; last_modified_date = 12345 } in
  let loop = { Loop.start_time = 0.0; end_time = 4.0; on = true } in
  let signature = { TimeSignature.numer = 4; denom = 4 } in
  let clip = {
    AudioClip.id = 1;
    name = "Audio Clip 1";
    start_time = 0.0;
    end_time = 8.0;
    loop;
    signature;
    sample_ref;
    fade = None;
  } in

  let change = `Added clip in
  let item = create_audio_clip_item change in

  check bool "Item name contains AudioClip" true (String.starts_with ~prefix:"AudioClip" item.name);
  check bool "Item is Added" true (item.change = Added);

  (* Check Name field *)
  let name_field = get_field (find_view_by_name "Name" item.children) in
  check bool "Name field is Added" true (name_field.change = Added);
  (match name_field.newval with
   | Some (Fstring s) -> check string "Clip name" "Audio Clip 1" s
   | _ -> fail "Expected Fstring for Name");

  (* Check Loop item exists *)
  let loop_item = get_item (find_view_by_name "Loop" item.children) in
  check bool "Loop item is Added" true (loop_item.change = Added);

  (* Check SampleRef item exists *)
  let sample_item = get_item (find_view_by_name "SampleRef" item.children) in
  check bool "SampleRef item is Added" true (sample_item.change = Added);

  let file_path_field = get_field (find_view_by_name "File Path" sample_item.children) in
  (match file_path_field.newval with
   | Some (Fstring s) -> check string "Sample file path" "/path/to/sample.wav" s
   | _ -> fail "Expected Fstring for File Path")

let build_automation_item_from_event_patch event_patch =
  let automation_patch = {
    Automation.Patch.id = 1;
    target = 2;
    events = [`Modified event_patch];
  } in
  create_automation_item ~get_pointee_name:(fun _ -> "Target") (`Modified automation_patch)

let get_single_event_summary item =
  check int "single event field" 1 (List.length item.children);
  let event_field = get_field (List.hd item.children) in
  match event_field.oldval with
  | Some (Fstring s) -> s
  | _ -> fail "Expected event summary as string old value"

let test_create_automation_item_curve_added_summary () =
  let event_patch = {
    Automation.EnvelopeEvent.Patch.time = `Modified { oldval = 1.0; newval = 2.0 };
    value = `Modified { oldval = Automation.FloatEvent 10.0; newval = Automation.FloatEvent 11.0 };
    curve = `Added {
        Automation.CurveControls.curve1_x = 0.1;
        curve1_y = 0.2;
        curve2_x = 0.3;
        curve2_y = 0.4;
      };
  } in
  let item = build_automation_item_from_event_patch event_patch in
  let summary = get_single_event_summary item in
  check string "combined summary with curve add"
    "Time: 1.00->2.00, Value: 10.00->11.00, Curve Added: Curve1=(0.10,0.20) Curve2=(0.30,0.40)"
    summary

let test_create_automation_item_curve_removed_summary () =
  let event_patch = {
    Automation.EnvelopeEvent.Patch.time = `Modified { oldval = 1.0; newval = 2.0 };
    value = `Unchanged;
    curve = `Removed {
        Automation.CurveControls.curve1_x = 0.5;
        curve1_y = 0.6;
        curve2_x = 0.7;
        curve2_y = 0.8;
      };
  } in
  let item = build_automation_item_from_event_patch event_patch in
  let summary = get_single_event_summary item in
  check string "combined summary with curve remove"
    "Time: 1.00->2.00, Curve Removed: Curve1=(0.50,0.60) Curve2=(0.70,0.80)"
    summary

let test_create_automation_item_curve_modified_summary () =
  let event_patch = {
    Automation.EnvelopeEvent.Patch.time = `Modified { oldval = 1.0; newval = 2.0 };
    value = `Modified { oldval = Automation.FloatEvent 10.0; newval = Automation.FloatEvent 11.0 };
    curve = `Modified {
        Automation.CurveControls.Patch.curve1_x = `Modified { oldval = 0.1; newval = 0.2 };
        curve1_y = `Modified { oldval = 0.2; newval = 0.3 };
        curve2_x = `Modified { oldval = 0.3; newval = 0.4 };
        curve2_y = `Modified { oldval = 0.4; newval = 0.5 };
      };
  } in
  let item = build_automation_item_from_event_patch event_patch in
  let summary = get_single_event_summary item in
  check string "combined summary with curve modify"
    "Time: 1.00->2.00, Value: 10.00->11.00, Curve: C1X: 0.10->0.20, C1Y: 0.20->0.30, C2X: 0.30->0.40, C2Y: 0.40->0.50"
    summary

let () =
  run "ViewModel" [
    "ViewBuilder.change_type_of", [
      test_case "Extracts change type correctly" `Quick test_change_type_of;
    ];
    "ViewBuilder.build_field", [
      test_case "Build field for Added" `Quick test_build_field_added;
      test_case "Build field for Removed" `Quick test_build_field_removed;
      test_case "Build field for Modified" `Quick test_build_field_modified;
    ];
    "ViewBuilder.build_item_from_fields", [
      test_case "Build item with field children" `Quick test_build_item_from_fields;
    ];
    "create_note_item", [
      test_case "Create note item for Added" `Quick test_create_note_item_added;
      test_case "Create note item for Modified" `Quick test_create_note_item_modified;
    ];
    "create_midi_clip_item", [
      test_case "Create item from patch" `Quick test_create_midi_clip_item;
    ];
    "create_audio_clip_item", [
      test_case "Create item for Added clip" `Quick test_create_audio_clip_item_added;
    ];
    "create_automation_item", [
      test_case "Combined summary includes added curve details" `Quick test_create_automation_item_curve_added_summary;
      test_case "Combined summary includes removed curve details" `Quick test_create_automation_item_curve_removed_summary;
      test_case "Combined summary includes modified curve details" `Quick test_create_automation_item_curve_modified_summary;
    ];
  ]
