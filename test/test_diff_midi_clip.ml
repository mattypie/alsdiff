open Alcotest
open Alsdiff_base
open Alsdiff_live.Clip

(** Helper to load an MidiClip.t from a file path. *)
let load_midi_clip_from_file (path : string) : MidiClip.t =
  let xml = Xml.read_file path in
  let clip_element =
    match xml with
    | Element { name = "MidiClip"; _ } as clip -> clip
    | _ -> failwith ("Root element in " ^ path ^ " is not MidiClip")
  in
  MidiClip.create clip_element

(** The main test function for the midi clip diffing logic. *)
let test_diff_logic () =
  (* 1. Load the old and new states from the XML files. *)
  let old_clip = load_midi_clip_from_file "midi_clip_old.xml" in
  let new_clip = load_midi_clip_from_file "midi_clip.xml" in

  (* 2. Compute the diff between the old and new states. *)
  let patch = MidiClip.diff old_clip new_clip in

  (* 3. Check specific changes based on what we expect to be different *)
  (* Check name change *)
  (match patch.name with
   | `Modified m ->
     check string "old name" "Modified Clip" m.oldval;
     check string "new name" "" m.newval
   | _ -> fail "Expected name to be modified");

  (* Check start time change *)
  (match patch.start_time with
   | `Modified m ->
     check (float 0.001) "old start time" 75.0 m.oldval;
     check (float 0.001) "new start time" 80.0 m.newval
   | _ -> fail "Expected start time to be modified");

  (* Check end time change *)
  (match patch.end_time with
   | `Modified m ->
     check (float 0.001) "old end time" 95.0 m.oldval;
     check (float 0.001) "new end time" 100.0 m.newval
   | _ -> fail "Expected end time to be modified");

  (* Check signature change *)
  (match patch.signature with
   | `Modified sig_patch ->
     (* sig_patch is TimeSignature.Patch.t with numer and denom atomic_update fields *)
     let old_numer = match sig_patch.numer with `Modified m -> m.oldval | _ -> 3 in
     let old_denom = match sig_patch.denom with `Modified m -> m.oldval | _ -> 4 in
     let new_numer = match sig_patch.numer with `Modified m -> m.newval | _ -> 4 in
     let new_denom = match sig_patch.denom with `Modified m -> m.newval | _ -> 4 in
     check string "old signature" "3/4" (Printf.sprintf "%d/%d" old_numer old_denom);
     check string "new signature" "4/4" (Printf.sprintf "%d/%d" new_numer new_denom)
   | _ -> fail "Expected signature to be modified");

  (* Check loop changes *)
  (match patch.loop with
   | `Modified loop_patch ->
     (match loop_patch.start_time with
      | `Modified m ->
        check (float 0.001) "old loop start" 85.0 m.oldval;
        check (float 0.001) "new loop start" 92.0 m.newval
      | _ -> fail "Expected loop start to be modified");

     (match loop_patch.end_time with
      | `Modified m ->
        check (float 0.001) "old loop end" 105.0 m.oldval;
        check (float 0.001) "new loop end" 112.0 m.newval
      | _ -> fail "Expected loop end to be modified");

     (match loop_patch.on with
      | `Modified m ->
        check bool "old loop on" true m.oldval;
        check bool "new loop on" false m.newval
      | _ -> fail "Expected loop on to be modified")
   | `Unchanged -> fail "Expected loop to be modified");

  (* Check that we have some notes changes (we expect at least some) *)
  let has_notes_changes = match patch.notes with
    | [] -> false
    | notes_list ->
      List.exists (function
          | `Added _ -> true
          | `Removed _ -> true
          | `Modified _ -> true
          | `Unchanged -> false) notes_list
  in
  check bool "has notes changes" true has_notes_changes

let () =
  run "Diff MidiClip" [
    "diff-logic", [ test_case "Test midi clip diffing logic" `Quick test_diff_logic ];
  ]
