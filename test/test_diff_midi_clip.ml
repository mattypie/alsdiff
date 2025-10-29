open Alcotest
open Alsdiff_base
open Alsdiff_live.Clip
open Alsdiff_output

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

  (* 3. Assert that we have a patch (there should be changes). *)
  (* MidiClip.diff returns a Patch.t directly, not an option *)
  check bool "patch exists" true true;

  (* 4. Check specific changes based on what we expect to be different *)
  (* Check name change *)
  (match patch.name with
  | `Modified m ->
      check string "old name" "Modified Clip" m.old;
      check string "new name" "" m.new_
  | _ -> fail "Expected name to be modified");

  (* Check start time change *)
  (match patch.start_time with
  | `Modified m ->
      check (float 0.001) "old start time" 75.0 m.old;
      check (float 0.001) "new start time" 80.0 m.new_
  | _ -> fail "Expected start time to be modified");

  (* Check end time change *)
  (match patch.end_time with
  | `Modified m ->
      check (float 0.001) "old end time" 95.0 m.old;
      check (float 0.001) "new end time" 100.0 m.new_
  | _ -> fail "Expected end time to be modified");

  (* Check signature change *)
  (match patch.signature with
  | `Modified m ->
      check string "old signature" "3/4" (Printf.sprintf "%d/%d" m.old.TimeSignature.numer m.old.TimeSignature.denom);
      check string "new signature" "4/4" (Printf.sprintf "%d/%d" m.new_.TimeSignature.numer m.new_.TimeSignature.denom)
  | _ -> fail "Expected signature to be modified");

  (* Check loop changes *)
  (match patch.loop with
  | `Patched loop_patch ->
      (match loop_patch.start_time with
      | `Modified m ->
          check (float 0.001) "old loop start" 85.0 m.old;
          check (float 0.001) "new loop start" 92.0 m.new_
      | _ -> fail "Expected loop start to be modified");

      (match loop_patch.end_time with
      | `Modified m ->
          check (float 0.001) "old loop end" 105.0 m.old;
          check (float 0.001) "new loop end" 112.0 m.new_
      | _ -> fail "Expected loop end to be modified");

      (match loop_patch.on with
      | `Modified m ->
          check bool "old loop on" true m.old;
          check bool "new loop on" false m.new_
      | _ -> fail "Expected loop on to be modified")
  | `Unchanged -> fail "Expected loop to be modified");

  (* Check that we have some notes changes (we expect at least some) *)
  let has_notes_changes = match patch.notes with
  | [] -> false
  | notes_list ->
      List.exists (function
        | `Added _ -> true
        | `Removed _ -> true
        | `Patched _ -> true
        | `Unchanged -> false) notes_list
  in
  check bool "has notes changes" true has_notes_changes

(** Test function for verifying the text output from TextOutput module. *)
let test_text_output () =
  (* 1. Load the old and new states from the XML files. *)
  let old_clip = load_midi_clip_from_file "midi_clip_old.xml" in
  let new_clip = load_midi_clip_from_file "midi_clip.xml" in

  (* 2. Compute the diff between the old and new states. *)
  let patch = MidiClip.diff old_clip new_clip in

  (* 3. Generate the text output using TextOutput. *)
  let text_output = Text_output.render_midi_clip patch in

  (* 4. Define the expected text output. *)
  let expected_lines = [
    "Midi Clip Patch:";
    "  ~ Name changed from Modified Clip to ";
    "  ~ Start time changed from 75.00 to 80.00";
    "  ~ End time changed from 95.00 to 100.00";
    "  ~ Time signature changed from 3/4 to 4/4";
    "  Loop Changes:";
    "    ~ Loop start changed from 85.00 to 92.00";
    "    ~ Loop end changed from 105.00 to 112.00";
    "    ~ Loop enabled changed from true to false";
  ] in

  (* 5. Assert that the generated text contains the expected text.
   * We check containment rather than exact match since notes changes can be complex
   * and we just want to verify the main fields are rendered correctly. *)
  let contains_substring haystack needle =
    let needle_len = String.length needle in
    let haystack_len = String.length haystack in
    let rec check_from pos =
      if pos > haystack_len - needle_len then false
      else
        let rec match_chars i =
          if i = needle_len then true
          else if haystack.[pos + i] = needle.[i] then match_chars (i + 1)
          else false
        in
        if match_chars 0 then true else check_from (pos + 1)
    in
    check_from 0
  in
  let contains_expected_lines =
    List.for_all (fun expected_line -> contains_substring text_output expected_line) expected_lines
  in
  check bool "text output contains expected lines" true contains_expected_lines

(** Alcotest test suite setup. *)
let () =
  run "Diff MidiClip" [
    "diff-logic", [ test_case "Test midi clip diffing logic" `Quick test_diff_logic ];
    "text-output", [ test_case "Test text output rendering" `Quick test_text_output ];
  ]
