open Alcotest
open Alsdiff_base
open Alsdiff_live.Clip
open Alsdiff_output

(** Helper to load an AudioClip.t from a file path. *)
let load_audio_clip_from_file (path : string) : AudioClip.t =
  let xml = Xml.read_file path in
  let clip_element =
    match xml with
    | Element { name = "AudioClip"; _ } as clip -> clip
    | _ -> failwith ("Root element in " ^ path ^ " is not AudioClip")
  in
  AudioClip.create clip_element

(** The main test function for the audio clip diffing logic. *)
let test_diff_logic () =
  (* 1. Load the old and new states from the XML files. *)
  let old_clip = load_audio_clip_from_file "audio_clip_old.xml" in
  let new_clip = load_audio_clip_from_file "audio_clip.xml" in

  (* 2. Compute the diff between the old and new states. *)
  let patch = AudioClip.diff old_clip new_clip in

  (* 3. Assert that we have a patch (there should be changes). *)
  (* AudioClip.diff returns a Patch.t directly, not an option *)
  check bool "patch exists" true true;

  (* 4. Check specific changes based on what we expect to be different *)
  (* Check name is unchanged *)
  (match patch.name with
  | `Unchanged -> check bool "name unchanged" true true
  | _ -> fail "Expected name to be unchanged");

  (* Check start time change *)
  (match patch.start_time with
  | `Modified m ->
      check (float 0.001) "old start time" 80.0 m.old;
      check (float 0.001) "new start time" 79.5 m.new_
  | _ -> fail "Expected start time to be modified");

  (* Check end time change *)
  (match patch.end_time with
  | `Modified m ->
      check (float 0.001) "old end time" 101.0 m.old;
      check (float 0.001) "new end time" 100.0 m.new_
  | _ -> fail "Expected end time to be modified");

  (* Check signature change *)
  (match patch.signature with
  | `Modified m ->
      check string "old signature" "3/8" (Printf.sprintf "%d/%d" m.old.TimeSignature.numer m.old.TimeSignature.denom);
      check string "new signature" "4/4" (Printf.sprintf "%d/%d" m.new_.TimeSignature.numer m.new_.TimeSignature.denom)
  | _ -> fail "Expected signature to be modified");

  (* Check loop changes *)
  (match patch.loop with
  | `Patched loop_patch ->
      (match loop_patch.start_time with
      | `Modified m ->
          check (float 0.001) "old loop start" 30.0 m.old;
          check (float 0.001) "new loop start" 26.13179997086247 m.new_
      | _ -> fail "Expected loop start to be modified");

      (match loop_patch.end_time with
      | `Modified m ->
          check (float 0.001) "old loop end" 50.0 m.old;
          check (float 0.001) "new loop end" 46.631799970862474 m.new_
      | _ -> fail "Expected loop end to be modified");

      (match loop_patch.on with
      | `Modified m ->
          check bool "old loop on" true m.old;
          check bool "new loop on" false m.new_
      | _ -> fail "Expected loop on to be modified")
  | `Unchanged -> fail "Expected loop to be modified");

  (* Check sample reference changes *)
  (match patch.sample_ref with
  | `Patched sample_ref_patch ->
      (match sample_ref_patch.file_path with
      | `Modified m ->
          check string "old file path" "/Users/krfantasy/Desktop/Prelude/Thick Air Project/Samples/Processed/Crop/Metal Sheet_old.wav" m.old;
          check string "new file path" "/Users/krfantasy/Desktop/Prelude/Thick Air Project/Samples/Processed/Crop/Metal Sheet [2022-04-27 164454].wav" m.new_
      | _ -> fail "Expected file path to be modified");

      (match sample_ref_patch.crc with
      | `Modified m ->
          check string "old crc" "12345" m.old;
          check string "new crc" "48320" m.new_
      | _ -> fail "Expected crc to be modified");

      (match sample_ref_patch.last_modified_date with
      | `Modified m ->
          check int64 "old last modified" 1742403846L m.old;
          check int64 "new last modified" 1742403845L m.new_
      | _ -> fail "Expected last modified date to be modified")
  | `Unchanged -> fail "Expected sample reference to be modified")

(** Test function for verifying the text output from TextOutput module. *)
let test_text_output () =
  (* 1. Load the old and new states from the XML files. *)
  let old_clip = load_audio_clip_from_file "audio_clip_old.xml" in
  let new_clip = load_audio_clip_from_file "audio_clip.xml" in

  (* 2. Compute the diff between the old and new states. *)
  let patch = AudioClip.diff old_clip new_clip in

  (* 3. Generate the text output using TextOutput. *)
  let text_output = Text_output.render_audio_clip patch in

  (* 4. Define the expected text output. *)
  let expected_lines = [
    "Audio Clip Patch:";
    "  ~ Start time changed from 80.00 to 79.50";
    "  ~ End time changed from 101.00 to 100.00";
    "  ~ Time signature changed from 3/8 to 4/4";
    "  Loop Changes:";
    "      ~ Loop start changed from 30.00 to 26.13";
    "      ~ Loop end changed from 50.00 to 46.63";
    "      ~ Loop enabled changed from true to false";
    "  Sample Reference Changes:";
    "      ~ File path changed from /Users/krfantasy/Desktop/Prelude/Thick Air Project/Samples/Processed/Crop/Metal Sheet_old.wav to /Users/krfantasy/Desktop/Prelude/Thick Air Project/Samples/Processed/Crop/Metal Sheet [2022-04-27 164454].wav";
    "      ~ CRC changed from 12345 to 48320";
    "      ~ Last modified changed from 1742403846 to 1742403845";
  ] in
  let expected_output = String.concat "\n" expected_lines in

  (* 5. Assert that the generated text matches the expected text. *)
  check string "text output" expected_output text_output

(** Alcotest test suite setup. *)
let () =
  run "Diff AudioClip" [
    "diff-logic", [ test_case "Test audio clip diffing logic" `Quick test_diff_logic ];
    "text-output", [ test_case "Test text output rendering" `Quick test_text_output ];
  ]
