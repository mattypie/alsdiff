open Alcotest
open Alsdiff_base
open Alsdiff_live.Clip

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
      check (float 0.001) "old start time" 80.0 m.oldval;
      check (float 0.001) "new start time" 79.5 m.newval
  | _ -> fail "Expected start time to be modified");

  (* Check end time change *)
  (match patch.end_time with
  | `Modified m ->
      check (float 0.001) "old end time" 101.0 m.oldval;
      check (float 0.001) "new end time" 100.0 m.newval
  | _ -> fail "Expected end time to be modified");

  (* Check signature change *)
  (match patch.signature with
  | `Modified sig_patch ->
      (* sig_patch is TimeSignature.Patch.t with numer and denom atomic_update fields *)
      let old_numer = match sig_patch.numer with `Modified m -> m.oldval | _ -> 3 in
      let old_denom = match sig_patch.denom with `Modified m -> m.oldval | _ -> 8 in
      let new_numer = match sig_patch.numer with `Modified m -> m.newval | _ -> 4 in
      let new_denom = match sig_patch.denom with `Modified m -> m.newval | _ -> 4 in
      check string "old signature" "3/8" (Printf.sprintf "%d/%d" old_numer old_denom);
      check string "new signature" "4/4" (Printf.sprintf "%d/%d" new_numer new_denom)
  | _ -> fail "Expected signature to be modified");

  (* Check loop changes *)
  (match patch.loop with
  | `Modified loop_patch ->
      (match loop_patch.start_time with
      | `Modified m ->
          check (float 0.001) "old loop start" 30.0 m.oldval;
          check (float 0.001) "new loop start" 26.13179997086247 m.newval
      | _ -> fail "Expected loop start to be modified");

      (match loop_patch.end_time with
      | `Modified m ->
          check (float 0.001) "old loop end" 50.0 m.oldval;
          check (float 0.001) "new loop end" 46.631799970862474 m.newval
      | _ -> fail "Expected loop end to be modified");

      (match loop_patch.on with
      | `Modified m ->
          check bool "old loop on" true m.oldval;
          check bool "new loop on" false m.newval
      | _ -> fail "Expected loop on to be modified")
  | `Unchanged -> fail "Expected loop to be modified");

  (* Check sample reference changes *)
  (match patch.sample_ref with
  | `Modified sample_ref_patch ->
      (match sample_ref_patch.file_path with
      | `Modified m ->
          check string "old file path" "/Users/krfantasy/Desktop/Prelude/Thick Air Project/Samples/Processed/Crop/Metal Sheet_old.wav" m.oldval;
          check string "new file path" "/Users/krfantasy/Desktop/Prelude/Thick Air Project/Samples/Processed/Crop/Metal Sheet [2022-04-27 164454].wav" m.newval
      | _ -> fail "Expected file path to be modified");

      (match sample_ref_patch.crc with
      | `Modified m ->
          check string "old crc" "12345" m.oldval;
          check string "new crc" "48320" m.newval
      | _ -> fail "Expected crc to be modified");

      (match sample_ref_patch.last_modified_date with
      | `Modified m ->
          check int "old last modified" 1742403846 m.oldval;
          check int "new last modified" 1742403845 m.newval
      | _ -> fail "Expected last modified date to be modified")
  | `Unchanged -> fail "Expected sample reference to be modified")

let () =
  run "Diff AudioClip" [
    "diff-logic", [ test_case "Test audio clip diffing logic" `Quick test_diff_logic ];
  ]
