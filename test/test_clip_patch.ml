open Alsdiff_base.Xml
open Alsdiff_live.Clip

let test_identical_clips () =
  (* Read the audio_clip.xml file twice *)
  let xml1 = read_file "audio_clip.xml" in
  let xml2 = read_file "audio_clip.xml" in

  (* Create two identical audio clips *)
  let clip1 = AudioClip.create xml1 in
  let clip2 = AudioClip.create xml2 in

  (* Diff them - should return a patch that has no changes *)
  let patch = AudioClip.diff clip1 clip2 in

  (* Check that the patch is empty by checking individual fields *)
  let is_empty =
    match patch with
    | { id = _; name = `Unchanged; start_time = `Unchanged; end_time = `Unchanged;
        loop = `Unchanged; signature = `Unchanged; sample_ref = `Unchanged; fade = `Unchanged } -> true
    | _ -> false
  in
  Alcotest.(check bool) "patch is empty" true is_empty

let test_modified_start_time () =
  (* Read the audio_clip.xml file twice *)
  let xml1 = read_file "audio_clip.xml" in
  let xml2 = read_file "audio_clip.xml" in

  (* Create two audio clips *)
  let clip1 = AudioClip.create xml1 in
  let clip2 = AudioClip.create xml2 in

  (* Modify the start time of clip2 *)
  let modified_clip2 = { clip2 with start_time = clip2.start_time +. 1.0 } in

  (* Diff them - should return a patch *)
  let patch = AudioClip.diff clip1 modified_clip2 in

  (* Check that start_time was modified *)
  match patch.start_time with
  | `Modified { oldval; newval } ->
    Alcotest.(check (float 0.001)) "old start_time" clip1.start_time oldval;
    Alcotest.(check (float 0.001)) "new start_time" modified_clip2.start_time newval
  | _ -> Alcotest.fail "Expected start_time to be Modified"

let test_modified_name () =
  (* Read the audio_clip.xml file twice *)
  let xml1 = read_file "audio_clip.xml" in
  let xml2 = read_file "audio_clip.xml" in

  (* Create two audio clips *)
  let clip1 = AudioClip.create xml1 in
  let clip2 = AudioClip.create xml2 in

  (* Modify the name of clip2 *)
  let modified_clip2 = { clip2 with name = "New Name" } in

  (* Diff them - should return a patch *)
  let patch = AudioClip.diff clip1 modified_clip2 in

  (* Check that name was modified *)
  match patch.name with
  | `Modified { oldval; newval } ->
    Alcotest.(check string) "old name" clip1.name oldval;
    Alcotest.(check string) "new name" modified_clip2.name newval
  | _ -> Alcotest.fail "Expected name to be Modified"

let () =
  Alcotest.run "AudioClipPatch" [
    "identical_clips",
    [
      Alcotest.test_case "identical clips should have no diff" `Quick test_identical_clips
    ];
    "modified_clips",
    [
      Alcotest.test_case "modified start_time should create patch" `Quick test_modified_start_time;
      Alcotest.test_case "modified name should create patch" `Quick test_modified_name
    ]
  ]
