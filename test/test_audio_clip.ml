open Alsdiff_base.Xml
open Alsdiff_live.Clip

let test_create_audio_clip () =
  (* Read the audio_clip.xml file *)
  let xml = read_file "audio_clip.xml" in

  (* Create audio clip from the XML *)
  let audio_clip = AudioClip.create xml in

  (* Expected values based on the XML file *)
  let expected_id = 17 in
  let expected_name = "Metal Sheet" in
  let expected_start_time = 79.5 in
  let expected_end_time = 100.0 in
  let expected_signature = { TimeSignature.numer = 4; denom = 4 } in
  let expected_sample_ref = {
    SampleRef.file_path = "/Users/krfantasy/Desktop/Prelude/Thick Air Project/Samples/Processed/Crop/Metal Sheet [2022-04-27 164454].wav";
    SampleRef.crc = "48320";
    SampleRef.last_modified_date = 1742403845L;
  } in
  let expected_loop = Some {
    Loop.start_time = 26.13179997086247;
    Loop.end_time = 46.631799970862474;
    Loop.on = false;
  } in

  (* Test basic fields *)
  Alcotest.(check int) "id" expected_id audio_clip.id;
  Alcotest.(check string) "name" expected_name audio_clip.name;
  Alcotest.(check (float 0.001)) "start_time" expected_start_time audio_clip.start_time;
  Alcotest.(check (float 0.001)) "end_time" expected_end_time audio_clip.end_time;
  Alcotest.(check (pair int int)) "signature" (expected_signature.numer, expected_signature.denom) (audio_clip.signature.numer, audio_clip.signature.denom);

  (* Test sample reference *)
  Alcotest.(check string) "sample_ref.file_path" expected_sample_ref.file_path audio_clip.sample_ref.file_path;
  Alcotest.(check string) "sample_ref.crc" expected_sample_ref.crc audio_clip.sample_ref.crc;
  Alcotest.(check int64) "sample_ref.last_modified_date" expected_sample_ref.last_modified_date audio_clip.sample_ref.last_modified_date;

  (* Test loop section *)
  match expected_loop with
  | Some expected_loop ->
    Alcotest.(check (float 0.001)) "loop.start_time" expected_loop.Loop.start_time audio_clip.loop.start_time;
    Alcotest.(check (float 0.001)) "loop.end_time" expected_loop.Loop.end_time audio_clip.loop.end_time;
    Alcotest.(check bool) "loop.on" expected_loop.Loop.on audio_clip.loop.on
  | None -> Alcotest.fail "Expected expected_loop to be Some"

let () =
  Alcotest.run "AudioClip" [
    "create_audio_clip",
    [
      Alcotest.test_case "parse AudioClip XML" `Quick test_create_audio_clip
    ]
  ]
