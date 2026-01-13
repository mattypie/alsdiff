open Alsdiff_base.Xml
open Alsdiff_live.Clip

let test_midi_clip_parsing () =
  (* Read the midi_clip.xml file *)
  let xml = read_file "midi_clip.xml" in

  (* Create midi clip from the XML *)
  let midi_clip = MidiClip.create xml in

  (* Expected values based on the XML file *)
  let expected_id = 2 in
  let expected_start_time = 80.0 in
  let expected_end_time = 100.0 in
  let expected_signature = { TimeSignature.numer = 4; TimeSignature.denom = 4 } in
  let expected_loop = Some {
      Loop.start_time = 92.0;
      Loop.end_time = 112.0;
      Loop.on = false;
    } in

  (* Test basic fields *)
  Alcotest.(check int) "id" expected_id midi_clip.id;
  Alcotest.(check (float 0.001)) "start_time" expected_start_time midi_clip.start_time;
  Alcotest.(check (float 0.001)) "end_time" expected_end_time midi_clip.end_time;
  Alcotest.(check (pair int int)) "signature" (expected_signature.numer, expected_signature.denom) (midi_clip.signature.numer, midi_clip.signature.denom);

  (* Test loop section *)
  match expected_loop with
  | Some expected_loop ->
    Alcotest.(check (float 0.001)) "loop.start_time" expected_loop.Loop.start_time midi_clip.loop.start_time;
    Alcotest.(check (float 0.001)) "loop.end_time" expected_loop.Loop.end_time midi_clip.loop.end_time;
    Alcotest.(check bool) "loop.on" expected_loop.Loop.on midi_clip.loop.on
  | None -> Alcotest.fail "Expected expected_loop to be Some"

let () =
  Alcotest.run "MidiClip" [
    "create_midi_clip",
    [
      Alcotest.test_case "parse MidiClip XML" `Quick test_midi_clip_parsing
    ]
  ]
