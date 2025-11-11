open Alsdiff_base.Xml
open Alsdiff_live.Track
open Alsdiff_live

let test_midi_track_xml_path =
  (* Try different paths to work with both dune exec and dune runtest *)
  if Sys.file_exists "test/midi_track.xml" then
    "test/midi_track.xml"
  else
    "midi_track.xml"


let test_midi_track_basic_properties () =
  (* Load midi track XML file *)
  let xml = read_file test_midi_track_xml_path in

  (* Create midi track from XML *)
  let midi_track = MidiTrack.create xml in

  (* Expected values based on XML file *)
  let expected_id = 116 in
  let expected_name = "18-Galaxy Voices Philipp & Fiona" in

  (* Test basic fields *)
  Alcotest.(check int) "id" expected_id midi_track.id;
  Alcotest.(check string) "name" expected_name midi_track.name


let test_midi_track_clips_and_notes () =
  (* Load midi track XML file *)
  let xml = read_file test_midi_track_xml_path in

  (* Create midi track from XML *)
  let midi_track = MidiTrack.create xml in

  (* Check that we have clips *)
  Alcotest.(check (int)) "clip count" 1 (List.length midi_track.clips);

  (* Get first clip for detailed note checking *)
  let first_clip = List.hd midi_track.clips in

  (* Check basic clip properties *)
  Alcotest.(check int) "first clip id" 9 first_clip.Clip.MidiClip.id;
  Alcotest.(check (float 0.001)) "first clip start_time" 24.0 first_clip.start_time;
  Alcotest.(check (float 0.001)) "first clip end_time" 68.0 first_clip.end_time;

  (* Check notes in the clip *)
  let notes_list = first_clip.Clip.MidiClip.notes in
  let actual_note_count = List.length notes_list in
  Alcotest.(check int) "note count" 5 actual_note_count;

  (* Check some specific note details *)
  if actual_note_count > 0 then
    let first_note = List.nth notes_list 0 in
    Alcotest.(check (float 0.001)) "first note time" 8.0 first_note.time;
    Alcotest.(check (float 0.001)) "first note duration" 8.0 first_note.duration;
    Alcotest.(check int) "first note velocity" 102 first_note.velocity;
    Alcotest.(check int) "first note note_id" 2 first_note.id;

    (* Check another note *)
    let second_note = List.nth notes_list 1 in
    Alcotest.(check (float 0.001)) "second note time" 32.0 second_note.time;
    Alcotest.(check (float 0.001)) "second note duration" 11.0 second_note.duration;
    Alcotest.(check int) "second note velocity" 102 second_note.velocity;
    Alcotest.(check int) "second note note_id" 5 second_note.id
  else
    Alcotest.fail "Expected at least one MIDI note in clip"


let test_midi_track_devices_order () =
  (* Load midi track XML file *)
  let xml = read_file test_midi_track_xml_path in

  (* Create midi track from XML *)
  let midi_track = MidiTrack.create xml in

  (* Check that we have multiple devices *)
  let actual_device_count = List.length midi_track.devices in
  Alcotest.(check (int)) "device count" 2 actual_device_count;

  (* Expected device names will be set after seeing actual output *)

  (* Check device names are in expected order *)
  let actual_device_names = List.map (fun device ->
    match device with
    | Device.Regular reg -> reg.device_name
    | Device.Plugin plug -> plug.device_name
    | Device.Group group -> group.device_name
  ) midi_track.devices in
  let expected_device_names = [
    "InstrumentGroupDevice";
    "MxDeviceAudioEffect"
  ] in

  let actual_display_names = List.map (fun device ->
    match device with
    | Device.Regular reg -> reg.display_name
    | Device.Plugin plug -> plug.display_name
    | Device.Group group -> group.display_name
  ) midi_track.devices in
  let expected_display_names = [
    "Galaxy Voices Philipp & Fiona";
    "Envelope Follower"
  ] in

  List.iteri (fun i expected_name ->
    let actual_name = List.nth actual_device_names i in
    Alcotest.(check string) ("device name " ^ string_of_int i) expected_name actual_name
  ) expected_device_names;

  List.iteri (fun i expected_name ->
    let actual_name = List.nth actual_display_names i in
    Alcotest.(check string) ("display name " ^ string_of_int i) expected_name actual_name
  ) expected_display_names



let  test_midi_track_mixer_properties () =
  (* Load midi track XML file *)
  let xml = read_file test_midi_track_xml_path in

  (* Create midi track from XML *)
  let midi_track = MidiTrack.create xml in

  (* Check main mixer properties *)
  Alcotest.(check (float 0.001)) "mixer volume" 1.0 midi_track.mixer.Mixer.volume;
  Alcotest.(check (float 0.001)) "mixer pan" 0.0 midi_track.mixer.pan


let () =
  Alcotest.run "MidiTrack" [
    "track_creation", [
      Alcotest.test_case "parse basic MidiTrack properties" `Quick test_midi_track_basic_properties;
      Alcotest.test_case "parse MidiTrack clips and notes" `Quick test_midi_track_clips_and_notes;
      Alcotest.test_case "parse MidiTrack devices order" `Quick test_midi_track_devices_order;
      Alcotest.test_case "parse MidiTrack mixer properties" `Quick test_midi_track_mixer_properties
    ]
  ]
