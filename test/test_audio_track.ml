open Alsdiff_base
open Alsdiff_base.Xml
open Alsdiff_live.Track
open Alsdiff_live

let test_audio_track_xml_path =
  (* Try different paths to work with both dune exec and dune runtest *)
  if Sys.file_exists "test/audio_track.xml" then
    "test/audio_track.xml"
  else
    "audio_track.xml"

let test_audio_track_basic_properties () =
  (* Load audio track XML file *)
  let xml = read_file test_audio_track_xml_path in

  (* Create audio track from XML *)
  let audio_track = AudioTrack.create xml in

  (* Expected values based on XML file *)
  let expected_id = 41 in
  let expected_name = "21-Metal Scrapes Texture 01" in

  (* Test basic fields *)
  Alcotest.(check int) "id" expected_id audio_track.id;
  Alcotest.(check string) "name" expected_name audio_track.name

let test_audio_track_clips () =
  (* Load audio track XML file *)
  let xml = read_file test_audio_track_xml_path in

  (* Create audio track from XML *)
  let audio_track = AudioTrack.create xml in

  (* Check that we have the expected number of clips *)
  Alcotest.(check int) "clip count" 7 (List.length audio_track.clips);

  (* Test first clip details *)
  let first_clip = List.hd audio_track.clips in
  Alcotest.(check int) "first clip id" 22 first_clip.Clip.AudioClip.id;

  (* Test all clip IDs are present and in order *)
  let expected_clip_ids = [22; 15; 5; 28; 31; 36; 37] in
  let actual_clip_ids = List.map (fun clip -> clip.Clip.AudioClip.id) audio_track.clips in
  Alcotest.(check (list int)) "clip IDs order" expected_clip_ids actual_clip_ids;

  (* Test clip names are properly parsed - this catches the name parsing bug *)
  let expected_clip_names = [
    "Metal Scrapes Texture 01";
    "Foley Small Metal";
    "Broken Glass";
    "Broken Glass";
    "Delay Feedback Screamer 1";
    "Delay Feedback Screamer 1";
    "Delay Feedback Screamer 1"
  ] in
  let actual_clip_names = List.map (fun clip -> clip.Clip.AudioClip.name) audio_track.clips in
  Alcotest.(check (list string)) "clip names" expected_clip_names actual_clip_names

let test_audio_track_automations () =
  (* Load audio track XML file *)
  let xml = read_file test_audio_track_xml_path in

  (* Create audio track from XML *)
  let audio_track = AudioTrack.create xml in

  (* Check that automation envelopes are correctly parsed *)
  (* Based on XML analysis, there should be no automation envelopes *)
  Alcotest.(check int) "automation count" 0 (List.length audio_track.automations)

let test_audio_track_devices () =
  (* Load audio track XML file *)
  let xml = read_file test_audio_track_xml_path in

  (* Create audio track from XML *)
  let audio_track = AudioTrack.create xml in

  (* Check that devices are correctly parsed *)
  (* Based on XML analysis, the Devices element exists but appears empty *)
  Alcotest.(check int) "device count" 0 (List.length audio_track.devices)

let test_audio_track_mixer () =
  (* Load audio track XML file *)
  let xml = read_file test_audio_track_xml_path in

  (* Create audio track from XML *)
  let audio_track = AudioTrack.create xml in

  (* Test that mixer is properly created with expected values from XML *)
  let sends_count = List.length audio_track.mixer.sends in
  Alcotest.(check int) "sends count" 1 sends_count;

  (* Basic mixer property checks against actual XML values *)
  let volume = audio_track.mixer.volume in
  let pan = audio_track.mixer.pan in
  Alcotest.(check (float 0.001)) "volume value" 0.6309573054 volume;
  Alcotest.(check (float 0.001)) "pan value" 0.0 pan;

  (* Test the first send amount against XML *)
  if sends_count > 0 then (
    let first_send = List.hd audio_track.mixer.sends in
    Alcotest.(check (float 0.001)) "first send amount" 0.0003162277571 first_send.amount
  )

let test_audio_track_comprehensive () =
  (* Load audio track XML file *)
  let xml = read_file test_audio_track_xml_path in

  (* Create audio track from XML *)
  let audio_track = AudioTrack.create xml in

  (* Comprehensive test of all fields together *)
  Alcotest.(check int) "id" 41 audio_track.id;
  Alcotest.(check string) "name" "21-Metal Scrapes Texture 01" audio_track.name;
  Alcotest.(check int) "clip count" 7 (List.length audio_track.clips);
  Alcotest.(check int) "automation count" 0 (List.length audio_track.automations);
  Alcotest.(check int) "device count" 0 (List.length audio_track.devices);

  (* Verify first few clip IDs to ensure proper ordering *)
  let first_clip_id = (List.hd audio_track.clips).Clip.AudioClip.id in
  let second_clip_id = (List.nth audio_track.clips 1).Clip.AudioClip.id in
  let third_clip_id = (List.nth audio_track.clips 2).Clip.AudioClip.id in
  Alcotest.(check int) "first clip ID" 22 first_clip_id;
  Alcotest.(check int) "second clip ID" 15 second_clip_id;
  Alcotest.(check int) "third clip ID" 5 third_clip_id

let test_audio_track_edge_case_empty_clips () =
  (* Create a minimal audio track XML with no clips *)
  let empty_clips_xml = Xml.Element {
    name = "AudioTrack";
    parent = None;
    attrs = ["Id", "100"];
    childs = [
      Xml.Element { name = "Name"; parent = None; attrs = [];
        childs = [Xml.Element { name = "EffectiveName"; parent = None; attrs = ["Value", "Empty Track"]; childs = [] }] };
      Xml.Element { name = "AutomationEnvelopes"; parent = None; attrs = [];
        childs = [Xml.Element { name = "Envelopes"; parent = None; attrs = []; childs = [] }] };
      Xml.Element { name = "DeviceChain"; parent = None; attrs = [];
        childs = [
          Xml.Element { name = "Devices"; parent = None; attrs = []; childs = [] };
          Xml.Element { name = "Mixer"; parent = None; attrs = [];
            childs = [
              Xml.Element { name = "Volume"; parent = None; attrs = [];
                childs = [Xml.Element { name = "Manual"; parent = None; attrs = ["Value", "0.0"]; childs = [] }] };
              Xml.Element { name = "Pan"; parent = None; attrs = [];
                childs = [Xml.Element { name = "Manual"; parent = None; attrs = ["Value", "0.0"]; childs = [] }] };
              Xml.Element { name = "On"; parent = None; attrs = [];
                childs = [Xml.Element { name = "Manual"; parent = None; attrs = ["Value", "false"]; childs = [] }] };
              Xml.Element { name = "SoloSink"; parent = None; attrs = ["Value", "false"]; childs = [] };
              Xml.Element { name = "Sends"; parent = None; attrs = []; childs = [] }
            ] }
        ] }
    ]
  } in

  let audio_track = AudioTrack.create empty_clips_xml in
  Alcotest.(check int) "empty track id" 100 audio_track.id;
  Alcotest.(check string) "empty track name" "Empty Track" audio_track.name;
  Alcotest.(check int) "empty track clip count" 0 (List.length audio_track.clips);
  Alcotest.(check int) "empty track automation count" 0 (List.length audio_track.automations);
  Alcotest.(check int) "empty track device count" 0 (List.length audio_track.devices)

let () =
  Alcotest.run "AudioTrack" [
    "track_creation", [
      Alcotest.test_case "parse basic AudioTrack properties" `Quick test_audio_track_basic_properties;
      Alcotest.test_case "parse AudioTrack clips" `Quick test_audio_track_clips;
      Alcotest.test_case "parse AudioTrack automations" `Quick test_audio_track_automations;
      Alcotest.test_case "parse AudioTrack devices" `Quick test_audio_track_devices;
      Alcotest.test_case "parse AudioTrack mixer properties" `Quick test_audio_track_mixer;
      Alcotest.test_case "comprehensive AudioTrack parsing" `Quick test_audio_track_comprehensive;
      Alcotest.test_case "handle empty track edge case" `Quick test_audio_track_edge_case_empty_clips;
    ]
  ]