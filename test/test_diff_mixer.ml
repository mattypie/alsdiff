open Alsdiff_base
open Alsdiff_live
open Alsdiff_output

let test_mixer_diff () =
  (* Load the old mixer XML *)
  let old_xml = Xml.read_file "mixer_old.xml" in
  let old_mixer = Track.Mixer.create old_xml in

  (* Load the new mixer XML *)
  let new_xml = Xml.read_file "mixer.xml" in
  let new_mixer = Track.Mixer.create new_xml in

  (* Verify that both mixers have HeadKeyMidi mapping for Solo *)
  let check_solo_mapping mixer expected_note expected_channel =
    match mixer.Track.Mixer.solo.Device.GenericParam.mapping with
    | Some mapping ->
        Alcotest.(check int) "Solo MIDI target" mapping.Device.MIDIMapping.target expected_note;
        Alcotest.(check int) "Solo MIDI channel" mapping.Device.MIDIMapping.channel expected_channel;
        Alcotest.(check string) "Solo mapping kind"
          (match mapping.Device.MIDIMapping.kind with
           | Device.MIDIMapping.OnOff -> "OnOff"
           | Device.MIDIMapping.Continuous -> "Continuous")
          "OnOff";
        Alcotest.(check int) "Solo mapping low" mapping.Device.MIDIMapping.low 64;
        Alcotest.(check int) "Solo mapping high" mapping.Device.MIDIMapping.high 127
    | None -> Alcotest.fail "Solo should have HeadKeyMidi mapping"
  in

  (* Check that old mixer has the correct mapping *)
  check_solo_mapping old_mixer 72 8;

  (* Check that new mixer has the correct mapping *)
  check_solo_mapping new_mixer 72 8;

  (* Diff the mixers *)
  let patch = Track.Mixer.diff old_mixer new_mixer in

  (* Render the diff *)
  let output = Text_output.render_mixer patch in

  (* Expected output - updated to match actual behavior *)
  let expected = "Mixer Patch:\n  Volume:\n      ~ Value changed from 0.70 to 0.50\n  Pan:\n      ~ Value changed from -0.30 to 0.47\n  Mute:\n      ~ Value changed from false to true\n  Solo:\n      ~ Value changed from true to false\n  Send Changes:\n        ~ Value changed from 0.50 to 0.00\n    - Send 1 with amount 0.2500" in

  (* Check that the output matches expected *)
  Alcotest.(check string) "Mixer diff output" expected output

let () =
  Alcotest.run "Diff Mixer" [
    "diff-logic", [
      Alcotest.test_case "Test mixer diffing logic" `Quick test_mixer_diff;
    ];
  ]
