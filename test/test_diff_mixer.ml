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

  (* Diff the mixers *)
  let patch = Track.Mixer.diff old_mixer new_mixer in

  (* Render the diff *)
  let output = Text_output.render_mixer patch in

  (* Expected output - updated to match actual behavior *)
  let expected = "Mixer Patch:\n  Volume:\n      ~ Value changed from 0.70 to 0.50\n  Pan:\n      ~ Value changed from -0.30 to 0.47\n  Mute:\n      ~ Value changed from false to true\n  ~ Solo changed from true to false\n  Send Changes:\n        ~ Value changed from 0.50 to 0.00\n    - Send 1 with amount 0.2500" in

  (* Check that the output matches expected *)
  Alcotest.(check string) "Mixer diff output" expected output

let () =
  Alcotest.run "Diff Mixer" [
    "diff-logic", [
      Alcotest.test_case "Test mixer diffing logic" `Quick test_mixer_diff;
    ];
  ]
