open Alsdiff_base
open Alsdiff_live

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

  (* Check volume change *)
  (match patch.Track.Mixer.Patch.volume with
   | `Modified volume_patch ->
     (match volume_patch.Device.GenericParam.Patch.value with
      | `Modified v ->
        (match v.oldval with
         | Device.Float old_val -> Alcotest.(check (float 0.01)) "old volume" 0.70 old_val
         | _ -> Alcotest.fail "Expected Float value");
        (match v.newval with
         | Device.Float new_val -> Alcotest.(check (float 0.01)) "new volume" 0.50 new_val
         | _ -> Alcotest.fail "Expected Float value")
      | _ -> Alcotest.fail "Expected volume value to be modified")
   | _ -> Alcotest.fail "Expected volume to be modified");

  (* Check pan change *)
  (match patch.Track.Mixer.Patch.pan with
   | `Modified pan_patch ->
     (match pan_patch.Device.GenericParam.Patch.value with
      | `Modified v ->
        (match v.oldval with
         | Device.Float old_val -> Alcotest.(check (float 0.01)) "old pan" (-0.30) old_val
         | _ -> Alcotest.fail "Expected Float value");
        (match v.newval with
         | Device.Float new_val -> Alcotest.(check (float 0.01)) "new pan" 0.47 new_val
         | _ -> Alcotest.fail "Expected Float value")
      | _ -> Alcotest.fail "Expected pan value to be modified")
   | _ -> Alcotest.fail "Expected pan to be modified");

  (* Check mute change *)
  (match patch.Track.Mixer.Patch.mute with
   | `Modified mute_patch ->
     (match mute_patch.Device.GenericParam.Patch.value with
      | `Modified v ->
        (match v.oldval with
         | Device.Bool old_val -> Alcotest.(check bool) "old mute" false old_val
         | _ -> Alcotest.fail "Expected Bool value");
        (match v.newval with
         | Device.Bool new_val -> Alcotest.(check bool) "new mute" true new_val
         | _ -> Alcotest.fail "Expected Bool value")
      | _ -> Alcotest.fail "Expected mute value to be modified")
   | _ -> Alcotest.fail "Expected mute to be modified");

  (* Check solo change *)
  (match patch.Track.Mixer.Patch.solo with
   | `Modified solo_patch ->
     (match solo_patch.Device.GenericParam.Patch.value with
      | `Modified v ->
        (match v.oldval with
         | Device.Bool old_val -> Alcotest.(check bool) "old solo" true old_val
         | _ -> Alcotest.fail "Expected Bool value");
        (match v.newval with
         | Device.Bool new_val -> Alcotest.(check bool) "new solo" false new_val
         | _ -> Alcotest.fail "Expected Bool value")
      | _ -> Alcotest.fail "Expected solo value to be modified")
   | _ -> Alcotest.fail "Expected solo to be modified");

  (* Check sends - should have 2 changes: one modified (Send 0) and one removed (Send 1) *)
  (match patch.Track.Mixer.Patch.sends with
   | [send0_change; send1_change] ->
     (* Check Send 0 is Modified *)
     (match send0_change with
      | `Modified send_patch ->
        (match send_patch.Track.Send.Patch.amount with
         | `Modified amount_patch ->
           (match amount_patch.Device.GenericParam.Patch.value with
            | `Modified v ->
              (match v.oldval with
               | Device.Float old_val -> Alcotest.(check (float 0.01)) "Send 0 old amount" 0.5 old_val
               | _ -> Alcotest.fail "Expected Float value");
              (match v.newval with
               | Device.Float new_val -> Alcotest.(check (float 0.01)) "Send 0 new amount" 0.000316 new_val
               | _ -> Alcotest.fail "Expected Float value")
            | _ -> Alcotest.fail "Expected amount to be modified")
         | _ -> Alcotest.fail "Expected amount to be modified")
      | _ -> Alcotest.fail "Expected Send 0 to be modified");

     (* Check Send 1 is Removed *)
     (match send1_change with
      | `Removed send ->
        Alcotest.(check int) "removed send id" 1 send.Track.Send.id
      | _ -> Alcotest.fail "Expected send to be removed")
   | _ -> Alcotest.fail "Expected exactly two send changes")

let () =
  Alcotest.run "Diff Mixer" [
    "diff-logic", [
      Alcotest.test_case "Test mixer diffing logic" `Quick test_mixer_diff;
    ];
  ]
