
open Alsdiff_live
open Alsdiff_output
open Alsdiff_live.Track



(* Helper to create a dummy Mixer *)
let make_mixer volume pan =
  {
    Mixer.volume = volume;
    Mixer.pan = pan;
    mute = false;
    solo = false;
    sends = [];
  }

(* Helper to create a dummy Midi Track *)
let make_midi_track id name mixer =
  Midi {
    MidiTrack.id = id;
    name = name;
    clips = [];
    automations = [];
    devices = [];
    mixer = mixer;
  }

let test_midi_track_diff () =
  let mixer1 = make_mixer 0.8 0.0 in
  let old_track = make_midi_track 1 "Midi Track" mixer1 in

  let mixer2 = make_mixer 0.5 0.0 in
  let new_track = make_midi_track 1 "Midi Track Renamed" mixer2 in

  let patch = Track.diff old_track new_track in
  let output = Text_output.render_track patch in

  let expected = "Midi Track Patch:\n  ~ Name changed from Midi Track to Midi Track Renamed\nMixer Patch:\n  ~ Volume changed from 0.8000 to 0.5000" in
  Alcotest.(check string) "Midi track diff output" expected output

let () =
  Alcotest.run "Diff Track" [
    "midi-track", [
      Alcotest.test_case "Test midi track diff" `Quick test_midi_track_diff;
    ];
  ]
