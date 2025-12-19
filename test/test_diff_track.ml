
open Alsdiff_live
open Alsdiff_output
open Alsdiff_live.Track



(* Helper to create a dummy Device.GenericParam.t *)
let make_generic_param name value =
  {
    Device.GenericParam.name = name;
    value = value;
    automation = 0;
    modulation = 0;
    mapping = None;
  }


(* Helper to create a dummy Track.Mixer *)
let make_mixer volume pan =
  {
    Track.Mixer.volume = make_generic_param "Volume" (Device.Float volume);
    Track.Mixer.pan = make_generic_param "Pan" (Device.Float pan);
    Track.Mixer.mute = make_generic_param "On" (Device.Bool false);
    Track.Mixer.solo = make_generic_param "Sololink" (Device.Bool false);
    Track.Mixer.sends = [];
  }

(* Helper to create a dummy RoutingSet.t *)
let make_empty_routing_set () =
  let make_routing route_type =
    {
      Track.Routing.route_type;
      target = "";
      upper_string = "";
      lower_string = "";
    }
  in
  {
    Track.RoutingSet.audio_in = make_routing Track.Routing.AudioIn;
    audio_out = make_routing Track.Routing.AudioOut;
    midi_in = make_routing Track.Routing.MidiIn;
    midi_out = make_routing Track.Routing.MidiOut;
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
    routings = make_empty_routing_set ();
  }

let test_midi_track_diff () =
  let mixer1 = make_mixer 0.8 0.0 in
  let old_track = make_midi_track 1 "Midi Track" mixer1 in

  let mixer2 = make_mixer 0.5 0.0 in
  let new_track = make_midi_track 1 "Midi Track Renamed" mixer2 in

  let patch = Track.diff old_track new_track in
  let output = Text_output.render_track patch in

  let expected = "Midi Track Patch:\n  ~ Name changed from Midi Track to Midi Track Renamed\n  Mixer Patch:\n    Volume:\n        ~ Value changed from 0.80 to 0.50" in
  Alcotest.(check string) "Midi track diff output" expected output

let () =
  Alcotest.run "Diff Track" [
    "midi-track", [
      Alcotest.test_case "Test midi track diff" `Quick test_midi_track_diff;
    ];
  ]
