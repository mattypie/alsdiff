
open Alsdiff_live
open Alsdiff_output
open Alsdiff_live.Device

(* Helper to create a dummy DeviceParam *)
let make_param name value =
  {
    DeviceParam.base = {
      GenericParam.name = name;
      value = value;
      automation = 0;
      modulation = 0;
    };
    mapping = None;
  }

(* Helper to create a dummy Regular Device *)
let make_regular_device id name params =
  Regular {
    id = id;
    device_name = name;
    display_name = name;
    pointee = id;
    enabled = make_param "Device On" (Bool true);
    params = params;
    preset = None;
  }

let test_regular_device_diff () =
  let param1 = make_param "Drive" (Float 0.5) in
  let param2 = make_param "Tone" (Float 0.2) in

  let old_device = make_regular_device 1 "Overdrive" [param1; param2] in

  let param1_mod = { param1 with base = { param1.base with Device.GenericParam.value = Float 0.8 } } in
  let new_device = make_regular_device 1 "Overdrive" [param1_mod; param2] in

  let patch = Device.diff old_device new_device in
  let output = Text_output.render_device patch in

  let expected = "Regular Device Patch:\n  Parameters Changes:\n      ~ Value changed from 0.50 to 0.80" in
  Alcotest.(check string) "Regular device diff output" expected output

let test_plugin_device_diff () =
  (* Mock Plugin Device *)
  (* For simplicity, we'll just test that it renders correctly *)
  let make_plugin_param id name index value =
    {
      PluginParam.id = id;
      index = index;
      base = {
        GenericParam.name = name;
        value = value;
        automation = 0;
        modulation = 0;
      };
    }
  in

  let old_plugin = Plugin {
    id = 2;
    device_name = "VST Plugin";
    display_name = "MySynth";
    pointee = 2;
    enabled = make_param "Device On" (Bool true);
    desc = {
      PluginDesc.name = "MySynth";
      uid = "vst3:mysynth";
      plugin_type = PluginDesc.Vst3;
      state = "";
    };
    params = [make_plugin_param 1 "Cutoff" 1 (Float 0.5)];
    preset = None;
  } in

  let new_plugin = Plugin {
    id = 2;
    device_name = "VST Plugin";
    display_name = "MySynth";
    pointee = 2;
    enabled = make_param "Device On" (Bool true);
    desc = {
      PluginDesc.name = "MySynth";
      uid = "vst3:mysynth";
      plugin_type = PluginDesc.Vst3;
      state = "";
    };
    params = [make_plugin_param 1 "Cutoff" 1 (Float 0.7)];
    preset = None;
  } in

  let patch = Device.diff old_plugin new_plugin in
  let output = Text_output.render_device patch in

  let expected = "Plugin Device Patch:\n  Parameters Changes:\n        ~ Value changed from 0.50 to 0.70" in
  Alcotest.(check string) "Plugin device diff output" expected output

let () =
  Alcotest.run "Diff Device" [
    "regular-device", [
      Alcotest.test_case "Test regular device diff" `Quick test_regular_device_diff;
    ];
    "plugin-device", [
      Alcotest.test_case "Test plugin device diff" `Quick test_plugin_device_diff;
    ];
  ]
