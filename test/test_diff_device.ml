
open Alsdiff_live
open Alsdiff_live.Device

(* Helper to create a dummy DeviceParam *)
let make_param name value =
  {
    DeviceParam.base = {
      GenericParam.name = name;
      value = value;
      automation = 0;
      modulation = 0;
      mapping = None;
    };
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

  let param1_mod = { DeviceParam.base = { param1.base with Device.GenericParam.value = Float 0.8 } } in
  let new_device = make_regular_device 1 "Overdrive" [param1_mod; param2] in

  let patch = Device.diff old_device new_device in

  (* Check that we got a RegularPatch *)
  (match patch with
   | Device.Patch.RegularPatch regular_patch ->
     (* The first param (Drive) should be Modified, second (Tone) should be Unchanged *)
     (* Check the Drive param change (first in params list) *)
     (match regular_patch.params with
      | drive_change :: _ ->
        (match drive_change with
         | `Modified param_patch ->
           (match param_patch.Device.DeviceParam.Patch.base with
            | `Modified generic_patch ->
              (match generic_patch.Device.GenericParam.Patch.value with
               | `Modified v ->
                 (match v.oldval with
                  | Device.Float old_val -> Alcotest.(check (float 0.01)) "old value" 0.5 old_val
                  | _ -> Alcotest.fail "Expected Float value");
                 (match v.newval with
                  | Device.Float new_val -> Alcotest.(check (float 0.01)) "new value" 0.8 new_val
                  | _ -> Alcotest.fail "Expected Float value")
               | _ -> Alcotest.fail "Expected value to be modified")
            | _ -> Alcotest.fail "Expected base to be modified")
         | _ -> Alcotest.fail "Expected param to be modified")
      | _ -> Alcotest.fail "Expected at least one param change")
   | _ -> Alcotest.fail "Expected RegularPatch")

let test_plugin_device_diff () =
  (* Mock Plugin Device *)
  (* Test that plugin device diffing works correctly *)
  let make_plugin_param id name index value =
    {
      PluginParam.id = id;
      index = index;
      base = {
        GenericParam.name = name;
        value = value;
        automation = 0;
        modulation = 0;
        mapping = None;
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

  (* Check that we got a PluginPatch *)
  (match patch with
   | Device.Patch.PluginPatch plugin_patch ->
     (* The Cutoff param should be Modified *)
     (* Check the Cutoff param change (first in params list) *)
     (match plugin_patch.params with
      | cutoff_change :: _ ->
        (match cutoff_change with
         | `Modified param_patch ->
           (match param_patch.Device.PluginParam.Patch.base with
            | `Modified generic_patch ->
              (match generic_patch.Device.GenericParam.Patch.value with
               | `Modified v ->
                 (match v.oldval with
                  | Device.Float old_val -> Alcotest.(check (float 0.01)) "old value" 0.5 old_val
                  | _ -> Alcotest.fail "Expected Float value");
                 (match v.newval with
                  | Device.Float new_val -> Alcotest.(check (float 0.01)) "new value" 0.7 new_val
                  | _ -> Alcotest.fail "Expected Float value")
               | _ -> Alcotest.fail "Expected value to be modified")
            | _ -> Alcotest.fail "Expected base to be modified")
         | _ -> Alcotest.fail "Expected param to be modified")
      | _ -> Alcotest.fail "Expected at least one param change")
   | _ -> Alcotest.fail "Expected PluginPatch")

let () =
  Alcotest.run "Diff Device" [
    "regular-device", [
      Alcotest.test_case "Test regular device diff" `Quick test_regular_device_diff;
    ];
    "plugin-device", [
      Alcotest.test_case "Test plugin device diff" `Quick test_plugin_device_diff;
    ];
  ]
