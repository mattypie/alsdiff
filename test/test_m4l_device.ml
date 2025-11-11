open Alsdiff_base.Xml
open Alsdiff_live

let test_m4l_device_xml_path =
  (* Try different paths to work with both dune exec and dune runtest *)
  if Sys.file_exists "test/m4l_device.xml" then
    "test/m4l_device.xml"
  else
    "m4l_device.xml"

let test_create_m4l_device () =
  (* Load the M4L device XML file *)
  let xml = read_file test_m4l_device_xml_path in

  (* Create a device from the XML *)
  let device = Device.create xml in

  (* Extract the RegularDevice from the variant *)
  let regular_device = match device with
    | Device.Regular reg -> reg
    |  _ -> failwith "Expected Regular device"
  in

  (* Verify the device properties *)
  Alcotest.(check int) "device id" 0 regular_device.id;
  Alcotest.(check string) "device name" "MxDeviceInstrument" regular_device.device_name;
  Alcotest.(check string) "display name" "Dark Forces" regular_device.display_name;
  Alcotest.(check int) "pointee id" 84269 regular_device.pointee;

  Alcotest.(check bool) "preset exists" true (Option.is_some regular_device.preset);

  let preset = Option.get regular_device.preset in
  (* Verify preset information *)
  Alcotest.(check int) "preset id" 1 preset.Device.PresetRef.id;
  Alcotest.(check string) "preset name" "Dark Forces" preset.Device.PresetRef.name;
  Alcotest.(check string) "preset relative path" "Presets/Instruments/Max Instrument/Cellular Degradation/Presets/Dark Forces.adv" preset.Device.PresetRef.relative_path;
  Alcotest.(check string) "preset path" "/Users/krfantasy/Music/Ableton/User Library/Presets/Instruments/Max Instrument/Cellular Degradation/Presets/Dark Forces.adv" preset.Device.PresetRef.path;
  Alcotest.(check string) "preset pack name" "" preset.Device.PresetRef.pack_name;
  Alcotest.(check int) "preset pack id" 0 preset.Device.PresetRef.pack_id;
  Alcotest.(check int) "preset file size" 0 preset.Device.PresetRef.file_size;
  Alcotest.(check int) "preset crc" 0 preset.Device.PresetRef.crc;

  (* Check preset type *)
  (match preset.Device.PresetRef.preset_type with
   | Device.PresetRef.UserPreset -> () (* Expected for M4L devices *)
   | Device.PresetRef.DefaultPreset -> Alcotest.fail "Expected UserPreset for M4L device");

  (* Verify enabled status *)
  (match regular_device.enabled.Device.DeviceParam.value with
   | Device.DeviceParam.Bool v -> Alcotest.(check bool) "device enabled" false v
   | _ -> Alcotest.fail "enabled parameter should be bool");
  Alcotest.(check int) "enabled automation id" 84268 regular_device.enabled.Device.DeviceParam.automation;

  (* Verify we have parameters *)
  Alcotest.(check (int)) "parameter count" 53 (List.length regular_device.params);

  (* Check a few specific parameters *)
  let open Device.DeviceParam in

  (* Check On parameter - this should be the first parameter *)
  let on_param = List.find (fun p -> p.name = "On") regular_device.params in
  (match on_param.value with
   | Bool v -> Alcotest.(check bool) "On parameter value" false v
   | _ -> Alcotest.fail "On parameter should be bool");
  Alcotest.(check int) "On parameter automation id" 84268 on_param.automation;

  (* Check Timeable parameters - this is what we actually get from the M4L device *)
  let timeable_param = List.find (fun p -> p.name = "Timeable") regular_device.params in
  (match timeable_param.value with
   | Float v ->
     (* Should be 22.0 for AM Mod Offset, but there are many Timeable parameters *)
     Alcotest.(check (float 0.01)) "Timeable parameter value" 22.0 v
   | _ -> Alcotest.fail "Timeable parameter should be float");
  Alcotest.(check int) "Timeable parameter automation id" 84270 timeable_param.automation;

  (* Verify the structure: we should have 1 On parameter and 52 Timeable parameters *)
  let on_params = List.filter (fun p -> p.name = "On") regular_device.params in
  let timeable_params = List.filter (fun p -> p.name = "Timeable") regular_device.params in
  Alcotest.(check int) "On parameters count" 1 (List.length on_params);
  Alcotest.(check int) "Timeable parameters count" 52 (List.length timeable_params)

let test_m4l_device_param_creation () =
  (* Test creating individual parameters from M4L device XML *)
  let xml = read_file test_m4l_device_xml_path in

  (* Extract a Timeable parameter from the M4L device *)
  let timeable_xmls = Alsdiff_base.Upath.find_all "/**/Timeable" xml in
  let timeable_xml = match timeable_xmls with
    | [(_, xml_elem)] -> xml_elem
    | [] -> failwith "No Timeable elements found"
    | _ -> snd (List.hd timeable_xmls) in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create timeable_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "param name" "Timeable" param.name;
  (match param.value with
   | Float v -> Alcotest.(check (float 0.01)) "param value" 22.0 v (* First Timeable should have value 22.0 *)
   | _ -> Alcotest.fail "parameter should be float");

  (* Verify no macro mapping *)
  (match param.mapping with
   | None -> () (* Expected - no macro mapping *)
   | Some _ -> Alcotest.fail "parameter should not have macro mapping")

let test_m4l_device_boolean_parameter () =
  (* Test the On parameter which should be boolean *)
  let xml = read_file test_m4l_device_xml_path in

  (* Extract the On parameter *)
  let on_xml = snd (Alsdiff_base.Upath.find "/On" xml) in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create on_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "param name" "On" param.name;
  (match param.value with
   | Bool v -> Alcotest.(check bool) "on value" false v
   | _ -> Alcotest.fail "On parameter should be bool");
  Alcotest.(check int) "on automation id" 84268 param.automation

let test_m4l_device_preset_ref () =
  (* Test creating PresetRef from M4L device XML *)
  let xml = read_file test_m4l_device_xml_path in

  (* Extract the FilePresetRef element *)
  let preset_ref_xml = snd (Alsdiff_base.Upath.find "/LastPresetRef/Value/FilePresetRef" xml) in

  (* Create a preset reference from the XML *)
  let preset = Device.PresetRef.create preset_ref_xml in

  (* Verify preset properties *)
  Alcotest.(check int) "preset id" 1 preset.id;
  Alcotest.(check string) "preset name" "Dark Forces" preset.name;
  (match preset.preset_type with
   | Device.PresetRef.UserPreset -> () (* Expected *)
   | Device.PresetRef.DefaultPreset -> Alcotest.fail "Expected UserPreset");
  Alcotest.(check string) "preset relative path" "Presets/Instruments/Max Instrument/Cellular Degradation/Presets/Dark Forces.adv" preset.relative_path;
  Alcotest.(check string) "preset path" "/Users/krfantasy/Music/Ableton/User Library/Presets/Instruments/Max Instrument/Cellular Degradation/Presets/Dark Forces.adv" preset.path

let test_m4l_device_parameter_values () =
  (* Test that parameter values are correctly parsed from Manual elements *)
  let xml = read_file test_m4l_device_xml_path in

  (* Extract Timeable parameters and verify they have the expected values *)
  let timeable_xmls = Alsdiff_base.Upath.find_all "/**/Timeable" xml in

  (* Check that we can find parameters with specific values *)
  let found_value_22 = List.exists (fun (_, xml_elem) ->
    let open Device.DeviceParam in
    let param = create xml_elem in
    match param.value with
    | Float v -> abs_float (v -. 22.0) < 0.01
    | _ -> false
  ) timeable_xmls in

  let found_value_23 = List.exists (fun (_, xml_elem) ->
    let open Device.DeviceParam in
    let param = create xml_elem in
    match param.value with
    | Float v -> abs_float (v -. 23.0) < 0.01
    | _ -> false
  ) timeable_xmls in

  Alcotest.(check bool) "found parameter with value 22.0" true found_value_22;
  Alcotest.(check bool) "found parameter with value 23.0" true found_value_23

let () =
  Alcotest.run "M4LDevice" [
    "device_creation", [
      Alcotest.test_case "create M4L device from XML" `Quick test_create_m4l_device;
    ];
    "parameter_tests", [
      Alcotest.test_case "create M4L parameter from XML" `Quick test_m4l_device_param_creation;
      Alcotest.test_case "create M4L boolean parameter" `Quick test_m4l_device_boolean_parameter;
      Alcotest.test_case "test M4L parameter values" `Quick test_m4l_device_parameter_values;
    ];
    "preset_tests", [
      Alcotest.test_case "create M4L preset reference" `Quick test_m4l_device_preset_ref;
    ];
  ]
