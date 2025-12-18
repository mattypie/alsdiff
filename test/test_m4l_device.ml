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

  (* Extract the Max4LiveDevice from the variant *)
  let m4l_device = match device with
    | Device.Max4Live m4l -> m4l
    |  _ -> failwith "Expected Max4Live device"
  in

  (* Verify the device properties *)
  Alcotest.(check int) "device id" 0 m4l_device.id;
  Alcotest.(check string) "device name" "MxDeviceInstrument" m4l_device.device_name;
  Alcotest.(check string) "display name" "Dark Forces" m4l_device.display_name;
  Alcotest.(check int) "pointee id" 84269 m4l_device.pointee;

  Alcotest.(check bool) "preset exists" true (Option.is_some m4l_device.preset);

  let preset = Option.get m4l_device.preset in
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
  (match m4l_device.enabled.Device.DeviceParam.base.Device.GenericParam.value with
   | Device.Bool v -> Alcotest.(check bool) "device enabled" false v
   | _ -> Alcotest.fail "enabled parameter should be bool");
  Alcotest.(check int) "enabled automation id" 84268 m4l_device.enabled.Device.DeviceParam.base.Device.GenericParam.automation;

  (* Verify we have parameters *)
  Alcotest.(check (int)) "parameter count" 52 (List.length m4l_device.params);

  (* Check a few specific parameters *)
  let open Device.Max4LiveParam in

  (* Note: On parameter is handled as the enabled field, not in params list *)

  (* Check parameters with specific names - this is what we actually get from the M4L device *)
  let am_mod_offset_param = List.find (fun p -> p.base.Device.GenericParam.name = "AM Mod Offset") m4l_device.params in
  (match am_mod_offset_param.base.Device.GenericParam.value with
   | Device.Float v ->
     (* Should be 22 for AM Mod Offset from the test XML *)
     Alcotest.(check (float 0.01)) "AM Mod Offset parameter value" 22.0 v
   | _ -> Alcotest.fail "AM Mod Offset parameter should be float");
  Alcotest.(check int) "AM Mod Offset parameter automation id" 84270 am_mod_offset_param.base.Device.GenericParam.automation;

  (* Check enum parameter *)
  let clear_grid_param = List.find (fun p -> p.base.Device.GenericParam.name = "Clear Grid") m4l_device.params in
  (match clear_grid_param.base.Device.GenericParam.value with
   | Device.Enum (v, desc) ->
     Alcotest.(check int) "Clear Grid enum value" 0 v;
     Alcotest.(check int) "Clear Grid enum min" 0 desc.min;
     Alcotest.(check int) "Clear Grid enum max" 1 desc.max;
     Alcotest.(check string) "Clear Grid enum 0" "off" desc.enums.(0);
     Alcotest.(check string) "Clear Grid enum 1" "on" desc.enums.(1)
   | _ -> Alcotest.fail "Clear Grid parameter should be enum");

  (* Verify the parameter count matches our expectations *)
  Alcotest.(check int) "AM Mod Offset parameters count" 1 (List.length (List.filter (fun p -> p.base.Device.GenericParam.name = "AM Mod Offset") m4l_device.params));
  Alcotest.(check int) "Clear Grid parameters count" 1 (List.length (List.filter (fun p -> p.base.Device.GenericParam.name = "Clear Grid") m4l_device.params))

let test_m4l_device_param_creation () =
  (* Test creating individual parameters from M4L device XML *)
  let xml = read_file test_m4l_device_xml_path in

  (* Extract M4L parameters from the device *)
  let float_params = Alsdiff_base.Upath.find_all "**/MxDFloatParameter" xml in
  let int_params = Alsdiff_base.Upath.find_all "**/MxDIntParameter" xml in
  let bool_params = Alsdiff_base.Upath.find_all "**/MxDBoolParameter" xml in
  let enum_params = Alsdiff_base.Upath.find_all "**/MxDEnumParameter" xml in
  let param_xmls = float_params @ int_params @ bool_params @ enum_params in
  let first_param_xml = match param_xmls with
    | [(_, xml_elem)] -> xml_elem
    | [] -> failwith "No M4L parameter elements found"
    | (_, xml_elem) :: _ -> xml_elem in

  (* Create a parameter from the XML *)
  let open Device.Max4LiveParam in
  let param = create "" first_param_xml in

  (* Verify parameter properties *)
  (match param.base.Device.GenericParam.value with
   | Device.Float _ -> () (* Could be any float value *)
   | Device.Int _ -> () (* Could be any int value *)
   | Device.Bool _ -> () (* Could be any bool value *)
   | Device.Enum (_, _) -> () (* Could be any enum *)
   );

  (* Verify basic structure *)
  Alcotest.(check bool) "parameter id > 0" true (param.id > 0);
  Alcotest.(check bool) "parameter name not empty" true (String.length param.base.Device.GenericParam.name > 0)

let test_m4l_device_boolean_parameter () =
  (* Test finding enum parameters which should have boolean-like behavior *)
  let xml = read_file test_m4l_device_xml_path in

  (* Extract enum parameters from the M4L device *)
  let enum_param_xmls = Alsdiff_base.Upath.find_all "**/MxDEnumParameter" xml in
  let first_enum_xml = match enum_param_xmls with
    | [(_, xml_elem)] -> xml_elem
    | [] -> failwith "No enum parameter elements found"
    | (_, xml_elem) :: _ -> xml_elem in

  (* Create a parameter from the XML *)
  let open Device.Max4LiveParam in
  let param = create "" first_enum_xml in

  (* Verify parameter properties *)
  (match param.base.Device.GenericParam.value with
   | Device.Enum (v, desc) ->
     (* Should be 0 or 1 for boolean-like enums *)
     Alcotest.(check bool) "enum value in range" true (v >= 0 && v <= 1);
     Alcotest.(check int) "enum min" 0 desc.min;
     Alcotest.(check bool) "enum max at least 1" true (desc.max >= 1)
   | _ -> Alcotest.fail "Enum parameter should be enum")

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
  (* Test that parameter values are correctly parsed from M4L parameters *)
  let xml = read_file test_m4l_device_xml_path in

  (* Extract M4L parameters and verify they have the expected values *)
  let float_params = Alsdiff_base.Upath.find_all "**/MxDFloatParameter" xml in
  let int_params = Alsdiff_base.Upath.find_all "**/MxDIntParameter" xml in
  let bool_params = Alsdiff_base.Upath.find_all "**/MxDBoolParameter" xml in
  let enum_params = Alsdiff_base.Upath.find_all "**/MxDEnumParameter" xml in
  let param_xmls = float_params @ int_params @ bool_params @ enum_params in

  (* Check that we can find parameters with specific names and values *)
  let found_am_mod_offset = List.exists (fun (_, xml_elem) ->
    let open Device.Max4LiveParam in
    let param = create "" xml_elem in
    param.base.Device.GenericParam.name = "AM Mod Offset"
  ) param_xmls in

  let found_clear_grid = List.exists (fun (_, xml_elem) ->
    let open Device.Max4LiveParam in
    let param = create "" xml_elem in
    param.base.Device.GenericParam.name = "Clear Grid"
  ) param_xmls in

  let found_float_param = List.exists (fun (_, xml_elem) ->
    let open Device.Max4LiveParam in
    let param = create "" xml_elem in
    match param.base.Device.GenericParam.value with
    | Device.Float _ -> true
    | _ -> false
  ) param_xmls in

  let found_enum_param = List.exists (fun (_, xml_elem) ->
    let open Device.Max4LiveParam in
    let param = create "" xml_elem in
    match param.base.Device.GenericParam.value with
    | Device.Enum _ -> true
    | _ -> false
  ) param_xmls in

  Alcotest.(check bool) "found AM Mod Offset parameter" true found_am_mod_offset;
  Alcotest.(check bool) "found Clear Grid parameter" true found_clear_grid;
  Alcotest.(check bool) "found float parameter" true found_float_param;
  Alcotest.(check bool) "found enum parameter" true found_enum_param

let test_m4l_device_patch_ref () =
  (* Test creating PatchRef from M4L device XML *)
  let xml = read_file test_m4l_device_xml_path in

  (* Extract the MxPatchRef element *)
  let patch_ref_xml = snd (Alsdiff_base.Upath.find "/PatchSlot/Value/MxPatchRef" xml) in

  (* Create a patch reference from the XML *)
  let open Device.PatchRef in
  let patch_ref = create patch_ref_xml in

  (* Verify patch reference properties *)
  Alcotest.(check int) "patch ref id" 4 patch_ref.id;
  Alcotest.(check string) "patch ref name" "Cellular Degradation" patch_ref.name;
  (match patch_ref.preset_type with
   | Device.PresetRef.UserPreset -> () (* Expected *)
   | Device.PresetRef.DefaultPreset -> Alcotest.fail "Expected UserPreset");
  Alcotest.(check string) "patch ref relative path" "Cellular Degradation.amxd" patch_ref.relative_path;
  Alcotest.(check string) "patch ref path" "/Users/krfantasy/Desktop/Prelude/Thick Air Project/Cellular Degradation.amxd" patch_ref.path;
  Alcotest.(check int) "patch ref pack id" 0 patch_ref.pack_id;
  Alcotest.(check int) "patch ref file size" 444848 patch_ref.file_size;
  Alcotest.(check int) "patch ref crc" 3851 patch_ref.crc;
  Alcotest.(check int64) "patch ref last mod date" 1742403843L patch_ref.last_mod_date

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
    "patch_ref_tests", [
      Alcotest.test_case "create M4L patch reference" `Quick test_m4l_device_patch_ref;
    ];
  ]
