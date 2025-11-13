open Alsdiff_base
open Alsdiff_base.Xml
open Alsdiff_live

(* Helper functions for different device XML paths *)
let test_compressor_device_xml_path =
  (* Try different paths to work with both dune exec and dune runtest *)
  if Sys.file_exists "test/compressor_device.xml" then
    "test/compressor_device.xml"
  else
    "compressor_device.xml"

let test_eq8_device_xml_path =
  if Sys.file_exists "test/eq8_device.xml" then
    "test/eq8_device.xml"
  else
    "eq8_device.xml"

let test_wavetable_device_xml_path =
  if Sys.file_exists "test/wavetable_device.xml" then
    "test/wavetable_device.xml"
  else
    "wavetable_device.xml"

(* Generic parameter extraction function *)
let extract_parameter_xml device_path parameter_name =
  let device_xml = read_file device_path in
  let param_path = Printf.sprintf "/%s" parameter_name in
  (param_path, snd (Upath.find param_path device_xml))

(* Convenience functions for specific devices *)
let extract_compressor_parameter_xml parameter_name =
  extract_parameter_xml test_compressor_device_xml_path parameter_name

let extract_eq8_parameter_xml parameter_name =
  extract_parameter_xml test_eq8_device_xml_path parameter_name

let extract_wavetable_parameter_xml parameter_name =
  extract_parameter_xml test_wavetable_device_xml_path parameter_name

let test_create_device_from_compressor_xml () =
  (* Load the compressor XML file *)
  let xml = read_file test_compressor_device_xml_path in

  (* Create a device from the XML *)
  let device = Device.create xml in

  (* Extract the RegularDevice from the variant *)
  let regular_device = match device with
    | Device.Regular reg -> reg
    | _ -> failwith "Expected Regular device"
  in

  (* Verify the device properties *)
  Alcotest.(check int) "device id" 2 regular_device.id;
  Alcotest.(check string) "device name" "Compressor2" regular_device.device_name;
  Alcotest.(check string) "display name" "Compressor" regular_device.display_name;
  Alcotest.(check int) "pointee id" 153207 regular_device.pointee;

  (* Verify we have parameters *)
  Alcotest.(check (int)) "parameter count" 25 (List.length regular_device.params);

  (* Check a few specific parameters *)
  let open Device.DeviceParam in
  let threshold_param = List.find (fun p -> p.name = "Threshold") regular_device.params in
  (match threshold_param.value with
   | Float v -> Alcotest.(check (float 0.01)) "threshold value" 1.0 v
   | _ -> Alcotest.fail "threshold parameter should be float");
  Alcotest.(check int) "threshold automation id" 153208 threshold_param.automation;

  let ratio_param = List.find (fun p -> p.name = "Ratio") regular_device.params in
  (match ratio_param.value with
   | Float v -> Alcotest.(check (float 0.01)) "ratio value" 2.50000024 v
   | _ -> Alcotest.fail "ratio parameter should be float");
  Alcotest.(check int) "ratio automation id" 153210 ratio_param.automation;

  let attack_param = List.find (fun p -> p.name = "Attack") regular_device.params in
  (match attack_param.value with
   | Float v -> Alcotest.(check (float 0.01)) "attack value" 0.9641109109 v
   | _ -> Alcotest.fail "attack parameter should be float");
  Alcotest.(check int) "attack automation id" 153214 attack_param.automation

let test_device_param_creation () =
  (* Use Threshold parameter from real compressor device XML *)
  let param_path, param_xml = extract_compressor_parameter_xml "Threshold" in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create param_path param_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "param name" "Threshold" param.name;
  (match param.value with
   | Float v -> Alcotest.(check (float 0.01)) "param value" 1.0 v (* Threshold value from real XML *)
   | _ -> Alcotest.fail "parameter should be float");
  Alcotest.(check int) "param automation id" 153208 param.automation

let test_device_param_without_macro_mapping () =
  (* Use Ratio parameter from real compressor device XML - it has no KeyMidi/macro mapping *)
  let param_path, param_xml = extract_compressor_parameter_xml "Ratio" in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create param_path param_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "param name" "Ratio" param.name;
  (match param.value with
   | Float v -> Alcotest.(check (float 0.01)) "param value" 2.50000024 v (* Ratio value from real XML *)
   | _ -> Alcotest.fail "parameter should be float");
  Alcotest.(check int) "param automation id" 153210 param.automation;
  (* Verify no macro mapping - Ratio parameter has no KeyMidi element *)
  (match param.mapping with
   | None -> () (* Expected - no macro mapping *)
   | Some _ -> Alcotest.fail "parameter should not have macro mapping");
  ()

(* ==================== EQ8 Device Tests ==================== *)

let test_create_eq8_device_from_xml () =
  (* Load the EQ8 XML file *)
  let xml = read_file test_eq8_device_xml_path in

  (* Create a device from the XML *)
  let device = Device.create xml in

  (* Extract the RegularDevice from the variant *)
  let regular_device = match device with
    | Device.Regular reg -> reg
    | _ -> failwith "Expected Regular device"
  in

  (* Verify the device properties *)
  Alcotest.(check int) "eq8 device id" 4 regular_device.id;
  Alcotest.(check string) "eq8 device name" "Eq8" regular_device.device_name;
  Alcotest.(check string) "eq8 display name" "EQ Eight" regular_device.display_name;
  Alcotest.(check int) "eq8 pointee id" 153285 regular_device.pointee;


  Alcotest.(check bool) "preset exists" true (Option.is_some regular_device.preset);

  let preset = Option.get regular_device.preset in
  (* Verify preset reference is AbletonDefaultPresetRef *)
  (match preset.preset_type with
   | Device.PresetRef.DefaultPreset -> () (* Expected *)
   | Device.PresetRef.UserPreset -> Alcotest.fail "Expected DefaultPreset, got UserPreset");

  Alcotest.(check int) "eq8 preset id" 1 preset.id;
  Alcotest.(check string) "eq8 preset name" "EQ Eight" preset.name;

  (* Verify we have the expected number of parameters *)
  Alcotest.(check (int)) "eq8 parameter count" 85 (List.length regular_device.params);

  (* Verify device is enabled *)
  let open Device.DeviceParam in
  (match regular_device.enabled.value with
   | Bool v -> Alcotest.(check bool) "eq8 device enabled" true v
   | _ -> Alcotest.fail "eq8 enabled parameter should be bool");
  Alcotest.(check int) "eq8 enabled automation id" 153284 regular_device.enabled.automation

let test_eq8_global_gain_parameter () =
  (* Extract GlobalGain parameter from EQ8 XML *)
  let param_path, param_xml = extract_eq8_parameter_xml "GlobalGain" in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create param_path param_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "eq8 global gain param name" "GlobalGain" param.name;
  (match param.value with
   | Float v -> Alcotest.(check (float 0.01)) "eq8 global gain value" 4.5 v
   | _ -> Alcotest.fail "eq8 global gain parameter should be float");
  Alcotest.(check int) "eq8 global gain automation id" 153286 param.automation

let test_eq8_scale_parameter () =
  (* Extract Scale parameter from EQ8 XML *)
  let param_path, param_xml = extract_eq8_parameter_xml "Scale" in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create param_path param_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "eq8 scale param name" "Scale" param.name;
  (match param.value with
   | Float v -> Alcotest.(check (float 0.01)) "eq8 scale value" 1.0 v
   | _ -> Alcotest.fail "eq8 scale parameter should be float");
  Alcotest.(check int) "eq8 scale automation id" 153288 param.automation

let test_eq8_band_parameters () =
  (* Extract first band's frequency parameter from EQ8 XML *)
  let device_xml = read_file test_eq8_device_xml_path in
  let freq_path, freq_param_xml = Upath.find "/Bands.0/ParameterA/Freq" device_xml in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let freq_param = create freq_path freq_param_xml in

  (* Verify frequency parameter properties *)
  Alcotest.(check string) "eq8 freq param name" "Bands.0/ParameterA/Freq" freq_param.name;
  (match freq_param.value with
   | Float v -> Alcotest.(check (float 0.001)) "eq8 band 0 frequency value" 77.237999 v
   | _ -> Alcotest.fail "eq8 frequency parameter should be float");
  Alcotest.(check int) "eq8 band 0 frequency automation id" 153292 freq_param.automation;

  (* Extract first band's gain parameter *)
  let gain_path, gain_param_xml = Upath.find "/Bands.0/ParameterA/Gain" device_xml in
  let gain_param = create gain_path gain_param_xml in

  (* Verify gain parameter properties *)
  Alcotest.(check string) "eq8 gain param name" "Bands.0/ParameterA/Gain" gain_param.name;
  (match gain_param.value with
   | Float v -> Alcotest.(check (float 0.01)) "eq8 band 0 gain value" (-2.85714293) v
   | _ -> Alcotest.fail "eq8 gain parameter should be float");
  Alcotest.(check int) "eq8 band 0 gain automation id" 153294 gain_param.automation;

  (* Extract first band's Q parameter *)
  let q_path, q_param_xml = Upath.find "/Bands.0/ParameterA/Q" device_xml in
  let q_param = create q_path q_param_xml in

  (* Verify Q parameter properties *)
  Alcotest.(check string) "eq8 q param name" "Bands.0/ParameterA/Q" q_param.name;
  (match q_param.value with
   | Float v -> Alcotest.(check (float 0.001)) "eq8 band 0 Q value" 0.7071067095 v
   | _ -> Alcotest.fail "eq8 Q parameter should be float");
  Alcotest.(check int) "eq8 band 0 Q automation id" 153296 q_param.automation

let test_eq8_mode_parameters () =
  (* Extract first band's mode parameter from EQ8 XML *)
  let device_xml = read_file test_eq8_device_xml_path in
  let mode_path, mode_param_xml = Upath.find "/Bands.0/ParameterA/Mode" device_xml in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let mode_param = create mode_path mode_param_xml in

  (* Verify mode parameter properties *)
  Alcotest.(check string) "eq8 mode param name" "Bands.0/ParameterA/Mode" mode_param.name;
  (match mode_param.value with
   | Float v -> Alcotest.(check (float 0.01)) "eq8 band 0 mode value" 2.0 v
   | _ -> Alcotest.fail "eq8 mode parameter should be float");
  Alcotest.(check int) "eq8 band 0 mode automation id" 153291 mode_param.automation

let test_eq8_is_on_parameters () =
  (* Extract first band's IsOn parameter from EQ8 XML *)
  let device_xml = read_file test_eq8_device_xml_path in
  let is_on_path, is_on_param_xml = Upath.find "/Bands.0/ParameterA/IsOn" device_xml in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let is_on_param = create is_on_path is_on_param_xml in

  (* Verify IsOn parameter properties *)
  Alcotest.(check string) "eq8 is_on param name" "Bands.0/ParameterA/IsOn" is_on_param.name;
  (match is_on_param.value with
   | Bool v -> Alcotest.(check bool) "eq8 band 0 IsOn value" true v
   | _ -> Alcotest.fail "eq8 IsOn parameter should be bool");
  Alcotest.(check int) "eq8 band 0 IsOn automation id" 153290 is_on_param.automation;

  (* Test ParameterB IsOn (should be false) *)
  let is_on_b_path, is_on_b_param_xml = Upath.find "/Bands.0/ParameterB/IsOn" device_xml in
  let is_on_b_param = create is_on_b_path is_on_b_param_xml in

  (* Verify ParameterB IsOn parameter properties *)
  (match is_on_b_param.value with
   | Bool v -> Alcotest.(check bool) "eq8 band 0 ParameterB IsOn value" false v
   | _ -> Alcotest.fail "eq8 IsOn parameter should be bool");
  Alcotest.(check int) "eq8 band 0 ParameterB IsOn automation id" 153298 is_on_b_param.automation

let test_eq8_all_bands_exist () =
  (* Verify that all 8 bands (Bands.0 through Bands.7) exist *)
  let device_xml = read_file test_eq8_device_xml_path in
  let open Device.DeviceParam in

  for i = 0 to 7 do
    let band_path = Printf.sprintf "/Bands.%d/ParameterA/Freq" i in
    let freq_path, freq_param_xml = Upath.find band_path device_xml in
    let freq_param = create freq_path freq_param_xml in

    (* Verify that each band has a frequency parameter *)
    Alcotest.(check string) (Printf.sprintf "eq8 band %d param name" i) (Printf.sprintf "Bands.%d/ParameterA/Freq" i) freq_param.name;
    (match freq_param.value with
     | Float v ->
       (* Verify that frequency values are reasonable (between 20Hz and 20kHz) *)
       Alcotest.(check (float 0.1)) (Printf.sprintf "eq8 band %d frequency reasonable" i)
         (max 20.0 (min 20000.0 v)) v
     | _ -> Alcotest.fail (Printf.sprintf "eq8 band %d frequency parameter should be float" i))
  done

let test_eq8_parameter_b_vs_parameter_a_differences () =
  (* Compare ParameterA and ParameterB in the first band to test differences *)
  let device_xml = read_file test_eq8_device_xml_path in
  let open Device.DeviceParam in

  (* Get ParameterA frequency *)
  let freq_a_path, freq_a_param_xml = Upath.find "/Bands.0/ParameterA/Freq" device_xml in
  let freq_a_param = create freq_a_path freq_a_param_xml in

  (* Get ParameterB frequency *)
  let freq_b_path, freq_b_param_xml = Upath.find "/Bands.0/ParameterB/Freq" device_xml in
  let freq_b_param = create freq_b_path freq_b_param_xml in

  (* Verify they have different values *)
  (match (freq_a_param.value, freq_b_param.value) with
   | (Float a, Float b) ->
     Alcotest.(check (float 0.001)) "eq8 freq A value" 77.237999 a;
     Alcotest.(check (float 0.001)) "eq8 freq B value" 40.0 b;
     Alcotest.(check bool) "eq8 frequencies are different" true (a <> b)
   | _ -> Alcotest.fail "eq8 both frequency parameters should be float");

  (* Verify they have different automation IDs *)
  Alcotest.(check int) "eq8 freq A automation id" 153292 freq_a_param.automation;
  Alcotest.(check int) "eq8 freq B automation id" 153300 freq_b_param.automation;
  Alcotest.(check bool) "eq8 automation IDs are different" true
    (freq_a_param.automation <> freq_b_param.automation)

let test_eq8_preset_reference_details () =
  (* Test detailed preset reference properties *)
  let xml = read_file test_eq8_device_xml_path in
  let device = Device.create xml in

  let regular_device = match device with
    | Device.Regular reg -> reg
    | _ -> failwith "Expected Regular device"
  in

  let preset = Option.get regular_device.preset in

  (* Verify preset type and detailed properties *)
  (match preset.preset_type with
   | Device.PresetRef.DefaultPreset -> () (* Expected *)
   | Device.PresetRef.UserPreset -> Alcotest.fail "eq8 Expected DefaultPreset");

  Alcotest.(check int) "eq8 preset id" 1 preset.id;
  Alcotest.(check string) "eq8 preset name" "EQ Eight" preset.name;

  (* Verify relative path contains EQ Eight *)
  let relative_path = preset.relative_path in
  let eq8_regexp = Str.regexp_string "EQ Eight" in
  let contains_eq8 = try ignore (Str.search_forward eq8_regexp relative_path 0); true with Not_found -> false in
  Alcotest.(check bool) "eq8 relative path contains EQ Eight" true contains_eq8;

  (* Verify path contains EQ Eight *)
  let path = preset.path in
  let path_contains_eq8 = try ignore (Str.search_forward eq8_regexp path 0); true with Not_found -> false in
  Alcotest.(check bool) "eq8 path contains EQ Eight" true path_contains_eq8

(* ==================== Wavetable Device Tests ==================== *)

let test_create_wavetable_device () =
  (* Load the wavetable XML file *)
  let xml = read_file test_wavetable_device_xml_path in

  (* Create a device from the XML *)
  let device = Device.create xml in

  (* Extract the RegularDevice from the variant *)
  let regular_device = match device with
    | Device.Regular reg -> reg
    | _ -> failwith "Expected Regular device"
  in

  (* Verify the basic device properties *)
  Alcotest.(check int) "wavetable device id" 0 regular_device.id;
  Alcotest.(check string) "wavetable device name" "InstrumentVector" regular_device.device_name;
  Alcotest.(check string) "wavetable display name" "InstrumentVector" regular_device.display_name;
  Alcotest.(check int) "wavetable pointee id" 25826 regular_device.pointee;

    Alcotest.(check bool) "preset exists" true (Option.is_some regular_device.preset);

  let preset = Option.get regular_device.preset in
  (* Verify preset information *)
  (match preset.preset_type with
   | Device.PresetRef.UserPreset -> ()
   | _ -> Alcotest.fail "wavetable Expected UserPreset");
  Alcotest.(check string) "wavetable preset name" "Quin" preset.name;
  Alcotest.(check string) "wavetable preset relative path" "../../../../../../../../../../../Quin.adv" preset.relative_path;

  (* Verify we have the expected number of parameters *)
  Alcotest.(check (int)) "wavetable parameter count" 93 (List.length regular_device.params);

  (* Test specific oscillator parameters *)
  let open Device.DeviceParam in

  (* Check the device enabled parameter *)
  (match regular_device.enabled.value with
   | Bool v -> Alcotest.(check bool) "wavetable device enabled value" true v
   | _ -> Alcotest.fail "wavetable device enabled parameter should be boolean");
  Alcotest.(check int) "wavetable device enabled automation id" 25825 regular_device.enabled.automation;

  (* Find oscillator 1 on parameter *)
  let osc1_on_param = List.find (fun p -> p.name = "Voice_Oscillator1_On") regular_device.params in
  (match osc1_on_param.value with
   | Bool v -> Alcotest.(check bool) "wavetable osc1 on value" true v
   | _ -> Alcotest.fail "wavetable osc1 on parameter should be boolean");
  Alcotest.(check int) "wavetable osc1 on automation id" 25827 osc1_on_param.automation;

  (* Find oscillator 1 transpose parameter *)
  let osc1_transpose_param = List.find (fun p -> p.name = "Voice_Oscillator1_Pitch_Transpose") regular_device.params in
  (match osc1_transpose_param.value with
   | Float v -> Alcotest.(check (float 0.01)) "wavetable osc1 transpose value" 0.0 v
   | _ -> Alcotest.fail "wavetable osc1 transpose parameter should be float");
  Alcotest.(check int) "wavetable osc1 transpose automation id" 25828 osc1_transpose_param.automation;

  (* Test filter parameters *)
  let filter1_freq_param = List.find (fun p -> p.name = "Voice_Filter1_Frequency") regular_device.params in
  (match filter1_freq_param.value with
   | Float v ->
     (* Filter frequency should be a positive float - check it's reasonable *)
     if v <= 0.0 then
       Alcotest.fail "wavetable filter1 frequency should be positive"
     else
       Alcotest.(check (float 0.01)) "wavetable filter1 freq value" v v
   | _ -> Alcotest.fail "wavetable filter1 frequency parameter should be float");

  (* Test envelope parameters *)
  let amp_env_attack_param = List.find (fun p -> p.name = "Voice_Modulators_AmpEnvelope_Times_Attack") regular_device.params in
  (match amp_env_attack_param.value with
   | Float v ->
     (* Attack time should be non-negative *)
     if v < 0.0 then
       Alcotest.fail "wavetable amp envelope attack should be non-negative"
     else
       Alcotest.(check (float 0.01)) "wavetable amp env attack value" v v
   | _ -> Alcotest.fail "wavetable amp envelope attack parameter should be float")

let test_wavetable_parameter_creation () =
  (* Test creating a specific parameter from XML *)
  let param_path, param_xml = extract_wavetable_parameter_xml "Voice_Oscillator1_On" in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create param_path param_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "wavetable param name" "Voice_Oscillator1_On" param.name;
  (match param.value with
   | Bool v -> Alcotest.(check bool) "wavetable param value" true v
   | _ -> Alcotest.fail "wavetable parameter should be boolean");
  Alcotest.(check int) "wavetable param automation id" 25827 param.automation;

  (* Verify no macro mapping for this parameter *)
  (match param.mapping with
   | None -> () (* Expected - no macro mapping *)
   | Some _ -> Alcotest.fail "wavetable parameter should not have macro mapping");
  ()

let test_wavetable_float_parameter () =
  (* Test creating a float parameter from XML *)
  let param_path, param_xml = extract_wavetable_parameter_xml "Voice_Oscillator1_Pitch_Transpose" in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create param_path param_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "wavetable float param name" "Voice_Oscillator1_Pitch_Transpose" param.name;
  (match param.value with
   | Float v -> Alcotest.(check (float 0.001)) "wavetable float param value" 0.0 v
   | _ -> Alcotest.fail "wavetable parameter should be float");
  Alcotest.(check int) "wavetable float param automation id" 25828 param.automation

let test_wavetable_preset_creation () =
  (* Test creating PresetRef from the XML *)
  let device_xml = read_file test_wavetable_device_xml_path in
  let preset_xml = Upath.find "/LastPresetRef/Value/*" device_xml |> snd in

  (* Create preset reference *)
  let preset = Device.PresetRef.create preset_xml in

  (* Verify preset properties *)
  Alcotest.(check int) "wavetable preset id" 1 preset.id;
  (match preset.preset_type with
   | Device.PresetRef.UserPreset -> ()
   | _ -> Alcotest.fail "wavetable Expected UserPreset");
  Alcotest.(check string) "wavetable preset name" "Quin" preset.name;
  Alcotest.(check string) "wavetable preset relative path" "../../../../../../../../../../../Quin.adv" preset.relative_path;
  Alcotest.(check string) "wavetable preset path" "/../../../../../../Quin.adv" preset.path;
  Alcotest.(check string) "wavetable preset pack name" "" preset.pack_name;
  Alcotest.(check int) "wavetable preset pack id" 0 preset.pack_id;
  Alcotest.(check int) "wavetable preset file size" 0 preset.file_size;
  Alcotest.(check int) "wavetable preset crc" 0 preset.crc

let test_wavetable_parameter_values () =
  (* Test that we can find and verify various parameter values *)
  let device_xml = read_file test_wavetable_device_xml_path in
  let device = Device.create device_xml in
  let regular_device = match device with
    | Device.Regular reg -> reg
    | _ -> failwith "Expected Regular device"
  in

  let open Device.DeviceParam in

  (* Test that all parameters have valid values *)
  List.iter (fun param ->
    match param.value with
    | Bool _ -> () (* Boolean values are always valid *)
    | Float f ->
      (* Float values should be finite *)
      if not (Float.is_finite f) then
        Alcotest.fail (Printf.sprintf "wavetable Parameter %s has non-finite float value: %f" param.name f)
    | Int i ->
      (* Int values should be reasonable *)
      if abs i > 1000000 then
        Alcotest.fail (Printf.sprintf "wavetable Parameter %s has suspicious int value: %d" param.name i)
  ) regular_device.params;

  (* Test that automation IDs are unique and positive *)
  let automation_ids = List.map (fun p -> p.automation) regular_device.params in
  let unique_ids = List.sort_uniq Stdlib.compare automation_ids in
  if List.length unique_ids <> List.length automation_ids then
    Alcotest.fail "wavetable Duplicate automation IDs found in parameters";

  (* All automation IDs should be non-negative *)
  List.iter (fun id ->
    if id < 0 then
      Alcotest.fail (Printf.sprintf "wavetable Negative automation ID found: %d" id)
  ) automation_ids

let () =
  Alcotest.run "RealDevices" [
    "compressor_tests", [
      Alcotest.test_case "create device from compressor XML" `Quick test_create_device_from_compressor_xml;
    ];
    "compressor_parameters", [
      Alcotest.test_case "create parameter from real XML" `Quick test_device_param_creation;
      Alcotest.test_case "parameter without macro mapping" `Quick test_device_param_without_macro_mapping;
    ];
    "eq8_device_creation", [
      Alcotest.test_case "create EQ8 device from XML" `Quick test_create_eq8_device_from_xml;
    ];
    "eq8_global_parameters", [
      Alcotest.test_case "test GlobalGain parameter" `Quick test_eq8_global_gain_parameter;
      Alcotest.test_case "test Scale parameter" `Quick test_eq8_scale_parameter;
    ];
    "eq8_band_parameters", [
      Alcotest.test_case "test band frequency/gain/Q parameters" `Quick test_eq8_band_parameters;
      Alcotest.test_case "test mode parameters" `Quick test_eq8_mode_parameters;
      Alcotest.test_case "test IsOn parameters" `Quick test_eq8_is_on_parameters;
      Alcotest.test_case "test all 8 bands exist" `Quick test_eq8_all_bands_exist;
      Alcotest.test_case "test ParameterA vs ParameterB differences" `Quick test_eq8_parameter_b_vs_parameter_a_differences;
    ];
    "eq8_preset_tests", [
      Alcotest.test_case "test preset reference details" `Quick test_eq8_preset_reference_details;
    ];
    "wavetable_device_creation", [
      Alcotest.test_case "create wavetable device from XML" `Quick test_create_wavetable_device;
    ];
    "wavetable_parameter_tests", [
      Alcotest.test_case "create boolean parameter from XML" `Quick test_wavetable_parameter_creation;
      Alcotest.test_case "create float parameter from XML" `Quick test_wavetable_float_parameter;
      Alcotest.test_case "verify parameter values" `Quick test_wavetable_parameter_values;
    ];
    "wavetable_preset_tests", [
      Alcotest.test_case "create preset reference from XML" `Quick test_wavetable_preset_creation;
    ];
  ]
