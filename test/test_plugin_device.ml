open Alsdiff_base.Xml
open Alsdiff_live

module PluginDevice = Device.PluginDevice
module PluginParam = Device.PluginParam
module PluginDesc = Device.PluginDesc

(* Helper function to get the plugin device XML path *)
let test_plugin_device_xml_path =
  if Sys.file_exists "test/plugin_device.xml" then
    "test/plugin_device.xml"
  else
    "plugin_device.xml"

(* ==================== PluginDevice Creation Tests ==================== *)

let test_create_plugin_device_from_xml () =
  (* Load the plugin device XML file *)
  let xml = read_file test_plugin_device_xml_path in

  (* Create a PluginDevice from the XML *)
  let plugin_device = PluginDevice.create xml in

  (* Verify the device properties *)
  Alcotest.(check int) "plugin device id" 13 plugin_device.id;
  Alcotest.(check int) "plugin pointee id" 203076 plugin_device.pointee;

  (* Verify plugin description *)
  let desc = plugin_device.desc in
  Alcotest.(check string) "plugin name" "Ozone 11 Imager" desc.name;

  (* Verify UID is correctly concatenated from fields *)
  let expected_uid = "1448301658-1095326287-2054123109-542143087" in
  Alcotest.(check string) "plugin uid" expected_uid desc.uid;

  (* Verify plugin type *)
  (match desc.plugin_type with
   | PluginDesc.Vst3 -> () (* Expected *)
   | _ -> Alcotest.fail "Expected Vst3 plugin type");

  (* Verify parameter count *)
  Alcotest.(check int) "plugin parameter count" 24 (List.length plugin_device.params)

let test_plugin_desc_creation () =
  (* Test PluginDesc creation specifically *)
  let device_xml = read_file test_plugin_device_xml_path in
  let plugin_desc_xml = Alsdiff_base.Upath.find "/PluginDesc" device_xml |> snd in
  let desc = PluginDesc.create plugin_desc_xml in

  (* Verify plugin description properties *)
  Alcotest.(check string) "plugin desc name" "Ozone 11 Imager" desc.name;
  Alcotest.(check string) "plugin desc uid" "1448301658-1095326287-2054123109-542143087" desc.uid;

  (* Verify plugin type *)
  (match desc.plugin_type with
   | PluginDesc.Vst3 -> () (* Expected *)
   | _ -> Alcotest.fail "Expected Vst3 plugin type")

let test_plugin_parameter_float_creation () =
  (* Test creating a float parameter from real XML *)
  let device_xml = read_file test_plugin_device_xml_path in
  let plugin_device = PluginDevice.create device_xml in
  let param = List.nth plugin_device.params 0 in

  (* Verify parameter properties *)
  Alcotest.(check int) "float param id" 0 param.PluginParam.id;
  Alcotest.(check string) "float param name" "GAIN: Input L (Locked)" param.base.Device.GenericParam.name;
  Alcotest.(check int) "float param index" 0 param.PluginParam.index;

  (* Verify parameter value *)
  (match param.base.Device.GenericParam.value with
   | Device.Float v -> Alcotest.(check (float 0.01)) "float param value" 0.0 v
   | _ -> Alcotest.fail "parameter should be float");

  (* Verify automation and modulation IDs *)
  Alcotest.(check int) "float param automation id" 203077 param.base.Device.GenericParam.automation;
  Alcotest.(check int) "float param modulation id" 203078 param.base.Device.GenericParam.modulation

let test_plugin_parameter_with_automation () =
  (* Test parameter with automation target *)
  let device_xml = read_file test_plugin_device_xml_path in
  let plugin_device = PluginDevice.create device_xml in
  let param = List.nth plugin_device.params 10 in

  (* Verify parameter properties *)
  Alcotest.(check int) "param id" 11 param.PluginParam.id;
  Alcotest.(check string) "param name" "IMG: Stereo/Main Band 1 Width Percent" param.base.Device.GenericParam.name;
  Alcotest.(check int) "param index" 10 param.PluginParam.index;

  (* Verify parameter value *)
  (match param.base.Device.GenericParam.value with
   | Device.Float v -> Alcotest.(check (float 0.01)) "param value" 0.0 v
   | _ -> Alcotest.fail "parameter should be float");

  (* Verify automation and modulation IDs *)
  Alcotest.(check int) "param automation id" 203097 param.base.Device.GenericParam.automation;
  Alcotest.(check int) "param modulation id" 203098 param.base.Device.GenericParam.modulation

let test_plugin_parameter_validation () =
  (* Test that all parameters have valid IDs and are properly structured *)
  let device_xml = read_file test_plugin_device_xml_path in
  let plugin_device = PluginDevice.create device_xml in

  (* Verify parameter count *)
  Alcotest.(check int) "plugin parameter count" 24 (List.length plugin_device.params);

  (* Extract all parameter IDs *)
  let param_ids = List.map (fun p -> p.PluginParam.id) plugin_device.params in
  let unique_ids = List.sort_uniq Stdlib.compare param_ids in

  (* Verify all IDs are unique *)
  let all_unique = (List.length param_ids = List.length unique_ids) in
  Alcotest.(check bool) "all parameter IDs are unique" true all_unique;

  (* Check all automation IDs are non-negative *)
  List.iteri (fun i param ->
      if param.PluginParam.base.Device.GenericParam.automation < 0 then
        Alcotest.fail (Printf.sprintf "Parameter %d has negative automation ID: %d" i param.PluginParam.base.Device.GenericParam.automation);
      if param.PluginParam.base.Device.GenericParam.modulation < 0 then
        Alcotest.fail (Printf.sprintf "Parameter %d has negative modulation ID: %d" i param.PluginParam.base.Device.GenericParam.modulation)
    ) plugin_device.params

let test_plugin_parameter_invalid_type_raises_exception () =
  (* Test that invalid parameter types raise exception *)
  let test_xml_content = {|
<InvalidParameterType Id="0">
  <ParameterName Value="Test Param"/>
  <ParameterId Value="0"/>
  <VisualIndex Value="0"/>
  <ParameterValue>
    <Manual Value="1.0"/>
  </ParameterValue>
</InvalidParameterType>
|} in

  let xml = read_string test_xml_content in

  (* Should raise an exception *)
  (try ignore (PluginParam.create xml); false
   with Alsdiff_base.Xml.Xml_error (_, msg) when msg = "Invalid parameter type InvalidParameterType" -> true
      | _ -> false)
  |> Alcotest.(check bool) "invalid parameter type raises exception" true

let () =
  Alcotest.run "PluginDevice" [
    "plugin_device_creation", [
      Alcotest.test_case "create PluginDevice from XML" `Quick test_create_plugin_device_from_xml;
      Alcotest.test_case "create PluginDesc from XML" `Quick test_plugin_desc_creation;
    ];
    "plugin_parameter_creation", [
      Alcotest.test_case "create float parameter from XML" `Quick test_plugin_parameter_float_creation;
      Alcotest.test_case "create parameter with automation" `Quick test_plugin_parameter_with_automation;
    ];
    "plugin_parameter_validation", [
      Alcotest.test_case "parameter validation" `Quick test_plugin_parameter_validation;
    ];
    "error_handling", [
      Alcotest.test_case "invalid parameter type raises exception" `Quick test_plugin_parameter_invalid_type_raises_exception;
    ];
  ]
