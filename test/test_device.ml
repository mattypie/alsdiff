open Alsdiff_base.Xml

let test_compressor_device_xml_path =
  (* Try different paths to work with both dune exec and dune runtest *)
  if Sys.file_exists "test/compressor_device.xml" then
    "test/compressor_device.xml"
  else
    "compressor_device.xml"

let test_create_device_from_compressor_xml () =
  (* Load the compressor XML file *)
  let xml = read_file test_compressor_device_xml_path in

  (* Create a device from the XML *)
  let device = Alsdiff_live.Device.create xml in

  (* Extract the RegularDevice from the variant *)
  let regular_device = match device with
    | Alsdiff_live.Device.Regular reg -> reg
    | Alsdiff_live.Device.Group _ -> failwith "Expected Regular device, got Group device"
  in

  (* Verify the device properties *)
  Alcotest.(check int) "device id" 2 regular_device.id;
  Alcotest.(check string) "device name" "Compressor2" regular_device.device_name;
  Alcotest.(check string) "preset name" "" regular_device.preset_name;
  Alcotest.(check int) "pointee id" 153207 regular_device.pointee;

  (* Verify we have parameters *)
  Alcotest.(check (int)) "parameter count" 25 (List.length regular_device.params);

  (* Check a few specific parameters *)
  let open Alsdiff_live.Device.DeviceParam in
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
  (* Create a simple parameter XML structure *)
  let param_xml = Element {
    name = "Threshold";
    attrs = [("Id", "100")];
    childs = [
      Element {
        name = "LomId";
        attrs = [("Value", "0")];
        childs = [];
        parent = None;
      };
      Element {
        name = "Manual";
        attrs = [("Value", "0.5")];
        childs = [];
        parent = None;
      };
      Element {
        name = "AutomationTarget";
        attrs = [("Id", "200")];
        childs = [
          Element {
            name = "LockEnvelope";
            attrs = [("Value", "0")];
            childs = [];
            parent = None;
          }
        ];
        parent = None;
      }
    ];
    parent = None;
  } in

  (* Create a parameter from the XML *)
  let open Alsdiff_live.Device.DeviceParam in
  let param = create param_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "param name" "Threshold" param.name;
  (match param.value with
   | Float v -> Alcotest.(check (float 0.01)) "param value" 0.5 v
   | _ -> Alcotest.fail "parameter should be float");
  Alcotest.(check int) "param automation id" 200 param.automation

let test_device_param_with_missing_values () =
  (* Create a parameter XML structure with missing values *)
  let param_xml = Element {
    name = "TestParam";
    attrs = [("Id", "101")];
    childs = [
      Element {
        name = "LomId";
        attrs = [("Value", "0")];
        childs = [];
        parent = None;
      }
      (* Missing Manual element - should default to 0.0 *)
      (* Missing AutomationTarget element - should default to 0 *)
    ];
    parent = None;
  } in

  (* Create a parameter from the XML *)
  let open Alsdiff_live.Device.DeviceParam in
  let param = create param_xml in

  (* Verify parameter properties with defaults *)
  Alcotest.(check string) "param name" "TestParam" param.name;
  (match param.value with
   | Float v -> Alcotest.(check (float 0.01)) "param value (default)" 0.0 v
   | _ -> Alcotest.fail "parameter should be float");
  Alcotest.(check int) "param automation id (default)" 0 param.automation

let test_device_creation_with_invalid_xml () =
  (* Create invalid XML (Data instead of Element) *)
  let invalid_xml = Data { value = "invalid"; parent = None } in

  (* This should raise an exception *)
  Alcotest.check_raises "invalid xml raises exception" (Failure "Invalid XML element for creating Device")
    (fun () -> ignore (Alsdiff_live.Device.RegularDevice.create invalid_xml))

let test_param_creation_with_invalid_xml () =
  (* Create invalid XML (Data instead of Element) *)
  let invalid_xml = Data { value = "invalid"; parent = None } in

  (* This should raise an exception *)
  Alcotest.check_raises "invalid xml raises exception" (Failure "Invalid XML element for creating DeviceParam")
    (fun () -> ignore (Alsdiff_live.Device.DeviceParam.create invalid_xml))

let () =
  Alcotest.run "Device" [
    "device_creation", [
      Alcotest.test_case "create device from compressor XML" `Quick test_create_device_from_compressor_xml;
      Alcotest.test_case "create parameter from XML" `Quick test_device_param_creation;
      Alcotest.test_case "create parameter with missing values" `Quick test_device_param_with_missing_values;
      Alcotest.test_case "device creation with invalid XML" `Quick test_device_creation_with_invalid_xml;
      Alcotest.test_case "param creation with invalid XML" `Quick test_param_creation_with_invalid_xml;
    ]
  ]
