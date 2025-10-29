open Alsdiff_base.Xml
open Alsdiff_live.Device

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
  let device = create xml in

  (* Verify the device properties *)
  Alcotest.(check int) "device id" 2 device.id;
  Alcotest.(check string) "device name" "Compressor2" device.device_name;
  Alcotest.(check string) "preset name" "" device.preset_name;
  Alcotest.(check int) "pointee id" 153207 device.pointee;

  (* Verify we have parameters *)
  Alcotest.(check (int)) "parameter count" 25 (List.length device.params);

  (* Check a few specific parameters *)
  let open Param in
  let threshold_param = List.find (fun p -> p.name = "Threshold") device.params in
  Alcotest.(check (float 0.01)) "threshold value" 1.0 threshold_param.value;
  Alcotest.(check int) "threshold automation id" 153208 threshold_param.automation;

  let ratio_param = List.find (fun p -> p.name = "Ratio") device.params in
  Alcotest.(check (float 0.01)) "ratio value" 2.50000024 ratio_param.value;
  Alcotest.(check int) "ratio automation id" 153210 ratio_param.automation;

  let attack_param = List.find (fun p -> p.name = "Attack") device.params in
  Alcotest.(check (float 0.01)) "attack value" 0.9641109109 attack_param.value;
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
  let param = Param.create param_xml in

  (* Verify parameter properties *)
  let open Param in
  Alcotest.(check string) "param name" "Threshold" param.name;
  Alcotest.(check (float 0.01)) "param value" 0.5 param.value;
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
  let param = Param.create param_xml in

  (* Verify parameter properties with defaults *)
  let open Param in
  Alcotest.(check string) "param name" "TestParam" param.name;
  Alcotest.(check (float 0.01)) "param value (default)" 0.0 param.value;
  Alcotest.(check int) "param automation id (default)" 0 param.automation

let test_device_creation_with_invalid_xml () =
  (* Create invalid XML (Data instead of Element) *)
  let invalid_xml = Data { value = "invalid"; parent = None } in

  (* This should raise an exception *)
  Alcotest.check_raises "invalid xml raises exception" (Failure "Invalid XML element for creating Device")
    (fun () -> ignore (create invalid_xml))

let test_param_creation_with_invalid_xml () =
  (* Create invalid XML (Data instead of Element) *)
  let invalid_xml = Data { value = "invalid"; parent = None } in

  (* This should raise an exception *)
  Alcotest.check_raises "invalid xml raises exception" (Failure "Invalid XML element for creating Param")
    (fun () -> ignore (Param.create invalid_xml))

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
