open Alsdiff_base.Xml
open Alsdiff_live

let test_group_device_xml_path =
  (* Try different paths to work with both dune exec and dune runtest *)
  if Sys.file_exists "test/group_device.xml" then
    "test/group_device.xml"
  else
    "group_device.xml"

let test_group_device_macros_from_xml () =
  (* Load the group device XML file *)
  let xml = read_file test_group_device_xml_path in

  (* Create a group device from the XML *)
  let group_device = Device.create xml in

  (* Extract the GroupDevice from the variant *)
  let group = match group_device with
    | Device.Group g -> g
    | Device.Regular _ -> failwith "Expected Group device, got Regular device"
  in

  (* Verify we have macros *)
  let macros = group.macros in
  Alcotest.(check (int)) "total 16 macros" 16 (List.length macros);

  (* Check that we have some macros with expected names *)
  let macro_names = List.map (fun m -> m.Device.Macro.name) macros in
  (* Just check that we have some macros and they're not empty *)
  Alcotest.(check int) "non-empty macro names count"
    (List.length (List.filter (fun n -> n <> "") macro_names))
    (List.length macro_names);

  (* Check first macro details *)
  let macro0 = List.nth macros 0 in
  Alcotest.(check (float 0.01)) "macro 0 manual value" 0.0 macro0.Device.Macro.manual;
  Alcotest.(check int) "macro 0 automation id" 24233 macro0.Device.Macro.automation;
  Alcotest.(check int) "macro 0 modulation id" 24234 macro0.Device.Macro.modulation;

  (* Check fourth macro details *)
  let macro1 = List.nth macros 3 in
  Alcotest.(check (float 0.01)) "macro 3 manual value" 82.0208 macro1.Device.Macro.manual;
  Alcotest.(check int) "macro 1 automation id" 24239 macro1.Device.Macro.automation;
  Alcotest.(check int) "macro 1 modulation id" 24240 macro1.Device.Macro.modulation;

  (* Check that all macros have expected structure *)
  List.iter (fun macro ->
    ignore (macro.Device.Macro.manual : float);
    ignore (macro.Device.Macro.automation : int);
    ignore (macro.Device.Macro.modulation : int);
  ) macros

let test_group_device_structure_from_xml () =
  (* Load the group device XML file *)
  let xml = read_file test_group_device_xml_path in

  (* Create a group device from the XML *)
  let group_device = Device.create xml in

  (* Extract the GroupDevice from the variant *)
  let group = match group_device with
    | Device.Group g -> g
    | Device.Regular _ -> failwith "Expected Group device, got Regular device"
  in

  (* Verify basic group device properties *)
  Alcotest.(check string) "group device display name" "Industrial FM Kick" group.display_name;
  (match group.enabled.Device.DeviceParam.value with
   | Device.DeviceParam.Bool b -> Alcotest.(check bool) "group device enabled" true b
   | _ -> Alcotest.fail "enabled should be bool");

  (* Verify we have branches *)
  Alcotest.(check int) "branch count" 1 (List.length group.branches);

  (* Verify we have macros *)
  Alcotest.(check (int)) "total 16 macros" 16 (List.length group.macros);

  (* Verify branches contain devices and mixer device *)
  let first_branch = List.hd group.branches in
  Alcotest.(check int) "first branch device count" 2 (List.length first_branch.devices);

  (* Check that first device in branch is an Operator device *)
  let first_device = List.hd first_branch.devices in
  (match first_device with
   | Device.Regular reg ->
       Alcotest.(check string) "first device name" "Operator" reg.device_name
   | Device.Group _ -> Alcotest.fail "Expected Regular device in branch")

let test_group_device_snapshots_from_xml () =
  (* Load the group device XML file *)
  let xml = read_file test_group_device_xml_path in

  (* Create a group device from the XML *)
  let group_device = Device.create xml in

  (* Extract the GroupDevice from the variant *)
  let group = match group_device with
    | Device.Group g -> g
    | Device.Regular _ -> failwith "Expected Group device, got Regular device"
  in

  (* Verify we have snapshots *)
  let snapshots = group.snapshots in
  Alcotest.(check int) "snapshot count" 3 (List.length snapshots);

  (* Check first snapshot (Basic) *)
  let snapshot0 = List.nth snapshots 0 in
  Alcotest.(check int) "snapshot 0 id" 6 snapshot0.Device.Snapshot.id;
  Alcotest.(check string) "snapshot 0 name" "Basic" snapshot0.Device.Snapshot.name;
  (* Should have 16 values from all macros *)
  Alcotest.(check int) "snapshot 0 values count" 16 (List.length snapshot0.Device.Snapshot.values);

  (* Check second snapshot (Scream) *)
  let snapshot1 = List.nth snapshots 1 in
  Alcotest.(check int) "snapshot 1 id" 7 snapshot1.Device.Snapshot.id;
  Alcotest.(check string) "snapshot 1 name" "Scream" snapshot1.Device.Snapshot.name;
  (* Should have 16 values from all macros *)
  Alcotest.(check int) "snapshot 1 values count" 16 (List.length snapshot1.Device.Snapshot.values);

  (* Check third snapshot (Harsh) *)
  let snapshot2 = List.nth snapshots 2 in
  Alcotest.(check int) "snapshot 2 id" 8 snapshot2.Device.Snapshot.id;
  Alcotest.(check string) "snapshot 2 name" "Harsh" snapshot2.Device.Snapshot.name;
  (* Should have 16 values from all macros *)
  Alcotest.(check int) "snapshot 2 values count" 16 (List.length snapshot2.Device.Snapshot.values);

  (* Verify specific values from first snapshot (Basic) *)
  let basic_values = snapshot0.Device.Snapshot.values in
  Alcotest.(check (float 0.01)) "basic value 0" 0.0 (List.nth basic_values 0);
  Alcotest.(check (float 0.01)) "basic value 1" 0.0 (List.nth basic_values 1);
  Alcotest.(check (float 0.01)) "basic value 2" 0.0 (List.nth basic_values 2);
  Alcotest.(check (float 0.01)) "basic value 3" 82.0208282 (List.nth basic_values 3)

let () =
  Alcotest.run "GroupDevice" [
    "macros", [
      Alcotest.test_case "group device macros from XML" `Quick test_group_device_macros_from_xml;
    ];
    "structure", [
      Alcotest.test_case "group device structure from XML" `Quick test_group_device_structure_from_xml;
    ];
    "snapshots", [
      Alcotest.test_case "group device snapshots from XML" `Quick test_group_device_snapshots_from_xml;
    ]
  ]
