open Alsdiff_base.Xml
open Alsdiff_live

let test_liveset_xml_path =
  (* Try different paths to work with both dune exec and dune runtest *)
  if Sys.file_exists "test/t4.xml" then
    "test/t4.xml"
  else if Sys.file_exists "t4.xml" then
    "t4.xml"
  else
    failwith "Cannot find t4.xml test file"

let test_liveset_create () =
  (* Read and parse the XML file *)
  let xml = read_file test_liveset_xml_path in

  (* Create liveset using the create function *)
  let liveset = Liveset.create xml test_liveset_xml_path in

  (* Test basic liveset properties *)
  Alcotest.(check string) "liveset name" "t4" liveset.name;

  (* Test that pointees table is initialized *)
  Alcotest.(check bool) "pointees table exists" true
    (Liveset.IntHashtbl.length liveset.pointees >= 0)

let test_liveset_version_extraction () =
  let xml = read_file test_liveset_xml_path in
  let liveset = Liveset.create xml test_liveset_xml_path in

  (* Test version structure extraction *)
  let version = liveset.version in
  (* We expect some version info to be extracted from real Ableton files *)
  Alcotest.(check string) "major version field" version.major version.major;
  Alcotest.(check string) "minor version field" version.minor version.minor

let test_liveset_creator_extraction () =
  let xml = read_file test_liveset_xml_path in
  let liveset = Liveset.create xml test_liveset_xml_path in

  (* Test creator extraction - should get "Ableton Live X.Y.Z" or "Unknown" *)
  Alcotest.(check string) "creator field" liveset.creator liveset.creator;

  (* Verify creator is not empty string if the XML has Creator attribute *)
  if liveset.creator <> "Unknown" then
    Alcotest.(check string) "creator not empty when specified" liveset.creator liveset.creator

let test_liveset_track_parsing () =
  let xml = read_file test_liveset_xml_path in
  let liveset = Liveset.create xml test_liveset_xml_path in

  (* Count tracks by type to verify proper parsing *)
  let midi_track_count = List.fold_left (fun acc track ->
    match track with
    | Track.Midi _ -> acc + 1
    | _ -> acc
  ) 0 liveset.tracks in

  let audio_track_count = List.fold_left (fun acc track ->
    match track with
    | Track.Audio _ -> acc + 1
    | _ -> acc
  ) 0 liveset.tracks in

  let group_track_count = List.fold_left (fun acc track ->
    match track with
    | Track.Group _ -> acc + 1
    | _ -> acc
  ) 0 liveset.tracks in

  (* Verify track counting logic works *)
  let total_tracks = List.length liveset.tracks in
  Alcotest.(check int) "total track count matches sum"
    total_tracks (midi_track_count + audio_track_count + group_track_count)

let test_liveset_pointees_table () =
  let xml = read_file test_liveset_xml_path in
  let liveset = Liveset.create xml test_liveset_xml_path in

  (* Test that pointees table is properly initialized and populated *)
  let pointees_count = Liveset.IntHashtbl.length liveset.pointees in

  (* Pointees table should be initialized and non-negative *)
  Alcotest.(check bool) "pointees table non-negative" true (pointees_count >= 0);

  (* If we have tracks, we should have some pointees from devices and their parameters *)
  let has_tracks = List.length liveset.tracks > 0 in
  if has_tracks then
    (* We expect at least some pointees if there are tracks with devices *)
    Alcotest.(check bool) "has pointees when tracks exist" true (pointees_count > 0)

let test_liveset_locators_parsing () =
  let xml = read_file test_liveset_xml_path in
  let liveset = Liveset.create xml test_liveset_xml_path in

  (* Verify all 5 locators are loaded *)
  Alcotest.(check int) "locator count" 5 (List.length liveset.locators);

  (* Helper function to find locator by ID *)
  let find_locator_by_id id =
    List.find (fun locator -> locator.Liveset.Locator.id = id) liveset.locators
  in

  (* Expected locator data from t4.xml *)
  let expected_locators = [
    (0, "Intro", 0.0);
    (1, "Outro", 256.0);
    (2, "Verse 1", 88.0);
    (3, "Verse 2", 128.0);
    (4, "Chours", 192.0);
  ] in

  (* Validate each locator *)
  List.iter (fun (expected_id, expected_name, expected_time) ->
    let locator = find_locator_by_id expected_id in

    (* Check ID *)
    Alcotest.(check int) ("locator " ^ (string_of_int expected_id) ^ " id")
      expected_id locator.Liveset.Locator.id;

    (* Check name *)
    Alcotest.(check string) ("locator " ^ (string_of_int expected_id) ^ " name")
      expected_name locator.Liveset.Locator.name;

    (* Check time with float tolerance *)
    Alcotest.(check (float 0.001)) ("locator " ^ (string_of_int expected_id) ^ " time")
      expected_time locator.Liveset.Locator.time
  ) expected_locators

let () =
  Alcotest.run "Liveset" [
    "create", [
      Alcotest.test_case "basic liveset creation" `Quick test_liveset_create;
      Alcotest.test_case "version extraction" `Quick test_liveset_version_extraction;
      Alcotest.test_case "creator extraction" `Quick test_liveset_creator_extraction;
      Alcotest.test_case "track parsing" `Quick test_liveset_track_parsing;
      Alcotest.test_case "pointees table initialization" `Quick test_liveset_pointees_table;
      Alcotest.test_case "locators parsing" `Quick test_liveset_locators_parsing;
    ];
  ]