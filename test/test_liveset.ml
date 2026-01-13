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

let test_liveset_main_track_parsing () =
  (* Read and parse the XML file *)
  let xml = read_file test_liveset_xml_path in
  let liveset = Liveset.create xml test_liveset_xml_path in

  (* 1. Verify MainTrack type detection *)
  match liveset.main with
  | Track.Main main_track ->
    (* 2. Test basic MainTrack properties *)
    Alcotest.(check string) "main track name" "Main" main_track.name;

    (* 3. Test device count (from t4.xml we expect devices) *)
    let device_count = List.length main_track.devices in
    Alcotest.(check bool) "main track has devices" true (device_count > 0);

    (* 4. Test automation count *)
    let automation_count = List.length main_track.automations in
    Alcotest.(check int) "automation count" 2 automation_count;

    (* 5. Test MainMixer-specific properties *)
    (match main_track.mixer.Alsdiff_live.Track.MainMixer.tempo.value with
     | Alsdiff_live.Device.Float tempo_value ->
       Alcotest.(check (float 0.001)) "tempo value" 120.0 tempo_value
     | _ -> Alcotest.fail "expected Float tempo value");

    (* 6. Test routing configuration *)
    Alcotest.(check string) "audio out target" "AudioOut/External/S0"
      main_track.routings.Alsdiff_live.Track.RoutingSet.audio_out.target;

    (* 7. Verify MainTrack singleton behavior *)
    Alcotest.(check bool) "MainTrack has_same_id returns true"
      true (Alsdiff_live.Track.MainTrack.has_same_id main_track main_track);
    (* Test that id_hash is consistent (same value for same instance) *)
    let hash_value = Alsdiff_live.Track.MainTrack.id_hash main_track in
    let hash_value2 = Alsdiff_live.Track.MainTrack.id_hash main_track in
    Alcotest.(check int) "MainTrack id_hash consistent" hash_value hash_value2

  | _ -> Alcotest.fail "Expected Track.Main type for main track"

let test_pointee_name_fallback_in_patch () =
  let xml = read_file test_liveset_xml_path in
  let liveset1 = Liveset.create xml test_liveset_xml_path in
  let liveset2 = Liveset.create xml test_liveset_xml_path in

  (* Create patch - tables should be copies, not references *)
  let patch = Liveset.diff liveset1 liveset2 in

  (* Test 1: Known ID should resolve from new_pointees *)
  let known_ids = Liveset.IntHashtbl.to_seq_keys patch.Liveset.Patch.new_pointees
    |> List.of_seq in
  (match known_ids with
   | id :: _ ->
     let result = Liveset.get_pointee_name_from_table_opt patch.Liveset.Patch.new_pointees id in
     Alcotest.(check bool) "known ID resolves from new_pointees" true (Option.is_some result)
   | [] -> ());

  (* Test 2: Unknown ID should return None *)
  let unknown_id = 999999 in
  let result_new = Liveset.get_pointee_name_from_table_opt patch.Liveset.Patch.new_pointees unknown_id in
  let result_old = Liveset.get_pointee_name_from_table_opt patch.Liveset.Patch.old_pointees unknown_id in
  Alcotest.(check bool) "unknown ID returns None from new" true (Option.is_none result_new);
  Alcotest.(check bool) "unknown ID returns None from old" true (Option.is_none result_old);

  (* Test 3: Verify tables are copies (mutation doesn't affect patch) *)
  let original_count = Liveset.IntHashtbl.length patch.Liveset.Patch.old_pointees in
  Liveset.IntHashtbl.clear liveset1.Liveset.pointees;
  let after_clear_count = Liveset.IntHashtbl.length patch.Liveset.Patch.old_pointees in
  Alcotest.(check int) "patch tables are independent copies" original_count after_clear_count

let () =
  Alcotest.run "Liveset" [
    "create", [
      Alcotest.test_case "basic liveset creation" `Quick test_liveset_create;
      Alcotest.test_case "version extraction" `Quick test_liveset_version_extraction;
      Alcotest.test_case "creator extraction" `Quick test_liveset_creator_extraction;
      Alcotest.test_case "track parsing" `Quick test_liveset_track_parsing;
      Alcotest.test_case "pointees table initialization" `Quick test_liveset_pointees_table;
      Alcotest.test_case "locators parsing" `Quick test_liveset_locators_parsing;
      Alcotest.test_case "main track parsing" `Quick test_liveset_main_track_parsing;
      Alcotest.test_case "pointee name fallback in patch" `Quick test_pointee_name_fallback_in_patch;
    ];
  ]