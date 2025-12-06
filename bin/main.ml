open Alsdiff_base
open Alsdiff_live
open Alsdiff_output
open Eio.Std


let extract_tracks (xml : Xml.t) : Track.t list =
  let track_elements = Upath.find_all "/Ableton/LiveSet/Tracks/*" xml in
  List.filter_map (fun (_, element) ->
    match element with
    | Xml.Element { name = "MidiTrack"; _ } -> Some (Track.create element)
    | Xml.Element { name = "AudioTrack"; _ } -> Some (Track.create element)
    | _ -> None
  ) track_elements

let get_track_info (track : Track.t) =
  match track with
  | Midi m -> m.name, m.id
  | Audio a -> a.name, a.id

let diff_tracks (old_tracks : Track.t list) (new_tracks : Track.t list) =
  Diff.diff_list_id (module Track) old_tracks new_tracks

let main ~domain_mgr =
  if Array.length Sys.argv <> 3 then (
    Printf.printf "Usage: %s <file1.als> <file2.als>\n" Sys.argv.(0);
    exit 1
  );

  let file1 = Sys.argv.(1) in
  let file2 = Sys.argv.(2) in

  let all_tracks_from_file file = File.open_als file |> extract_tracks in
  let tracks1, tracks2 =
    Fiber.pair
      (fun () -> Eio.Domain_manager.run domain_mgr (fun () -> all_tracks_from_file file1))
      (fun () -> Eio.Domain_manager.run domain_mgr (fun () -> all_tracks_from_file file2))
  in

  let changes = diff_tracks tracks1 tracks2 in

  List.iter (fun change ->
      match change with
      | `Modified patch ->
        Printf.printf "Track modified:\n%s\n" (Text_output.render_track patch)
      | `Added track ->
        let name, id = get_track_info track in
        Printf.printf "Track '%s' (ID: %d) added.\n" name id
      | `Removed track ->
        let name, id = get_track_info track in
        Printf.printf "Track '%s' (ID: %d) removed.\n" name id
      | `Unchanged -> ()
  ) changes

let () =
  Printexc.record_backtrace true;
  Eio_main.run @@ fun env ->
    main ~domain_mgr:(Eio.Stdenv.domain_mgr env)
