open Alsdiff_base
open Alsdiff_live
open Alsdiff_output
open Eio.Std


let create_liveset_from_file file =
  let xml = File.open_als file in
  Liveset.create xml file

let get_track_info (track : Track.t) =
  match track with
  | Midi m -> m.name, m.id
  | Audio a | Group a | Return a -> a.name, a.id
  | Main m -> m.name, 0

let display_liveset_changes patch =
  let open Alsdiff_base.Diff in

  (* Name change *)
  (match patch.Liveset.Patch.name with
   | `Unchanged -> ()
   | `Modified m -> Printf.printf "LiveSet name changed: %s -> %s\n" m.oldval m.newval);

  (* Version change *)
  (match patch.Liveset.Patch.version with
   | `Unchanged -> ()
   | `Modified _v -> Printf.printf "LiveSet version changed\n");

  (* Creator change *)
  (match patch.Liveset.Patch.creator with
   | `Unchanged -> ()
   | `Modified m -> Printf.printf "LiveSet creator changed: %s -> %s\n" m.oldval m.newval)

let main ~domain_mgr =
  if Array.length Sys.argv <> 3 then (
    Printf.printf "Usage: %s <file1.als> <file2.als>\n" Sys.argv.(0);
    exit 1
  );

  let file1 = Sys.argv.(1) in
  let file2 = Sys.argv.(2) in

  let liveset1, liveset2 =
    Fiber.pair
      (fun () -> Eio.Domain_manager.run domain_mgr (fun () -> create_liveset_from_file file1))
      (fun () -> Eio.Domain_manager.run domain_mgr (fun () -> create_liveset_from_file file2))
  in

  let liveset_patch = Liveset.diff liveset1 liveset2 in

  (* Display LiveSet-level changes *)
  display_liveset_changes liveset_patch;

  (* Process track changes *)
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
  ) liveset_patch.Liveset.Patch.tracks

let () =
  Printexc.record_backtrace true;
  Eio_main.run @@ fun env ->
    main ~domain_mgr:(Eio.Stdenv.domain_mgr env)
